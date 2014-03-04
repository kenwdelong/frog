unit FrogAlgo;

interface

uses
	FrogObj, Func1D, Retrieval, FrogTrace, NLO, StrategyMgr, Classes, Grids, Forms,
  EFieldCals, OutputCals, Results, Exper, AllCals, RunTypeU;

type
	TFrogAlgo = class(TFrogObject)
  	private
      mBestError: double;
      mE, mEk: TEField;
      mFrogI, mFrogIk: TFrogTrace;
      EBest: TEField;
      mNowInPause: Boolean;
      mTerminated: Boolean;
      mAlgorithmActive: Boolean;  // just a safety measure
      mRunning: Boolean;
      mResultList: TResults;
      mForceStrategyChange: boolean;
      mCurrentError: double;
      mErrorMessage: string;
      mIterationNumber: integer;
      mOnIterationComplete: TNotifyEvent;
      mOnStrategyChange: TNotifyEvent;
      function GetOutputCals: TOutputCals;
      property OutputCals: TOutputCals read GetOutputCals;
      procedure InitLog(pNLO: TNLOInteraction);
      procedure LogResults;
      //procedure PrintMemoryStatistics;
    protected
      mAllCals: TAllCals;
      mLog: TStringList;
      mNLOInteraction: TNLOInteraction;
      mRetrieval: TRetrieval;
      mStrategyManager: TStrategyManager;
      procedure InitRetrieval; virtual;
      procedure AbortFrog;
      procedure InitExperimental; virtual;
      procedure SynchronizeUnits; virtual;
    public
      procedure SaveResults;
      procedure RunAlgorithm;
      procedure Pause;
      procedure Resume;
      procedure Terminate;
      procedure ClearAlgorithm;
      procedure ChangeStrategy;
      procedure InitAlgorithm;
      function PlotInitialField: boolean;
      function PixelUnits: boolean;
      function CalculateOriginalTB: TResults;
      function CalculateRetrievedTB: TResults;
      function IsExperimental: boolean;
      function StrategyName: string;
      property Paused: Boolean read mNowInPause;
      property Active: Boolean read mAlgorithmActive;
      property Running: Boolean read mRunning;
      property BestField: TEField read EBest;
      property E: TEField read mE;
      property Ek: TEField read mEk;
      property FrogI: TFrogTrace read mFrogI;
      property FrogIk: TFrogTrace read mFrogIk;
      property CurrentError: double read mCurrentError;
      property BestError: double read mBestError;
      property ResultList: TResults read mResultList;
      property ErrorMessage: string read mErrorMessage;
      property IterationNumber: integer read mIterationNumber;
      property OnIterationComplete: TNotifyEvent read mOnIterationComplete write mOnIterationComplete;
      property OnStrategyChange: TNotifyEvent read mOnStrategyChange write mOnStrategyChange;
      constructor Create(pAllCals: TAllCals);
      destructor Destroy; override;
  end;

implementation

uses
	SysUtils, HistoryForm, Dialogs, TimeFreq;

constructor TFrogAlgo.Create(pAllCals: TAllCals);
begin
	inherited Create;

  mAllCals := pAllCals;
  mAlgorithmActive := False;
  mRunning := False;
  mForceStrategyChange := False;
  EBest := nil;
end;

destructor TFrogAlgo.Destroy;
begin
  if mAlgorithmActive then ClearAlgorithm;
  EBest.Free;
  EBest := nil;
  inherited Destroy;
end;

procedure TFrogAlgo.InitAlgorithm;
var
  N, i: integer;
begin

  mNLOInteraction := mAllCals.AlgoCals.NLOClass.Create;
  mStrategyManager := TStrategyManager.Create(mAllCals.StrategyCals);
  N := mAllCals.AlgoCals.N;

  // save previous frog.dat as frogold.dat
  mLog := TStringList.Create;
  try
    mLog.LoadFromFile(OutputCals.FilePath + '\frog.dat');
    mLog.SaveToFile(OutputCals.FilePath + '\frogold.dat');
  except
  end;
  mLog.Clear;

  // Fields
  mE := TEField.Create(N);
  mEk := TEField.Create(N);
  EBest.Free;
  EBest := TEField.Create(N);

  // Construct the Frog Traces
  mFrogI := TOriginalFrogTrace.Create(N);
  mFrogIk := TRetrievedFrogTrace.Create(N);

  // Init the first guess (always done!)
  mAllCals.FirstGuessCals.GenerateE(Ek);

  // Get the original Frog trace.
  InitLog(mNLOInteraction);
  mLog.Add('*********** Test Field. ************');
  // For the time being, I'm going to leave this as a case statement, because
  // so much of the info needed is here.  We could use a callback pattern, and
  // divide the cases into procedures, but we have circular dependencies.
  case mAllCals.AlgoCals.RunTypeEnum of
    rteTheory:
      begin
        mAllCals.InitialFieldCals.GenerateE(E);
        mAllCals.InitialFieldCals.InitLog(mLog);
      end;
    rteExperimental:
      begin
        try
          InitExperimental;
        except on e:Exception do
        begin  // This will activate if there are errors reading data from file
					// This exception is re-raised and the message is printed in TFrogAlgoMgr.DoFrog.
          //ShowMessage(e.Message);
          //for i := 0 to mLog.Count - 1 do
          //  ShowMessage(mLog.Strings[i]);
          AbortFrog;
          raise;
        end;
        end;
      end;
    rteReadIn:
      begin
        try
          mAllCals.ReadIn.LoadField(E);
          Ek.SetMyUnits(E.DelT, E.Lam0);
          mAllCals.ReadIn.InitLog(mLog);
        except on e:Exception do
        begin
          ShowMessage(e.Message);
          AbortFrog;
          raise;
        end;
        end;
      end;
  end;

  InitRetrieval;
  SynchronizeUnits;
  if PlotInitialField then
    mRetrieval.MakeInitialFrogTrace(E);

  mCurrentError := mRetrieval.CurrentError;
  mAlgorithmActive := True;
end;

procedure TFrogAlgo.InitExperimental;
begin
  mAllCals.Exper.LoadOriginalFrogTrace(FrogI, mNLOInteraction);
  // Use dummy E (no E for exper) to calibrate the Frog traces
      // E is a property referring to mE.
  E.SetMyUnits(mAllCals.Exper.ModDelT, mAllCals.Exper.ModLam0/mNLOInteraction.SpectrumMultiplier);
  mFrogI.SynchronizeUnits(E);
  mFrogIk.SynchronizeUnits(E);
  mAllCals.Exper.InitLog(mLog);
  mAllCals.MargCals.RunMarginals(FrogI, mNLOInteraction, OutputCals, mLog);
end;

procedure TFrogAlgo.InitRetrieval;
begin
  mRetrieval := TRetrieval.Create(mNLOInteraction, FrogI, FrogIk, Ek);

end;

procedure TFrogAlgo.SynchronizeUnits;
var
  frogWavl: double;
  timeFreq: TTimeFreqCalibration;
begin
  // For experimental, the E and Ek units are set according to the Frog data
  // otherwise, set the frog trace units according to the e fields
  case mAllCals.AlgoCals.RunTypeEnum of
    rteTheory, rteReadIn:
      begin
        frogWavl := Ek.Lam0/mNLOInteraction.SpectrumMultiplier;
        timeFreq := TTimeFreqCalibration.CreateFromDelT(Ek.DelT, frogWavl, Ek.N);
        FrogI.SynchronizeUnitsFromTF(timeFreq);
        FrogIk.SynchronizeUnitsFromTF(timeFreq);
        timeFreq.Free;
      end;
    rteExperimental:
      begin
        mE.SetMyUnits(FrogI.DelT, FrogI.Lam0*mNLOInteraction.SpectrumMultiplier);
        mEk.SetMyUnits(FrogI.DelT, FrogI.Lam0*mNLOInteraction.SpectrumMultiplier);
      end;
  end;
end;

function TFrogAlgo.PlotInitialField: boolean;
begin
  PlotInitialField := (mAllCals.AlgoCals.RunType.HasInitialField);
end;

function TFrogAlgo.PixelUnits: boolean;
begin
  PixelUnits := (mAllCals.AlgoCals.RunTypeEnum = rteTheory) and (mAllCals.InitialFieldCals.Units = unPixel);
end;

function TFrogAlgo.IsExperimental: boolean;
begin
  IsExperimental := (mAllCals.AlgoCals.RunTypeEnum = rteExperimental);
end;

function TFrogAlgo.StrategyName: string;
begin
  if not (mStrategyManager = nil) then
    StrategyName := mStrategyManager.CurrentStrategy.Name
  else
    StrategyName := 'No current Strategy active.';
end;

procedure TFrogAlgo.ClearAlgorithm;
begin
  mRetrieval.Free;
  mRetrieval := nil;

	FrogIk.Free;
  mFrogIk := nil;
  FrogI.Free;
  mFrogI := nil;

  Ek.Free;
  mEk := nil;

  E.Free;
  mE := nil;

  // The log is now owned by frmHistory, it will dispose of it when it's done
  //   God I wish we had garbage collection.
  //mLog.Free;
  //mLog := nil;
  mStrategyManager.Free;
  mStrategyManager := nil;
  mNLOInteraction.Free;
  mNLOInteraction := nil;

  mAlgorithmActive := False;
end;


procedure TFrogAlgo.RunAlgorithm;
var
  startTime, stopTime: TDateTime;
  //frmTest: TfrmFieldGraph;
begin
  mRunning := True;
	mTerminated := False;
  mNowInPause := False;
  mBestError := 1e20;
  mRetrieval.CurrentStrategy := mStrategyManager.InitialStrategy;
  mLog.Add(' ');
  mLog.Add('--- ');
  mLog.Add('Central wavelength of retrieved field is ' +  FloatToStrF(Ek.Lam0, ffGeneral, 4, 0) + ' nm.');
  mLog.Add(' ');
  mLog.Add(mRetrieval.CurrentStrategy.Name);
  //startTime := Now;
  //frmTest := TfrmFieldGraph.Create(Application);

  while not mTerminated do
  begin
  	try
  		mRetrieval.DoOneIteration;
    except
      Ek.CopyFrom(EBest);
    	Ek.Perturb(0.05);
      mRetrieval.InitForRetrieval;
      mLog.add('Error.');
      Continue;
    end;

    mCurrentError := mRetrieval.CurrentError;
    mIterationNumber := mRetrieval.K;
    mErrorMessage := IntToStr(mIterationNumber) + '   ' + FloatToStrF(mCurrentError, ffGeneral, 5, 5);
    mLog.Add(mErrorMessage);
    //PrintMemoryStatistics;

    if CurrentError < mBestError then
    begin
    	mBestError := CurrentError;
      EBest.CopyFrom(Ek);
    end;

    mStrategyManager.UpdateStrategy(mCurrentError, mBestError);

    if Assigned(mOnIterationComplete) then mOnIterationComplete(Self);

    if mForceStrategyChange then
    begin
      mStrategyManager.ChangeStrategy;
      mForceStrategyChange := False;
    end;

    if mStrategyManager.StrategyChanged then
    begin
    	mStrategyManager.NewStartField(Ek, Ebest);
      mRetrieval.InitForRetrieval;
    	mRetrieval.CurrentStrategy := mStrategyManager.CurrentStrategy;
      mLog.Add(' ');
      mLog.Add(mRetrieval.CurrentStrategy.Name);
      if Assigned(mOnStrategyChange) then mOnStrategyChange(Self);
    end;

    Application.ProcessMessages;
    while (mNowInPause and not mTerminated) do Application.ProcessMessages;
    //if mRetrieval.k > 100 then mTerminated := True;
  end;
  //stopTime := Now;
  //frmControlAlgo.memFrogError.Lines.Add(FloatToStr(24*3600*(stopTime - startTime)));
  //mLog.Add(FloatToStr(24*3600*(stopTime - startTime)));  // It's this one
  //frmTest.Free;

  mRetrieval.CalcBestTrace(EBest);
  // This next line makes sure that the best fields are displayed on the screen
  mEk.CopyFrom(EBest);
  mLog.Add(' '); mLog.Add(' ');
  mLog.Add('The minimum FROG error was ' + FloatToStrF(mBestError, ffGeneral, 5, 5));
  CalculateRetrievedTB;
  LogResults;

  //UpdateDBLogs;
  frmHistory.AddItem(mNLOInteraction.Name, mAllCals.Exper.FileName, EBest.N,
                        EBest.FWHM, mBestError, mLog);

  mRunning := False;
end;


// Called from AlgoMgr.SaveResults
procedure TFrogAlgo.SaveResults;
var
  SpectrumIsPixelUnits: Boolean;
begin
  // Preconditions: mLog is filled with the final data (ShowResults is called before SaveResults).

  SpectrumIsPixelUnits := (mAllCals.AlgoCals.RunTypeEnum = rteTheory) and (mAllCals.InitialFieldCals.Units = unPixel);
  mLog.SaveToFile(OutputCals.FilePath + '\frog.dat');
  OutputCals.SaveEField(EBest, 'Ek.dat');
  OutputCals.SaveSpectrum(EBest, 'Speck.dat', SpectrumIsPixelUnits);
  OutputCals.SaveAutocorrelation(EBest, 'Autok.dat');

  // Save both frog traces
  OutputCals.SaveFrogTrace(FrogI,EBest, 'a.dat', SpectrumIsPixelUnits, mAllCals.AlgoCals.NLOType);
  OutputCals.SaveFrogTrace(FrogIk,EBest, 'arecon.dat', SpectrumIsPixelUnits, mAllCals.AlgoCals.NLOType);

  // Save the marginals
  OutputCals.SaveMarginals(FrogI, FrogIk, EBest, 'marg.dat', SpectrumIsPixelUnits, mAllCals.AlgoCals.NLOType);

  if mAllCals.AlgoCals.RunType.HasInitialField then
  begin
    OutputCals.SaveEField(E, 'E.dat');
    OutputCals.SaveSpectrum(E, 'Spec.dat', SpectrumIsPixelUnits);
    OutputCals.SaveAutocorrelation(E, 'Auto.dat');
  end;
end;

procedure TFrogAlgo.Pause;
begin
  mNowInPause := True;
end;

procedure TFrogAlgo.Resume;
begin
  mNowInPause := False;
end;

procedure TFrogAlgo.Terminate;
begin
  mTerminated := True;
end;

procedure TFrogAlgo.ChangeStrategy;
begin
  mForceStrategyChange := True;
end;

procedure TFrogAlgo.InitLog(pNLO: TNLOInteraction);
begin
  mLog.Add(pNLO.Name + ' FROG');
  mLog.Add('Grid size is ' + IntToStr(Ek.N));
  mLog.Add(' ');
end;

procedure TFrogAlgo.LogResults;
begin
  if mAllCals.AlgoCals.RunType.HasInitialField then
  begin
    mLog.Add(' ');
    mLog.Add('**************** Original Field ******************');
    mLog.Add('Temporal FWHM: ' + FloatToStrF(mResultList.OrigTW, ffGeneral, 5, 5) + ' fs.');
    mLog.Add('Spectral FWHM: ' + FloatToStrF(mResultList.OrigSW, ffGeneral, 5, 5) + ' nm.');
    mLog.Add('Autocorrelation FWHM: ' + FloatToStrF(mResultList.OrigAW, ffGeneral, 5, 5) + ' fs.');
    mLog.Add('Time-Bandwidth Product - FWHM: ' + FloatToStrF(mResultList.OrigFTB, ffGeneral, 5, 5));
    mLog.Add('Time-Bandwidth Product - RMS: ' + FloatToStrF(mResultList.OrigRTB, ffGeneral, 5, 5));
    mLog.Add('Time-Bandwidth Product - Temporal Laplacian: ' + FloatToStrF(mResultList.OrigTL, ffGeneral, 5, 5));
    mLog.Add('Time-Bandwidth Product - Spectral Laplacian: ' + FloatToStrF(mResultList.OrigSL, ffGeneral, 5, 5));
  end;

  mLog.Add(' ');
  mLog.Add('**************** Retrieved Field ******************');
  mLog.Add('Temporal FWHM: ' + FloatToStrF(mResultList.BestTW, ffGeneral, 5, 5) + ' fs.');
  mLog.Add('Spectral FWHM: ' + FloatToStrF(mResultList.BestSW, ffGeneral, 5, 5) + ' nm.');
  mLog.Add('Autocorrelation FWHM: ' + FloatToStrF(mResultList.BestAW, ffGeneral, 5, 5) + ' fs.');
  mLog.Add('Time-Bandwidth Product - FWHM: ' + FloatToStrF(mResultList.BestFTB, ffGeneral, 5, 5));
  mLog.Add('Time-Bandwidth Product - RMS: ' + FloatToStrF(mResultList.BestRTB, ffGeneral, 5, 5));
  mLog.Add('Time-Bandwidth Product - Temporal Laplacian: ' + FloatToStrF(mResultList.BestTL, ffGeneral, 5, 5));
  mLog.Add('Time-Bandwidth Product - Spectral Laplacian: ' + FloatToStrF(mResultList.BestSL, ffGeneral, 5, 5));

end;

function TFrogAlgo.CalculateOriginalTB: TResults;
begin
    E.CalculateTimeBandwidthProducts;

    mResultList.OrigTW := E.TemporalFWHM;
    mResultList.OrigSW := E.SpectralFWHM;
    mResultList.OrigAW := E.AutocorrelationFWHM;
    mResultList.OrigFTB := E.FWHM_TBP;
    mResultList.OrigRTB := E.RMS_TBP;
    mResultList.OrigTL := E.TemporalLaplacianTBP;
    mResultList.OrigSL := E.SpectralLaplacianTBP;

    CalculateOriginalTB := mResultList;
end;

function TFrogAlgo.CalculateRetrievedTB: TResults;
begin
  EBest.CalculateTimeBandwidthProducts;

  mResultList.BestTW := EBest.TemporalFWHM;
  mResultList.BestSW := EBest.SpectralFWHM;
  mResultList.BestAW := EBest.AutocorrelationFWHM;
  mResultList.BestFTB := EBest.FWHM_TBP;
  mResultList.BestRTB := EBest.RMS_TBP;
  mResultList.BestTL := EBest.TemporalLaplacianTBP;
  mResultList.BestSL := EBest.SpectralLaplacianTBP;

  CalculateRetrievedTB := mResultList;
end;

procedure TFrogAlgo.AbortFrog;
begin
  ClearAlgorithm;
end;

{procedure TFrogAlgo.PrintMemoryStatistics;
var
	stats: THeapStatus;
begin
	stats := GetHeapStatus;
  mLog.Add('  TotalAddrSpace ' + IntToStr(stats.TotalAddrSpace));
  mLog.Add('  TotalUncommitted ' + IntToStr(stats.TotalUncommitted));
  mLog.Add('  TotalCommitted ' + IntToStr(stats.TotalCommitted));
  mLog.Add('               TotalAllocated ' + IntToStr(stats.TotalAllocated));
  mLog.Add('  TotalFree ' + IntToStr(stats.TotalFree));
end;
}

function TFrogAlgo.GetOutputCals: TOutputCals;
begin
     GetOutputCals := mAllCals.OutputCals;
end;

end.
