unit AlgoMgr;

interface

uses FrogObj, Grapher, FrogAlgo, AlgoControl, Func1D, AllCals;

type
  TFrogAlgoMgr = class(TFrogObject)
    private
      mGrapher: TGrapher;
      procedure CloseWindows;
      procedure BeginAlgorithm(Sender: TObject);
      procedure StopAlgorithm(Sender: TObject);
      procedure QuitAlgorithm(Sender: TObject);
      procedure PauseAlgorithm(Sender: TObject);
      procedure ResumeAlgorithm(Sender: TObject);
      procedure ChangeStrategy(Sender: TObject);
      procedure IterationComplete(Sender: TObject);
      procedure StrategyChanged(Sender: TObject);
      function DisplayGraphics: boolean;
      procedure UpdateGraphics(pIter: integer);
      procedure SaveResults;
    protected
      mAllCals: TAllCals;
      mWindowsOpen: Boolean;
      mFrogAlgo: TFrogAlgo;
      procedure InitWindows;
      procedure InitDisplay;
      procedure InitAlgoControlEvents;
      procedure InitAlgoEvents;
    public
      procedure DoFrog;
      function BestField: TEField;
      property WindowsOpen: boolean read mWindowsOpen;
      constructor Create;
      destructor Destroy; override;
  end;

implementation

uses Forms, WindowMgr, AlgoCals, Results, ErrorGraph, CalsForm, SysUtils,
   Controls, Dialogs, QuerySaveForm, EFieldCals;

constructor TFrogAlgoMgr.Create;
begin
     // You don't want to create an AllCals here and pass it to the form,
     // becuase the XFROG forms inherit from this one.  Then every time you
     // created a form, you'd need to create the cals object, and destroy
     // it when you destroy the form.  Instead, the forms live forever and
     // manage the lifecycle of the AllCals objects.  Sigh. 
end;

destructor TFrogAlgoMgr.Destroy;
begin
  inherited Destroy;
end;

procedure TFrogAlgoMgr.DoFrog;
begin
  if mFrogAlgo <> nil then
    if mFrogAlgo.Running then Exit;
  // frmCalibrations is declared in CalsForm, and autocreated
  frmCalibrations.ShowModal;
  if (frmCalibrations.ModalResult = mrCancel) then Exit;
  mAllCals := frmCalibrations.AllCals;

  if mFrogAlgo = nil then
    mFrogAlgo := TFrogAlgo.Create(frmCalibrations.AllCals);
  try
    mFrogAlgo.InitAlgorithm;
  except on e:Exception do
  begin
  	ShowMessage(e.message);
    mFrogAlgo.Free;
    mFrogAlgo := nil;
    Exit;
  end;
  end;

  InitAlgoEvents;
  InitWindows;
  InitDisplay;
end;

// Fill the windows with the proper gobbledy-gook
procedure TFrogAlgoMgr.InitDisplay;
var
  resultList: TResults;
begin
  mGrapher.InitGraphics(mFrogAlgo.E, mFrogAlgo.Ek, mFrogAlgo.frogI,
                        mFrogAlgo.frogIk, mFrogAlgo.PlotInitialField);

  // The Results form
  frmResults.Clear;
  if mFrogAlgo.PlotInitialField then
  begin
    resultList := mFrogAlgo.CalculateOriginalTB;
    frmResults.DisplayOriginal(resultList, mFrogAlgo.PixelUnits);
  end;

  // The XY Graph
  frmErrorGraph.Clear;
  if mFrogAlgo.IsExperimental then
    frmErrorGraph.SetAxisRange(arNarrow)
  else
    frmErrorGraph.SetAxisRange(arWide);

  // The algoctrl form
  frmControlAlgo.Clear;
  frmControlAlgo.StatusMessage(mFrogAlgo.StrategyName);
  frmControlAlgo.SetFocus;  // Doesn't seem to work.
  frmControlAlgo.Add('Initial Error: ' + FloatToStrF(mFrogAlgo.CurrentError, ffGeneral, 5, 5));
end;

procedure TFrogAlgoMgr.InitWindows;
begin
	if mWindowsOpen then Exit;
  mGrapher := TGrapher.Create;
  // This method opens the four main windows.
  mGrapher.InitWindows;

  // Display the results form and algo control!
  frmControlAlgo := TFrmControlAlgo.Create(Application);
  frmControlAlgo.Left := mGrapher.RightHandEdge + 5;
  frmControlAlgo.Top := mGrapher.TopEdge;
  //frmControlAlgo.TheBossIs(Self);
  WindowManager.InitializeThisWindow(frmControlAlgo);
  InitAlgoControlEvents;

  frmResults := TFrmResults.Create(Application);
  frmResults.Top := mGrapher.BottomEdge + 5;
  frmResults.Left := mGrapher.LeftHandEdge;
  WindowManager.InitializeThisWindow(frmResults);

  frmErrorGraph := TfrmErrorGraph.Create(Application);
  frmErrorGraph.Top := frmControlAlgo.Top + frmControlAlgo.Height + 5;
  frmErrorGraph.Left := frmResults.Left + frmResults.Width + 5;
  WindowManager.InitializeThisWindow(frmErrorGraph);

  mWindowsOpen := True;
end;

procedure TFrogAlgoMgr.CloseWindows;
begin

  WindowManager.SaveAsDefaults(frmErrorGraph);
  frmErrorGraph.Free;

  WindowManager.SaveAsDefaults(frmResults);
  frmResults.Free;

  WindowManager.SaveAsDefaults(frmControlAlgo);
  //frmControlAlgo.Free;  // This is causing some problems

  // This method uses WindowMgr and dels the four main windows.
  mGrapher.CleanUp;
  mGrapher.Free;
  mGrapher := nil;

  mWindowsOpen := False;
end;

// *** AlgoControl Window Events
procedure TFrogAlgoMgr.InitAlgoControlEvents;
begin
  frmControlAlgo.OnBegin := BeginAlgorithm;
  frmControlAlgo.OnStop := StopAlgorithm;
  frmControlAlgo.OnQuit := QuitAlgorithm;
  frmControlAlgo.OnPause := PauseAlgorithm;
  frmControlAlgo.OnResume := ResumeAlgorithm;
  frmControlAlgo.OnForceChange := ChangeStrategy;
end;

procedure TFrogAlgoMgr.BeginAlgorithm(Sender: TObject);
begin

	try
  begin
  	if not mFrogAlgo.Active then
  	begin
      mFrogAlgo.InitAlgorithm;
    	InitDisplay;
  	end;
  	mFrogAlgo.RunAlgorithm;
  end;
  except
  	Exit;
	end;

  // RunAlgorithm will return after stop is pressed.

  // Comment here for demo - also in TadGrapher line 112.
  SaveResults;
  UpdateGraphics(1);
  frmControlAlgo.Add('The minimum FROG error was ' + FloatToStrF(mFrogAlgo.BestError, ffGeneral, 5, 5));
  frmResults.DisplayRetrieved(mFrogAlgo.ResultList, mFrogAlgo.BestError,
                          mFrogAlgo.PixelUnits);
  mFrogAlgo.ClearAlgorithm;
end;

procedure TFrogAlgoMgr.SaveResults;
var
	dirPath: string;
  doSave: boolean;
begin
  dirPath := mAllCals.OutputCals.FilePath;
  doSave := True;
  if FileExists(dirPath + '/frog.dat') and not mAllCals.OutputCals.Overwrite then
  begin
	  frmQuerySave := TfrmQuerySave.Create(dirPath);
     frmQuerySave.ShowModal;
	  mAllCals.OutputCals.FilePath := frmQuerySave.NewDir;
     doSave := frmQuerySave.DoSave;
	  frmQuerySave.Free;
     frmQuerySave := nil;
  end;
 	if doSave then
 		mFrogAlgo.SaveResults;
end;

procedure TFrogAlgoMgr.StopAlgorithm(Sender: TObject);
begin
  mFrogAlgo.Terminate;
end;

procedure TFrogAlgoMgr.QuitAlgorithm(Sender: TObject);
begin
  // In the case that the algo was not run, ie the user just hit Quit, the
  //  algo is not cleared.  This should fix it.
  if not (mFrogAlgo = nil) then
  begin
    if mFrogAlgo.Active then mFrogAlgo.ClearAlgorithm;
  end;
  CloseWindows;
end;

procedure TFrogAlgoMgr.PauseAlgorithm(Sender: TObject);
begin
  mFrogAlgo.Pause;
end;

procedure TFrogAlgoMgr.ResumeAlgorithm(Sender: TObject);
begin
  mFrogAlgo.Resume;
end;

procedure TFrogAlgoMgr.ChangeStrategy(Sender: TObject);
begin
  mFrogAlgo.ChangeStrategy;
end;

// *** Algorithm events
procedure TFrogAlgoMgr.InitAlgoEvents;
begin
  mFrogAlgo.OnIterationComplete := IterationComplete;
  mFrogAlgo.OnStrategyChange := StrategyChanged;
end;

procedure TFrogAlgoMgr.IterationComplete(Sender: TObject);
begin
  frmControlAlgo.Add(mFrogAlgo.ErrorMessage);
  frmControlAlgo.SetBestError(mFrogAlgo.BestError);
  frmErrorGraph.AddAPoint(mFrogAlgo.IterationNumber, mFrogAlgo.CurrentError);
  if DisplayGraphics then
    UpdateGraphics(mFrogAlgo.IterationNumber);
end;

procedure TFrogAlgoMgr.StrategyChanged(Sender: TObject);
begin
  frmControlAlgo.StatusMessage(mFrogAlgo.StrategyName);
  frmErrorGraph.AddAMark(mFrogAlgo.IterationNumber, mFrogAlgo.StrategyName);
  UpdateGraphics(1);
end;

function TFrogAlgoMgr.DisplayGraphics: boolean;
begin
  DisplayGraphics := mAllCals.OutputCals.DisplayGraphics;
end;

procedure TFrogAlgoMgr.UpdateGraphics(pIter: integer);
begin
  mGrapher.UpdateGraphics(pIter, mFrogAlgo.E, mFrogAlgo.Ek, mFrogAlgo.FrogI,
                        mFrogAlgo.FrogIk, mFrogAlgo.PlotInitialField);
end;

function TFrogAlgoMgr.BestField: TEField;
begin
  if mFrogAlgo <> nil then
    BestField := mFrogAlgo.BestField
  else
    BestField := nil;
end;

end.
