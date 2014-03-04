unit TadMgr;

interface

uses FrogObj, Func1D, ReadIn, OutputCals, Numerics, SysUtils;

type
  TTadCals = record
    mTadpoleCals: TReadIn;
    mReferenceCals: TReadIn;
    mOutputCals: TOutputCals;
    mTestSpecCals: TReadIn;
  end;

  TTadpoleAlgorithmManager = class(TFrogObject)
    private
      mTadCals: TTadCals;
      mTadSpec: TSpectrum;
      mFFTData: TEField;
      mERef: TEField;
      mTestSpec: TSpectrum;
      mResultField: TEField;
      mCutoff: double;
      mFloor: double;
      mTadpoleActive: Boolean;
      procedure SaveLogFile;
      procedure GridFFTData;
      procedure GetRefData(pDest: TEField);
      procedure GetTestSpec;
    public
      property TadpoleActive: Boolean read mTadpoleActive;
      procedure RunAlgo(pCutoff, pFloor: double);
      procedure InitAlgo;
      procedure ResetAlgo;
      procedure SaveResults;
      procedure Deactivate;
      function HasTestSpec: Boolean;
      procedure Abort;
      constructor Create(pTadCals: TTadCals);
      destructor Destroy; override;
  end;

implementation

uses TadGrapher, Dialogs, Classes, TestPlot;

constructor TTadpoleAlgorithmManager.Create(pTadCals: TTadCals);
begin
  inherited Create;
  mTadCals := pTadCals;
  gTadGrapher := TTadpoleGrapher.Create(Self);
  mResultField := nil;
  mTadpoleActive := False;
end;

destructor TTadpoleAlgorithmManager.Destroy;
begin
  mTadSpec.Free;
  mFFTData.Free;
  gTadGrapher.Free;
  gTadGrapher := nil;
  inherited Destroy;
end;

// This one needs to read in the data and get it gridded
procedure TTadpoleAlgorithmManager.InitAlgo;
var
  N: integer;
begin
  try
    mTadSpec := mTadCals.mTadpoleCals.LoadSpectrum;  // This call Creates mTadSpec!!
  except on e:Exception do
  begin
    ShowMessage(e.Message);
    Exit;
  end;
  end;
  gTadGrapher.OpenWindows;
  mTadpoleActive := True;

  // Next step - put the Tadpole data into the FFT structure (nonlinear scaling?)
  N := 2;
  while N < mTadSpec.N do
    N := N*2;
  mFFTData := TEField.Create(N);
  GridFFTData;
  mFFTData.InverseTransform;       // Now it's in time domain
  mFFTData.Norm;
  mFFTData.SetIntensityAndPhase;

  // Get the Reference Field
  mERef := TEField.Create(mFFTData.N);
  mERef.SetMyUnits(mFFTData.DelT, mFFTData.Lam0);
  GetRefData(mERef);
  // mERef is now in the FREQ domain!
  // mFFTData is in dtTIME!

  // If we have a Test Spectrum, go get it!
  if not mTadCals.mTestSpecCals.IsNull then
  begin
    mTestSpec := TSpectrum.Create(mFFTData.N);
    mTestSpec.SetMyUnits(mFFTData.DelT, mFFTData.Lam0);
    GetTestSpec;
  end;

  // Then graph the FFT
  gTadGrapher.GraphTadpoleSpectrum(mTadSpec, mERef);
  gTadGrapher.GraphFFTData(mFFTData);

  // Then institute the algorithm
  RunAlgo(0.1, 1e-4);
  // Then figure out about cleanup of the objects
  // Rock!

end;

// Get the Tadpole field as indicated by the current parms.
procedure TTadpoleAlgorithmManager.RunAlgo(pCutoff, pFloor: double);
var
  cutoff, i, sign: integer;
  inten, phase: double;
  frmTestPlot: TFrmTestPlot;
begin
  // Save current vales
  mCutoff := pCutoff;
  mFloor := pFloor;

  if (mResultField <> nil) and (mResultField.N <> mFFTData.N) then
  begin
    mResultField.Free;
    mResultField := nil;
  end;
  if mResultField = nil then
    mResultField := TEField.Create(mFFTData.N);
  mResultField.CopyFrom(mFFTData);
  // Because mFFTData is in dtTIME, so is mResultField

  // Find the cutoff pixel
  cutoff := Round(pCutoff/mFFTData.DelT) + (mFFTData.N div 2);
  for i := 0 to mResultField.N - 1 do
  begin
    if (pCutoff/mFFTData.DelT) < 0 then if i > cutoff then
    begin
      mResultField.Re^[i] := 0;
      mResultField.Im^[i] := 0;
      sign := 1;
    end;
    //if pCutoff > 0 then if i < cutoff then  // fixed 9/5/00
    if (pCutoff/mFFTData.DelT) > 0 then if i < cutoff then
    begin
      mResultField.Re^[i] := 0;
      mResultField.Im^[i] := 0;
      sign := -1;
    end;
  end;

  //frmTestPlot := TFrmTestPlot.Create;
  //frmTestPlot.SetTitle('Afer transform');
  //frmTestPlot.PlotRealAndIm(mResultField);

  mResultField.Center;
  mResultField.Transform;
  mResultField.SetIntensityAndPhase;

  // Now mResultField is dtFREQ, so is mERef.
  if pFloor >= 1.0 then pFloor := 0.99;
  for i := 0 to mResultField.N - 1 do
  begin
    if mTadCals.mTestSpecCals.IsNull then
    begin
      if (mERef.Intensity^[i]) > sqrt(pFloor) then
        // The signal is in the real part of mResultField, so we need a sqrt
        //  on the top to get the signal.  Then mERef is a normal field, and
        //  we need to divide by the sqrt of the spectrum.  Thus, 2 sqrts.
        inten := sqrt(mResultField.Intensity^[i]/mERef.Intensity^[i])
      else
        inten := 0;
    end
    else
      inten := sqrt(mTestSpec.Intensity^[i]);
    phase := mERef.Phase^[i] - sign*mResultField.Phase^[i];
    mResultField.Re^[i] := inten*cos(phase);
    mResultField.Im^[i] := inten*sin(-phase);
  end;

  gTadGrapher.GraphField(mResultField);
end;

procedure TTadpoleAlgorithmManager.ResetAlgo;
begin
  mTadSpec.Free;
  mTadSpec := nil;
  mFFTData.Free;
  mFFTData := nil;
  mERef.Free;
  mERef := nil;
  mResultField.Free;
  mResultField := nil;
end;

// Called by TadGrapher
procedure TTadpoleAlgorithmManager.SaveResults;
begin
  if mResultField = nil then Exit;
  // Save Results
  mTadCals.mOutputCals.SaveEField(mResultField, 'TadE.dat');
  mTadCals.mOutputCals.SaveSpectrum(mResultField, 'TadSpec.dat', False);
  SaveLogFile;

  // Free the objects
  ResetAlgo;
  //mTadpoleActive := False;
end;

procedure TTadpoleAlgorithmManager.Deactivate;
begin
	mTadpoleActive := False;
end;

procedure TTadpoleAlgorithmManager.GridFFTData;
var
  Lam0, delT: double;
  w, N: integer;
  tempSpec: TSpectrum;
begin
  // Set up mFFTData's calibrations
  // The DelLam must be the same
  Lam0 := mTadSpec.Lam0;
  delT := -Lam0*Lam0/(300.0*mFFTData.N*mTadSpec.DelLam);
  mFFTData.SetMyUnits(delT, Lam0);
  N := mFFTData.N;
  tempSpec := TSpectrum.Create(N);
  try
    tempSpec.SetMyUnits(delT, Lam0);
    mTadCals.mTadpoleCals.GridSpectrum(mTadSpec, tempSpec);
    for w := 0 to N - 1 do
    begin
      mFFTData.Re^[w] := tempSpec.Intensity^[w];
      mFFTData.Im^[w] := 0.0;
    end;
  finally
    tempSpec.Free;
  end;

end;

procedure TTadpoleAlgorithmManager.GetRefData(pDest: TEField);
var
  tERef: TEField;
begin
  if mTadCals.mReferenceCals.ReferencePulseSource = rsReadIn then
  begin
    tERef := TEField.Create(pDest.N);
    try
      mTadCals.mReferenceCals.LoadField(tERef);
      tERef.Transform;
      pDest.Transform;
      mTadCals.mReferenceCals.GridEField(tERef, pDest);
    finally
      tERef.Free;
    end;
  end
  else if mTadCals.mReferenceCals.ReferencePulseSource = rsFrog then
  begin
    tERef := mTadCals.mReferenceCals.ReferencePulse;
    tERef.Transform;
    pDest.Transform;
    mTadCals.mReferenceCals.GridEField(tERef, pDest);
    tERef.InverseTransform;  // We may need to use this field again!
  end;

  pDest.Norm;
  pDest.SetIntensityAndPhase;
end;

procedure TTadpoleAlgorithmManager.GetTestSpec;
var
  tempSpec: TSpectrum;
begin
  tempSpec := mTadCals.mTestSpecCals.LoadSpectrum;
  try
    mTadCals.mTestSpecCals.GridSpectrum(tempSpec, mTestSpec);
  finally
    tempSpec.Free;
  end;
  mTestSpec.Norm;
end;

function TTadpoleAlgorithmManager.HasTestSpec: Boolean;
begin
  HasTestSpec := not mTadCals.MTestSpecCals.IsNull;
end;

procedure TTadpoleAlgorithmManager.Abort;
begin
  gTadGrapher.Abort;
  //gTadGrapher.CloseWindows;
end;

procedure TTadpoleAlgorithmManager.SaveLogFile;
var
  log: TStringList;
begin
  log := TStringList.Create;
  log.Add('TADPOLE Data in ' + mTadCals.mTadpoleCals.ReadInFile);
  if mTadCals.mReferenceCals.ReferencePulseSource = rsFrog then
    log.Add('Reference Data comes from a FROG trace')
  else if mTadCals.mReferenceCals.ReferencePulseSource = rsReadIn then
    log.Add('Reference Data in ' + mTadCals.mReferenceCals.ReadInFile);
  if not (mTadCals.mTestSpecCals.IsNull) then
    log.Add('Using test pulse spectrum in ' + mTadCals.mTestSpecCals.ReadInFile);

  log.Add(' ');
  log.Add('Algorithm cutoff: ' + FloatToStr(mCutoff));
  log.Add('Algorithm floor: ' + FloatToStr(mFloor));

  log.Add(' ' );
  log.Add('***** Retrieved Field Parameter *****');
  Log.Add('Temporal FWHM: ' + FloatToStrF(mResultField.TemporalFWHM, ffGeneral, 5, 5) + ' fs.');
  Log.Add('Spectral FWHM: ' + FloatToStrF(mResultField.SpectralFWHM, ffGeneral, 5, 5) + ' nm.');
  Log.Add('Autocorrelation FWHM: ' + FloatToStrF(mResultField.AutocorrelationFWHM, ffGeneral, 5, 5) + ' fs.');
  Log.Add('Time-Bandwidth Product - FWHM: ' + FloatToStrF(mResultField.FWHM_TBP, ffGeneral, 5, 5));
  Log.Add('Time-Bandwidth Product - RMS: ' + FloatToStrF(mResultField.RMS_TBP, ffGeneral, 5, 5));
  Log.Add('Time-Bandwidth Product - Temporal Laplacian: ' + FloatToStrF(mResultField.TemporalLaplacianTBP, ffGeneral, 5, 5));
  Log.Add('Time-Bandwidth Product - Spectral Laplacian: ' + FloatToStrF(mResultField.SpectralLaplacianTBP, ffGeneral, 5, 5));

  log.SaveToFile(mTadCals.mOutputCals.FilePath + '\Tadpole.dat');
  log.Free;
end;

end.
