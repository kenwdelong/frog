unit TadGrapher;

interface

uses FrogObj, Func1D, TadMgr;

type
  TTadpoleGrapher = class(TFrogObject)
    private
      mTadMgr: TTadpoleAlgorithmManager;
      mWindowsOpen: Boolean;
    public
      procedure OpenWindows;
      procedure CloseWindows;
      procedure GraphField(pE: TEField);
      procedure GraphTadpoleSpectrum(pTadSpec: TSpectrum; pERef: TEField);
      procedure GraphFFTData(pFFTData: TEField);
      procedure RunAlgo(pCutoff, pFloor: double);
      function HasTestSpec: Boolean;
      procedure Abort;
      property WindowsOpen: Boolean read mWindowsOpen write mWindowsOpen;
      constructor Create(pTadMgr: TTadpoleAlgorithmManager);
  end;

var
  gTadGrapher: TTadpoleGrapher;

const
  SPACING = 5;
  LEFT_SIDE = 10;
  FORM_HEIGHT = 210;
  FORM_WIDTH = 344;

implementation

uses TadGraph, Forms, FieldGraph, FFTGraph, TadResults, WindowMgr;

constructor TTadpoleGrapher.Create(pTadMgr: TTadpoleAlgorithmManager);
begin
  mWindowsOpen := False;
  mTadMgr := pTadMgr;
end;

procedure TTadpoleGrapher.OpenWindows;
begin
  if mWindowsOpen then Exit;

  frmTadGraph := TfrmTadGraph.Create(Application);
  frmTadGraph.Left := LEFT_SIDE;
  frmTadGraph.Top := 5;
  frmTadGraph.Height := FORM_HEIGHT;
  frmTadGraph.Width := FORM_WIDTH;
  WindowManager.InitializeThisWindow(frmTadGraph);

  frmTadResults := TfrmTadResults.CreateWithGrapher(Application, Self);
  frmTadResults.Left := frmTadGraph.Left + frmTadGraph.Width + SPACING;
  frmTadResults.Top := frmTadGraph.Top;
  WindowManager.InitializeThisWindow(frmTadResults);

  frmFFTGraph := TfrmFFTGraph.CreateWithGrapher(Application, Self);
  frmFFTGraph.Top := frmTadGraph.Top + frmTadGraph.Height + SPACING;
  frmFFTGraph.Left := LEFT_SIDE;
  frmFFTGraph.Height := FORM_HEIGHT;
  frmFFTGraph.Width := FORM_WIDTH*2;
  WindowManager.InitializeThisWindow(frmFFTGraph);

  frmEGraph := TfrmFieldGraph.Create(Application);
  frmEGraph.Caption := 'Temporal Field';
  frmEGraph.Top := frmFFTGraph.Top + frmFFTGraph.Height + SPACING;
  frmEGraph.Left := LEFT_SIDE;
  frmEGraph.Height := FORM_HEIGHT;
  frmEGraph.Width := FORM_WIDTH;
  WindowManager.InitializeThisWindow(frmEGraph);

  frmSpecGraph := TfrmFieldGraph.Create(Application);
  frmSpecGraph.Caption := 'Spectral Field';
  frmSpecGraph.Chart1.LeftAxis.AutomaticMaximum := True;
  frmSpecGraph.Top := frmEGraph.Top;
  frmSpecGraph.Left := frmEGraph.Left + frmEGraph.Width + SPACING;
  frmSpecGraph.Height := FORM_HEIGHT;
  frmSpecGraph.Width := FORM_WIDTH;
  frmSpecGraph.Chart1.BottomAxis.Title.Caption := 'Wavelength (nm)';
  WindowManager.InitializeThisWindow(frmSpecGraph);

  mWindowsOpen := True;
end;

procedure TTadpoleGrapher.CloseWindows;
begin
  WindowManager.SaveAsDefaults(frmTadGraph);
  frmTadGraph.Free;
  frmTadGraph := nil;

  WindowManager.SaveAsDefaults(frmFFTGraph);
  frmFFTGraph.Free;
  frmFFTGraph := nil;

  WindowManager.SaveAsDefaults(frmEGraph);
  frmEGraph.Free;
  frmEGraph := nil;

  WindowManager.SaveAsDefaults(frmSpecGraph);
  frmSpecGraph.Free;
  frmSpecGraph := nil;

  WindowManager.SaveAsDefaults(frmTadResults);
  //frmTadResults.Free;
  //frmTadResults := nil;
  mWindowsOpen := False;

  // Comment here for demo version - Also in AlgoMgr line 193
  mTadMgr.SaveResults;
  mTadMgr.Deactivate;
end;

procedure TTadpoleGrapher.Abort;
begin
  WindowManager.SaveAsDefaults(frmTadResults);
  frmTadResults.Free;
  frmTadResults := nil;
  CloseWindows;
end;

procedure TTadpoleGrapher.GraphField(pE: TEField);
var
  TadResults: TTadResults;
begin
  // Pre-cond: pE is in dtFREQ
  frmSpecGraph.Clear;
  frmSpecGraph.GraphSpectrum(pE, nil, true);
  pE.InverseTransform;
  frmEGraph.Clear;
  frmEGraph.GraphEField(pE, nil, true);

  pE.CalculateTimeBandwidthProducts;
  TadResults.TempWidth := pE.TemporalFWHM;
  TadResults.SpecWidth := pE.SpectralFWHM;
  TadResults.AutoWidth := pE.AutocorrelationFWHM;
  TadResults.FWHM := pE.FWHM_TBP;
  TadResults.RMS := pE.RMS_TBP;
  TadResults.TL := pE.SpectralLaplacianTBP;
  TadResults.SL := pE.TemporalLaplacianTBP;
  frmTadResults.DisplayResults(TadResults);
end;

procedure TTadpoleGrapher.RunAlgo(pCutoff, pFloor: double);
begin
  // Dispatching method for events from the FFT form
  mTadMgr.RunAlgo(pCutoff, pFloor);
end;

procedure TTadpoleGrapher.GraphTadpoleSpectrum(pTadSpec: TSpectrum; pERef: TEField);
begin
  frmTadGraph.GraphTadSpectrum(pTadSpec, pERef);
end;

procedure TTadpoleGrapher.GraphFFTData(pFFTData: TEField);
begin
  frmFFTGraph.GraphFFTData(pFFTData);
end;

function TTadpoleGrapher.HasTestSpec: Boolean;
begin
  HasTestSpec := mTadMgr.HasTestSpec;
end;

end.
