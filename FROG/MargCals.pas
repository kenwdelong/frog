unit MargCals;

interface

uses FrogObj, ReadIn, NLO, FrogTrace, OutputCals, classes;

type
  TMargCals = class(TFrogObject)
    private
      mForceMarginals: boolean;
    public
      SpecCals: TReadIn;
      AutoCals: TReadIn;
      SHGSpecCals: TReadIn;
      property ForceMarginals: boolean read mForceMarginals write mForceMarginals;
      procedure RunMarginals(pFrogI: TFrogTrace; pNLO: TNLOInteraction; pOutputCals: TOutputCals; pLog: TStrings);
      constructor Create;
      destructor Destroy; override;
  end;


implementation

uses Func1D, MargGraph, Forms, WindowMgr, TestPlot;

constructor TMargCals.Create;
begin
  inherited Create;
  SpecCals := TReadIn.Create;
  AutoCals := TReadIn.Create;
  SHGSpecCals := TReadIn.Create;
  ForceMarginals := False;
end;

destructor TMargCals.Destroy;
begin
  SHGSpecCals.Free;
  AutoCals.Free;
  SpecCals.Free;
end;

procedure TMargCals.RunMarginals(pFrogI: TFrogTrace; pNLO: TNLOInteraction; pOutputCals: TOutputCals; pLog: TStrings);
var
  HaveSpec, HaveAuto, HaveSHGSpec: Boolean;
  xSpectrum, xSHGSpectrum: TSpectrum;
  xAutocorrelation: TAutocorrelation;
  Marginal, FrogMarginal: TMarginal;
  //DelT, Lam0: double;
  //test: TfrmTestPlot;
begin
  xSpectrum := nil;
  xAutocorrelation := nil;
  xSHGSpectrum := nil;
  Marginal := nil;
  frmMargGraph := nil;
  FrogMarginal := nil;

  HaveSpec := (SpecCals.ReadInFile <> SpecCals.NullFile);
  HaveAuto := (AutoCals.ReadInFile <> AutoCals.NullFile);
  HaveSHGSpec := (SHGSpecCals.ReadInFile <> SHGSpecCals.NullFile);

  if pNLO.NeedSpectrum and (not HaveSpec) then Exit;
  if pNLO.NeedAutocorrelation and (not HaveAuto) then Exit;
  if pNLO.NeedSHGSpectrum and (not HaveSHGSpec) then Exit;

  // If we make it to here, then we are ready to marg it!
  try
    if pNLO.NeedSpectrum then
    begin
      xSpectrum := SpecCals.LoadSpectrum;
      pLog.Add('Spectrum used for Marginal Calculation:' + SpecCals.ReadInFile);
    end;
    if pNLO.NeedAutocorrelation then
    begin
      xAutocorrelation := AutoCals.LoadAutocorrelation;
      pLog.Add('Autocorrelation used for Marginal Calculation:' + AutoCals.ReadInFile);
    end;
    if pNLO.NeedSHGSpectrum then
    begin
      xSHGSpectrum := SHGSpecCals.LoadSpectrum;
      pLog.Add('SHG Spectrum used for Marginal Calculation:' + SHGSpecCals.ReadInFile);
    end;

    // In NLO.CalculateMarginal, TReadIn.GridSpectrum is called.  This takes into
    // account that the data read in was constant wavelenght, and puts it onto
    // a constant frequency grid (just like the FROG trace).
    Marginal := pNLO.CalculateMarginal(xSpectrum, xAutocorrelation, xSHGSpectrum);
    FrogMarginal := pFrogI.ZeroOrderFreqMarginal;
    //frmTestPlot.PlotIntensity(FrogMarginal);
    //FrogMarginal.ScaleForNonlinearWavelength;
    //frmTestPlot.PlotIntensity(FrogMarginal);

    frmMargGraph := TfrmMargGraph.Create(Application);
    WindowManager.InitializeThisWindow(frmMargGraph);
    frmMargGraph.GraphMarginals(Marginal, FrogMarginal);
    frmMargGraph.ForceAgreement := mForceMarginals;
    frmMargGraph.ShowModal;
    if frmMargGraph.ForceAgreement then mForceMarginals := True;

    pOutputCals.SaveExperimentalMarginal(Marginal, 'ExpMarg.dat');

    if ForceMarginals then
    // Show the new marginals, too.
    begin
      pFrogI.ForceAgreementWithMarginal(Marginal);
      FrogMarginal.Free;
      FrogMarginal := pFrogI.ZeroOrderFreqMarginal;
      frmMargGraph.GraphMarginals(Marginal, FrogMarginal);
      frmMargGraph.Caption := 'Marginals, after forcing agreement.';
      frmMargGraph.ShowModal;
    end;
  finally
    FrogMarginal.Free;
    WindowManager.SaveAsDefaults(frmMargGraph);
    frmMargGraph.Free;
    Marginal.Free;
    xSHGSpectrum.Free;
    xAutocorrelation.Free;
    xSpectrum.Free;
  end;
end;


end.
