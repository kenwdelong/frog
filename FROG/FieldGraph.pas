unit FieldGraph;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeEngine, Series, ExtCtrls, TeeProcs, Chart, Func1D, Menus;

type
  TfrmFieldGraph = class(TForm)
    Chart1: TChart;
    RetrIntensity: TFastLineSeries;
    RetrPhase: TFastLineSeries;
    PrintDialog1: TPrintDialog;
    PopupMenu1: TPopupMenu;
    Print1: TMenuItem;
    OrigIntensity: TFastLineSeries;
    OrigPhase: TFastLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Print1Click(Sender: TObject);
  private
    originalFieldIsPlotted: Boolean;
    procedure PlotTemporalField(pE: TEField; pIntensity, pPhase: TFastLineSeries);
    procedure PlotSpectralField(pE: TEField; pIntensity, pPhase: TFastLineSeries);
    procedure FixUpSmallPhases;
    procedure FixUpLegend(pE, pEk: TEField; pPlotInitialField: Boolean);
  public
    { Public declarations }
    procedure GraphEField(pE, pEk: TEField; pPlotInitialField: Boolean);
    procedure GraphSpectrum(pE, pEk: TEField; pPlotInitialField: Boolean);
    procedure GraphBareEField(pE: TEField);
    procedure Clear;
  end;

var
  frmEGraph, frmSpecGraph: TfrmFieldGraph;

implementation

uses Printers, Math, ClipBrd;

{$R *.DFM}

procedure TfrmFieldGraph.FormCreate(Sender: TObject);
begin
  Height := 244;
  Width := 247;
  Chart1.PopupMenu := PopupMenu1;
end;

procedure TfrmFieldGraph.GraphEField(pE, pEk: TEField; pPlotInitialField: Boolean);
begin
  FixUpLegend(pE, pEk, pPlotInitialField);
  // Pre-cond: pE is in dtTIME!
  if not (pEk = nil) then
    PlotTemporalField(pEk, RetrIntensity, RetrPhase);
  if (not originalFieldIsPlotted) and pPlotInitialField then
  begin
    PlotTemporalField(pE, OrigIntensity, OrigPhase);
    originalFieldIsPlotted := true;
  end;
  FixUpSmallPhases;
end;

procedure TfrmFieldGraph.PlotTemporalField(pE: TEField; pIntensity, pPhase: TFastLineSeries);
var
  t, N: integer;
  tt: double;
  tE: TEField;
begin
  // I don't think you are supposed to fuck with the actual field values
  tE := TEField.Create(pE.N);
  tE.SetMyUnits(pE.DelT, pE.Lam0);
  tE.CopyFrom(pE);

  pIntensity.Clear;
  pPhase.Clear;
  tE.Norm;
  tE.SetIntensityAndPhase;
  tE.ClipPhase(1e-4);
  N := tE.N;
  for t := 0 to N - 1 do
  begin
    tt := (t - N div 2)*tE.DelT;
    pIntensity.AddXY(tt, tE.Intensity^[t], '', clTeeColor);
    pPhase.AddXY(tt, tE.Phase^[t], '' , clTeeColor);
  end;

  tE.Free;
end;

procedure TfrmFieldGraph.GraphSpectrum(pE, pEk: TEField; pPlotInitialField: Boolean);
begin
  FixUpLegend(pE, pEk, pPlotInitialField);
  Chart1.BottomAxis.Title.Caption := 'Wavelength (nm)';
  // Pre-cond: pE is in dtFREQ!
  if not (pEk = nil) then
    PlotSpectralField(pEk, RetrIntensity, RetrPhase);
  if (not originalFieldIsPlotted) and pPlotInitialField then
  begin
    PlotSpectralField(pE, OrigIntensity, OrigPhase);
    originalFieldIsPlotted := true;
  end;
  FixUpSmallPhases;
end;

procedure TfrmFieldGraph.PlotSpectralField(pE: TEField; pIntensity, pPhase: TFastLineSeries);
var
  w, N: integer;
  lam: double;
  tE: TEField;
begin
  // I don't think you are supposed to fuck with the actual field values
  tE := TEField.Create(pE.N);
  tE.SetMyUnits(pE.DelT, pE.Lam0);
  tE.CopyFrom(pE);

  pIntensity.Clear;
  pPhase.Clear;
  tE.Norm;
  tE.SetIntensityAndPhase;
  tE.ClipPhase(1e-4);
  N := tE.N;
  for w := 0 to N - 1 do
  begin
    lam := tE.Lam0 + (w - N div 2)*tE.DelLam;
    pIntensity.AddXY(lam, tE.Intensity^[w], '', clTeeColor);
    pPhase.AddXY(lam, tE.Phase^[w], '' , clTeeColor);
  end;

  tE.Free;
end;

procedure TfrmFieldGraph.FixUpSmallPhases;
var
  maxphase: double;
begin
  maxphase := Max(OrigPhase.MaxYValue, RetrPhase.MaxYValue);
  if maxphase < 1e-11 then
  begin
    Chart1.RightAxis.AutomaticMaximum := false;
    Chart1.RightAxis.Maximum := 5e-12;
  end
  else
  begin
    Chart1.RightAxis.AutomaticMaximum := true;
  end;
end;

procedure  TfrmFieldGraph.FixUpLegend(pE, pEk: TEField; pPlotInitialField: Boolean);
begin
  if (not (pE = nil)) and (not (pEk = nil)) and pPlotInitialField then
    Chart1.Legend.Visible := true
  else
    Chart1.Legend.Visible := false;
end;

procedure TfrmFieldGraph.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caNone;
end;

procedure TfrmFieldGraph.GraphBareEField(pE: TEField);
var
  t, N: integer;
  tt: double;
begin
  RetrIntensity.Clear;
  RetrPhase.Clear;
  pE.SetIntensityAndPhase;
  N := pE.N;
  for t := 0 to N - 1 do
  begin
    tt := (t - N div 2)*pE.DelT;
    RetrIntensity.AddXY(tt, pE.Intensity^[t], '', clTeeColor);
    RetrPhase.AddXY(tt, pE.Phase^[t], '' , clTeeColor);
  end;
end;

procedure TfrmFieldGraph.Clear;
begin
  OrigIntensity.Clear;
  OrigPhase.Clear;
  RetrIntensity.Clear;
  RetrPhase.Clear;
  originalFieldIsPlotted := false;
end;


procedure TfrmFieldGraph.Print1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
  begin
    Chart1.PrintOrientation(poLandscape);
    Chart1.Print;
  end;
end;


{
I cannot find a way to get something out of the TChart that will Assign to
the clipboard.  There must be some sort of Canvas -> TPicture conversion function
somewhere, but I can't locate it.
procedure TfrmFieldGraph.Copyto1Click(Sender: TObject);
begin
  Clipboard.Assign(Chart1.DelphiCanvas);
end;
}

end.
