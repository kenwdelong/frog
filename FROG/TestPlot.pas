unit TestPlot;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeEngine, Series, ExtCtrls, TeeProcs, Chart, Func1D;

type
  TfrmTestPlot = class(TForm)
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
  	constructor Create; 
  	procedure PlotIntensity(pField : T1DFunction);
    procedure PlotRealAndIm(pField : TComplex1DFunction);
    procedure SetTitle(pTitle: string);
    procedure Clear;
    { Public declarations }
  end;

var
  frmTestPlot: TfrmTestPlot;

implementation

{$R *.DFM}

{
Use like this:
  frmTestPlot.SetTitle('Afer transform');
  frmTestPlot.PlotRealAndIm(Spectrum);
}

constructor TfrmTestPlot.Create;
begin
	inherited Create(Application);
end;

procedure TfrmTestPlot.FormCreate(Sender: TObject);
begin
	//Width := 500;
  //Height := 400;
  Top := 50;
  Left := 50;
end;

procedure TFrmTestPlot.PlotIntensity(pField : T1DFunction);
var
	N, i: integer;
begin
	Clear;
  N := pField.N;
  for i := 0 to N - 1 do
  begin
    Series1.AddXY(i, pField.Intensity^[i], '', clTeeColor);
  end;
  ShowModal;
end;

procedure TFrmTestPlot.PlotRealAndIm(pField : TComplex1DFunction);
var
	N, i: integer;
begin
	Clear;
  N := pField.N;
  for i := 0 to N - 1 do
  begin
    Series1.AddXY(i, pField.Re^[i], '', clTeeColor);
    Series2.AddXY(i, pField.Im^[i], '', clTeeColor);
  end;
  ShowModal;
end;

procedure TfrmTestPlot.SetTitle(pTitle: string);
begin
  Chart1.Title.Text.Clear;
	Chart1.Title.Text.Add(pTitle);
end;

procedure TfrmTestPlot.Clear;
begin
	Series1.Clear;
  Series2.Clear;
end;

end.
