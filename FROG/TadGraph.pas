unit TadGraph;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeeShape, TeEngine, Series, ExtCtrls, TeeProcs, Chart, Func1D, Menus;

type
  TfrmTadGraph = class(TForm)
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    PopupMenu1: TPopupMenu;
    PrintDialog1: TPrintDialog;
    Print1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Print1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GraphTadSpectrum(pTadSpec: TSpectrum; pRef: TEField);
  end;

var
  frmTadGraph: TfrmTadGraph;

implementation

uses Printers;

{$R *.DFM}

procedure TfrmTadGraph.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caNone;
end;

procedure TfrmTadGraph.FormCreate(Sender: TObject);
begin
  //Height := 213;
  //Width := 344;
  Chart1.PopupMenu := PopupMenu1;
end;

procedure TfrmTadGraph.GraphTadSpectrum(pTadSpec: TSpectrum; pRef: TEField);
var
  i, N: integer;
  lam: double;
begin
  // Pre-cond: Both fields are already in the FREQ domain!
  Series1.Clear;
  N := pTadSpec.N;
  for i := 0 to N - 1 do
  begin
    lam := (i - N div 2)*pTadSpec.DelLam + pTadSpec.Lam0;
    Series1.AddXY(lam, pTadSpec.Intensity^[i], '', clTeeColor);
  end;

  Series2.Clear;
  N := pRef.N;
  for i := 0 to N - 1 do
  begin
    lam := (i - N div 2)*pRef.DelLam + pRef.Lam0;
    Series2.AddXY(lam, pRef.Intensity^[i], '', clTeeColor);
  end;
end;

procedure TfrmTadGraph.Print1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
  begin
    Chart1.PrintOrientation(poLandscape);
    Chart1.Print;
  end;
end;

end.
