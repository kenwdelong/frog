unit MargGraph;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, TeeProcs, TeEngine, Chart, Func1D, Series, Menus, StdCtrls;

type
  TfrmMargGraph = class(TForm)
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    PopupMenu1: TPopupMenu;
    Print1: TMenuItem;
    PrintDialog1: TPrintDialog;
    Zoom1: TMenuItem;
    ZoomOut1: TMenuItem;
    ShiftRight1: TMenuItem;
    ShiftLeft1: TMenuItem;
    ForceAgreement1: TMenuItem;
    cmdContinue: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Print1Click(Sender: TObject);
    procedure Zoom1Click(Sender: TObject);
    procedure ZoomOut1Click(Sender: TObject);
    procedure ShiftRight1Click(Sender: TObject);
    procedure ShiftLeft1Click(Sender: TObject);
    procedure ForceAgreement1Click(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Chart1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cmdContinueClick(Sender: TObject);
  private
    { Private declarations }
    mForceAgreement: boolean;
    mClickLocation: integer;
  public
    { Public declarations }
    property ForceAgreement: boolean read mForceAgreement write mForceAgreement;
    procedure GraphMarginals(pMarg, pFrogMarg: TMarginal);
  end;

var
  frmMargGraph: TfrmMargGraph;

implementation

uses Printers;

{$R *.DFM}

procedure TfrmMargGraph.FormCreate(Sender: TObject);
begin
  //Height := 340;
  //Width := 450;
  Top := 80;
  Left := 50;

  Chart1.PopupMenu := PopupMenu1;
end;

procedure TfrmMargGraph.GraphMarginals(pMarg, pFrogMarg: TMarginal);
var
  N, i: integer;
  lam: double;
begin
  N := pMarg.N;
  pMarg.Norm;
  for i := 0 to N - 1 do
  begin
    lam := pMarg.GetLamAt(i);
    Series1.AddXY(lam, pMarg.Intensity^[i], '', clTeeColor);
  end;

  N := pFrogMarg.N;
  pFrogMarg.Norm;
  for i := 0 to N - 1 do
  begin
    lam := pFrogMarg.GetLamAt(i);
    Series2.AddXY(lam, pFrogMarg.Intensity^[i], '', clTeeColor);
  end;
end;

procedure TfrmMargGraph.Print1Click(Sender: TObject);
begin
  if PrintDialog1.Execute then
  begin
    Chart1.PrintOrientation(poLandscape);
    Chart1.Print;
  end;
end;

procedure TfrmMargGraph.Zoom1Click(Sender: TObject);
var
	range, relativeClickPosition, newCenter: double;
begin
	 range := Chart1.BottomAxis.Maximum - Chart1.BottomAxis.Minimum;
  relativeClickPosition := (mClickLocation - Chart1.ChartRect.Left)/(Chart1.ChartRect.Right - Chart1.ChartRect.Left);
  newCenter := Chart1.BottomAxis.Minimum + range*relativeClickPosition;
	 Chart1.ZoomPercent(140);

	 range := Chart1.BottomAxis.Maximum - Chart1.BottomAxis.Minimum;
  Chart1.BottomAxis.Minimum := newCenter - range/2;
  Chart1.BottomAxis.Maximum := newCenter + range/2;

  Chart1.LeftAxis.Minimum := 0;
  Chart1.LeftAxis.Maximum := 1;
end;

procedure TfrmMargGraph.ZoomOut1Click(Sender: TObject);
begin
	Chart1.ZoomPercent(80);
  Chart1.LeftAxis.Minimum := 0;
  Chart1.LeftAxis.Maximum := 1;
end;

procedure TfrmMargGraph.ShiftRight1Click(Sender: TObject);
var
	range: double;
begin
	range := Chart1.BottomAxis.Maximum - Chart1.BottomAxis.Minimum;
  // Shift range by one-fifth
  range := range/5.0;
  Chart1.BottomAxis.Maximum := Chart1.BottomAxis.Maximum - range;
  Chart1.BottomAxis.Minimum := Chart1.BottomAxis.Minimum - range;
end;

procedure TfrmMargGraph.ShiftLeft1Click(Sender: TObject);
var
	range: double;
begin
	range := Chart1.BottomAxis.Maximum - Chart1.BottomAxis.Minimum;
  // Shift range by one-fifth
  range := range/5.0;
  Chart1.BottomAxis.Maximum := Chart1.BottomAxis.Maximum + range;
  Chart1.BottomAxis.Minimum := Chart1.BottomAxis.Minimum + range;
end;

procedure TfrmMargGraph.ForceAgreement1Click(Sender: TObject);
begin
     mForceAgreement := True;
     ModalResult := mrOk;  // See comment at cmdContinue
end;

procedure TfrmMargGraph.FormHide(Sender: TObject);
begin
     Series1.Clear;
     Series2.Clear;
end;

procedure TfrmMargGraph.FormShow(Sender: TObject);
begin
     if mForceAgreement = True then
        ForceAgreement1.Visible := False;
end;

procedure TfrmMargGraph.Chart1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     mClickLocation := X;
end;

procedure TfrmMargGraph.cmdContinueClick(Sender: TObject);
begin
     // The old way of getting out of here was to use the little x button at the top.
     // This results in a ModalResult of 2, which is most likely mrCancel.
     // The code that calls this form, MargCals.RunMarginals, doesn't check the ModalResult.
     // But if you ever do, be careful of this difference.
     ModalResult := mrOk;
end;

end.
