unit FFTGraph;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, TeeProcs, TeEngine, Chart, TadGrapher, TeeShape, Series, Func1D,
  StdCtrls, Menus;

type
  TfrmFFTGraph = class(TForm)
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TChartShape;
    edtFloor: TEdit;
    cmdRunAlgo: TButton;
    lblFloor: TLabel;
    PopupMenu1: TPopupMenu;
    Linear1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Chart1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Chart1GetNextAxisLabel(Sender: TChartAxis;
      LabelIndex: Integer; var LabelValue: Double; var Stop: Boolean);
    procedure Chart1GetAxisLabel(Sender: TChartAxis; Series: TChartSeries;
      ValueIndex: Integer; var LabelText: String);
    procedure edtFloorExit(Sender: TObject);
    procedure cmdRunAlgoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Linear1Click(Sender: TObject);
  private
    { Private declarations }
    mTadGrapher: TTadpoleGrapher;
    mXCoord: double;
    mFloor: double;
    procedure SetCutoff(X: Integer);
  public
    { Public declarations }
    procedure GraphFFTData(pFFTData: TEField);
    constructor CreateWithGrapher(pOwner: TComponent; pTadGrapher: TTadpoleGrapher);
  end;

var
  frmFFTGraph: TfrmFFTGraph;

implementation

uses Math;

{$R *.DFM}

constructor TfrmFFTGraph.CreateWithGrapher(pOwner: TComponent; pTadGrapher: TTadpoleGrapher);
begin
  mTadGrapher := pTadGrapher;
  inherited Create(pOwner);
  Chart1.PopupMenu := PopupMenu1;
end;

procedure TfrmFFTGraph.FormCreate(Sender: TObject);
begin
  //Height := 213;
  //Width := 688;

  Series2.X0 := 0;
  Series2.X1 := 0;
  Series2.Y0 := 0.0001;
  Series2.Y1 := 1;

  Series1.AddXY(-5,3,'',clTeeColor);
  Series1.AddXY(4,6,'',clTeeColor);

  mFloor := 1e-4;
  mXCoord := 0.1;
  edtFloor.Text := FloatToStrF(mFloor, ffExponent, 2, 2);
  edtFloor.Left := Width - edtFloor.Width - 10;
  edtFloor.Top := 3;
  cmdRunAlgo.Left := edtFloor.Left;
  cmdRunAlgo.Top := edtFloor.Top + edtFloor.Height + 2;
  lblFloor.Left := edtFloor.Left - lblFloor.Width - 3;
  lblFloor.Top := edtFloor.Top;

  ActiveControl := Chart1;
end;

procedure TfrmFFTGraph.Chart1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  XCoord: double;
  ScreenPoint, FormPoint: TPoint;
begin
  if (Button = mbRight) then
  begin
    FormPoint := Point(X, Y);
    ScreenPoint := Self.ClientToScreen(FormPoint);
    PopupMenu1.Popup(ScreenPoint.X, ScreenPoint.Y);
    Exit;
  end;

  SetCutoff(X);
  {
  XCoord := Series1.XScreenToValue(X);
  mXCoord := XCoord;
  Series2.X0 := XCoord;
  Series2.X1 := XCoord;
  Series2.Y0 := Chart1.LeftAxis.Minimum;
  Series2.Y1 := Chart1.LeftAxis.Maximum;
  Chart1.Title.Text.Clear;
  Chart1.Title.Text.Add( 'Cutoff at ' + FloatToStrF(XCoord, ffGeneral, 4, 0) + ' fs');
  mTadGrapher.RunAlgo(XCoord, mFloor);
  }
end;

procedure TfrmFFTGraph.SetCutoff(X: Integer);
var
  XCoord: double;
  ScreenPoint, FormPoint: TPoint;
begin
  XCoord := Series1.XScreenToValue(X);
  mXCoord := XCoord;
  Series2.X0 := XCoord;
  Series2.X1 := XCoord;
  Series2.Y0 := Chart1.LeftAxis.Minimum;
  Series2.Y1 := Chart1.LeftAxis.Maximum;
  Chart1.Title.Text.Clear;
  Chart1.Title.Text.Add( 'Cutoff at ' + FloatToStrF(XCoord, ffGeneral, 4, 0) + ' fs');
  mTadGrapher.RunAlgo(XCoord, mFloor);
end;

procedure TfrmFFTGraph.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caNone;
end;

procedure TfrmFFTGraph.GraphFFTData(pFFTData: TEField);
var
  i, N2: integer;
begin
  Series1.Clear;
  N2 := pFFTData.N div 2;
  for i := 0 to pFFTData.N - 1 do
  begin
    Series1.AddXY((i - N2)*pFFTData.DelT, pFFTData.Intensity^[i],'',clTeeColor);
  end;
  Chart1.LeftAxis.AdjustMaxMin;
  Series2.Y0 := Chart1.LeftAxis.Minimum;
  Series2.Y1 := Chart1.LeftAxis.Maximum;
end;

procedure TfrmFFTGraph.Chart1GetNextAxisLabel(Sender: TChartAxis;
  LabelIndex: Integer; var LabelValue: Double; var Stop: Boolean);
var
  bottom: integer;
begin
  if not Chart1.LeftAxis.Logarithmic then Exit;

  Chart1.LeftAxis.Minimum := Series1.MinYValue;
  if Sender = Chart1.LeftAxis then
  begin
    bottom := Trunc(Log10(Chart1.LeftAxis.Minimum));
    LabelValue := Power(10.0, bottom + LabelIndex - 1);
    Stop := False;
  end;
end;

procedure TfrmFFTGraph.Chart1GetAxisLabel(Sender: TChartAxis;
  Series: TChartSeries; ValueIndex: Integer; var LabelText: String);
var
  theValue: double;
begin
  if not Chart1.LeftAxis.Logarithmic then Exit;
  if Sender = Chart1.LeftAxis then
  begin
    theValue := StrToFloat(LabelText);
    LabelText := FloatToStrF(theValue, ffExponent, 0, 2);
  end;
end;

procedure TfrmFFTGraph.edtFloorExit(Sender: TObject);
begin
  try
    mFloor := StrToFloat(edtFloor.Text);
  except
  end;
  edtFloor.Text := FloatToStrF(mFloor, ffExponent, 2, 2);
end;

procedure TfrmFFTGraph.cmdRunAlgoClick(Sender: TObject);
begin
  mTadGrapher.RunAlgo(mXCoord, mFloor);
end;

procedure TfrmFFTGraph.FormShow(Sender: TObject);
begin
  if mTadGrapher.HasTestSpec then
  begin
    cmdRunAlgo.Enabled := False;
    edtFloor.Enabled := False;
    lblFloor.Enabled := False;
  end
  else
  begin
    cmdRunAlgo.Enabled := True;
    edtFloor.Enabled := True;
    lblFloor.Enabled := True;
  end;
end;

procedure TfrmFFTGraph.Linear1Click(Sender: TObject);
begin
  //if not (Sender = Chart1.LeftAxis) then Exit;
  if Chart1.LeftAxis.Logarithmic = True then
  begin
    Chart1.LeftAxis.Logarithmic := False;
    Linear1.Caption := '&Logarithmic';
  end
  else
  begin
    Chart1.LeftAxis.Logarithmic := True;
    Linear1.Caption := '&Linear';
  end;
end;

end.
