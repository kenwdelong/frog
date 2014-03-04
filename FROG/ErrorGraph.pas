unit ErrorGraph;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TeEngine, Series, ExtCtrls, TeeProcs, Chart, FrogObj;

type
  TMarkEntry = class(TFrogObject)
  private
    mIndex: integer;
    mLabel: string;
  public
    property IndexVal: integer read mIndex write mIndex;
    property LabelText: string read mLabel write mLabel;
    constructor Create(pIndex: integer; pLabel: string);
  end;

  TAxisRange = (arWide, arNarrow);

  TfrmErrorGraph = class(TForm)
    chtError: TChart;
    Series1: TLineSeries;
    procedure FormCreate(Sender: TObject);
    procedure Series1GetMarkText(Sender: TChartSeries; ValueIndex: Integer;
      var MarkText: String);
  private
    marks: array[1..20] of TMarkEntry;
    nextMark: integer;
  public
    { Public declarations }
    procedure AddAPoint(k:integer; error: double);
    procedure AddAMark(k: integer; Status: string);
    procedure Clear();
    procedure SetAxisRange(ar: TAxisRange);
  end;

var
  frmErrorGraph: TfrmErrorGraph;

const
  INITIAL_WIDTH = 100;

implementation

{$R *.DFM}

procedure TfrmErrorGraph.FormCreate(Sender: TObject);
begin
  Height := 300;
  Width := 450;
  nextMark := Low(marks);
end;

procedure TFrmErrorGraph.AddAPoint(k:integer; error: double);
begin
	Series1.AddXY(k, error, '', clBlue);

  if (k > INITIAL_WIDTH) then
  begin
  	chtError.BottomAxis.Minimum := k - INITIAL_WIDTH;
    chtError.BottomAxis.Maximum := k;
  end;
end;

// What a mess - I have to build my own hashtable!
procedure TFrmErrorGraph.AddAMark(k: integer; Status: string);
var
  markEntry: TMarkEntry;
  oldMark: TMarkEntry;
begin
  markEntry := TMarkEntry.Create(k, Status);
  oldMark := marks[nextMark];
  if not (oldMark = nil) then oldMark.Destroy;
  marks[nextMark] := markEntry;
  nextMark := nextMark + 1;
  if nextMark > High(marks) then
    nextMark := Low(marks);
end;

procedure TFrmErrorGraph.Clear();
var
  i: integer;
  markEntry: TMarkEntry;
begin
  Series1.Clear;
  chtError.BottomAxis.Minimum := 0;
  chtError.BottomAxis.Maximum := INITIAL_WIDTH;

  // Reset the marks
  for i := Low(marks) to High(marks) do
  begin
    markEntry := marks[i];
    if not (markEntry = nil) then
      markEntry.Free;
    marks[i] := nil;
  end;
  nextMark := Low(marks);
end;

procedure TfrmErrorGraph.Series1GetMarkText(Sender: TChartSeries;
  ValueIndex: Integer; var MarkText: String);
var
  i: integer;
  markEntry: TMarkEntry;
begin
  for i := Low(marks) to High(marks) do
  begin
    markEntry := marks[i];
    if not (markEntry = nil) then
      if markEntry.IndexVal = ValueIndex then
      begin
        MarkText := markEntry.LabelText;
        Exit;
      end;
  end;
  MarkText := '';
end;

procedure TfrmErrorGraph.SetAxisRange(ar: TAxisRange);
begin
  if ar = arWide then
    chtError.LeftAxis.Minimum := 1e-10
  else
    chtError.LeftAxis.Minimum := 1e-4;
end;




constructor TMarkEntry.Create(pIndex: integer; pLabel: string);
begin
  mLabel := pLabel;
  mIndex := pIndex;
end;

end.
