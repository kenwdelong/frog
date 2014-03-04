unit HistoryForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids;

type
  TfrmHistory = class(TForm)
    stgHistory: TStringGrid;
    cmdDisplayLog: TButton;
    procedure FormCreate(Sender: TObject);
    procedure stgHistorySelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure cmdDisplayLogClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    mNLOCol: integer;
    mNCol: integer;
    mFileCol: integer;
    mErrorCol: integer;
    mFWHMCol: integer;
    mRow: integer;
    mHistory: TStringList;
    mLogForms: TStringList;
    function getHistory: TStringList;
    function GetLogForms: TStringList;
    property History: TStringList read getHistory;
    property LogForms: TStringList read GetLogForms;
  public
    { Public declarations }
    procedure AddItem(pNLOName, pFileName: string; pN: integer; pFWHM, pError: double; pLog: TStringList);
  end;

var
  frmHistory: TfrmHistory;

implementation

uses LogForm;

{$R *.DFM}

procedure TfrmHistory.FormCreate(Sender: TObject);
begin
  //Height := 321;
  //Width := 501;

  mHistory := nil;
  mLogForms := nil;

  mNLOCol := stgHistory.FixedCols;
  mNCol := stgHistory.FixedCols + 1;
  mFileCol := stgHistory.FixedCols + 2;
  mFWHMCol := stgHistory.FixedCols + 3;
  mErrorCol := stgHistory.FixedCols + 4;

  if stgHistory.FixedCols = 1 then
    stgHistory.ColWidths[0] := 30;

  if stgHistory.FixedRows = 1 then
  begin
    stgHistory.Cells[mNLOCol,0] := 'NLO';
    stgHistory.Cells[mNCol,0] := 'N';
    stgHistory.Cells[mFileCol,0] := 'File';
    stgHistory.Cells[mErrorCol,0] := 'Error';
    stgHistory.Cells[mFWHMCol,0] := 'FWHM';
  end;

  mRow := 1;
end;

procedure TfrmHistory.stgHistorySelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  mRow := Row;
end;

function TfrmHistory.getHistory: TStringList;
begin
  if mHistory = nil then
    mHistory := TStringList.Create;
  getHistory := mHistory;
end;

function TfrmHistory.GetLogForms: TStringList;
begin
  if mLogForms = nil then
    mLogForms := TStringList.Create;
  getLogForms := mLogForms;
end;

procedure TfrmHistory.AddItem(pNLOName, pFileName: string; pN: integer; pFWHM, pError: double; pLog: TStringList);
var
  i: integer;
begin
  i := stgHistory.RowCount - 1;  // -1 because TStringGrid is zero-based array
  stgHistory.Cells[mNLOCol, i] := pNLOName;
  stgHistory.Cells[mFileCol, i] := pFileName;
  stgHistory.Cells[mNCol, i] := IntToStr(pN);
  stgHistory.Cells[mFWHMCol, i] := FloatToStrF(pFWHM, ffGeneral, 4, 0) + ' fs';
  stgHistory.Cells[mErrorCol, i] := FloatToStrF(pError, ffGeneral, 4, 0);
  History.AddObject(IntToStr(i), pLog);
  stgHistory.RowCount := i + 2;
end;

procedure TfrmHistory.FormDestroy(Sender: TObject);
var
  i, count: integer;
begin
  count := History.Count;
  for i := count - 1 downto 0 do
    History.Objects[i].Free;

  mHistory.Free;
  mHistory := nil;
  mLogForms.Free;
end;

procedure TfrmHistory.cmdDisplayLogClick(Sender: TObject);
begin
  if (mRow < 1) or (mRow >= stgHistory.RowCount - 1) then Exit;
  if stgHistory.RowCount <= 2 then Exit;

  frmLog := TfrmLog.Create(Self);
  LogForms.AddObject(IntToStr(LogForms.Count), frmLog);
  // Subtract one for the fixed row
  frmLog.ShowLog(History.Objects[mRow - 1] as TStringList);
  frmLog.Show;
end;

procedure TfrmHistory.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i, count: integer;
begin
  count := LogForms.Count;
  for i := count - 1 downto 0 do
  begin
    (LogForms.Objects[i] as TfrmLog).Free;
    LogForms.Delete(i);
  end;

  Action := caHide;
end;

end.
