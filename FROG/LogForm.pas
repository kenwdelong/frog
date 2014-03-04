unit LogForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus;

type
  TfrmLog = class(TForm)
    memLog: TMemo;
    PrintDialog1: TPrintDialog;
    MainMenu1: TMainMenu;
    Print1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Print1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ShowLog(pLog: TStringList);
  end;

var
  frmLog: TfrmLog;

implementation

uses Printers;

{$R *.DFM}

procedure TfrmLog.ShowLog(pLog: TStringList);
begin
  memLog.Lines := pLog as TStrings;
end;

procedure TfrmLog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfrmLog.Print1Click(Sender: TObject);
var
  Line: Integer;
  PrintText: TextFile;   {declares a file variable}
begin
  if PrintDialog1.Execute then
  begin
    AssignPrn(PrintText);   {assigns PrintText to the printer}
    Rewrite(PrintText);     {creates and opens the output file}
    Printer.Canvas.Font := memLog.Font;  {assigns Font settings to the canvas}
    for Line := 0 to memLog.Lines.Count - 1 do
      Writeln(PrintText, memLog.Lines[Line]);	{writes the contents of the Memo1 to the printer object}

    CloseFile(PrintText); {Closes the printer variable}
  end;
end;

procedure TfrmLog.FormCreate(Sender: TObject);
begin
  //Height := 400;
  //Width := 520;
end;

end.
