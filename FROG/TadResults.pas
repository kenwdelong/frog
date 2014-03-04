unit TadResults;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TadGrapher;

type

  TTadResults = record
    TempWidth: double;
    SpecWidth: double;
    AutoWidth: double;
    FWHM: double;
    RMS: double;
    TL: double;
    SL: double;
  end;

  TfrmTadResults = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    lblTempWidth: TLabel;
    lblSpecWidth: TLabel;
    lblAutoWidth: TLabel;
    lblFWHM: TLabel;
    lblRMS: TLabel;
    lblTempL: TLabel;
    lblSpecL: TLabel;
    cmdQuit: TButton;
    cmdHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cmdQuitClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cmdHelpClick(Sender: TObject);
  private
    { Private declarations }
    mTadGrapher: TTadpoleGrapher;
    mIAmDone: Boolean;
  public
    { Public declarations }
    procedure DisplayResults(pTadResults: TTadResults);
    constructor CreateWithGrapher(pOwner: TComponent; pTadGrapher: TTadpoleGrapher);
  end;

var
  frmTadResults: TfrmTadResults;

implementation

{$R *.DFM}

constructor TfrmTadResults.CreateWithGrapher(pOwner: TComponent; pTadGrapher: TTadpoleGrapher);
begin
  inherited Create(pOwner);
  mTadGrapher := pTadGrapher;
end;

procedure TfrmTadResults.FormCreate(Sender: TObject);
begin
  //Height := 213;
  //Width := 254;
  mIAmDone := False;
end;

procedure TfrmTadResults.cmdQuitClick(Sender: TObject);
begin
  //Visible := False;
  mTadGrapher.CloseWindows;
  mIAmDone := True;
  Close;
end;

procedure TfrmTadResults.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caNone;
  if mIAmDone then Action := caFree;
end;

procedure TfrmTadResults.DisplayResults(pTadResults: TTadResults);
begin
  lblTempWidth.Caption := FloatToStrF(pTadResults.TempWidth, ffGeneral, 4, 0) + ' fs';
  lblSpecWidth.Caption := FloatToStrF(pTadResults.SpecWidth, ffGeneral, 4, 0) + ' nm';
  lblAutoWidth.Caption := FloatToStrF(pTadResults.AutoWidth, ffGeneral, 4, 0) + ' fs';
  lblFWHM.Caption := FloatToStrF(pTadResults.FWHM, ffGeneral, 4, 0);
  lblRMS.Caption := FloatToStrF(pTadResults.RMS, ffGeneral, 4, 0);
  lblTempL.Caption := FloatToStrF(pTadResults.TL, ffGeneral, 4, 0);
  lblSpecL.Caption := FloatToStrF(pTadResults.SL, ffGeneral, 4, 0);
end;

procedure TfrmTadResults.cmdHelpClick(Sender: TObject);
begin
  Application.HelpContext(40);
end;

end.
