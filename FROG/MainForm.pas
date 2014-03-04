unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, Buttons, ToolWin, ComCtrls, AlgoMgr, TadMgr, Menus, XAlgoMgr,
  Func1D;

type
  TfrmMainForm = class(TForm)
    tlbToolBar: TToolBar;
    spbFrogAlgo: TSpeedButton;
    spbLog: TSpeedButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    N1: TMenuItem;
    Quit1: TMenuItem;
    Frog1: TMenuItem;
    Windows1: TMenuItem;
    Setup1: TMenuItem;
    spbTadpole: TSpeedButton;
    About1: TMenuItem;
    Tadpole1: TMenuItem;
    AboutFROG1: TMenuItem;
    Windows2: TMenuItem;
    UseDefaultPositions1: TMenuItem;
    Contents1: TMenuItem;
    N2: TMenuItem;
    IntroductiontoFROG1: TMenuItem;
    spbXFrog: TSpeedButton;
    XFrog1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure spbLogClick(Sender: TObject);
    procedure spbFrogAlgoClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Quit1Click(Sender: TObject);
    procedure spbTadpoleClick(Sender: TObject);
    procedure AboutFROG1Click(Sender: TObject);
    procedure UseDefaultPositions1Click(Sender: TObject);
    procedure Contents1Click(Sender: TObject);
    procedure IntroductiontoFROG1Click(Sender: TObject);
    procedure spbXFrogClick(Sender: TObject);
  private
    { Private declarations }
    mXAlgoMgr: TXFrogAlgoMgr;
    function AlgoActive: Boolean;
    function BestE: TEField;
  public
    { Public declarations }
  end;

var
  frmMainForm: TfrmMainForm;
  mAlgoMgr: TFrogAlgoMgr;
  mTadMgr: TTadpoleAlgorithmManager;

const
  DEFAULT_STRING = 'Use &Default Positions';
  CUSTOM_STRING = 'Use &Custom Positions';


implementation

uses StratTypes, NLO, PG, EFieldCals, CalsForm, TadCals, HistoryForm,
  About, WindowMgr;

{$R *.DFM}

procedure TfrmMainForm.FormCreate(Sender: TObject);
var
	enableTadpole, enableXFROG: Boolean;
begin
  // Control for the various versions: F, FT, FTX, FX
  enableTadpole := True;
  enableXFROG := True;
  // Controls for the demo version are at AlgoMgr 193, TadGrapher 112

  if not enableTadpole then
  begin
  	spbTadpole.Visible := False;
  	spbTadpole.Enabled := False;
  	Tadpole1.Visible := False;
  	Tadpole1.Enabled := False;
  end;

  if not enableXFROG then
  begin
  	spbXFrog.Visible := False;
  	spbXFrog.Enabled := False;
  	XFrog1.Visible := False;
  	XFrog1.Enabled := False;
  end;


  mAlgoMgr := nil;
  mTadMgr := nil;
  mXAlgoMgr := nil;
  WindowManager := TWindowManager.Create;

  UseDefaultPositions1.Caption := DEFAULT_STRING;
  WindowManager.UseDefaults := False;

  // And not only that, set up the help file properly
  Application.HelpFile := GetCurrentDir + '\' + Application.HelpFile;
  Application.HelpFile := ExtractFilePath(Application.ExeName) + 'doc\FROG.chm';
end;

procedure TfrmMainForm.spbLogClick(Sender: TObject);
begin
  frmHistory.Show;
end;

procedure TfrmMainForm.spbFrogAlgoClick(Sender: TObject);
begin
  // The conditions for opening a window are different for FROG and TADPOLE
  if mTadMgr <> nil then
    if mTadMgr.TadpoleActive then Exit;
  if mXAlgoMgr <> nil then
    if mXAlgoMgr.WindowsOpen then Exit;

  if mAlgoMgr = nil then mAlgoMgr := TFrogAlgoMgr.Create;
  mAlgoMgr.DoFrog;
end;

procedure TfrmMainForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  mXAlgoMgr.Free;
  mAlgoMgr.Free;
  mTadMgr.Free;
  WindowManager.Free;
  WindowManager := nil;
end;

procedure TfrmMainForm.Quit1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmMainForm.spbTadpoleClick(Sender: TObject);
begin
  if mTadMgr = nil then
    mTadMgr := TTadpoleAlgorithmManager.Create(TadCals.TadAllCals);

  if AlgoActive then Exit;

  TadCals.TadAllCals.mReferenceCals.ReferencePulse := BestE;
  frmTadCals.ShowModal;
  if frmTadCals.ModalResult = mrOK then
    try
      mTadMgr.InitAlgo;
    except
      mTadMgr.Abort;
    end;
end;

function TfrmMainForm.BestE: TEField;
begin
  BestE := nil;
  if mAlgoMgr <> nil then
    BestE := mAlgoMgr.BestField;
end;

procedure TfrmMainForm.AboutFROG1Click(Sender: TObject);
begin
  AboutBox := TAboutBox.Create(Self);
  AboutBox.ShowModal;
end;

procedure TfrmMainForm.UseDefaultPositions1Click(Sender: TObject);
begin
  if UseDefaultPositions1.Caption = DEFAULT_STRING then
  begin
    WindowManager.UseDefaults := True;
    UseDefaultPositions1.Caption := CUSTOM_STRING;
  end
  else
  begin
    WindowManager.UseDefaults := False;
    UseDefaultPositions1.Caption := DEFAULT_STRING;
  end;
end;

procedure TfrmMainForm.Contents1Click(Sender: TObject);
begin
  Application.HelpContext(0);
end;

procedure TfrmMainForm.IntroductiontoFROG1Click(Sender: TObject);
begin
  Application.HelpContext(14);
end;

function TfrmMainForm.AlgoActive: Boolean;
begin
  // Only Tadpole uses this now, so just do the conditions for Tadpole
  Result := False;
  if mAlgoMgr <> nil then Result := (Result or mAlgoMgr.WindowsOpen);
  if mTadMgr <> nil then Result := (Result or mTadMgr.TadpoleActive);
  if mXAlgoMgr <> nil then Result := (Result or mXAlgoMgr.WindowsOpen);
end;

procedure TfrmMainForm.spbXFrogClick(Sender: TObject);
begin
  if mAlgoMgr <> nil then if mAlgoMgr.WindowsOpen then Exit;
  if mTadMgr <> nil then if mTadMgr.TadpoleActive then Exit;

  if mXAlgoMgr = nil then
    mXAlgoMgr := TXFrogAlgoMgr.Create;
  mXAlgoMgr.ReferenceField := BestE;
  mXAlgoMgr.DoXFrog;
end;

end.
