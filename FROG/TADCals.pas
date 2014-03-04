unit TADCals;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, FileCtrl, ExtCtrls, Func1D, OutputCals, ReadIn, TadMgr;

type
  TfrmTADCals = class(TForm)
    pgcTADCals: TPageControl;
    tbsInputs: TTabSheet;
    tbsRef: TTabSheet;
    lblTAD: TLabel;
    edtTadFileName: TEdit;
    cmdChangeFile: TButton;
    tbsOutputs: TTabSheet;
    drvOutput: TDriveComboBox;
    dirOutput: TDirectoryListBox;
    dlgOpen: TOpenDialog;
    cmdOK: TButton;
    cmdHelp: TButton;
    lblRef: TLabel;
    rdgRefPulse: TRadioGroup;
    edtRefFileName: TEdit;
    cmdRefFile: TButton;
    chkRefUseHeader: TCheckBox;
    edtRefLam0: TEdit;
    lblRefLam0: TLabel;
    edtTestSpec: TEdit;
    cmdTestSpec: TButton;
    lblTestSpec: TLabel;
    cmdClearTestSpec: TButton;
    cmdCancel: TButton;
    lblPhaseBlank: TLabel;
    edtPhaseBlank: TEdit;
    procedure drvOutputChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rdgRefPulseClick(Sender: TObject);
    procedure cmdRefFileClick(Sender: TObject);
    procedure chkRefUseHeaderClick(Sender: TObject);
    procedure edtRefLam0Exit(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmdChangeFileClick(Sender: TObject);
    procedure cmdTestSpecClick(Sender: TObject);
    procedure cmdClearTestSpecClick(Sender: TObject);
    procedure cmdCancelClick(Sender: TObject);
    procedure cmdHelpClick(Sender: TObject);
    procedure edtPhaseBlankExit(Sender: TObject);
  private
    { Private declarations }
    mReferenceReady: Boolean;
    mTadDataReady: Boolean;
    procedure RefreshOutputTab;
    procedure RefreshReferenceTab;
    procedure RefreshTadDataTab;
    procedure CheckOk;
  public
    { Public declarations }
  end;

var
  frmTADCals: TfrmTADCals;

  TadAllCals: TTadCals;  // just a record
  mOutputCals: TOutputCals;
  mTadpoleCals: TReadIn;
  mReferenceCals: TReadIn;
  mTestSpecCals: TReadIn;

implementation

uses WindowMgr;

{$R *.DFM}

procedure TfrmTADCals.FormCreate(Sender: TObject);
begin
  inherited;
  Top := 100;
  Left := 30;
  //Height := 388;
  //Width := 426;

  mOutputCals := TOutputCals.Create;
  mTadpoleCals := TReadIn.Create;
  mReferenceCals := TReadIn.Create;
  mTestSpecCals := TReadIn.Create;

  TadAllCals.mTadpoleCals := mTadpoleCals;
  TadAllCals.mReferenceCals := mReferenceCals;
  TadAllCals.mOutputCals := mOutputCals;
  TadAllCals.mTestSpecCals := mTestSpecCals;

  mReferenceReady := False;
  mTadDataReady := False;

  if not WindowManager.DefaultTadPathNull then
  begin
    dlgOpen.InitialDir := WindowManager.DefaultTadPath;
    dirOutput.Directory := WindowManager.DefaultTadPath;
    drvOutput.Drive := (WindowManager.DefaultTadPath)[1];
  end;

  //mTadpoleCals.ReadInFile := 'C:\Femtosoft\code\Frog2.0\ATADSpectrum.dat';
  //mReferenceCals.ReadInFile := 'C:\Femtosoft\code\Frog2.0\ATADRefQ2.dat';
  //mTestSpecCals.ReadInFile := 'C:\Femtosoft\code\Frog2.0\ATADTestSpec.dat';
end;

procedure TfrmTADCals.FormDestroy(Sender: TObject);
begin
  mTestSpecCals.Free;
  mOutputCals.Free;
  mTadpoleCals.Free;
  mReferenceCals.Free;
end;

procedure TfrmTADCals.cmdOKClick(Sender: TObject);
begin
  WindowManager.DefaultTadPath := dirOutput.Directory;
  ModalResult := mrOK;
end;

procedure TfrmTADCals.FormShow(Sender: TObject);
begin
  RefreshOutputTab;
  RefreshReferenceTab;
  RefreshTadDataTab;
end;

procedure TfrmTADCals.CheckOk;
begin
  if mReferenceReady and mTadDataReady then
    cmdOK.Enabled := True
  else
    cmdOK.Enabled := False;
end;

// ************* Output Tab **********************

procedure TfrmTADCals.drvOutputChange(Sender: TObject);
begin
  dirOutput.Drive := drvOutput.Drive;
end;

procedure TfrmTADCals.edtPhaseBlankExit(Sender: TObject);
begin
  try
    mOutputCals.PhaseBlank := StrToFloat(edtPhaseBlank.Text);
  except on EConvertError do
  end;
  RefreshOutputTab;
end;

procedure TfrmTADCals.RefreshOutputTab;
begin
  mOutputCals.FilePath := dirOutput.Directory;
  dirOutput.Drive := drvOutput.Drive;
  edtPhaseBlank.Text := FloatToStrF(mOutputCals.PhaseBlank, ffGeneral, 2, 0);
end;

// ***************** Reference Tab *********************
procedure TfrmTADCals.rdgRefPulseClick(Sender: TObject);
begin
  case rdgRefPulse.ItemIndex of
    Ord(rsFrog): mReferenceCals.ReferencePulseSource := rsFrog;
    Ord(rsReadIn): mReferenceCals.ReferencePulseSource := rsReadIn;
  end;
  RefreshReferenceTab;
end;

procedure TfrmTADCals.cmdRefFileClick(Sender: TObject);
begin
  dlgOpen.Title := 'Select File Containing Reference E-field';
  if dlgOpen.Execute then
  begin
    mReferenceCals.ReadInFile := dlgOpen.Filename;
    dlgOpen.InitialDir := ExtractFileDir(dlgOpen.FileName);
  end;
  RefreshReferenceTab;
end;

procedure TfrmTADCals.chkRefUseHeaderClick(Sender: TObject);
begin
  mReferenceCals.UseHeader := chkRefUseHeader.Checked;
  RefreshReferenceTab;
end;

procedure TfrmTADCals.edtRefLam0Exit(Sender: TObject);
begin
  try
    mReferenceCals.Lam0 := StrToFloat(edtRefLam0.Text);
  except on EConvertError do
  end;
  RefreshReferenceTab;
end;

procedure TfrmTADCals.RefreshReferenceTab;
begin
  edtRefFilename.Text := mReferenceCals.ReadInFile;
  edtRefLam0.Text := FloatToStrF(mReferenceCals.Lam0, ffGeneral, 4, 0);
  if mReferenceCals.ReadInFile <> mReferenceCals.NullFile then
  begin
    chkRefUseHeader.Enabled := True;
    if (mReferenceCals.Lam0 <> 0) then
      mReferenceReady := True
    else
      mReferenceReady := False;
  end
  else
  begin
    chkRefUseHeader.Enabled := False;
    mReferenceReady := False;
  end;

  chkRefUseHeader.Checked := mReferenceCals.UseHeader;

  if TadAllCals.mReferenceCals.ReferencePulse = nil then
  begin
    rdgRefPulse.Enabled := False;
    rdgRefPulse.ItemIndex := Ord(rsReadIn);
  end
  else
  begin
    rdgRefPulse.Enabled := True;
  end;

  if rdgRefPulse.ItemIndex = Ord(rsFrog) then
  begin
    chkRefUseHeader.Enabled := False;
    edtRefFileName.Enabled := False;
    cmdRefFile.Enabled := False;
    edtRefLam0.Enabled := False;
    lblRefLam0.Enabled := False;
    // ?    Fixed March 28, 2002 to be included in version 3.09
    mReferenceReady := True;
  end
  else
  begin
    chkRefUseHeader.Enabled := True;
    edtRefFileName.Enabled := True;
    cmdRefFile.Enabled := True;
    edtRefLam0.Enabled := True;
    lblRefLam0.Enabled := True;
  end;

  CheckOK;
end;


// *************** Tad Data Tab *********************
procedure TfrmTADCals.cmdChangeFileClick(Sender: TObject);
begin
  dlgOpen.Title := 'Select File Containing Tadpole Data';
  if dlgOpen.Execute then
  begin
    mTadpoleCals.ReadInFile := dlgOpen.Filename;
    dlgOpen.InitialDir := ExtractFileDir(dlgOpen.FileName);
  end;
  RefreshTadDataTab;
end;

procedure TfrmTADCals.RefreshTadDataTab;
begin
  edtTadFileName.Text := mTadpoleCals.ReadInFile;
  edtTestSpec.Text := mTestSpecCals.ReadInFile;
  if (mTadpoleCals.ReadInFile <> mTadpoleCals.NullFile) then
    mTadDataReady := True
  else
    mTadDataReady := False;

  CheckOk;
end;

procedure TfrmTADCals.cmdTestSpecClick(Sender: TObject);
begin
  dlgOpen.Title := 'Select File Containing Test Pulse Spectrum';
  if dlgOpen.Execute then
  begin
    mTestSpecCals.ReadInFile := dlgOpen.Filename;
    dlgOpen.InitialDir := ExtractFileDir(dlgOpen.FileName);
  end;
  RefreshTadDataTab;
end;

procedure TfrmTADCals.cmdClearTestSpecClick(Sender: TObject);
begin
  mTestSpecCals.ReadInFile := mTestSpecCals.NullFile;
  RefreshTadDataTab;
end;

procedure TfrmTADCals.cmdCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmTADCals.cmdHelpClick(Sender: TObject);
begin
  Application.HelpContext(pgcTADCals.ActivePage.HelpContext);
end;

end.
