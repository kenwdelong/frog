unit CalsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, FileCtrl, Grids, RunTypeU,
  AlgoCals, FrogAlgo, ReadIn, Exper, AllCals, Data.Bind.EngExt,
  Vcl.Bind.DBEngExt, Data.Bind.Components, HTMLHelpViewer;

type
  TfrmCalibrations = class(TForm)
    rdgNLO: TRadioGroup;
    rdgGridSize: TRadioGroup;
    rdgDataSource: TRadioGroup;
    pgcCals: TPageControl;
    tbsStrategies: TTabSheet;
    tbsInitialField: TTabSheet;
    tbsExpData: TTabSheet;
    tbsInputData: TTabSheet;
    tbsOutputs: TTabSheet;
    tbsMarginals: TTabSheet;
    tbsFirstGuess: TTabSheet;
    dirOutput: TDirectoryListBox;
    lblOutputDirectory: TLabel;
    lblSpectrum: TLabel;
    lblAutocorrelation: TLabel;
    lblSHGSPectrum: TLabel;
    edtSpectrum: TEdit;
    edtAutocorrelation: TEdit;
    edtSHGSpectrum: TEdit;
    cmdSpectrum: TButton;
    cmdAutocorrelation: TButton;
    cmdSHGSpectrum: TButton;
    rdgInitialField: TRadioGroup;
    lblWidth: TLabel;
    lblChirp: TLabel;
    lblSPM: TLabel;
    edtWidth: TEdit;
    edtChirp: TEdit;
    edtFSPM: TEdit;
    dlgOpen: TOpenDialog;
    lblChirpUnits: TLabel;
    lblWidthUnits: TLabel;
    drvOutput: TDriveComboBox;
    LblFilename: TLabel;
    chkReadinHeader: TCheckBox;
    edtReadInLam0: TEdit;
    LblCenterWav: TLabel;
    LblCarrier: TLabel;
    edtReadInFilename: TEdit;
    cmdReadInFile: TButton;
    LblExpFile: TLabel;
    SizeBox: TGroupBox;
    NTauLabel: TLabel;
    NLamLabel: TLabel;
    EdtNTau: TEdit;
    EdtNLam: TEdit;
    CalBox: TGroupBox;
    TimeCalLabel: TLabel;
    WavCalLabel: TLabel;
    TCUnitLabel: TLabel;
    WCUnitLabel: TLabel;
    EdtDelT: TEdit;
    EdtDelLam: TEdit;
    WaveBox: TGroupBox;
    lblExpLam0: TLabel;
    lblExpLam1: TLabel;
    CenWavLabel: TLabel;
    FirstWavLabel: TLabel;
    EdtLamCenter: TEdit;
    EdtLam1: TEdit;
    chkExperHeader: TCheckBox;
    rdgOrder: TRadioGroup;
    edtExperFile: TEdit;
    cmdExperDataFile: TButton;
    LblLam0: TLabel;
    RdbDomain: TRadioGroup;
    TimeDomainGroup: TGroupBox;
    LbLTempShape: TLabel;
    LblTWidth: TLabel;
    LblTChirp: TLabel;
    lblThySPM: TLabel;
    LblTCP: TLabel;
    LblTWidthU: TLabel;
    LblTChirpU: TLabel;
    LblTCPU: TLabel;
    LblDelT: TLabel;
    LblSPMU: TLabel;
    CbxTempShape: TComboBox;
    EdtTempWidth: TEdit;
    EdtTChirp: TEdit;
    EdtSPM: TEdit;
    EdtTCP: TEdit;
    BtnTimeDouble: TButton;
    edtDelTT: TEdit;
    FreqDomainGroup: TGroupBox;
    LblSQP: TLabel;
    LblSCP: TLabel;
    LblFChirp: TLabel;
    LblFWidth: TLabel;
    LblFreqShape: TLabel;
    LblFWidthU: TLabel;
    LblFChirpU: TLabel;
    LblSCPU: TLabel;
    LblSQPU: TLabel;
    LblDelF: TLabel;
    EdtSQP: TEdit;
    EdtSCP: TEdit;
    EdtFLinC: TEdit;
    EdtFreqWidth: TEdit;
    CbxFreqShape: TComboBox;
    BtnFreqDouble: TButton;
    EdtDelF: TEdit;
    RdbUnits: TRadioGroup;
    EdtLam0: TEdit;
    stgStrats: TStringGrid;
    cmdHelp: TButton;
    cmdOK: TButton;
    lblFSPMUnits: TLabel;
    rdgDownsize: TRadioGroup;
    cbxStrats: TComboBox;
    lblStart: TLabel;
    lblLast: TLabel;
    lblBest: TLabel;
    lblPerturb: TLabel;
    cmdInsert: TButton;
    cmdDelete: TButton;
    lblPhaseBlank: TLabel;
    edtPhaseBlank: TEdit;
    cmdClearSHGSpec: TButton;
    cmdClearAuto: TButton;
    cmdClearSpec: TButton;
    chkGraphics: TCheckBox;
    rdgBinning: TRadioGroup;
    cmdCancel: TButton;
    Label1: TLabel;
    chkOverwrite: TCheckBox;
    Label2: TLabel;
    chkForceMarginal: TCheckBox;
    rdgFormat: TRadioGroup;
    BindingsList1: TBindingsList;
    rdgWavelengthCentering: TRadioGroup;
    procedure drvOutputChange(Sender: TObject);
    procedure FormCreate(Sender: TObject); virtual;
    procedure cmdOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RdbUnitsClick(Sender: TObject);
    procedure RdbDomainClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rdgGridSizeClick(Sender: TObject);
    procedure EdtTempWidthExit(Sender: TObject);
    procedure EdtTChirpExit(Sender: TObject);
    procedure EdtSPMExit(Sender: TObject);
    procedure EdtTCPExit(Sender: TObject);
    procedure edtDelTTExit(Sender: TObject);
    procedure EdtFreqWidthExit(Sender: TObject);
    procedure EdtFLinCExit(Sender: TObject);
    procedure EdtSCPExit(Sender: TObject);
    procedure EdtSQPExit(Sender: TObject);
    procedure EdtDelFExit(Sender: TObject);
    procedure EdtLam0Exit(Sender: TObject);
    procedure CbxTempShapeChange(Sender: TObject);
    procedure CbxFreqShapeChange(Sender: TObject);
    procedure rdgDataSourceClick(Sender: TObject);
    procedure BtnTimeDoubleClick(Sender: TObject);
    procedure dirOutputChange(Sender: TObject);
    procedure rdgNLOClick(Sender: TObject);
    procedure rdgInitialFieldClick(Sender: TObject);
    procedure edtWidthExit(Sender: TObject);
    procedure edtChirpExit(Sender: TObject);
    procedure edtFSPMExit(Sender: TObject);
    procedure cmdReadInFileClick(Sender: TObject);
    procedure chkReadinHeaderClick(Sender: TObject);
    procedure edtReadInLam0Exit(Sender: TObject);
    procedure rdgDownsizeClick(Sender: TObject);
    procedure cmdExperDataFileClick(Sender: TObject);
    procedure EdtNTauExit(Sender: TObject);
    procedure EdtNLamExit(Sender: TObject);
    procedure EdtDelTExit(Sender: TObject);
    procedure EdtLamCenterExit(Sender: TObject);
    procedure EdtLam1Exit(Sender: TObject);
    procedure rdgOrderClick(Sender: TObject);
    procedure chkExperHeaderClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EdtDelLamExit(Sender: TObject);
    procedure stgStratsSelectCell(Sender: TObject; Col, Row: Integer;
      var CanSelect: Boolean);
    procedure stgStratsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure stgStratsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbxStratsChange(Sender: TObject);
    procedure stgStratsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure cmdInsertClick(Sender: TObject);
    procedure cmdDeleteClick(Sender: TObject);
    procedure cmdSpectrumClick(Sender: TObject);
    procedure cmdAutocorrelationClick(Sender: TObject);
    procedure cmdSHGSpectrumClick(Sender: TObject);
    procedure edtPhaseBlankExit(Sender: TObject);
    procedure cmdClearSpecClick(Sender: TObject);
    procedure cmdClearAutoClick(Sender: TObject);
    procedure cmdClearSHGSpecClick(Sender: TObject);
    procedure chkGraphicsClick(Sender: TObject);
    procedure rdgBinningClick(Sender: TObject);
    procedure rdgWavelengthCenteringClick(Sender: TObject);
    procedure cmdHelpClick(Sender: TObject);
    procedure edtReadInFilenameExit(Sender: TObject);
    procedure edtExperFileExit(Sender: TObject);
    procedure edtReadInLam0Change(Sender: TObject);
    procedure cmdCancelClick(Sender: TObject);
    procedure cbxStratsEnter(Sender: TObject);
    procedure cbxStratsExit(Sender: TObject);
    procedure cbxStratsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure chkOverwriteClick(Sender: TObject);
    procedure chkForceMarginalClick(Sender: TObject);
    procedure rdgFormatClick(Sender: TObject);
  private
    { Private declarations }
    mAllCals: TAllCals;
    mRow: integer;
    mCol: integer;
    mAutoSetOutputDir: Boolean;
    procedure ShowComboBox;
    procedure EnableTimeDomain;
    procedure EnableFreqDomain;
    procedure EnablePixelUnits;
    procedure EnableRealUnits;
    procedure RefreshOutputTab;
    procedure RefreshMargTab;
    procedure EnableSpectrum(pEnable: boolean);
    procedure EnableAutocorrelation(pEnable: boolean);
    procedure EnableSHGSpectrum(pEnable: boolean);
  protected
    procedure RefreshFirstGuessTab;
    procedure RefreshReadInTab;
    procedure InitStratTab;
    procedure RefreshStratTab;
    procedure RefreshExperTab; virtual;
  public
    { Public declarations }
    property AllCals: TAllCals read mAllCals write mAllCals;
    procedure RefreshInitialFieldTab;
  end;

var
  frmCalibrations: TfrmCalibrations;


implementation

uses Func1D, Gen2, NLO, MainForm, WindowMgr, EFieldCals, OutputCals;

{$R *.DFM}

procedure TfrmCalibrations.FormCreate(Sender: TObject);
begin
  Top := 100;
  Left := 30;
  //Height := 514;
  //Width := 824;
  mAutoSetOutputDir := True;

  // Set up the Radio Boxes on the main page
  rdgNLO.ItemIndex := rdgNLO.Items.IndexOf('&PG');
  rdgGridSize.ItemIndex := rdgGridSize.Items.IndexOf('64');
  rdgDataSource.ItemIndex := 0;

  pgcCals.ActivePage := tbsInitialField;

  mAllCals := TAllCals.Create;

  // We need to pick up the app path, it seems like the dirOutput always starts
  //  with the app path
  if not WindowManager.DefaultFrogPathNull then
  begin
    dlgOpen.InitialDir := WindowManager.DefaultFrogPath;
    dirOutput.Directory := WindowManager.DefaultFrogPath;
    drvOutput.Drive := (WindowManager.DefaultFrogPath)[1];
  end;

  // Init the mAlgoCals
  // Set the defaults from AlgoCals (where they were read in from the registry)
  rdgGridSize.ItemIndex := rdgGridSize.Items.IndexOf(IntToStr(AllCals.AlgoCals.N));
  rdgDataSource.ItemIndex := Ord(AllCals.AlgoCals.RunTypeEnum);
  rdgNLO.ItemIndex := Ord(AllCals.AlgoCals.NLOType);
  // Synch the rest of the form - these three could be RefreshAlgoCals.
  rdgGridSizeClick(Self);
  rdgDataSourceClick(Self);
  rdgNLOClick(Self);

  RefreshInitialFieldTab;
  RefreshFirstGuessTab;
  RefreshReadInTab;
  RefreshExperTab;
  RefreshOutputTab;
  InitStratTab;
  RefreshStratTab;

  ActiveControl := pgcCals;
end;

procedure TfrmCalibrations.cmdOKClick(Sender: TObject);
begin
  cmdOK.SetFocus;  // Makes sure that OnExit processing for text fields is done
  WindowManager.DefaultFrogPath := dirOutput.Directory;
  ModalResult := mrOk;
  //Close;
end;

procedure TfrmCalibrations.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TfrmCalibrations.FormDestroy(Sender: TObject);
begin
  mAllCals.Free;
  mAllCals := nil;
end;

procedure TfrmCalibrations.rdgGridSizeClick(Sender: TObject);
begin
  AllCals.AlgoCals.N := StrToInt(rdgGridSize.Items[rdgGridSize.ItemIndex]);

  mAllCals.InitialFieldCals.N := AllCals.AlgoCals.N;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.rdgDataSourceClick(Sender: TObject);
begin
  case rdgDataSource.ItemIndex of
    Ord(rteTheory):
      begin
      AllCals.AlgoCals.RunTypeEnum := rteTheory;
      tbsInitialField.TabVisible := True;
      tbsExpData.TabVisible := False;
      tbsInputData.TabVisible := False;
      tbsMarginals.TabVisible := False;
      pgcCals.ActivePage := tbsInitialField;
      RefreshInitialFieldTab;
      RefreshFirstGuessTab;
      end;
    Ord(rteExperimental):
      begin
      AllCals.AlgoCals.RunTypeEnum := rteExperimental;
      tbsInitialField.TabVisible := False;
      tbsExpData.TabVisible := True;
      tbsInputData.TabVisible := False;
      tbsMarginals.TabVisible := True;
      pgcCals.ActivePage := tbsExpData;
      RefreshExperTab;
      end;
    Ord(rteReadIn):
      begin
      AllCals.AlgoCals.RunTypeEnum := rteReadIn;
      tbsInitialField.TabVisible := False;
      tbsExpData.TabVisible := False;
      tbsInputData.TabVisible := True;
      tbsMarginals.TabVisible := False;
      pgcCals.ActivePage := tbsInputData;
      RefreshReadInTab;
      end;
  end;
end;

procedure TfrmCalibrations.rdgNLOClick(Sender: TObject);
begin
  if mAllCals = nil then Exit;
  case rdgNLO.ItemIndex of
    Ord(nlPG): AllCals.AlgoCals.NLOType := nlPG;
    Ord(nlSHG): AllCals.AlgoCals.NLOType := nlSHG;
    Ord(nlSD): AllCals.AlgoCals.NLOType := nlSD;
    Ord(nlTHG): AllCals.AlgoCals.NLOType := nlTHG;
  end;
  RefreshMargTab;
end;

procedure TfrmCalibrations.FormShow(Sender: TObject);
begin
  case rdgDataSource.ItemIndex of
    Ord(rteTheory):
      begin
        RefreshInitialFieldTab;
        RefreshFirstGuessTab;
      end;
    Ord(rteExperimental):
    begin
      RefreshExperTab;
      RefreshMargTab;
    end;
    Ord(rteReadIn):
      RefreshReadInTab;
  end;
  RefreshOutputTab; // saving dir might be changed at save time in AlgoMgr.
end;


// *************** Initial Field tab *********************

procedure TfrmCalibrations.RdbUnitsClick(Sender: TObject);
begin

  case rdbUnits.ItemIndex of
    Ord(unPixel):
    begin
      mAllCals.InitialFieldCals.Units := unPixel;
      EnablePixelUnits;
    end;
    Ord(unReal):
    begin
      mAllCals.InitialFieldCals.Units := unReal;
      EnableRealUnits;
    end;
  else
    raise Exception.Create('Invalid Unit Type');
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.RdbDomainClick(Sender: TObject);
begin
  case rdbDomain.ItemIndex of
    Ord(dtTime):
      begin
        mAllCals.InitialFieldCals.Domain := dtTime;
        //EnableTimeDomain;
      end;
    Ord(dtFreq):
      begin
        mAllCals.InitialFieldCals.Domain := dtFreq;
        //EnableFreqDomain;
      end;
  end;

  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.EnableTimeDomain;
begin
  //RefreshInitialFieldTab;

  EdtFreqWidth.Text := '';
  EdtFLinC.Text := '';

  { Disable appropiate controls }
  CbxFreqShape.Enabled := False;
  LblFreqShape.Enabled := False;
  EdtFreqWidth.Enabled := False;
  LblFWidth.Enabled := False;
  LblFWidthU.Enabled := False;
  EdtFLinC.Enabled := False;
  LblFChirp.Enabled := False;
  LblFChirpU.Enabled := False;
  BtnFreqDouble.Enabled := False;
  if mAllCals.InitialFieldCals.Units = unReal then
  begin
  	LblDelF.Enabled := False;
    EdtDelF.Enabled := False;
  end;

  // Enable appropriate controls
  CbxTempShape.Enabled := True;
  LblTempShape.Enabled := True;
  EdtTempWidth.Enabled := True;
  LblTWidthU.Enabled := True;
  LblTWidth.Enabled := True;
  EdtTChirp.Enabled := True;
  LblTChirp.Enabled := True;
  LblTChirpU.Enabled := True;
  BtnTimeDouble.Enabled := True;
  if mAllCals.InitialFieldCals.Units = unReal then
  begin
  	LblDelT.Enabled := True;
    EdtDelTT.Enabled := True;
  end;
end;

procedure TfrmCalibrations.EnableFreqDomain;
begin
	//RefreshInitialFieldTab;

  EdtTempWidth.Text := '';
  EdtTChirp.Text := '';

	// Enable freq domain Controls}
  CbxFreqShape.Enabled := True;
  LblFreqShape.Enabled := True;
  EdtFreqWidth.Enabled := True;
  LblFWidth.Enabled := True;
  LblFWidthU.Enabled := True;
  EdtFLinC.Enabled := True;
  LblFChirp.Enabled := True;
  LblFChirpU.Enabled := True;
  BtnFreqDouble.Enabled := True;
  if mAllCals.InitialFieldCals.Units = unReal then
  begin
  	LblDelF.Enabled := True;
    EdtDelF.Enabled := True;
  end;

  // Disable time domain
  CbxTempShape.Enabled := False;
  LblTempShape.Enabled := False;
  EdtTempWidth.Enabled := False;
  LblTWidth.Enabled := False;
  LbLTWidthU.Enabled := False;
  EdtTChirp.Enabled := False;
  LblTChirp.Enabled := False;
  LblTChirpU.Enabled := False;
  BtnTimeDouble.Enabled := False;
  if mAllCals.InitialFieldCals.Units = unReal then
  begin
  	LblDelT.Enabled := False;
    EdtDelTT.Enabled := False;
  end;
end;

procedure TfrmCalibrations.EnablePixelUnits;
begin
//  RefreshInitialFieldTab;

  EdtDelTT.Text := '';
  EdtDelF.Text := '';
  EdtLam0.Text := '';

  // Disable appropriate controls
  LblDelT.Enabled := False;
  EdtDelTT.Enabled := False;
  LblDelF.Enabled := False;
  EdtDelF.Enabled := False;
  LblLam0.Enabled := False;
  EdtLam0.Enabled := False;

  // Change labels
  LblTWidthU.Caption := 'Pixels';
  LblTChirpU.Caption := '1/Pix^2';
  LblTCPU.Caption := '1/Pix^3';
  LblFWidthU.Caption := 'Pix';
  LblFChirpU.Caption := '1/Pix^2';
  LblSCPU.Caption := '1/Pix^3';
  LblSQPU.Caption := '1/Pix^4';

  // First Guess Tab
  lblWidthUnits.Caption := 'pix';
  lblChirpUnits.Caption := 'pix^-2';
end;

procedure TfrmCalibrations.EnableRealUnits;
begin
  // Enable appropiate controls
  if mAllCals.InitialFieldCals.Domain = dtTime then
  begin
  	LblDelT.Enabled := True;
  	EdtDelTT.Enabled := True;
  end;

  if mAllCals.InitialFieldCals.Domain = dtFreq then
  begin
  	LblDelF.Enabled := True;
  	EdtDelF.Enabled := True;
  end;

  LblLam0.Enabled := True;
  EdtLam0.Enabled := True;

  // Change labels
  LblTWidthU.Caption := 'fs';
  LblTChirpU.Caption := '1/fs^2';
  LblTCPU.Caption := '1/fs^3';
  LblFWidthU.Caption := 'nm';
  LblFChirpU.Caption := 'fs^2';
  LblSCPU.Caption := 'fs^3';
  LblSQPU.Caption := 'fs^4';

  // First Guess Form
  if mAllCals.InitialFieldCals.Domain = dtTime then lblWidthUnits.Caption := 'fs'
  else if mAllCals.InitialFieldCals.Domain = dtFreq then lblWidthUnits.Caption := 'nm';
  if mAllCals.InitialFieldCals.Domain = dtTime then lblChirpUnits.Caption := 'fs^-2'
  else if mAllCals.InitialFieldCals.Domain = dtFreq then lblChirpUnits.Caption := 'fs^2';
end;

procedure TfrmCalibrations.RefreshInitialFieldTab;
begin
  // Temporal side
  EdtTempWidth.Text := FloatToStrF(mAllCals.InitialFieldCals.TempWidth, ffGeneral, 4, 0);
  EdtTChirp.Text := FloatToStrF(mAllCals.InitialFieldCals.TempChirp, ffGeneral, 4, 0);
  EdtSPM.Text := FloatToStrF(mAllCals.InitialFieldCals.SPM, ffGeneral, 4, 0);
  EdtTCP.Text := FloatToStrF(mAllCals.InitialFieldCals.TCP, ffGeneral, 4, 0);
  if mAllCals.InitialFieldCals.Units = unReal then
    EdtDelTT.Text := FloatToStrF(mAllCals.InitialFieldCals.DelT, ffGeneral, 4, 0);

  // Frequency side
  EdtFreqWidth.Text := FloatToStrF(mAllCals.InitialFieldCals.SpecWidth, ffGeneral, 4, 0);
  EdtFLinC.Text := FloatToStrF(mAllCals.InitialFieldCals.SpecChirp, ffGeneral, 4, 0);
  EdtSCP.Text := FloatToStrF(mAllCals.InitialFieldCals.SCP, ffGeneral, 4, 0);
  EdtSQP.Text := FloatToStrF(mAllCals.InitialFieldCals.SQP, ffGeneral, 4, 0);
  if mAllCals.InitialFieldCals.Units = unReal then
    EdtDelF.Text := FloatToStrF(mAllCals.InitialFieldCals.DelLam, ffGeneral, 4, 0);

  if mAllCals.InitialFieldCals.Units = unReal then
    EdtLam0.Text := FloatToStrF(mAllCals.InitialFieldCals.Lam0, ffGeneral, 4, 0);

  // The domain
  if mAllCals.InitialFieldCals.Domain = dtTime then
    EnableTimeDomain
  else if mAllCals.InitialFieldCals.Domain = dtFreq then
    EnableFreqDomain;

  // Units
  if mAllCals.InitialFieldCals.Units = unReal then
    EnableRealUnits
  else if mAllCals.InitialFieldCals.Units = unPixel then
    EnablePixelUnits;

  // Keep the FirstGuess in synch
  mAllCals.FirstGuessCals.Domain := mAllCals.InitialFieldCals.Domain;
  mAllCals.FirstGuessCals.Units := mAllCals.InitialFieldCals.Units;
  mAllCals.FirstGuessCals.N := mAllCals.InitialFieldCals.N;
  mAllCals.FirstGuessCals.Lam0 := mAllCals.InitialFieldCals.Lam0;
  mAllCals.FirstGuessCals.DelT := mAllCals.InitialFieldCals.DelT;
    // Important: set Lam0 first, if you set Delt, the cals object changes the delLam
    // based on the old lam0, and screws it up if you are in the freq domain.

  if AllCals.AlgoCals.RunTypeEnum = rteTheory then cmdOk.Enabled := True;
end;

procedure TfrmCalibrations.EdtTempWidthExit(Sender: TObject);
begin
  try
    mAllCals.InitialFieldCals.TempWidth := StrToFloat(EdtTempWidth.Text);
  except on EConvertError do
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.EdtTChirpExit(Sender: TObject);
begin
  try
    mAllCals.InitialFieldCals.TempChirp := StrToFloat(EdtTChirp.Text);
  except on EConvertError do
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.EdtSPMExit(Sender: TObject);
begin
  try
    mAllCals.InitialFieldCals.SPM := StrToFloat(EdtSPM.Text);
  except on EConvertError do
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.EdtTCPExit(Sender: TObject);
begin
  try
    mAllCals.InitialFieldCals.TCP := StrToFloat(EdtTCP.Text);
  except on EConvertError do
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.edtDelTTExit(Sender: TObject);
begin
  try
    mAllCals.InitialFieldCals.DelT := StrToFloat(EdtDelTT.Text);
  except on EConvertError do
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.EdtFreqWidthExit(Sender: TObject);
begin
  try
    mAllCals.InitialFieldCals.SpecWidth := StrToFloat(EdtFreqWidth.Text);
  except on EConvertError do
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.EdtFLinCExit(Sender: TObject);
begin
  try
    mAllCals.InitialFieldCals.SpecChirp := StrToFloat(EdtFLinC.Text);
  except on EConvertError do
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.EdtSCPExit(Sender: TObject);
begin
  try
    mAllCals.InitialFieldCals.SCP := StrToFloat(EdtSCP.Text);
  except on EConvertError do
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.EdtSQPExit(Sender: TObject);
begin
  try
    mAllCals.InitialFieldCals.SQP := StrToFloat(EdtSQP.Text);
  except on EConvertError do
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.EdtDelFExit(Sender: TObject);
begin
  try
    mAllCals.InitialFieldCals.DelLam := StrToFloat(EdtDelF.Text);
  except on EConvertError do
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.EdtLam0Exit(Sender: TObject);
begin
  try
    mAllCals.InitialFieldCals.Lam0 := StrToFloat(EdtLam0.Text);
  except on EConvertError do
  end;
  RefreshInitialFieldTab;
end;

procedure TfrmCalibrations.CbxTempShapeChange(Sender: TObject);
begin
  case CbxTempShape.ItemIndex of
    Ord(fsGaussian): mAllCals.InitialFieldCals.TempShape := fsGaussian;
    Ord(fsSech): mAllCals.InitialFieldCals.TempShape := fsSech;
    Ord(fsSuperGaussian): mAllCals.InitialFieldCals.TempShape := fsSuperGaussian;
  end;
end;

procedure TfrmCalibrations.CbxFreqShapeChange(Sender: TObject);
begin
  case CbxFreqShape.ItemIndex of
    Ord(fsGaussian): mAllCals.InitialFieldCals.SpecShape := fsGaussian;
    Ord(fsSech): mAllCals.InitialFieldCals.SpecShape := fsSech;
    Ord(fsSuperGaussian): mAllCals.InitialFieldCals.SpecShape := fsSuperGaussian;
  end;
end;

procedure TfrmCalibrations.BtnTimeDoubleClick(Sender: TObject);
begin
  frmGen2 := TFrmGen2.Create(Application);
  try
    frmGen2.SetUp(mAllCals.InitialFieldCals);
    frmGen2.ShowModal;
  finally
    frmGen2.Free;
  end;
end;


// ******************* Outputs Tab ***********************
procedure TfrmCalibrations.drvOutputChange(Sender: TObject);
begin
  dirOutput.Drive := drvOutput.Drive;
end;

procedure TfrmCalibrations.dirOutputChange(Sender: TObject);
begin
  AllCals.OutputCals.FilePath := dirOutput.Directory;
  // Only reset the AutoSetOutputDir if the directory was changed from
  //  outputs tab, not from the other tabs
  if(pgcCals.ActivePage = tbsOutputs) then mAutoSetOutputDir := False;
end;

procedure TfrmCalibrations.RefreshOutputTab;
begin
  edtPhaseBlank.Text := FloatToStrF(AllCals.OutputCals.PhaseBlank, ffGeneral, 2, 0);
  AllCals.OutputCals.FilePath := dirOutput.Directory;
  dirOutput.Drive := drvOutput.Drive;
  chkGraphics.Checked := AllCals.OutputCals.DisplayGraphics;
  chkOverwrite.Checked := AllCals.OutputCals.Overwrite;
  rdgFormat.ItemIndex := Ord(AllCals.OutputCals.OutputFormat);
end;

procedure TfrmCalibrations.edtPhaseBlankExit(Sender: TObject);
begin
  try
    AllCals.OutputCals.PhaseBlank := StrToFloat(edtPhaseBlank.Text);
  except on EConvertError do
  end;
  RefreshOutputTab;
end;

procedure TfrmCalibrations.chkGraphicsClick(Sender: TObject);
begin
  AllCals.OutputCals.DisplayGraphics := chkGraphics.Checked;
end;

procedure TfrmCalibrations.chkOverwriteClick(Sender: TObject);
begin
	AllCals.OutputCals.Overwrite := chkOverwrite.Checked;
end;

procedure TfrmCalibrations.rdgFormatClick(Sender: TObject);
begin
 // this sucks, but how else do you get from int to enum?
 if rdgFormat.ItemIndex = Ord(ofClassic) then
     AllCals.OutputCals.OutputFormat := ofClassic
 else if rdgFormat.ItemIndex = Ord(ofFemtosoft) then
     AllCals.OutputCals.OutputFormat := ofFemtosoft;
 AllCals.OutputCals.SaveFormat;
end;

// ******************* First Guess Tab ***********************
procedure TfrmCalibrations.rdgInitialFieldClick(Sender: TObject);
begin
  case rdgInitialField.ItemIndex of
    Ord(eiNoise): mAllCals.FirstGuessCals.InitType := eiNoise;
    Ord(eiPhaseNoise): mAllCals.FirstGuessCals.InitType := eiPhaseNoise;
    Ord(eiIntensityNoise): mAllCals.FirstGuessCals.InitType := eiIntensityNoise;
    Ord(eiCustom): mAllCals.FirstGuessCals.InitType := eiCustom;
  end;

  if mAllCals.FirstGuessCals.InitType = eiCustom then
  begin
    lblWidth.Enabled := True;
    edtWidth.Enabled := True;
    edtWidth.Text := '10';
    lblWidthUnits.Enabled := True;
    lblChirp.Enabled := True;
    edtChirp.Enabled := True;
    edtChirp.Text := '0';
    lblChirpUnits.Enabled := True;
    if mAllCals.FirstGuessCals.Domain = dtTime then
    begin
      lblSPM.Enabled := True;
      edtFSPM.Enabled := True;
      edtFSPM.Text := '0';
      lblFSPMUnits.Enabled := True;
    end;
  end
  else
  begin
    lblWidth.Enabled := False;
    edtWidth.Enabled := False;
    edtWidth.Text := '';
    lblWidthUnits.Enabled := False;
    lblChirp.Enabled := False;
    edtChirp.Enabled := False;
    edtChirp.Text := '';
    lblChirpUnits.Enabled := False;
    lblSPM.Enabled := False;
    edtFSPM.Enabled := False;
    edtFSPM.Text := '';
    lblFSPMUnits.Enabled := False;
  end;

end;

procedure TfrmCalibrations.RefreshFirstGuessTab;
begin
  rdgInitialFieldClick(Self);
  if mAllCals.FirstGuessCals.Domain = dtTime then
  begin
    edtWidth.Text := FloatToStrF(mAllCals.FirstGuessCals.TempWidth, ffGeneral, 4, 0);
    edtChirp.Text := FloatToStrF(mAllCals.FirstGuessCals.TempChirp, ffGeneral, 4, 0);
    edtFSPM.Text := FloatToStrF(mAllCals.FirstGuessCals.SPM, ffGeneral, 4, 0);
  end
  else
  begin
    edtWidth.Text := FloatToStrF(mAllCals.FirstGuessCals.SpecWidth, ffGeneral, 4, 0);
    edtChirp.Text := FloatToStrF(mAllCals.FirstGuessCals.SpecChirp, ffGeneral, 4, 0);
  end;
end;

procedure TfrmCalibrations.edtWidthExit(Sender: TObject);
begin
  try
    if mAllCals.FirstGuessCals.Domain = dtTime then
      mAllCals.FirstGuessCals.TempWidth := StrToFloat(edtWidth.Text)
    else
      mAllCals.FirstGuessCals.SpecWidth := StrToFloat(edtWidth.Text);
  except on EConvertError do
  end;
  RefreshFirstGuessTab;
end;

procedure TfrmCalibrations.edtChirpExit(Sender: TObject);
begin
  try
    if mAllCals.FirstGuessCals.Domain = dtTime then
      mAllCals.FirstGuessCals.TempChirp := StrToFloat(edtChirp.Text)
    else
      mAllCals.FirstGuessCals.SpecChirp := StrToFloat(edtChirp.Text);
  except on EConvertError do
  end;
  RefreshFirstGuessTab;
end;

procedure TfrmCalibrations.edtFSPMExit(Sender: TObject);
begin
  try
    if mAllCals.FirstGuessCals.Domain = dtTime then
      mAllCals.FirstGuessCals.SPM := StrToFloat(edtFSPM.Text);
  except on EConvertError do
  end;
  RefreshFirstGuessTab;
end;

// ******************* Input Data Tab ***********************
//  for ReadIn  - crummy name

procedure TfrmCalibrations.RefreshReadInTab;
begin
  edtReadInFilename.Text := AllCals.ReadIn.ReadInFile;
  edtReadInLam0.Text := FloatToStrF(AllCals.ReadIn.Lam0, ffGeneral, 4, 0);
  rdgDownsize.ItemIndex := Ord(AllCals.ReadIn.DownsizeType);
  if AllCals.ReadIn.ReadInFile <> AllCals.ReadIn.NullFile then
  begin
    chkReadInHeader.Enabled := True;
    cmdOk.Enabled := True;
  end
  else
  begin
    chkReadInHeader.Enabled := False;
    if AllCals.AlgoCals.RunTypeEnum = rteReadIn then cmdOk.Enabled := False;
  end;
  if (AllCals.ReadIn.Lam0 = 0) and (AllCals.AlgoCals.RunTypeEnum = rteReadIn) then
    cmdOk.Enabled := False;
  chkReadInHeader.Checked := AllCals.ReadIn.UseHeader;
end;

procedure TfrmCalibrations.edtReadInFilenameExit(Sender: TObject);
begin
  AllCals.ReadIn.ReadInFile := edtReadInFilename.Text;
end;

procedure TfrmCalibrations.cmdReadInFileClick(Sender: TObject);
begin
  dlgOpen.Title := 'Select file containing E-field';
  if dlgOpen.Execute then
  begin
    AllCals.ReadIn.ReadInFile := dlgOpen.Filename;
    dlgOpen.InitialDir := ExtractFileDir(dlgOpen.FileName);
    //  Keep the output directories in synch
    if mAutoSetOutputDir then
    begin
      drvOutput.Drive := (ExtractFileDrive(AllCals.ReadIn.ReadInFile))[1];
      dirOutput.Directory := ExtractFileDir(AllCals.ReadIn.ReadInFile);
    end;
  end;
  RefreshReadInTab;
end;

procedure TfrmCalibrations.chkReadinHeaderClick(Sender: TObject);
begin
  AllCals.ReadIn.UseHeader := chkReadinHeader.Checked;
  RefreshReadInTab;
end;

procedure TfrmCalibrations.edtReadInLam0Exit(Sender: TObject);
begin
  try
    AllCals.ReadIn.Lam0 := StrToFloat(edtReadInLam0.Text);
  except on EConvertError do
  end;
  RefreshReadInTab;
end;

procedure TfrmCalibrations.edtReadInLam0Change(Sender: TObject);
begin
  // This is just to add some user friendliness, I hate to duplicate the code.
  try
    if (AllCals.ReadIn.ReadInFile <> AllCals.ReadIn.NullFile)
                  and (StrToFloat(edtReadInLam0.Text) <> 0) then
      cmdOK.Enabled := True
    else
      cmdOK.Enabled := False;
  except
  end;
end;

procedure TfrmCalibrations.rdgDownsizeClick(Sender: TObject);
begin
  case rdgDownsize.ItemIndex of
    Ord(dtInterpolate): AllCals.ReadIn.DownsizeType := dtInterpolate;
    Ord(dtTruncate): AllCals.ReadIn.DownsizeType := dtTruncate;
  end;
  RefreshReadInTab;
end;


// ******************* Experimental Data Tab ***********************

procedure TfrmCalibrations.RefreshExperTab;
begin
  edtExperFile.Text := AllCals.Exper.ExperFile;
  edtNTau.Text := FloatToStrF(AllCals.Exper.NTau, ffGeneral, 4, 0);
  edtNLam.Text := FloatToStrF(AllCals.Exper.NLam, ffGeneral, 4, 0);
  edtDelT.Text := FloatToStrF(AllCals.Exper.DelT, ffGeneral, 4, 0);
  edtDelLam.Text := FloatToStrF(AllCals.Exper.DelLam, ffGeneral, 4, 0);
  edtLamCenter.Text := FloatToStrF(AllCals.Exper.Lam0, ffGeneral, 4, 0);
  edtLam1.Text := FloatToStrF(AllCals.Exper.Lam1, ffGeneral, 4, 0);
  rdgOrder.ItemIndex := Ord(AllCals.Exper.DataOrder);
  rdgBinning.ItemIndex := Ord(AllCals.Exper.Binning);
  rdgWavelengthCentering.ItemIndex := Ord(AllCals.Exper.WavelengthCentering);
  chkExperHeader.Checked := AllCals.Exper.UseHeader;

  // Can we allow them to click OK?
  if AllCals.Exper.ExperFile <> AllCals.Exper.NullFile then
  begin
    chkExperHeader.Enabled := True;
    if (AllCals.Exper.NTau <> 0) and (AllCals.Exper.NLam <> 0) and (AllCals.Exper.DelT <> 0) and (AllCals.Exper.DelLam <>0) and (AllCals.Exper.Lam0 <> 0 ) then
      cmdOk.Enabled := True
    else
      cmdOK.Enabled := False;
  end
  else
  begin
    chkExperHeader.Enabled := False;
    if AllCals.AlgoCals.RunTypeEnum = rteExperimental then cmdOk.Enabled := False;
  end;
end;

procedure TfrmCalibrations.edtExperFileExit(Sender: TObject);
begin
  AllCals.Exper.ExperFile := edtExperFile.Text;
end;

procedure TfrmCalibrations.cmdExperDataFileClick(Sender: TObject);
begin
  dlgOpen.Title := 'Select file with experimental FROG trace';
  if dlgOpen.Execute then
  begin
    AllCals.Exper.ExperFile := dlgOpen.FileName;
    dlgOpen.InitialDir := ExtractFileDir(dlgOpen.FileName);
    //  Keep the output directories in synch
    if mAutoSetOutputDir then
    begin
      drvOutput.Drive := (ExtractFileDrive(AllCals.Exper.ExperFile))[1];
      dirOutput.Directory := ExtractFileDir(AllCals.Exper.ExperFile);
    end;
    // uncheck the header
    AllCals.Exper.UseHeader := False;
  end;

  RefreshExperTab;
end;

procedure TfrmCalibrations.EdtNTauExit(Sender: TObject);
begin
  try
    AllCals.Exper.NTau := StrToInt(edtNTau.Text);
  except on EConvertError do
  end;
  RefreshExperTab;
end;

procedure TfrmCalibrations.EdtNLamExit(Sender: TObject);
begin
  try
    AllCals.Exper.NLam := StrToInt(edtNLam.Text);
  except on EConvertError do
  end;
  RefreshExperTab;
end;

procedure TfrmCalibrations.EdtDelTExit(Sender: TObject);
begin
  try
    AllCals.Exper.DelT := StrToFloat(edtDelT.Text);
  except on EConvertError do
  end;
  RefreshExperTab;
end;

procedure TfrmCalibrations.EdtDelLamExit(Sender: TObject);
begin
  try
    AllCals.Exper.DelLam := StrToFloat(edtDelLam.Text);
  except on EConvertError do
  end;
end;

procedure TfrmCalibrations.EdtLamCenterExit(Sender: TObject);
begin
  try
    if edtLamCenter.Text <> FloatToStrF(AllCals.Exper.Lam0, ffGeneral, 4, 0) then
      AllCals.Exper.Lam0 := StrToFloat(edtLamCenter.Text);
  except on EConvertError do
  end;
  RefreshExperTab;
end;

procedure TfrmCalibrations.EdtLam1Exit(Sender: TObject);
begin
  try
    // Keep this formatting synchronized with that in RefreshExperTab!
    if edtLam1.Text <> FloatToStrF(AllCals.Exper.Lam1, ffGeneral, 4, 0) then
      AllCals.Exper.Lam1 := StrToFloat(edtLam1.Text);
  except on EConvertError do
  end;
  RefreshExperTab;
end;

procedure TfrmCalibrations.rdgOrderClick(Sender: TObject);
begin
  case rdgOrder.ItemIndex of
    Ord(doWavelength): AllCals.Exper.DataOrder := doWavelength;
    Ord(doDelay): AllCals.Exper.DataOrder := doDelay;
  end;
  RefreshExperTab;
end;

procedure TfrmCalibrations.rdgBinningClick(Sender: TObject);
begin
  case rdgBinning.ItemIndex of
    Ord(bnWavelength): AllCals.Exper.Binning := bnWavelength;
    Ord(bnFrequency): AllCals.Exper.Binning := bnFrequency;
  end;
  RefreshExperTab;
end;

procedure TfrmCalibrations.rdgWavelengthCenteringClick(Sender: TObject);
begin
  case rdgWavelengthCentering.ItemIndex of
    Ord(wcCenterOfMass): AllCals.Exper.WavelengthCentering := wcCenterOfMass;
    Ord(wcPeak): AllCals.Exper.WavelengthCentering := wcPeak;
  end;
  RefreshExperTab;
end;

procedure TfrmCalibrations.chkExperHeaderClick(Sender: TObject);
begin
  AllCals.Exper.UseHeader := chkExperHeader.Checked;
  RefreshExperTab;
end;

// ******************* Strategies Tab ***********************
var
  NAME_COL: integer;
  SWITCH_COL: integer;
  NEW_FIELD_COL: integer;
  SPECIAL_COL: integer;

procedure TfrmCalibrations.RefreshStratTab;
var
  i, col, row: integer;
begin
  stgStrats.RowCount := stgStrats.FixedRows + AllCals.StrategyCals.NumStrategies;
  stgStrats.Refresh;
  for i := 1 to AllCals.StrategyCals.NumStrategies do
  begin
    row := i + stgStrats.FixedRows - 1;
    col := stgStrats.FixedCols - 1;
    if col = 0 then stgStrats.Cells[col, row] := IntToStr(i);
    stgStrats.Cells[NAME_COL, row] := AllCals.StrategyCals.Name[i];
    stgStrats.Cells[SWITCH_COL, row] := IntToStr(AllCals.StrategyCals.SwitchAfter[i]);
    stgStrats.Cells[NEW_FIELD_COL, row] := FloatToStrF(AllCals.StrategyCals.NewField[i], ffGeneral, 3, 2);
    stgStrats.Cells[SPECIAL_COL, row] := FloatToStrF(AllCals.StrategyCals.Special[i], ffGeneral, 3, 2);
  end;
end;

procedure TfrmCalibrations.stgStratsSelectCell(Sender: TObject; Col,
  Row: Integer; var CanSelect: Boolean);
begin
  try
    if mCol = SWITCH_COL then
      AllCals.StrategyCals.SwitchAfter[mRow + 1 - stgStrats.FixedRows] := StrToInt(stgStrats.Cells[mCol, mRow])
    else if mCol = NEW_FIELD_COL then
      AllCals.StrategyCals.NewField[mRow + 1 - stgStrats.FixedRows] := StrToFloat(stgStrats.Cells[mCol, mRow])
    else if mCol = SPECIAL_COL then
      AllCals.StrategyCals.Special[mRow + 1 - stgStrats.FixedRows] := StrToInt(stgStrats.Cells[mCol, mRow]);
  except on EConvertError do
  end;
  RefreshStratTab;

  mRow := Row;
  mCol := Col;

  if mCol = NAME_COL then
    stgStrats.EditorMode := False
  else
    stgStrats.EditorMode := True;
end;

procedure TfrmCalibrations.stgStratsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
 junk: Boolean;
begin
  if Key = VK_ESCAPE then
  begin
    cbxStrats.Visible := False;
    stgStrats.SetFocus;
  end;

  if Key = VK_RETURN then
    stgStratsSelectCell(Self, mCol, mRow, junk);
end;

procedure TfrmCalibrations.stgStratsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if mCol = NAME_COL then ShowComboBox;
end;

procedure TfrmCalibrations.ShowComboBox;
var
  i, colLeft, rowTop: integer;
begin
  colLeft := 0;
  // Fixed cols have to be handled specially
  for i := stgStrats.LeftCol - 1 to mCol - 1 do
    colLeft := colLeft + stgStrats.ColWidths[i] + stgStrats.GridLineWidth;
  rowTop := 0;
  for i := stgStrats.TopRow - 1 to mRow - 1 do
    rowTop := rowTop + stgStrats.RowHeights[i] + stgStrats.GridLineWidth;

  cbxStrats.Left := stgStrats.Left + colLeft + 1;
  cbxStrats.Top := stgStrats.Top + rowTop + 1;
  cbxStrats.Width := stgStrats.ColWidths[mCol];
  for i := 0 to cbxStrats.DropDownCount - 1 do
    if cbxStrats.Items[i] = stgStrats.Cells[mCol, mRow] then
    begin
      cbxStrats.ItemIndex := i;
      Break;
    end;
  cbxStrats.Visible := True;
  cbxStrats.SetFocus;
end;

// StratIndex := mRow + 1 - FixedRows
// mRow := StratIndex - 1 + FixedRows
procedure TfrmCalibrations.cbxStratsChange(Sender: TObject);
begin
  AllCals.StrategyCals.Name[mRow + 1 - stgStrats.FixedRows] := cbxStrats.Items[cbxStrats.ItemIndex];
  cbxStrats.Visible := False;
  stgStrats.SetFocus;
  RefreshStratTab;
end;

procedure TfrmCalibrations.InitStratTab;
var
  i: integer;
begin
  NAME_COL := stgStrats.FixedCols;
  SWITCH_COL := stgStrats.FixedCols + 1;
  NEW_FIELD_COL := stgStrats.FixedCols + 2;
  SPECIAL_COL := stgStrats.FixedCols + 3;

  cbxStrats.Clear;
  for i := 0 to AllCals.StrategyCals.TotalNumStrategies - 1 do
  begin
    cbxStrats.Items.Add(AllCals.StrategyCals.NameOfStrategy(i));
  end;

  if stgStrats.FixedCols = 1 then
    stgStrats.ColWidths[0] := 30;

  if stgStrats.FixedRows = 1 then
  begin
    stgStrats.Cells[NAME_COL,0] := 'Strategy';
    stgStrats.Cells[SWITCH_COL,0] := 'Min. Iterations';
    stgStrats.Cells[NEW_FIELD_COL,0] := 'Start Field';
    stgStrats.Cells[SPECIAL_COL,0] := 'Shortcut';
  end;
end;

procedure TfrmCalibrations.stgStratsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  {try
    if ACol = SWITCH_COL then
      AllCals.StrategyCals.SwitchAfter[ARow + 1 - stgStrats.FixedRows] := StrToInt(Value)
    else if ACol = NEW_FIELD_COL then
      AllCals.StrategyCals.NewField[ARow + 1 - stgStrats.FixedRows] := StrToFloat(Value)
    else if ACol = SPECIAL_COL then
      AllCals.StrategyCals.Special[ARow + 1 - stgStrats.FixedRows] := StrToFloat(Value);
  except on EConvertError do
  end;
  RefreshStratTab;}
end;

procedure TfrmCalibrations.cmdInsertClick(Sender: TObject);
begin
    AllCals.StrategyCals.InsertStrategy(mRow + 1 - stgStrats.FixedRows);
    RefreshStratTab;
end;

procedure TfrmCalibrations.cmdDeleteClick(Sender: TObject);
begin
    AllCals.StrategyCals.DeleteStrategy(mRow + 1 - stgStrats.FixedRows);
    RefreshStratTab;
end;

procedure TfrmCalibrations.cbxStratsEnter(Sender: TObject);
begin
  cmdCancel.Cancel := false;
end;

procedure TfrmCalibrations.cbxStratsExit(Sender: TObject);
begin
  cmdCancel.Cancel := True;
end;

procedure TfrmCalibrations.cbxStratsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    cbxStrats.Visible := False;
    stgStrats.SetFocus;
  end;
end;



// ******************* Marginals Tab ***********************

procedure TfrmCalibrations.RefreshMargTab;
var
  nlo: TNLOInteraction;
begin
  edtSpectrum.Text := AllCals.MargCals.SpecCals.ReadInFile;
  edtAutocorrelation.Text := AllCals.MargCals.AutoCals.ReadInFile;
  edtSHGSpectrum.Text := AllCals.MargCals.SHGSpecCals.ReadInFile;
  chkForceMarginal.Checked := AllCals.MargCals.ForceMarginals;

  nlo := AllCals.AlgoCals.NLOClass.Create;
  EnableSpectrum(nlo.NeedSpectrum);
  EnableAutocorrelation(nlo.NeedAutocorrelation);
  EnableSHGSpectrum(nlo.NeedSHGSpectrum);
  nlo.Free;
end;

procedure TfrmCalibrations.EnableSpectrum(pEnable: boolean);
begin
       lblSpectrum.Enabled := pEnable;
       edtSpectrum.Enabled := pEnable;
       cmdSpectrum.Enabled := pEnable;
       cmdClearSpec.Enabled := pEnable;
end;
procedure TfrmCalibrations.EnableAutocorrelation(pEnable: boolean);
begin
       lblAutocorrelation.Enabled := pEnable;
       edtAutocorrelation.Enabled := pEnable;
       cmdAutocorrelation.Enabled := pEnable;
       cmdClearAuto.Enabled := pEnable;
end;
procedure TfrmCalibrations.EnableSHGSpectrum(pEnable: boolean);
begin
       lblSHGSPectrum.Enabled := pEnable;
       edtSHGSpectrum.Enabled := pEnable;
       cmdSHGSpectrum.Enabled := pEnable;
       cmdClearSHGSpec.Enabled := pEnable;
end;

procedure TfrmCalibrations.cmdSpectrumClick(Sender: TObject);
begin
  dlgOpen.Title := 'Select file with experimental spectrum';
  if dlgOpen.Execute then
  begin
    AllCals.MargCals.SpecCals.ReadInFile := dlgOpen.FileName;
    dlgOpen.InitialDir := ExtractFileDir(dlgOpen.FileName);
  end;
  RefreshMargTab;
end;

procedure TfrmCalibrations.cmdAutocorrelationClick(Sender: TObject);
begin
  dlgOpen.Title := 'Select file with experimental autocorrelation';
  if dlgOpen.Execute then
  begin
    AllCals.MargCals.AutoCals.ReadInFile := dlgOpen.FileName;
    dlgOpen.InitialDir := ExtractFileDir(dlgOpen.FileName);
  end;
  RefreshMargTab;
end;

procedure TfrmCalibrations.cmdSHGSpectrumClick(Sender: TObject);
begin
  dlgOpen.Title := 'Select file with experimental SHG spectrum';
  if dlgOpen.Execute then
  begin
    AllCals.MargCals.SHGSpecCals.ReadInFile := dlgOpen.FileName;
    dlgOpen.InitialDir := ExtractFileDir(dlgOpen.FileName);
  end;
  RefreshMargTab;
end;

procedure TfrmCalibrations.cmdClearSpecClick(Sender: TObject);
begin
  AllCals.MargCals.SpecCals.ReadInFile := AllCals.MargCals.SpecCals.NullFile;
  RefreshMargTab;
end;

procedure TfrmCalibrations.cmdClearAutoClick(Sender: TObject);
begin
  AllCals.MargCals.AutoCals.ReadInFile := AllCals.MargCals.AutoCals.NullFile;
  RefreshMargTab;
end;

procedure TfrmCalibrations.cmdClearSHGSpecClick(Sender: TObject);
begin
  AllCals.MargCals.SHGSpecCals.ReadInFile := AllCals.MargCals.SHGSpecCals.NullFile;
  RefreshMargTab;
end;

procedure TfrmCalibrations.chkForceMarginalClick(Sender: TObject);
begin
  AllCals.MargCals.ForceMarginals := chkForceMarginal.Checked;
end;




procedure TfrmCalibrations.cmdHelpClick(Sender: TObject);
begin
  Application.HelpContext(pgcCals.ActivePage.HelpContext);
end;

procedure TfrmCalibrations.cmdCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;



end.
