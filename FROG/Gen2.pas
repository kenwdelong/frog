unit Gen2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, EFieldCals, Func1D;

type
  TFrmGen2 = class(TForm)
    Label1: TLabel;
    EdtPhase: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EdtSep: TEdit;
    LblSep: TLabel;
    Label6: TLabel;
    EdtChirp: TEdit;
    Panel1: TPanel;
    LblSPMCub: TLabel;
    EdtSPMCub: TEdit;
    LblCubQuad: TLabel;
    EdtCubQuad: TEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    LblChirp: TLabel;
    LblSPMCubU: TLabel;
    LblCubQuadU: TLabel;
    EdtHeight: TEdit;
    Label5: TLabel;
    EdtWidth: TEdit;
    LblWidth: TLabel;
    BtnHelp: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure EdtPhaseExit(Sender: TObject);
    procedure EdtHeightExit(Sender: TObject);
    procedure EdtSepExit(Sender: TObject);
    procedure EdtChirpExit(Sender: TObject);
    procedure EdtSPMCubExit(Sender: TObject);
    procedure EdtCubQuadExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EdtWidthExit(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
  private
    { Private declarations }
    FLabelRight: integer;
    mCals: TEFieldCals;
    procedure SetDomain;
		procedure SetUnits;
  public
    { Public declarations }
    procedure SetUp(pCals: TEFieldCals);
    procedure RefreshForm;
  end;

var
  FrmGen2: TFrmGen2;

implementation

{$R *.DFM}

procedure TFrmGen2.FormCreate(Sender: TObject);
var
	count: integer;
begin
	FLabelRight := EdtPhase.Left - 11;
  //Height := 311;
  //Width := 322;
  Left := 169;
  Top := 165;

  // The label controls have problems with large and small fonts
  // This seems to help
  for count := 0 to ComponentCount - 1 do
  	if Components[count] is TLabel then
    begin
    	(Components[count] as TLabel).AutoSize := False;
    	(Components[count] as TLabel).AutoSize := True;
    end;

  // No RefreshForm here because mCals is nil
end;

procedure TFrmGen2.SetUp(pCals: TEFieldCals);
begin
	mCals := pCals;
  SetDomain;
  SetUnits;

  // Set up default values
  if mCals.Domain = dtTime then
    if mCals.TempHeight2 = 0 then
    begin
      mCals.TempHeight2 := 1.0;
      mCals.TempSep2 := 12.0;
      mCals.TempWidth2 := 6.0;
    end
  else
    if mCals.SpecHeight2 = 0 then
    begin
      mCals.SpecHeight2 := 1.0;
      mCals.SpecSep2 := 12.0;
      mCals.SpecWidth2 := 6.0;
    end;

  RefreshForm;
end;

procedure TFrmGen2.SetDomain;
begin
	case mCals.Domain of
  	dtTime:
    begin
    	LblSPMCub.Caption := 'SPM';
      LblCubQuad.Caption := 'Cubic Phase';
    end;
    dtFreq:
    begin
    	LblSPMCub.Caption := 'Cubic Phase';
      LblCubQuad.Caption := 'Quadratic Phase';
    end;
  end;

  LblCubQuad.Left := FLabelRight - LblCubQuad.Width;
  LblSPMCub.Left := FLabelRight - LblSPMCub.Width;
end;

procedure TFrmGen2.SetUnits;
begin
	if mCals.Domain = dtTime then
  begin
		case mCals.Units of
  		unPixel:
    	begin
    		LblSep.Caption := 'Pixels';
        LblWidth.Caption := 'Pixels';
      	LblChirp.Caption := '1/pix^2';
      	LblSPMCubU.Caption := 'rad';
      	LblCubQuadU.Caption := '1/pix^3';
    	end;
    	unReal:
    	begin
    		LblSep.Caption := 'fs';
        LblWidth.Caption := 'fs';
      	LblChirp.Caption := '1/fs^2';
      	LblSPMCubU.Caption := 'rad';
      	LblCubQuadU.Caption := '1/fs^3';
    	end;
  	end;
  end

  else if mCals.Domain = dtFreq then
  begin
		case mCals.Units of
  		unPixel:
    	begin
    		LblSep.Caption := 'Pixels';
        LblWidth.Caption := 'Pixels';
      	LblChirp.Caption := '1/pix^2';
      	LblSPMCubU.Caption := '1/pix^3';
      	LblCubQuadU.Caption := '1/pix^4';
    	end;
    	unReal:
    	begin
    		LblSep.Caption := 'nm';
        LblWidth.Caption := 'nm';
      	LblChirp.Caption := 'fs^2';
      	LblSPMCubU.Caption := 'fs^3';
      	LblCubQuadU.Caption := 'fs^4';
    	end;
  	end;
  end;
end;

procedure TFrmGen2.BtnOKClick(Sender: TObject);
begin
	ModalResult := mrOK;
end;

procedure TFrmGen2.BtnCancelClick(Sender: TObject);
begin
  case mCals.Domain of
    dtTime: mCals.TempHeight2 := 0.0;
    dtFreq: mCals.SpecHeight2 := 0.0;
  end;
	ModalResult := mrCancel;
end;

procedure TFrmGen2.EdtPhaseExit(Sender: TObject);
begin
  try
    case mCals.Domain of
      dtTime: mCals.TempPhase2 := StrToFloat(EdtPhase.Text);
      dtFreq: mCals.SpecPhase2 := StrToFloat(EdtPhase.Text);
    end;
  except on EConvertError do
  end;
  RefreshForm;
end;

procedure TFrmGen2.EdtHeightExit(Sender: TObject);
begin
  try
    case mCals.Domain of
      dtTime: mCals.TempHeight2 := StrToFloat(EdtHeight.Text);
      dtFreq: mCals.SpecHeight2 := StrToFloat(EdtHeight.Text);
    end;
  except on EConvertError do
  end;
  RefreshForm;
end;

procedure TFrmGen2.EdtSepExit(Sender: TObject);
begin
  try
    case mCals.Domain of
      dtTime: mCals.TempSep2 := StrToFloat(EdtSep.Text);
      dtFreq: mCals.SpecSep2 := StrToFloat(EdtSep.Text);
    end;
  except on EConvertError do
  end;
  RefreshForm;
end;

procedure TFrmGen2.EdtChirpExit(Sender: TObject);
begin
  try
    case mCals.Domain of
      dtTime: mCals.TempChirp2 := StrToFloat(EdtChirp.Text);
      dtFreq: mCals.SpecChirp2 := StrToFloat(EdtChirp.Text);
    end;
  except on EConvertError do
  end;
  RefreshForm;
end;

procedure TFrmGen2.EdtSPMCubExit(Sender: TObject);
begin
  try
    case mCals.Domain of
      dtTime: mCals.SPM2 := StrToFloat(EdtSPMCub.Text);
      dtFreq: mCals.SCP2 := StrToFloat(EdtSPMCub.Text);
    end;
  except on EConvertError do
  end;
  RefreshForm;
end;

procedure TFrmGen2.EdtCubQuadExit(Sender: TObject);
begin
  try
    case mCals.Domain of
      dtTime: mCals.TCP2 := StrToFloat(EdtCubQuad.Text);
      dtFreq: mCals.SQP2 := StrToFloat(EdtCubQuad.Text);
    end;
  except on EConvertError do
  end;
  RefreshForm;
end;

procedure TFrmGen2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caHide;
end;

procedure TFrmGen2.EdtWidthExit(Sender: TObject);
begin
  try
    case mCals.Domain of
      dtTime: mCals.TempWidth2 := StrToFloat(EdtWidth.Text);
      dtFreq: mCals.SpecWidth2 := StrToFloat(EdtWidth.Text);
    end;
  except on EConvertError do
  end;
  RefreshForm;
end;

procedure TFrmGen2.BtnHelpClick(Sender: TObject);
begin
	Application.HelpContext(BtnHelp.HelpContext);
end;

procedure TFrmGen2.RefreshForm;
begin
  if mCals.Domain = dtTime then
  begin
    edtPhase.Text := FloatToStrF(mCals.TempPhase2, ffGeneral, 4, 0);
    edtHeight.Text := FloatToStrF(mCals.TempHeight2, ffGeneral, 4, 0);
    edtSep.Text := FloatToStrF(mCals.TempSep2, ffGeneral, 4, 0);
    edtWidth.Text := FloatToStrF(mCals.TempWidth2, ffGeneral, 4, 0);
    edtChirp.Text := FloatToStrF(mCals.TempChirp2, ffGeneral, 4, 0);
    edtSPMCub.Text := FloatToStrF(mCals.SPM2, ffGeneral, 4, 0);
    edtCubQuad.Text := FloatToStrF(mCals.TCP2, ffGeneral, 4, 0);
  end
  else if mCals.Domain = dtFreq then
  begin
    edtPhase.Text := FloatToStrF(mCals.SpecPhase2, ffGeneral, 4, 0);
    edtHeight.Text := FloatToStrF(mCals.SpecHeight2, ffGeneral, 4, 0);
    edtSep.Text := FloatToStrF(mCals.SpecSep2, ffGeneral, 4, 0);
    edtWidth.Text := FloatToStrF(mCals.SpecWidth2, ffGeneral, 4, 0);
    edtChirp.Text := FloatToStrF(mCals.SpecChirp2, ffGeneral, 4, 0);
    edtSPMCub.Text := FloatToStrF(mCals.SCP2, ffGeneral, 4, 0);
    edtCubQuad.Text := FloatToStrF(mCals.SQP2, ffGeneral, 4, 0);
  end;
end;

end.
