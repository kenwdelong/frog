unit Results;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
	TResults = record
		BestTW: double;
  	BestSW: double;
  	BestAW: double;
  	BestFTB: double;
  	BestRTB: double;
  	BestTL: double;
  	BestSL: double;
		OrigTW: double;
  	OrigSW: double;
  	OrigAW: double;
  	OrigFTB: double;
  	OrigRTB: double;
  	OrigTL: double;
  	OrigSL: double;
	end;

  TFrmResults = class(TForm)
    LblError: TLabel;
    GbxOrig: TGroupBox;
    LblOTW: TLabel;
    LblOSW: TLabel;
    LblOAW: TLabel;
    LblOFTB: TLabel;
    LblORTB: TLabel;
    LblOTL: TLabel;
    LblOSL: TLabel;
    BtnHelp: TButton;
    GbxWidths: TGroupBox;
    LblTW: TLabel;
    LblSW: TLabel;
    LblAW: TLabel;
    gbxTimeBandwidth: TGroupBox;
    LblFTB: TLabel;
    LblRTB: TLabel;
    LblTL: TLabel;
    LblSL: TLabel;
    gbxRetrieved: TGroupBox;
    lblRTW: TLabel;
    lblRSW: TLabel;
    lblRAW: TLabel;
    lblRFTB: TLabel;
    lblRRTB: TLabel;
    lblRTL: TLabel;
    lblRSL: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnHelpClick(Sender: TObject);
  private
    { Private declarations }
    mErrorMessage: string;
  public
    { Public declarations }
  	procedure DisplayRetrieved(pResults: TResults; pBestError: double;
							pPixelUnits: Boolean);
    procedure DisplayOriginal(pResults:TResults; pPixelUnits: Boolean);
    procedure Clear;
	end;

var
  FrmResults: TFrmResults;

implementation

{$R *.DFM}

procedure TFrmResults.FormCreate(Sender: TObject);
begin
  // If I don't do this, the form goes nuts at default.  I think this is
  // because it's got a bsSizable border.
  Height := 261;
  Width := 358;

  mErrorMessage := 'The FROG Error was: ';
  Clear;
end;

procedure TFrmResults.DisplayRetrieved(pResults: TResults; pBestError: double;
							pPixelUnits: Boolean);
var
	SuffixT, SuffixF: string;
begin
  if pPixelUnits then
  begin
  	SuffixT := ' pix';
    SuffixF := ' pix';
  end
  else
	begin
  	SuffixT := ' fs';
    SuffixF := ' nm';
  end;

	LblError.Caption := mErrorMessage + FloatToStrF(pBestError, FFGeneral, 4, 0);

  { Retrieved Field }
  LblRTW.Caption := FloatToStrF(pResults.BestTW, ffGeneral, 4, 0) + SuffixT;
  LblRSW.Caption := FloatToStrF(pResults.BestSW, ffGeneral, 4, 0) + SuffixF;
  LblRAW.Caption := FloatToStrF(pResults.BestAW, ffGeneral, 4, 0) + SuffixT;
  LblRFTB.Caption := FloatToStrF(pResults.BestFTB, ffGeneral, 4, 0);
  LblRRTB.Caption := FloatToStrF(pResults.BestRTB, ffGeneral, 4, 0);
  LblRTL.Caption := FloatToStrF(pResults.BestTL, ffGeneral, 4, 0);
  LblRSL.Caption := FloatToStrF(pResults.BestSL, ffGeneral, 4, 0);

end;

procedure TFrmResults.DisplayOriginal(pResults:TResults; pPixelUnits: Boolean);
var
	SuffixT, SuffixF: string;
begin
  if pPixelUnits then
  begin
  	SuffixT := ' pix';
    SuffixF := ' pix';
  end
  else
	begin
  	SuffixT := ' fs';
    SuffixF := ' nm';
  end;

  LblOTW.Caption := FloatToStrF(pResults.OrigTW, ffGeneral, 4, 0) + SuffixT;
  LblOSW.Caption := FloatToStrF(pResults.OrigSW, ffGeneral, 4, 0) + SuffixF;
  LblOAW.Caption := FloatToStrF(pResults.OrigAW, ffGeneral, 4, 0) + SuffixT;
  LblOFTB.Caption := FloatToStrF(pResults.OrigFTB, ffGeneral, 4, 0);
  LblORTB.Caption := FloatToStrF(pResults.OrigRTB, ffGeneral, 4, 0);
  LblOTL.Caption := FloatToStrF(pResults.OrigTL, ffGeneral, 4, 0);
  LblOSL.Caption := FloatToStrF(pResults.OrigSL, ffGeneral, 4, 0);
end;

procedure TFrmResults.BtnOKClick(Sender: TObject);
begin
	ModalResult := mrOK;
end;

procedure TFrmResults.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caNone;
end;

procedure TFrmResults.BtnHelpClick(Sender: TObject);
begin
	Application.HelpContext(BtnHelp.HelpContext);
end;

procedure TFrmResults.Clear;
begin
  LblRTW.Caption := '';
  LblRSW.Caption := '';
  LblRAW.Caption := '';
  LblRFTB.Caption := '';
  LblRRTB.Caption := '';
  LblRTL.Caption := '';
  LblRSL.Caption := '';
  LblOTW.Caption := '';
  LblOSW.Caption := '';
  LblOAW.Caption := '';
  LblOFTB.Caption := '';
  LblORTB.Caption := '';
  LblOTL.Caption := '';
  LblOSL.Caption := '';


  LblError.Caption := mErrorMessage;
end;

end.
