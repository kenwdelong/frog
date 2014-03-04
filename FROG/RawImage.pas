unit RawImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, Math, RawData, FROGColorTable;

type
  TFrmRaw = class(TForm)
    MnuRaw: TMainMenu;
    Noise1: TMenuItem;
    FullSpectrum1: TMenuItem;
    SingleValue1: TMenuItem;
    Edge1: TMenuItem;
    FourierFilter1: TMenuItem;
    GridData1: TMenuItem;
    PnlMag: TPanel;
    Data1: TMenuItem;
    Extract1: TMenuItem;
    ResampleTimeAxis1: TMenuItem;
    Help1: TMenuItem;
    pnlPixel: TPanel;
    SpatialChirp1: TMenuItem;
    CorrectSpatialChirp1: TMenuItem;
    CleanupPixesl1: TMenuItem;
    ImgRaw: TImage;
    procedure FormCreate(Sender: TObject);
    procedure GridData1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FullSpectrum1Click(Sender: TObject);
    procedure SingleValue1Click(Sender: TObject);
    procedure Edge1Click(Sender: TObject);
    procedure FourierFilter1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ImgRawMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImgRawMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImgRawMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Extract1Click(Sender: TObject);
    procedure ResampleTimeAxis1Click(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpatialChirp1Click(Sender: TObject);
    procedure CorrectSpatialChirp1Click(Sender: TObject);
    procedure CleanupPixesl1Click(Sender: TObject);
  private
    { Private declarations }
    mOrigin, mMovePt: TPoint;
    mMaxDataPixel: TPoint;
    mDrawing: Boolean;
    mImgCreated: Boolean;
    mRawData: TRawData;
    mMagx, mMagy, mDeMagx, mDeMagy: integer;
    mColorTable: TFROGColorTable;
    procedure DrawRawTrace;
    procedure CalculateMags;
    procedure SetRawData(pRawData: TRawData);
    procedure DoDrawRawTrace;
  public
    { Public declarations }
    property RawData: TRawData write SetRawData;
    property Magx: integer read mMagx write mMagx;
    property Magy: integer read mMagy write mMagy;
    property DeMagx: integer read mDeMagx write mDeMagx;
    property DeMagy: integer read mDeMagy write mDeMagy;
  end;

var
  FrmRaw: TFrmRaw;

implementation

uses
	Numerics;

{$R *.DFM}

procedure TFrmRaw.FormCreate(Sender: TObject);
var
	Bitmap: TBitmap;
begin
	mDrawing := False;
	mImgCreated := False;
	mOrigin := Point(0, 0);
  mMovePt := Point(0, 0);
	Height := 300 + 61;
	Width := 300 + 12;
	Bitmap := TBitmap.Create;
	Bitmap.Width := ImgRaw.Width;
	Bitmap.Height := ImgRaw.Height;
	//ImgRaw.Picture.Graphic := Bitmap;  //Appears to work either way
  ImgRaw.Picture.Bitmap := Bitmap;
  mImgCreated := True;
  Extract1.Enabled := False;
  mColorTable := TFROGColorTable.Create;
  mMagx := 1;
  mMagy := 1;
  mDeMagx := 1;
  mDeMagy := 1;
end;

procedure TFrmRaw.SetRawData(pRawData: TRawData);
begin
	mRawData := pRawData;
  if not (mRawData = nil) then
  	CalculateMags;
end;

procedure TFrmRaw.GridData1Click(Sender: TObject);
begin
	mImgCreated := False;   // So that it doesn't resize while closing
	ModalResult := mrOK;
end;

procedure TFrmRaw.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	Action := caHide;
  // if we got here from GridData, it's ok
  // if they pressed the little 'x', then it's a cancel
  //if ModalResult = 0 then ModalResult := mrCancel;
  //if ModalResult = 0 then Action := caNone;
  //if ModalResult = mrCancel then Action := caNone;
  // When you hit the little x, ModalResult is mrCancel (2).
end;

procedure TFrmRaw.FullSpectrum1Click(Sender: TObject);
begin
	mRawData.FullSpectrumNoiseSubtraction;
  DrawRawTrace;
end;

procedure TFrmRaw.SingleValue1Click(Sender: TObject);
begin
	mRawData.PixelNoiseSubtraction;
	DrawRawTrace;
end;

procedure TFrmRaw.Edge1Click(Sender: TObject);
begin
	mRawData.EdgeNoiseSubtraction;
	DrawRawTrace;
end;

procedure TFrmRaw.SpatialChirp1Click(Sender: TObject);
var
   slope: double;
begin
     slope := mRawData.CalculateAndCorrectSpatialChirp(false);
     ShowMessage('The spatial chirp slope is ' + FloatToStrF(slope, ffGeneral, 4, 0) + ' pixels per pixel.');
end;

procedure TFrmRaw.CorrectSpatialChirp1Click(Sender: TObject);
var
   slope: double;
begin
     slope := mRawData.CalculateAndCorrectSpatialChirp(true);
     ShowMessage('The spatial chirp slope is ' + FloatToStrF(slope, ffGeneral, 4, 0) + ' pixels per pixel.');
     DrawRawTrace;
end;

procedure TFrmRaw.FourierFilter1Click(Sender: TObject);
var
	Text: string;
 ClickedOK: Boolean;
 rad, res: double;
 code: integer;
begin
	Text := '1.0';
 rad := 1.0;
 ClickedOK := InputQuery('Fourier Filter Radius',
 						'Input Fourier Filter half-max radius (1.0 = N/2): ', Text);

 // Check that it's a double
 Val(Text, res, code);
 if code = 0 then
 	rad := res;

 if ClickedOK then
	begin
		mRawData.LowPassNoiseSubtraction(rad);
		DrawRawTrace;
	end;
end;

procedure TFrmRaw.FormResize(Sender: TObject);
var
	ARect: TRect;
const
  VERTICAL_OVERHEAD: Integer = 78;
  HORIZONTAL_OVERHEAD: Integer = 12;
begin
	if mImgCreated then
	begin
	  ImgRaw.Visible := False;
  	with ImgRaw.Canvas do
    begin
    	CopyMode := cmWhiteness;
      ARect := Rect(0, 0, Width, Height);
      CopyRect(ARect, ImgRaw.Canvas, ARect);
      CopyMode := cmSrcCopy;
  	end;
 	  ImgRaw.Height := Height - VERTICAL_OVERHEAD;
 	  ImgRaw.Width := Width - HORIZONTAL_OVERHEAD;
 	  ImgRaw.Picture.Graphic.Height := Height - VERTICAL_OVERHEAD;
 	  ImgRaw.Picture.Graphic.Width := Width - HORIZONTAL_OVERHEAD;

 	//ImgRaw.Height := ClientHeight;
 	//ImgRaw.Width := ClientWidth;
 	//ImgRaw.Picture.Graphic.Height := ClientHeight;
 	//ImgRaw.Picture.Graphic.Width := ClientWidth;
   {
   pnlMag.Caption := 'Frame(' + IntToStr(Height) + ',' + IntToStr(Width) +
                     '),ImgRaw(' + IntToStr(ImgRaw.Height) + ',' + IntToStr(ImgRaw.Width) +
                     '),Graphic(' + IntToStr(ImgRaw.Picture.Graphic.Height) + ',' + IntToStr(ImgRaw.Picture.Graphic.Width) +')' +
                     ',Client(' + IntToStr(ClientHeight) + ',' + IntToStr(ClientWidth) + ')';
   }
		DrawRawTrace;
    ImgRaw.Visible := True;
	end;
end;

procedure TFrmRaw.ImgRawMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	// Remove an old rectangle if there
	if (mOrigin.x <> 0) and (mOrigin.y <> 0) and (mMovePt.x <> 0) and (mMovePt.y <> 0) then
	begin
  	ImgRaw.Canvas.Pen.Mode := pmNotXor;
    ImgRaw.Canvas.Rectangle(mOrigin.X, mOrigin.Y, mMovePt.X, mMovePt.Y);
    ImgRaw.Canvas.Pen.Mode := pmCopy;
  end;
	mOrigin := Point(X, Y);
  mMovePt := Point(X, Y);
  mDrawing := True;
end;

procedure TFrmRaw.ImgRawMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
	Tau, Lam: integer;
  Val: double;
begin
	if mDrawing then
  begin
  	if X > mMaxDataPixel.X then X := mMaxDataPixel.X;
  	if Y > mMaxDataPixel.Y then Y := mMaxDataPixel.Y;
	  if X < 0 then X := 0;
  	if Y < 0 then Y := 0;
  	ImgRaw.Canvas.Pen.Mode := pmNotXor;
    ImgRaw.Canvas.Rectangle(mOrigin.X, mOrigin.Y, mMovePt.X, mMovePt.Y);
    ImgRaw.Canvas.Rectangle(mOrigin.X, mOrigin.Y, X, Y);
    mMovePt := Point(X, Y);
    ImgRaw.Canvas.Pen.Mode := pmCopy;
  end
  else
  begin
  	// Set the pixel value
    Tau := ((DeMagx*X) div Magx);
	  Lam := ((DeMagy*Y) div Magy);
    if (Tau < 0) or (Tau >= mRawData.NTau) or (Lam < 0) or (Lam >= mRawData.NLam) then
    	pnlPixel.Caption := 'Pixel'
    else
    begin
			Val := mRawData.Vals^[Tau*mRawData.NLam + Lam];
			pnlPixel.Caption := 'Pixel(' + IntToStr(Tau) + ',' + IntToStr(Lam) + '): ' + FloatToStrF(Val, ffGeneral, 4, 0);
    end;
  end;
end;

procedure TFrmRaw.ImgRawMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	if X > mMaxDataPixel.X then X := mMaxDataPixel.X;
 	if Y > mMaxDataPixel.Y then Y := mMaxDataPixel.Y;
  if X < 0 then X := 0;
  if Y < 0 then Y := 0;
	ImgRaw.Canvas.Pen.Mode := pmNotXor;
  ImgRaw.Canvas.Rectangle(mOrigin.X, mOrigin.Y, mMovePt.X, mMovePt.Y);
  ImgRaw.Canvas.Rectangle(mOrigin.X, mOrigin.Y, X, Y);
  mMovePt := Point(X, Y);
  ImgRaw.Canvas.Pen.Mode := pmCopy;
	mDrawing := False;

  Extract1.Enabled := True;
end;

procedure TFrmRaw.Extract1Click(Sender: TObject);
var
  ARect: TRect;
begin
	mRawData.ExtractData(mOrigin, mMovePt, MagX, MagY, DeMagX, DeMagY);
  with ImgRaw.Canvas do
  begin
  	CopyMode := cmWhiteness;
  	ARect := Rect(0, 0, Width, Height);
    CopyRect(ARect, ImgRaw.Canvas, ARect);
    CopyMode := cmSrcCopy;
  end;
  DrawRawTrace;
  mOrigin := Point(0,0);
  mMovePt := Point(0,0);
  Extract1.Enabled := False;
end;

procedure TFrmRaw.ResampleTimeAxis1Click(Sender: TObject);
var
	Text: string;
	ClickedOK: Boolean;
	res: double;
	code: integer;
  ARect: TRect;
begin
	ClickedOK := InputQuery('Resample Time Delay',
 						'New Time Delay (currently ' + FloatToStr(mRawData.DelT)+ ' fs): ', Text);
 if not ClickedOK then Exit;
 // Val converts a string into a number
	Val(Text, res, code);
	if not (code = 0) then
  begin
  	MessageDlg('Not a valid input.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if Abs(res) < Abs(mRawData.DelT) then
  begin
  	MessageDlg('New delay must be larger than old delay.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if ClickedOK then
  begin
  	mRawData.ResampleDelay(res);
  	with ImgRaw.Canvas do
  	begin
  		CopyMode := cmWhiteness;
  		ARect := Rect(0, 0, Width, Height);
    	CopyRect(ARect, ImgRaw.Canvas, ARect);
    	CopyMode := cmSrcCopy;
  	end;
    DrawRawTrace;
  end;
end;

procedure TFrmRaw.Help1Click(Sender: TObject);
begin
	Application.HelpContext(9);
end;

procedure TFrmRaw.FormDestroy(Sender: TObject);
begin
  mColorTable.Free;
end;

procedure TfrmRaw.CalculateMags;
var
	tMagx, tMagy, tDeMagx, tDeMagy, tX, tY: integer;
begin
		// This code was in Extract Data
		{ Magnification value }
    tMagx := 0;
    tMagy := 0;
    if (not (mRawData.NTau = 0)) and (not (mRawData.NLam = 0)) then
    begin
    	tMagx := ImgRaw.Width div mRawData.NTau;
    	tMagy := ImgRaw.Height div mRawData.NLam;
    end;
  	if tMagx = 0 then tMagx := 1;
  	if tMagy = 0 then tMagy := 1;

  	{ Demagnification values }
    tDeMagx := 0;
    tDeMagy := 0;
    if ( (not (ImgRaw.Width = 0)) and (not (ImgRaw.Height = 0))) then
    begin
    	tDeMagx := Ceil(mRawData.NTau/ImgRaw.Width);
    	tDeMagy := Ceil(mRawData.NLam/ImgRaw.Height);
    end;
    if tDeMagx = 0 then tDeMagx := 1;
	  if tDeMagy = 0 then tDeMagy := 1;

    Magx := tMagx;
    Magy := tMagy;
    DeMagx := tDeMagx;
    DeMagy := tDeMagy;

    // The  pixel at the end of the data
		tX := ((mRawData.NTau*Magx) div DeMagx) - 1;
    tY := ((MRawData.NLam*Magy) div DeMagy) - 1;
    mMaxDataPixel := Point(tX, tY);
end;

procedure TfrmRaw.DrawRawTrace;
begin
	if mRawData = nil then Exit;
  if ModalResult = mrCancel then Exit;
	DoDrawRawTrace;
end;

procedure TfrmRaw.DoDrawRawTrace;
var
   ARect: TRect;
   x, y, i, j, k, l: longint;
   ColorVal: integer;
   max, RawVal: double;
   NTau, NLam: integer;
   Vals: PArray;
begin
		NTau := mRawData.NTau;
    NLam := mRawData.NLam;
    Vals := mRawData.Vals;
    CalculateMags;

			{ Start by setting the image canvas to black }
     with ImgRaw.Canvas do
     begin
          CopyMode := cmBlackness;
          ARect := Rect(0, 0, (NTau*Magx) div DeMagX, (NLam*Magy) div DeMagY);
          CopyRect(ARect, ImgRaw.Canvas, ARect);
          CopyMode := cmSrcCopy;
     end;

     { Find the peak of the trace }
     max := 0;
     for i:= 0 to (NLam*NTau-1) do
     	if(Vals^[i] > max) then max := Vals^[i];

     { Draw the new raw FROG trace, non-black pixels only! }
     // Loop over screen pixels
     ImgRaw.Visible := False;
     for i := 0 to ((NTau*MagX) div DeMagX) - 1 do
     	for j := 0 to ((NLam*Magy) div DeMagY) - 1 do
       	begin
         	RawVal := 0.0;
         	for k := 0 to DeMagX -1 do
           	for l := 0 to DeMagY - 1 do
             	begin
               	x := ((DeMagX*i) div Magx) + k;
                y := ((DeMagY*j) div Magy) + l;
         				RawVal := RawVal + Vals^[x*NLam + y];
              end;
          RawVal := RawVal/(DeMagX*DeMagY);
          if RawVal > 0 then
           	ColorVal := Trunc(100*Sqrt(RawVal/max))
          else
           	ColorVal := 0;
          if(ColorVal > 0) then
           	ImgRaw.Canvas.Pixels[i, j] := mColorTable.ColorTable[ColorVal];
        end;
     ImgRaw.Visible := True;
     PnlMag.Caption := 'MagX: ' + FloatToStr(Magx/DemagX) +
                        ',  MagY: ' + FloatToStr(Magy/DeMagY);
     
end;



procedure TFrmRaw.CleanupPixesl1Click(Sender: TObject);
begin
     mRawData.PixelCleanup;
     DrawRawTrace;
end;

end.
