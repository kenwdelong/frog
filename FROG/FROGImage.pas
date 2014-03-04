unit FROGImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, FrogTrace, FROGColorTable, Func1D;

type
  TFrmFrogImage = class(TForm)
    SbxImage: TScrollBox;
    ImgImage: TImage;
    PnlPhase: TPanel;
    popCopy: TPopupMenu;
    Copy1: TMenuItem;
    Print1: TMenuItem;
    PrintDialog1: TPrintDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Print1Click(Sender: TObject);
  private
    { Private declarations }
    ImgCreated: Boolean;
    FResized : Boolean;
    mTrace: TFrogTrace;
    mColorTable: TFrogColorTable;
    mE: TEField;
    mEk: TEField;
    mPlotInitialField: boolean;
  public
    { Public declarations }
    procedure DrawFROGTrace(pTrace: TFROGTrace);
    procedure PlotField(pE, pEk: TEField; pPlotInitialField: Boolean);
    property Resized: Boolean read FResized;
  end;

  // I tried to make two subclasses here, one for FROG traces and one for fields, but I couldn't
  // make the FormResize event get called polymorphically.  So here's a hack.

implementation

uses ClipBrd, Printers, Math;

{$R *.DFM}

procedure TFrmFrogImage.FormCreate(Sender: TObject);
var
	Bitmap: TBitmap;
begin
	ImgCreated := False;
	Height := 190;
	Width := 150;

	//ImgImage.Height := Height - 61;
	//ImgImage.Width := Width - 12;
	Bitmap := TBitmap.Create;
	Bitmap.Width := ImgImage.Width;
	Bitmap.Height := ImgImage.Height + 0;
	ImgImage.Picture.Graphic := Bitmap;
  ImgCreated := True;
  FResized := False;

  ImgImage.PopupMenu := popCopy;
  mColorTable := TFrogColorTable.Create;
end;


procedure TFrmFrogImage.FormResize(Sender: TObject);
var
	NewBitmap: TBitmap;
  ARect: TRect;
begin
	SbxImage.Height := Height - 57;
	SbxImage.Width := Width - 8;
  if ImgCreated then
  begin
  	with ImgImage.Canvas do
    begin
    	 CopyMode := cmWhiteness;
      ARect := Rect(0, 0, Width, Height);
      CopyRect(ARect, ImgImage.Canvas, ARect);
      CopyMode := cmSrcCopy;
    end;
  	ImgImage.Picture.Graphic.Height := ImgImage.Height;
  	ImgImage.Picture.Graphic.Width := ImgImage.Width;
  end;

  FResized := True;

  if not (mTrace = nil) then
     DrawFROGTrace(nil);

  if not (mEk = nil) then
     PlotField(nil, nil, mPlotInitialField);

end;

procedure TFrmFrogImage.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Action := caNone;
end;

procedure TFrmFrogImage.Copy1Click(Sender: TObject);
begin
  Clipboard.Assign(ImgImage.Picture.Bitmap) // was ImgImage.Picture
end;

procedure TFrmFrogImage.Print1Click(Sender: TObject);
var
  aRect: TRect;
begin
  if PrintDialog1.Execute then
  begin
    aRect := Rect(50, 50, 1389, 1220);
    Printer.BeginDoc;
    //Printer.Canvas.Draw(50, 50, ImgImage.Picture.Graphic);
    Printer.Canvas.StretchDraw(aRect, ImgImage.Picture.Graphic);
    //Printer.Canvas.Draw(50, 50, ImgImage.Picture.Bitmap);
    Printer.EndDoc;
  end;
end;



procedure TFrmFrogImage.DrawFROGTrace(pTrace: TFROGTrace);
var
	ARect: TRect;
  x, y, i, j: longint;
  ColorVal, Mag, DeMag, N, k, l: integer;
  max, Fact, Val: double;
begin
     if not (pTrace = nil) then
     begin
          if mTrace = nil then
             mTrace := TRetrievedFrogTrace.Create(pTrace.N);
          mTrace.CopyFrom(pTrace);
     end;
     if mTrace = nil then
        Exit;
     N := mTrace.N;

     { Magnification value }
  	if ImgImage.Height > ImgImage.Width then
    begin
     	Mag := ImgImage.Width div N;
       DeMag := Ceil(N/ImgImage.Width);
    end
    else
    begin
     	Mag := ImgImage.Height div N;
       DeMag := Ceil(N/ImgImage.Height);
    end;
    if Mag = 0 then Mag := 1;
    if DeMag = 0 then DeMag := 1;

    { Start by setting the image canvas to black }
    with ImgImage.Canvas do
    begin
    	CopyMode := cmBlackness;
      ARect := Rect(0, 0, N*Mag div DeMag, N*Mag div DeMag);
      CopyRect(ARect, ImgImage.Canvas, ARect);
      CopyMode := cmSrcCopy;
    end;

    { Find the peak of the trace }
    if not (pTrace = nil) then
        max := pTrace.Max
    else
        max := mTrace.Max;

    { Draw the new FROG trace, non-black pixels only! }
    Fact := 100.0/sqrt(max);
    ImgImage.Visible := False;
    if DeMag = 1 then
    begin
    	for x := 0 to N - 1 do
    		for y := 0 to N - 1 do
       		begin
         		ColorVal := Trunc(Fact*Sqrt(mTrace.Vals^[x*N + y]));
          	if(ColorVal > 0) then
           		for i := 0 to Mag - 1 do
             		for j := 0 to Mag - 1 do
        					ImgImage.Canvas.Pixels[Mag*x + i, Mag*(N - y - 1) + j] := mColorTable.ColorTable[ColorVal];
        	end;
    end
    else //if DeMag > 1 then
    begin
    	for i := 0 to (N div DeMag) - 1 do
      	for j := 0 to (N div DeMag) - 1 do
        begin
        	Val := 0.0;
          for k := 0 to DeMag - 1 do
          	for l := 0 to DeMag - 1 do
            	begin
            		x := DeMag*i + k;
              	y := DeMag*j + l;
              	Val := Val + mTrace.Vals^[x*N + y];
            	end;
          Val := Val/(DeMag*DeMag);
          ColorVal := Trunc(Fact*Sqrt(Val));
          if ColorVal > 0 then
          	ImgImage.Canvas.Pixels[i, (N div DeMag) - 1 - j] := mColorTable.ColorTable[ColorVal];
        end;
    end;
    ImgImage.Visible := True;
    ImgImage.Update;
    FResized := False;
end;

procedure TFrmFROGImage.FormDestroy(Sender: TObject);
begin
     if not (mTrace = nil) then
     begin
        mTrace.Free;
        mTrace := nil;
     end;
end;



{ ************************ Fields *********************** }
const EPS = 1e-4;
procedure TFrmFrogImage.PlotField(pE, pEk: TEField; pPlotInitialField: Boolean);
var
	i, HH, N: integer;
	XStep, phasemax, phasemaxk, intenmax, intenmaxk: double;
begin;

      if not (pEk = nil) then
      begin
           if mEk = nil then
           begin
                mEk := TEField.Create(pEk.N);
                mE := TEField.Create(pEk.N);
           end;

           if not (pEk.N = mEk.N) then
           begin
                mEk.Free;
                mEk := TEField.Create(pEk.N);
                mE.Free;
                mE := TEField.Create(pEk.N);
           end;

           mEk.CopyFrom(pEk);
           if pPlotInitialField then
              mE.CopyFrom(pE);
      end;
      if mEk = nil then
         Exit;


  N := mEk.N;
  mPlotInitialField := pPlotInitialField;
	 XStep := ImgImage.Width/N;     // The X step size

  // Max intensity for original field
  intenmax := 0.0;
  phasemax := 0.0;
  if pPlotInitialField then
  begin
	  mE.SetIntensityAndPhase;
    mE.ClipPhase(EPS);
	  intenmax := mE.IntenMax;
    phasemax := mE.PhaseMax;
  end;

  mEk.SetIntensityAndPhase;
  mEk.ClipPhase(EPS);
  intenmaxk := mEk.IntenMax;
  phasemaxk := mEk.PhaseMax;

  if phasemaxk > phasemax then  phasemax := phasemaxk;
  if phasemax = 0 then phasemax := 1;

	 ImgImage.Visible := False;  // Speeds it up.
  with ImgImage do
  begin
  	HH := Height - 1;
    // Draw the original Intesity
    if pPlotInitialField then
    begin
    	 Canvas.Pen.Color := clBlack;
      Canvas.MoveTo(0, Trunc(HH*(1 - mE.Intensity^[0]/intenmax)));
      for i := 1 to N - 1 do
       	Canvas.LineTo(Trunc(i*XStep), Trunc(HH*(1 - mE.Intensity^[i]/intenmax)));
    end;

    // Draw the reconstructed intensity
    Canvas.Pen.Color := clGreen;
    Canvas.MoveTo(0, Trunc(HH*(1 - mEk.Intensity^[0]/intenmaxk)));
    for i := 1 to N - 1 do
    	Canvas.LineTo(Trunc(i*XStep), Trunc(HH*(1 - mEk.Intensity^[i]/intenmaxk)));

		// Plot the original phase.
    PnlPhase.Caption := 'Max. phase: '
    		+ FloatToStrF(phasemax, ffGeneral, 3, 1) + ' rad';
    PnlPhase.Update;
    if pPlotInitialField then
    begin
    	Canvas.Pen.Color := clBlue;
      Canvas.MoveTo(0, Trunc(HH*(phasemax - mE.Phase^[0])/phasemax));
      for i := 1 to N - 1 do
      	Canvas.LineTo(Trunc(i*XStep), Trunc(HH*(phasemax - mE.Phase^[i])/phasemax));
    end;

    // reconstructed phase
    Canvas.Pen.Color := clRed;
    Canvas.MoveTo(0, Trunc(HH*(phasemax - mEk.Phase^[0])/phasemax));
    for i := 1 to N - 1 do
    	Canvas.LineTo(Trunc(i*XStep), Trunc(HH*(phasemax - mEk.Phase^[i])/phasemax));

  end;  // end of with FieldGraph.ImgImage do
  ImgImage.Visible := True;
  ImgImage.Update;
  
end;



end.
