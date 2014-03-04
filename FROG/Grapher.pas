unit Grapher;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs,
  StdCtrls, ExtCtrls, Math, Func1D, FrogTrace, FROGImage, FrogObj, Numerics,
  RawImage, FieldGraph, FROGColorTable;

type
	TGrapher = class(TFrogObject)
  private
    mWindowsCreated: Boolean;
    mESpec: TEField;
  	TargetFROGTrace: TfrmFrogImage;
    ReconFROGTrace: TfrmFrogImage;
    //EFieldWin: TfrmFrogImage;
    //SpecWin: TfrmFrogImage;
    EFieldWin: TfrmFieldGraph;
    SpecWin: TfrmFieldGraph;
    mColorTable: TFROGColorTable;
		procedure ClearWin(FieldGraph: TfrmFrogImage);
 		procedure DrawFROGTrace(Trace: TFROGTrace; DestWindow: TfrmFrogImage);
		procedure PlotField(E, Ek: TEField; FieldGraph: TfrmFrogImage; pPlotInitialField: Boolean);
  public
    procedure InitWindows;
  	procedure InitGraphics(E, Ek: TEField; frogI, frogIk: TFROGTrace; pPlotInitialField: Boolean);
		procedure UpdateGraphics(k: integer; E, Ek: TEField; frogI, frogIk: TFROGTrace; pPlotInitialField: Boolean);
    procedure CleanUp;
    function RightHandEdge: integer;
    function BottomEdge: integer;
    function LeftHandEdge: integer;
    function TopEdge: integer;
 		constructor Create;
    destructor Destroy; override;
  end;


implementation

uses MainForm, WindowMgr;

constructor TGrapher.Create;
begin
	inherited Create;

  mColorTable := TFROGColorTable.Create();

  mESpec := nil;
  mWindowsCreated := False;
end;

destructor TGrapher.Destroy;
begin
	mColorTable.Free;
  inherited;
end;

procedure TGrapher.InitWindows;
begin
  if mWindowsCreated then Exit;

 {Create the FROG Trace Window and plot the trace}
 TargetFROGTrace := TfrmFrogImage.Create(frmMainForm);
 TargetFROGTrace.Top := 5;
 TargetFROGTrace.Left := 5;
 TargetFROGTrace.Caption := 'Original';
 WindowManager.InitializeThisWindow(TargetFROGTrace);

 { Create the E-field window}
 //EFieldWin := TfrmFrogImage.Create(frmMainForm);
 EFieldWin := TfrmFieldGraph.Create(frmMainForm);
 EFieldWin.Left := TargetFROGTrace.Left + TargetFROGTrace.Width + 5;
 EFieldWin.Top := TargetFROGTrace.Top;
 EFieldWin.Caption := 'E-field';
 EFieldWin.HelpContext := 29;
 WindowManager.InitializeThisWindow(EFieldWin);

 { Create the spectrum window}
 //SpecWin := TfrmFrogImage.Create(frmMainForm);
 SpecWin := TfrmFieldGraph.Create(frmMainForm);
 SpecWin.Left := EFieldWin.Left;
 SpecWin.Top := TargetFROGTrace.Top + TargetFROGTrace.Height + 5;
 SpecWin.Caption := 'Spectrum';
 SpecWin.HelpContext := 29;
 WindowManager.InitializeThisWindow(SpecWin);

 {Create the Recon FROG Trace Window}
 ReconFROGTrace := TfrmFrogImage.Create(frmMainForm);
 ReconFROGTrace.Top := SpecWin.Top;
 ReconFROGTrace.Left := TargetFROGTrace.Left;
 ReconFROGTrace.Caption := 'Retrieved';
 WindowManager.InitializeThisWindow(ReconFROGTrace);

  mWindowsCreated := True;
end;

procedure TGrapher.InitGraphics(E, Ek: TEField; frogI, frogIk: TFROGTrace; pPlotInitialField: Boolean);
begin
  TargetFrogTrace.DrawFROGTrace(frogI);
  EFieldWin.Clear;
  SpecWin.Clear;

  if pPlotInitialField then
  begin
    mESpec.Free;   // If there isn't one, it should be nil from the constructor
    mESpec := TEField.Create(E.N);
    mESpec.CopyFrom(E);
    mESpec.Transform;
  end;

  UpdateGraphics(0, E, Ek, frogI, frogIk, pPlotInitialField);
end;

procedure TGrapher.UpdateGraphics(k: integer; E, Ek: TEField; frogI, frogIk: TFROGTrace;
																			pPlotInitialField: Boolean);
begin;

	 // Draw FROG trace
	 if (k <= 10) or (k mod 5 = 0) then
    ReconFROGTrace.DrawFROGTrace(frogIk);


	 // Draw Fields
  //ClearWin(EFieldWin);
  //EFieldWin.PlotField(E, Ek, pPlotInitialField);
  EFieldWin.GraphEField(E, Ek, pPlotInitialField);

	 // Draw Spectra
  //ClearWin(SpecWin);
  Ek.Transform;
  //SpecWin.PlotField(mESpec, Ek, pPlotInitialField);
  SpecWin.GraphSpectrum(mESpec, Ek, pPlotInitialField);
  Ek.InverseTransform;
end;

procedure TGrapher.ClearWin(FieldGraph: TfrmFrogImage);
var
	ARect: TRect;
begin
     with FieldGraph.ImgImage.Canvas do
     begin
     	{ Set to white }
     	CopyMode := cmWhiteness;
     	ARect := Rect(0, 0, FieldGraph.ImgImage.Width, FieldGraph.ImgImage.Height);
     	CopyRect(ARect, FieldGraph.ImgImage.Canvas, ARect);
     	CopyMode := cmSrcCopy;
     end;
end;






procedure TGrapher.CleanUp;
begin
  if mWindowsCreated then
  begin
    WindowManager.SaveAsDefaults(TargetFROGTrace);
    TargetFROGTrace.Free;
    WindowManager.SaveAsDefaults(ReconFROGTrace);
    ReconFROGTrace.Free;
    WindowManager.SaveAsDefaults(EFieldWin);
    EFieldWin.Free;
    WindowManager.SaveAsDefaults(SpecWin);
    SpecWin.Free;
    mWindowsCreated := False;
  end;
  mESpec.Free;

end;

function TGrapher.RightHandEdge: integer;
begin
  RightHandEdge := EFieldWin.Left + EFieldWin.Width;
end;

function TGrapher.BottomEdge: integer;
begin
  BottomEdge := ReconFrogTrace.Top + ReconFrogTrace.Height;
end;

function TGrapher.LeftHandEdge: integer;
begin
  LeftHandEdge := ReconFrogTrace.Left;
end;

function TGrapher.TopEdge: integer;
begin
  TopEdge := TargetFrogTrace.Top;
end;











procedure TGrapher.DrawFROGTrace(Trace: TFROGTrace; DestWindow: TfrmFrogImage);
var
	ARect: TRect;
  x, y, i, j: longint;
  ColorVal, Mag, DeMag, N, k, l: integer;
  max, Fact, Val: double;
begin
	N := Trace.N;
	with DestWindow do
	begin
     { Magnification value }
  	if ImgImage.Height > ImgImage.Width then
    begin
     	Mag := ImgImage.Width div N;
      DeMag := Ceil(N/ImgImage.Height);
    end
    else
    begin
     	Mag := ImgImage.Height div N;
      DeMag := Ceil(N/ImgImage.Width);
    end;
    if Mag = 0 then Mag := 1;
    if DeMag = 0 then DeMag := 1;

    { Start by setting the image canvas to black }
    with DestWindow.ImgImage.Canvas do
    begin
    	CopyMode := cmBlackness;
      ARect := Rect(0, 0, N*Mag div DeMag, N*Mag div DeMag);
      CopyRect(ARect, DestWindow.ImgImage.Canvas, ARect);
      CopyMode := cmSrcCopy;
    end;

    { Find the peak of the trace }
    max := Trace.Max;

    { Draw the new FROG trace, non-black pixels only! }
    Fact := 100.0/sqrt(max);
    ImgImage.Visible := False;
    if DeMag = 1 then
    begin
    	for x := 0 to N - 1 do
    		for y := 0 to N - 1 do
       		begin
         		ColorVal := Trunc(Fact*Sqrt(Trace.Vals^[x*N + y]));
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
              	Val := Val + Trace.Vals^[x*N + y];
            	end;
          Val := Val/(DeMag*DeMag);
          ColorVal := Trunc(Fact*Sqrt(Val));
          if ColorVal > 0 then
          	ImgImage.Canvas.Pixels[i, (N div DeMag) - 1 - j] := mColorTable.ColorTable[ColorVal];
        end;
    end;
    ImgImage.Visible := True;
    ImgImage.Update;
	end; // with DestWindow do
end;


const EPS = 1e-4;
procedure TGrapher.PlotField(E, Ek: TEField; FieldGraph: TfrmFrogImage;
							 pPlotInitialField: Boolean);
var
	i, HH, N: integer;
	XStep, phasemax, phasemaxk, intenmax, intenmaxk: double;
begin;

  N := Ek.N;
	XStep := FieldGraph.ImgImage.Width/N;     // The X step size

  // Max intensity for original field
  intenmax := 0.0;
  phasemax := 0.0;
  if pPlotInitialField then
  begin
	  E.SetIntensityAndPhase;
    E.ClipPhase(EPS);
	  intenmax := E.IntenMax;
    phasemax := E.PhaseMax;
  end;

  Ek.SetIntensityAndPhase;
  Ek.ClipPhase(EPS);
  intenmaxk := Ek.IntenMax;
  phasemaxk := Ek.PhaseMax;

  if phasemaxk > phasemax then  phasemax := phasemaxk;
  if phasemax = 0 then phasemax := 1;

	FieldGraph.ImgImage.Visible := False;  // Speeds it up.
  with FieldGraph.ImgImage do
  begin
  	HH := Height - 1;
    { Draw the original Intesity }
    if pPlotInitialField then
    begin
    	Canvas.Pen.Color := clBlack;
      Canvas.MoveTo(0, Trunc(HH*(1 - E.Intensity^[0]/intenmax)));
      for i := 1 to N - 1 do
      	Canvas.LineTo(Trunc(i*XStep), Trunc(HH*(1 - E.Intensity^[i]/intenmax)));
    end;

    // Draw the reconstructed intensity
    Canvas.Pen.Color := clGreen;
    Canvas.MoveTo(0, Trunc(HH*(1 - Ek.Intensity^[0]/intenmaxk)));
    for i := 1 to N - 1 do
    	Canvas.LineTo(Trunc(i*XStep), Trunc(HH*(1 - Ek.Intensity^[i]/intenmaxk)));

		// Plot the original phase.
    FieldGraph.PnlPhase.Caption := 'Max. phase: '
    		+ FloatToStrF(phasemax, ffGeneral, 3, 1) + ' rad';
    FieldGraph.PnlPhase.Update;
    if pPlotInitialField then
    begin
    	Canvas.Pen.Color := clBlue;
      Canvas.MoveTo(0, Trunc(HH*(phasemax - E.Phase^[0])/phasemax));
      for i := 1 to N - 1 do
      	Canvas.LineTo(Trunc(i*XStep), Trunc(HH*(phasemax - E.Phase^[i])/phasemax));
    end;

    // reconstructed phase
    Canvas.Pen.Color := clRed;
    Canvas.MoveTo(0, Trunc(HH*(phasemax - Ek.Phase^[0])/phasemax));
    for i := 1 to N - 1 do
    	Canvas.LineTo(Trunc(i*XStep), Trunc(HH*(phasemax - Ek.Phase^[i])/phasemax));

  end;  // end of with FieldGraph.ImgImage do
  FieldGraph.ImgImage.Visible := True;
  FieldGraph.ImgImage.Update;
end;


end.
