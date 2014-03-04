unit RawData;

interface

uses FrogObj, NLO, Numerics, FrogTrace, Windows, Classes;

type
  TRawData = class(TFrogObject)
    private
      mNTau: integer;
      mNLam: integer;
      mDelT: double;
      mDelLam: double;
      mLam0: double;
      mVals: PArray;
      mNLO: TNLOInteraction;
      mLog: TStringList;
      mIsCenteredInDelay: boolean;
      mFractionalDelayCenter: double;
      mCenterWavelengthByPeak: boolean;
      procedure SwapVals(var X, Y: integer);
      function Window(tp: integer; wp, r: double): double;
      function CenterTraceInDelay: integer;
      function GenerateZODM(pZODM: PArray): integer;
      procedure TauShift(pTauFrac: double);
      function FindZODMMax(pZODM: PArray; pTau0: integer): double;
      function ZODMInt(pTau: double): double;
      function FindCenterInFrequency: double;
      function GetCenterOfMass(pData: PArray; pLength: integer): double;
      function Max(a, b: integer): integer;
      function Min(a, b: integer): integer;
    public
      procedure GridData(pFrogI: TFrogTrace; pConstantWavelength: boolean);
      procedure ExtractData(Origin, MovePt: TPoint; MagX, MagY, DeMagX, DeMagY: integer);
      procedure ResampleDelay(NewDelay: double);
      procedure ShowMarginals(pNLO: TNLOInteraction; pFrogI: TFrogTrace);
      procedure InitLog(pLog: TStringList);
      procedure FullSpectrumNoiseSubtraction;
      procedure PixelNoiseSubtraction;
      procedure EdgeNoiseSubtraction;
      procedure LowPassNoiseSubtraction(pRad: double);
      procedure PixelCleanup;
      function CalculateAndCorrectSpatialChirp(pCorrect: boolean): double;
      property NTau: integer read mNTau write mNTau;
      property NLam: integer read mNLam write mNLam;
      property DelT: double read mDelT write mDelT;
      property DelLam: double read mDelLam write mDelLam;
      property Lam0: double read mLam0 write mLam0;
      property Vals: PArray read mVals;
      constructor Create(pNTau, pNLam: integer; pDelT, pDelLam, pLam0: double; pNLO: TNLOInteraction;
          pCenterWavelengthByPeak: boolean);
      destructor Destroy; override;
  end;

const
  NBACK = 2;

implementation

uses SysUtils, Math, Dialogs;

constructor TRawData.Create(pNTau, pNLam: integer; pDelT, pDelLam, pLam0: double; pNLO: TNLOInteraction;
      pCenterWavelengthByPeak: boolean);
begin
  inherited Create;
  mNTau := pNTau;
  mNLam := pNLam;
  mDelT := pDelT;
  mDelLam := pDelLam;
  mLam0 := pLam0;
  mNLO := pNLO;
  mCenterWavelengthByPeak := pCenterWavelengthByPeak;
  GetMem(mVals, mNTau*mNLam*SizeOf(double));
  mLog := TStringList.Create;
  mIsCenteredInDelay := False;
  mFractionalDelayCenter := 0;
end;

destructor TRawData.Destroy;
begin
  mLog.Free;
  FreeMem(mVals);
  inherited;
end;

procedure TRawData.ExtractData(Origin, MovePt: TPoint;
														MagX, MagY, DeMagX, DeMagY: integer);
var
	NewRaw: PArray;
  NewNTau, NewNLam, TauL, TauH, LamL, LamH, Tau, W, newCenter: integer;
  newLam0: double;
begin
	// translate points into array locations
  TauL := ((DeMagX*Origin.X) div Magx);
  LamL := ((DeMagY*Origin.Y) div Magy);
  TauH := ((DeMagX*MovePt.X) div Magx);
  LamH := ((DeMagY*MovePt.Y) div Magy);
  if TauL > TauH then SwapVals(TauL, TauH);
  if LamL > LamH then SwapVals(LamL, LamH);
  NewNTau := TauH - TauL + 1;
  NewNLam := LamH - LamL + 1;
  mLog.Add('Data extracted from (' + IntToStr(TauL) +',' + IntToStr(LamL) + ') to (' + IntToStr(TauH) +',' + IntToStr(LamH) + ')');

  // copy old data into new array
  GetMem(NewRaw, NewNTau*NewNLam*SizeOf(double));
  try
    for Tau := 0 to (NewNTau - 1) do
  	  for W := 0 to (NewNLam - 1) do
      begin
    	  NewRaw^[Tau*NewNLam + W] := mVals^[(TauL+Tau)*mNLam + (LamL+W)];
      end;

  finally
    // Destroy old array
    FreeMem(mVals);
  end;

  // Find new center in wavelength
  newCenter := (LamH + LamL + 1) div 2;
  newLam0 := (newCenter - mNLam div 2)*mDelLam + 1.0*mLam0;
  mLam0 := newLam0;

  mVals := NewRaw;
  mNTau := NewNTau;
  mNLam := NewNLam;
end;

procedure TRawData.SwapVals(var X, Y: integer);
var
	temp: integer;
begin
	temp := X;
  X := Y;
  Y := temp;
end;

procedure TRawData.ResampleDelay(NewDelay: double);
var
	tp, hw, sum, csum, r, height: double;
  i, start, stop, NewNTau, w, t: integer;
  tRaw: PArray;
begin
  mLog.Add('Delay resampled to ' + FloatToStrF(NewDelay, ffGeneral, 4, 0) + 'fs.');
	r := Abs(NewDelay/mDelT);
  NewNTau := Trunc(mNTau/r);
  hw := Abs(r)/2.0;
  if (hw < 1.0) then hw := 1.01;

  // Do the resampling.
  GetMem(tRaw, NewNTau*SizeOf(double));
  try
  for w := 0 to mNLam - 1 do
  begin
  	for t := 0 to NewNTau - 1 do
    begin
    	tp := 0.5*hw + r*t;
      sum := 0.0;
      csum := 0.0;
      start := 1 + Trunc(tp - hw);
      if t = 0 then start := 0;
      stop := Trunc(tp + hw);
      if start > stop then stop := start;
      for i := start to stop do
      begin
      	if (i >= 0) and (i < mNTau) then
        begin
        	height := 1.0 - Abs(i-tp)/hw;
          if height < 0 then Continue;
          sum := sum + height*mVals^[i*mNLam + w];
          csum := csum + height;
        end;
      end;
      if csum = 0 then csum := 1.0;
      tRaw^[t] := sum/csum;
    end;
    for t := 0 to NewNTau -1 do
      mVals^[t*mNLam + w] := tRaw^[t];
  end;
  finally
    FreeMem(tRaw);
  end;

  mDelT := NewDelay;
  mNTau := NewNTau;
end;

procedure TRawData.GridData(pFrogI: TFrogTrace; pConstantWavelength: boolean);
var
  DelFGrid, DelFRaw, r, f0Grid, f0Raw, lam, wp, scaler, f, PeakRawLam, w0: double;
  tau0, tau, w, N, N2, tp, i: integer;
begin
  tau0 := CenterTraceInDelay;
  w0 := FindCenterInFrequency;
  PeakRawLam := mLam0 + mDelLam*(w0 - mNLam div 2);  // This will be the new center wavelength
  f0Raw := 300.0/mLam0;           // The frequency at the center pixel of the raw data
  f0Grid := 300.0/(PeakRawLam);   // The frequency at the center pixel of the gridded data
  DelFGrid := 1.0/(mDelT*pFrogI.N);   // this is what delf will be in the gridded trace
  DelFRaw := -(300.0/(mLam0*mLam0))*mDelLam;  // DelF of the raw data at the center
  r := DelFGrid/DelFRaw;
  N := pFrogI.N;
  N2 := N div 2;
  mLog.Add('There were ' + FloatToStrF(r, ffGeneral, 4, 0) + ' too many wavelength points.');
  //mLog.Add('Trace centered on tau pixel ' + IntToStr(tau0));
  mLog.Add('Wavelength Center at pixel ' + FloatToStrF(w0, ffGeneral, 4, 0));

  for tau := 0 to N - 1 do
  begin
    tp := tau + tau0 - N2;
    for w := 0 to N - 1 do
    begin
      i := tau*N + w;
      if (tp < 0) or (tp >= mNTau) then
      begin
        pFrogI.Vals^[i] := 0;
        Continue;
      end;

      try
        // Find the freq/wavl of this pixel
        f := f0Grid + (w - N2)*DelFGrid;
        lam := 300.0/f;
      except on EMathError do
        begin
          pFrogI.Vals^[i] := 0.0;
          Continue;
        end;
      end;

      if pConstantWavelength then
      begin
        // Now find where this wavl falls in the raw data
        wp := (mNLam div 2) + (lam - mLam0)/mDelLam;
        // This next line converts from I(lam) to I(f)
        scaler := (lam/mLam0)*(lam/mLam0);
      end
      else
      begin
        // For constant frequency
        wp := (mNLam div 2) + (f - f0Raw)/DelFRaw;
        scaler := 1.0;
      end;

      if (wp < 0) or (wp >= mNLam) then
      begin
        pFrogI.Vals^[i] := 0;
        Continue;
      end;

      pFrogI.Vals^[i] := scaler*Window(tp, wp, r);
    end;
  end;

  // Fix up ZOFM stuff
  // for w0 = 231, nlam = 450, dellam 0.123, +0.738 works
  //mLam0 := mLam0 + (w0 - mNLam div 2)*mDelLam;
  mLog.Add('Wavelength shift of ' + FloatToStrF(PeakRawLam - mLam0, ffGeneral, 4, 0) + ' nm.');
  mLam0 := PeakRawLam;
end;

function TRawData.Window(tp: integer; wp, r: double): double;
var
  i, start, stop: integer;
  hw, sum, csum, height: double;
begin
	hw := Abs(r)/2.0;
	if (hw < 1.0) then hw := 0.9999;
 // changed from 1.01 on Apr 7, 2004, version 3.1.0.  When reading back in the
 // a.dat files, this dramatically improved the convergence!
 // With hw = 1.0, you are guaranteed to always have two points in the window.  If
 // the grid point and raw point are exactly coincident, there will be 3 points in
 // the window, but the other two will have height =0.
 // With hw = 0.9999, there's a teeny tiny chance that you'll be off by 0.01% from an
 // exact match to the raw data point, and then with this "narrow" window you'll miss
 // the other two points and just have a single point contribute.

	sum := 0.0;
	csum := 0.0;
	start := 1 + Trunc(wp - hw);
	stop := Trunc(wp + hw);
	if(start > stop) then stop := start;
	for i := start to stop do
  begin
		if (i >= 0) and (i < mNLam) then
    begin
			height := 1.0 - Abs(i - wp)/hw;
			if (height < 0.0) then Continue;
			sum := sum + height*mVals^[tp*mNLam + i];
			csum := csum + height;
		end;
	end;

	if(sum < 0.0) then sum := 0.0;    // No negative FROG pixels
	if (csum = 0.0) then csum := 1.0;
	Window := (sum/csum);
end;

function TRawData.CenterTraceInDelay: integer;
var
  tau0: integer;
  ZODM: PArray;
begin
  if not mIsCenteredInDelay then
  begin
    // Find the tau = 0 position. */
    GetMem(ZODM, mNTau*SizeOf(double));
    try
      tau0 := GenerateZODM(ZODM);
	     mFractionalDelayCenter := FindZODMMax(zodm, tau0);
      mLog.Add('Trace center in delay found at pixel ' + FloatToStrF(mFractionalDelayCenter, ffGeneral, 4, 0));
	     // Shift the FROG trace fractionally to center it jus-s-st right */
	     TauShift(mFractionalDelayCenter);
      mIsCenteredInDelay := True;
    finally
      FreeMem(ZODM);
    end;
  end;
  tau0 := Trunc(mFractionalDelayCenter + 0.5);
  CenterTraceInDelay := tau0;
end;

// This function fills a PRE ALLOCATED array with the ZODM numbers
// and returns the pixel where the max was found.
function TRawData.GenerateZODM(pZODM: PArray): integer;
var
  zodmmax: double;
  tau0, tau, w: integer;
begin
 	zodmmax := 0.0;
	  for tau := 0 to mNTau - 1 do
   begin
		  pZODM^[tau] := 0.0;
		  for w := 0 to mNLam - 1 do
     begin
       pZODM^[tau] := pZODM^[tau] + mVals^[tau*mNLam + w];
		    if (pZODM^[tau] > zodmmax) then
       begin
			    zodmmax := pZODM^[tau];
			    tau0 := tau;
       end;
     end;
   end;
   GenerateZODM := tau0;
end;

// This shifts the trace only the fractional pixel amount.  The main GridData
// method up above does the integral pixel shift.
procedure TRawData.TauShift(pTauFrac: double);
var
  y: PArray;
  w, tau: integer;
  del: double;
begin
  GetMem(y, mNTau*SizeOf(double));
  try

	del := pTauFrac - Trunc(pTauFrac);
	for w := 0 to mNLam - 1 do
  begin
		for tau := 0 to mNTau -1 do
      y^[tau] := mVals^[tau*mNLam + w];

		// The inner points */
		for tau := 1 to mNTau - 2 do
    begin
			if (del >= 0.5) then
				mVals^[tau*mNLam + w] := y^[tau] - (1.0 - del)*(y^[tau]-y^[tau - 1])
			else
				mVals^[tau*mNLam + w] := y^[tau] + del*(y^[tau+1] - y^[tau]);
    end;

		// Finish the endpoints */
		if (del >= 0.5) then
    begin
			mVals^[w] := y^[0] - (1.0 - del)*(y^[1] - y^[0]);
			mVals^[(mNTau-1)*mNLam + w] := y^[mNTau-1] - (1.0 - del)*(y^[mNTau-1]-y^[mNTau - 2]);
		end
		else
    begin
			mVals^[w] := y^[0] + del*(y^[1] - y^[0]);
			mVals^[(mNTau-1)*mNLam + w] := y^[mNTau-1] + del*(y^[mNTau-1] - y^[mNTau-2]);
		end;

		if(mVals^[w] < 0.0) then mVals^[w] := 0.0;
		if(mVals^[(mNTau-1)*mNLam + w] < 0.0) then mVals^[(mNTau-1)*mNLam + w] := 0.0;
	end;

  finally
    FreeMem(y);
  end;
end;

var
  gZODM, gTau: PArray;

function TRawData.FindZODMMax(pZODM: PArray; pTau0: integer): double;
var
  xmin, sum, tsum: double;
  i: integer;
begin
  // For most NLOs, solve for the peak of the ZODM with Brent
  if mNLO.NLOType <> nlSHG then
  begin
    GetMem(gTau, mNTau*SizeOf(double));
    try
      for i := 0 to mNTau - 1 do gTau^[i] := 1.0*i;
	     gZodm := pZODM;  // Used in ZODMInt, the fcn to minimize, below.
		   TNumerics.Brent(pTau0 - 3, pTau0, pTau0 + 3, ZODMInt, 1.0e-3, xmin);
    finally
      FreeMem(gTau);
    end;
  end
  else
  begin
    // In the case of SHG, the marginal is symmetric, so center it by center-of-mass. */
    xmin := GetCenterOfMass(pZODM, mNTau);
  end;

	 FindZODMMax := xmin;
end;

// This is the function that is minimized to find the center of the ZODM, above.
function TRawData.ZODMInt(pTau: double): double;
var
  ans: double;
begin
  // This doesn't have to be 1.0: we just need to invert the ZODM
  // so that the min is actually the max!
	 ans := (1.0 - TNumerics.Interp(gTau, gZodm, pTau, mNTau, 4) );
	 ZODMInt := ans;
end;

// Finds the center of mass of a preallocated array.
function TRawData.GetCenterOfMass(pData: PArray; pLength: integer): double;
var
   sum, tsum, xmin: double;
   i: integer;
begin
    sum := 0;
    tsum := 0.0;
		 for i := 0 to pLength - 1 do
    begin
		  	sum := sum + pData^[i];
			  tsum := tsum + i*pData^[i];
		 end;
    if sum <> 0 then
		   xmin := tsum/sum
    else
      xmin := pLength/2.0;

    GetCenterOfMass := xmin;
end;

function TRawData.FindCenterInFrequency: double;
var
  tau, w, w0: integer;
  ZOFMMax, num, den: double;
  ZOFM: PArray;
begin
	// Find the center of the trace in wavelength */
  GetMem(ZOFM, mNLam*SizeOf(double));
  try

	zofmmax := 0.0;
	for w := 0 to mNLam - 1 do
  begin
		zofm^[w] := 0.0;
		for tau := 0 to mNTau - 1 do
      zofm^[w] := ZOFM^[w] + mVals^[tau*mNLam + w];
		if (zofm^[w] > zofmmax) then
    begin
			zofmmax := zofm^[w];
			w0 := w;
		end;
	end;

  // Find center-of-mass of ZOFM
  num := 0; den := 0;
  for w := 0 to mNLam -1 do
  begin
  	num := num + w*zofm^[w];
    den := den + zofm^[w];
  end;

  finally
    FreeMem(ZOFM);
  end;
  // If everything went ok, use the center-of-mass as the Frequency center
  // changed from w0 max on Nov 1, 1999, version 3.04.
  // Changed to return a double, Apr 7, 2004 for version 3.1.0.
  // Allowed both Oct 13, 2011, version 3.2.4
  if (den = 0.0) or mCenterWavelengthByPeak then
  begin
  	FindCenterInFrequency := w0;
    mLog.Add('Centered wavelength by peak at ' + FloatToStrF(w0, ffGeneral, 4, 0));
  end
  else
  begin
  	FindCenterInFrequency := (num/den);
    mLog.Add('Centered wavelength by center of mass at ' + FloatToStrF(num/den, ffGeneral, 4, 0));
  end;
end;

procedure TRawData.FullSpectrumNoiseSubtraction;
var
  w, tau :integer;
  Background: PArray;
begin
  mLog.Add('Full spectrum noise subtraction.');
  GetMem(Background, mNLam*SizeOf(double));
  try
		for w := 0 to mNLam - 1 do
      Background^[w] := 0.0;

		// Average the spectrum over the first few and last few time delays */
		for tau := 0 to NBACK - 1 do
			for w := 0 to mNLam - 1 do
				Background^[w] := Background^[w] + mVals^[tau*mNLam + w];

		for tau := mNTau - 1 downto mNTau - NBACK do
			for w := 0 to mNLam - 1 do
				Background^[w] := Background^[w] + mVals^[tau*mNLam + w];

    // Normalize it
		for w := 0 to mNLam - 1 do
      Background^[w] := Background^[w]/(2.0*NBACK);

		// And subtract it */
		for tau := 0 to mNTau - 1 do
			for w := 0 to mNLam - 1 do
      begin
				mVals^[tau*mNLam + w] := mVals^[tau*mNLam + w] - Background^[w];
        if mVals^[tau*mNLam + w] < 0 then mVals^[tau*mNLam + w] := 0;
      end;
  finally
    FreeMem(Background);
  end;
end;

procedure TRawData.PixelNoiseSubtraction;
var
  w, tau: integer;
  min: double;
begin
  // Find the minimum pixel
  min := mVals^[0];
  for tau := 0 to mNTau - 1 do
    for w := 0 to mNLam - 1 do
      if mVals^[tau*mNLam + w] < min then min := mVals^[tau*mNLam + w];

  mLog.Add('Minimum pixel value of ' + FloatToStrF(min, ffGeneral, 4, 0) + ' was subtracted from each pixel.');

  // And subtract it */
  for tau := 0 to mNTau - 1 do
    for w := 0 to mNLam - 1 do
    begin
      mVals^[tau*mNLam + w] := mVals^[tau*mNLam + w] - min;
      if mVals^[tau*mNLam + w] < 0 then mVals^[tau*mNLam + w] := 0;
    end;
end;

procedure TRawData.EdgeNoiseSubtraction;
var
  w, tau, count: integer;
  sum: double;
begin
  mLog.Add('Edge noise subtraction.');
  sum := 0;
  count := 0;
  // Left side
  for tau := 0 to NBACK - 1 do
    for w := 0 to mNLam - 1 do
    begin
      sum := sum + mVals^[tau*mNLam + w];
      Inc(count);
    end;

  // Right side
  for tau := mNTau - 1 downto mNTau - NBACK do
    for w := 0 to mNLam - 1 do
    begin
      sum := sum + mVals^[tau*mNLam + w];
      Inc(count);
    end;

  // Bottom side - but dont' count corners twice
  for tau := NBACK to mNTau - 1 - NBACK do
    for w := 0 to NBACK - 1 do
    begin
      sum := sum + mVals^[tau*mNLam + w];
      Inc(count);
    end;

  // Top side
  for tau := NBACK to mNTau - 1 - NBACK do
    for w := mNLam - 1 downto mNLam - 1 - NBACK do
    begin
      sum := sum + mVals^[tau*mNLam + w];
      Inc(count);
    end;

  // Normalize it
  sum := sum/count;

  // And subtract it */
  for tau := 0 to mNTau - 1 do
    for w := 0 to mNLam - 1 do
    begin
      mVals^[tau*mNLam + w] := mVals^[tau*mNLam + w] - sum;
      if mVals^[tau*mNLam + w] < 0 then mVals^[tau*mNLam + w] := 0;
    end;
end;

procedure TRawData.LowPassNoiseSubtraction(pRad: double);
var
  size, tau, w, flag, startTau, startLam, size2: integer;
  r2, x2, dist: double;
  re, im, z: PArray;
begin
  mLog.Add('Fourier filter with radius ' + FloatToStrF(pRad, ffGeneral, 4, 0) + ' applied.');
	size := 2;
  while ((size < mNTau) or (size < mNLam)) do size := size*2;
	size2 := 2*size;

	startTau := (size - mNTau) div 2;
	startLam := (size - mNLam) div 2;
  GetMem(z, size2*SizeOf(double));
  GetMem(re, size*size*SizeOf(double));
  GetMem(im, size*size*SizeOf(double));
  try
    // Fill with zeros
    for tau := 0 to size - 1 do
      for w := 0 to size - 1 do
      begin
        re^[tau*size + w] := 0;
        im^[tau*size + w] := 0;
      end;

   // Fill with data
    for tau := 0 to mNTau - 1 do
      for w := 0 to mNLam - 1 do
        re^[(startTau + tau)*size + (startLam + w)] := mVals^[tau*mNLam + w];

	// Fourier transform - along w
  for tau := 0 to size - 1 do
  begin
    flag := 1;
    for w := 0 to size - 1 do
    begin
      z^[2*w] := re^[tau*size + w]*flag;
      z^[2*w + 1] := im^[tau*size + w]*flag;
      flag := flag*(-1);
    end;
    TNumerics.Four1(z, size, 1);
    for w := 0 to size - 1 do
    begin
      re^[tau*size + w] := z^[2*w]*flag;
      im^[tau*size + w] := z^[2*w + 1]*flag;
      flag := flag*(-1);
    end;
  end;

	// Fourier transform - along tau
  for w := 0 to size - 1 do
  begin
    flag := 1;
    for tau := 0 to size - 1 do
    begin
      z^[2*tau] := re^[tau*size + w]*flag;
      z^[2*tau + 1] := im^[tau*size + w]*flag;
      flag := flag*(-1);
    end;
    TNumerics.Four1(z, size, 1);
    for tau := 0 to size - 1 do
    begin
      re^[tau*size + w] := z^[2*tau]*flag;
      im^[tau*size + w] := z^[2*tau + 1]*flag;
      flag := flag*(-1);
    end;
  end;

	// The filter */
	r2 := pRad*pRad*size*size/4.0;
  for tau := 0 to size - 1 do
  begin
		x2 := (1.0*tau - size/2)*(1.0*tau - size/2);
    for w := 0 to size - 1 do
    begin
			dist := x2 + (1.0*w - size/2)*(1.0*w - size/2);
      if dist > r2 then
      begin
        re^[tau*size + w] := 0;
        im^[tau*size + w] := 0;
      end;
    end;
  end;

	// Inverse Fourier transform - along w
  for tau := 0 to size - 1 do
  begin
    flag := 1;
    for w := 0 to size - 1 do
    begin
      z^[2*w] := re^[tau*size + w]*flag;
      z^[2*w + 1] := im^[tau*size + w]*flag;
      flag := flag*(-1);
    end;
    TNumerics.Four1(z, size, -1);
    for w := 0 to size - 1 do
    begin
      re^[tau*size + w] := z^[2*w]*flag;
      im^[tau*size + w] := z^[2*w + 1]*flag;
      flag := flag*(-1);
    end;
  end;

	// Inverse Fourier transform - along tau
  for w := 0 to size - 1 do
  begin
    flag := 1;
    for tau := 0 to size - 1 do
    begin
      z^[2*tau] := re^[tau*size + w]*flag;
      z^[2*tau + 1] := im^[tau*size + w]*flag;
      flag := flag*(-1);
    end;
    TNumerics.Four1(z, size, -1);
    for tau := 0 to size - 1 do
    begin
      re^[tau*size + w] := z^[2*tau]*flag;
      im^[tau*size + w] := z^[2*tau + 1]*flag;
      flag := flag*(-1);
    end;
  end;

	// Unload array
  for tau := 0 to mNTau - 1 do
    for w := 0 to mNLam - 1 do
      mVals^[tau*mNLam + w] := re^[(startTau + tau)*size + (startLam + w)];

  finally
    FreeMem(im);
    FreeMem(re);
    FreeMem(z);
  end;
end;

procedure TRawData.ShowMarginals(pNLO: TNLOInteraction; pFrogI: TFrogTrace);
begin

end;

procedure TRawData.InitLog(pLog: TStringList);
begin
  pLog.AddStrings(mLog);
end;

function TRawData.CalculateAndCorrectSpatialChirp(pCorrect: boolean): double;
var
   tau0, tau, w, tauMirror: integer;
   tau0frac, com1, com2, slope, weight, weightSum, slopeSum: double;
   ZODM, Spectrum: PArray;
   shift, shiftedVal, vhigh, vlow, newVal: double;
   low, high: integer;
begin
  // Find the center delay of the trace, and shift over.  The trace is now
  // centered at tau0.
  tau0 := CenterTraceInDelay;

  // Now, for each spectrum, get the Center-of-mass
  weightSum := 0.0;
  slopeSum := 0.0;
  GetMem(Spectrum, mNLam*SizeOf(double));
  try
    for tau := 0 to tau0 - 1 do
    begin
      // Find the mirror spectrum on the other side of tau0
      tauMirror := 2*tau0 - tau;
      if tauMirror > mNTau then continue;
      // get the spectra COM
      weight := 0.0;
        // The left-hand spectrum
      for w := 0 to mNLam - 1 do
      begin
        Spectrum^[w] := mVals^[tau*mNLam + w];
        weight := weight + Spectrum^[w];
      end;
      com1 := GetCenterOfMass(Spectrum, mNLam);
        // The right-hand spectrum
      for w := 0 to mNLam - 1 do
      begin
        Spectrum^[w] := mVals^[tauMirror*mNLam + w];
        weight := weight + Spectrum^[w];
      end;
      com2 := GetCenterOfMass(Spectrum, mNLam);

      // The slope for this pair
      slope := (com2 - com1)/(2.0*(tau0 - tau));

      // Accumulate the numbers for the weighted average
      weightSum := weightSum + weight;
      slopeSum := slopeSum + slope*weight;
    end;

    if weightSum = 0.0 then
      slope := 0.0
    else
      slope := slopeSum/weightSum;
    mLog.Add('Spatial Chirp is ' + FloatToStrF(slope, ffGeneral, 4, 0));

    // Now correct for the chirp.
    if pCorrect then
    begin
      for tau := 0 to mNTau - 1 do
      begin
        // Calculate the shift necessary.
        shift := (tau - tau0)*slope;
        for w := 0 to mNLam - 1 do
        begin
          Spectrum^[w] := mVals^[tau*mNLam + w];
        end;
        // Interpolate
        for w := 0 to mNLam - 1 do
        begin
          shiftedVal := w + shift;
          low := Trunc(shiftedVal);
          high := low + 1;
          if (low >= 0) and (low < mNLam) then
             vlow := Spectrum^[low]
          else
             vlow := 0.0;
          if (high < mNLam) and (high >= 0) then
             vhigh := Spectrum^[high]
          else
             vhigh := 0.0;
          newVal := vlow + (shiftedVal - low)*(vhigh - vlow);
          mVals^[tau*mNLam + w] := newVal;
        end;
      end;
    end;
  finally
      FreeMem(Spectrum);
  end;
  CalculateAndCorrectSpatialChirp := slope;
end;

procedure TRawData.PixelCleanup;
var
  i, zeroLimit, nonzeroLimit, edges, tau, w, t, y: integer;
  lowT, highT, lowY, highY, zeros, nonzeros: integer;
  breakout: boolean;
  value, val, ZERO, maxValue: double;
  points: PArray;
begin
     maxValue := 0.0;
     for i := 0 to mNTau*mNLam -1 do
     begin
          value := mVals^[i];
          if value > maxValue then
             maxValue := value;
     end;
     if maxValue = 0.0 then
        Exit;
     
     ZERO := 0.00007;
     GetMem(points, mNTau*mNLam*SizeOf(double));

     try
				for i := 0 to mNTau*mNLam - 1 do
				begin
					points[i] := 0;
				end;

       zeroLimit := 0;	// The number of zeros that will mark you for zapping
       nonzeroLimit := 0;  // The number of nonzeros that will save your butt
       // process each point
       for tau := 0 to nTau-1 do
       begin
				     for w := 0 to nLam - 1 do
            begin
                 // If it's already zero, stop screwing around
                 value := mVals^[tau*mNLam + w];
                 if value = 0.0 then
                    Continue;

                 // Find out if we're at the edge or corner
                 edges := 0;
                 if (tau = 0) or (tau = mNTau -1) then Inc(edges);
					        if (w = 0) or (w = mNLam - 1) then Inc(edges);

                 // Corners
                 if edges = 2 then
					        begin
						           mVals^[tau*mNLam + w] := 0.0;
						           Continue;
                 end
					        else if edges = 1 then
					        begin
						           zeroLimit := 3;
						           nonzeroLimit := 3;
                 end
					        else if edges = 0 then
					        begin
						           zeroLimit := 5;
						           nonzeroLimit := 4;
                 end;

					        // Run around the neighbors and find out who's a LOSER!
					        lowT := Max(0, tau - 1);
					        highT := Min(mNTau - 1, tau + 1);
					        lowY := Max(0, w - 1);
					        highY := Min(mNLam - 1, w + 1);
					        zeros := 0;
                 nonzeros := 0;
					        breakout := false;
					        for t := lowT to highT do
					        begin
						           for y := lowY to highY do
                      begin
							              if (t = tau) and (w = y) then
								            Continue;
                           val := mVals^[t*mNLam + y]/maxValue;
							              if (val > ZERO) then
								               Inc(nonzeros)
                           else
                               Inc(zeros);

                           if nonzeros >= nonzeroLimit then
								               Continue;
                           if zeros >= zeroLimit then
							              begin
								                 // Register a hit at tau, w
                                points^[tau*mNLam + w] := 1.0;
								                 breakout := true;
								                 Break;
                           end;
                      end;
						           if breakout then
							            break;
                 end;
            end;
       end;

			  // Then set the zapped pixels to zero.
			  for i := 0 to mNTau*mNLam-1 do
       begin
            if points^[i] = 1.0 then
               mVals^[i] := 0.0;
			  end;
  finally
       FreeMem(points);
  end;

end;

function TRawData.Max(a, b: integer): integer;
begin
     if(a > b) then
          Max := a
     else
         Max := b;
end;

function TRawData.Min(a, b: integer): integer;
begin
     if(a < b) then
          Min := a
     else
         Min := b;
end;

end.
