unit ReadIn;

interface

uses FrogObj, Func1D, Numerics, Classes;

type
  TDownsizeType = (dtInterpolate, dtTruncate);
  TReferencePulseSource = (rsFrog, rsReadIn);

  TReadIn = class(TFrogObject)
    private
      mFileName: string;
      mDelT: double;     // In Tadpole, this is DelLam
      mLam0: double;
      mN: integer;
      mShrunk: Boolean;
      mDownsizeType: TDownsizeType;
      mUseHeader: Boolean;
      mNullFile: string;
      mReferencePulseSource: TReferencePulseSource;  // only used for TADPOLE readins.
      mRefPulse: TEField;
      procedure ChangeUseHeader(pUseHeader: Boolean);
      function GetFileSize: integer;
      function GetSpectrumFileSize: integer;
      function SetProperDelT(pRe, pIm: PArray; pN: integer): double;
      procedure FetchHeaders;
      function Window(wp, r: double; pSource: PArray; pN: integer): double;
    public
      procedure LoadField(pEk: TEField);
      function LoadSpectrum: TSpectrum;
      function LoadAutocorrelation: TAutocorrelation;
      procedure GridAutocorrelation(pSource, pDest: TAutocorrelation);
      procedure GridSpectrum(pSource, pDest: TSpectrum);
      procedure GridEField(pSource, pDest: TEField);
      procedure InitLog(pLog: TStringList);
      function IsNull: Boolean;
      procedure SetEFieldDelT(pSource, pDest: TEField);
      property ReadInFile: string read mFileName write mFileName;
      property N: integer read mN;
      property DelT: double read mDelT write mDelT;
      property Lam0: double read mLam0 write mLam0;
      property DownsizeType: TDownsizeType read mDownsizeType write mDownsizeType;
      property UseHeader: Boolean read mUseHeader write ChangeUseHeader;
      property NullFile: string read mNullFile;
      property ReferencePulseSource: TReferencePulseSource read mReferencePulseSource write mReferencePulseSource;
      property ReferencePulse: TEField read mRefPulse write mRefPulse;
      constructor Create;
  end;

implementation

uses Dialogs, SysUtils;

constructor TReadIn.Create;
begin
  mNullFile := 'None Selected';
  mFileName := mNullFile;
  mUseHeader := False;
  mLam0 := 0;
  mDelT := 0;
  mDownsizeType := dtInterpolate;
  mRefPulse := nil;
end;

procedure TReadIn.ChangeUseHeader(pUseHeader: Boolean);
begin
  mUseHeader := pUseHeader;
  if pUseHeader then
    FetchHeaders
  else
  begin
    mLam0 := 0;
    //mDelT := 0;
  end;
end;

procedure TReadIn.FetchHeaders;
var
  F: TextFile;
begin
  AssignFile(F, mFileName);
  try
    Reset(F);
  except
    ShowMessage('Error opening file ' + ExtractFileName(mFileName));
    Exit;
  end;

  try
    //Read(F, mDelT);
    Read(F, mLam0);
  except
    ShowMessage('Error reading header in ' + ExtractFileName(mFileName));
  end;
  CloseFile(F);
end;

function TReadIn.GetFileSize: integer;
var
  F: TextFile;
  i: integer;
  s: string;
  junk1: double;
begin
  AssignFile(F, mFileName);
  try
    Reset(F);
  except
    raise Exception.Create('Error opening ' + ExtractFileName(mFileName));
  end;

  try
    // If there are headers, get rid of 'em
    if mUseHeader then
    begin
      Read(F, junk1);
      // Gotta clear the linefeed
      ReadLn(F, s);
    end;

    // read in data
    i := 0;
    while (not Eof(F)) do
    begin
      ReadLn(F, s);
      Inc(i);
    end;

  finally
    CloseFile(F);
  end;
  GetFileSize := i;
end;

procedure TReadIn.LoadField(pEk: TEField);
var
  F: TextFile;
  i, N, top: integer;
  junk1, junk2, junk3, tDelT, t0: double;
  tRe, tIm: PArray;
begin
  //if mN <> pEk.N then ShowMessage('mN != pEk.N in TReadIn.LoadField');
  mN := pEk.N;
  N := GetFileSize;
  GetMem(tRe, N*SizeOf(double));
  GetMem(tIm, N*SizeOf(double));
  try
    AssignFile(F, mFileName);
    try
      Reset(F);
    except
      raise Exception.Create('Error opening ' + ExtractFileName(mFileName));
    end;

    try
      // If there are headers, get rid of 'em
      if mUseHeader then
      begin
        Read(F, junk1);
      end;

  	  // read in data
  	  i := 0;
      while (not Eof(F)) and (i < N) do
      begin
    	  Read(F, t0, junk2, junk3, tRe^[i], tIm^[i]);
        if i = 0 then mDelT := t0;
        if i = 1 then mDelT := t0 - mDelT;
        Inc(i);
      end;

      // Check for leftover data - this shouldn't happen since N is filesize
      Read(F, junk1);
      if not Eof(F) then MessageDlg('Leftover data!', mtWarning, [mbOK], 0);
    finally
  	  CloseFile(F);
    end;

    tDelT := SetProperDelT(tRe, tIm, N);
    //The size of tRe and tIm is N
    if mN < N then top := mN else top := N;
    for i := 0 to top - 1 do
    begin
      pEk.Re^[i] := tRe^[i];
      pEk.Im^[i] := tIm^[i];
    end;
    for i := N to mN - 1 do  // If the file is smaller than mN, fill with zeros
    begin
      pEk.Re^[i] := 0;
      pEk.Im^[i] := 0;
    end;
    pEk.SetMyUnits(tDelT, mLam0);
    pEk.Center;

  finally
    FreeMem(tIm);
    FreeMem(tRe);
  end;
end;

// Called from LoadField, above.
function TReadIn.SetProperDelT(pRe, pIm: PArray; pN: integer): double;
var
  r, hw, sumr, sumi, csum, height: double;
  t, t0, start, stop, i: integer;
  tRe, tIm: PArray;
begin
  // pN - the number of lines read from the file - also the size of pRe and pIm
  // mN - the size of the target EField
  if pN <= mN then
  begin
    mShrunk := False;
    SetProperDelT := mDelT;
  end
  else
  begin
    mShrunk := True;
    // interp or truncate the arrays and reset delt
    case mDownsizeType of
      dtInterpolate:
        begin
          GetMem(tRe, mN*SizeOf(double));
          GetMem(tIm, mN*SizeOf(double));
          try
          r := pN/mN;
          hw := r/2.0;
	        if (hw <= 0.5) then hw := 0.51;
	        if(hw <= 1.0) then hw := 0.9999;  // See RawData.Window
          for t := 0 to mN - 1 do
          begin
            sumr := 0.0; sumi := 0;
	          csum := 0.0;
	          start := 1 + Trunc(r*t - hw);
            if (r*t - hw) < 0 then start := 0;
	          stop := Trunc(r*t + hw);
	          if(start > stop) then stop := start;
	          for i := start to stop do
            begin
		          if (i >= 0) and (i < pN) then
              begin
			          height := 1.0 - Abs(i - t*r)/hw;
			          if (height < 0.0) then Continue;
			          sumr := sumr + height*pRe^[i];
			          sumi := sumi + height*pIm^[i];
			          csum := csum + height;
              end;
	          end;
            if csum = 0.0 then csum := 1.0;
            tRe^[t] := sumr/csum;
            tIm^[t] := sumi/csum;
          end;
          for t := 0 to mN - 1 do
          begin
            pRe^[t] := tRe^[t];
            pIm^[t] := tIm^[t];
          end;

          finally
            FreeMem(tIm);
            FreeMem(tRe);
          end;
          SetProperDelT := mDelT*r;
        end;
      dtTruncate:
        begin
          t0 := (pN - mN) div 2;
          for t := 0 to mN - 1 do
          begin
            pRe^[t] := pRe^[t + t0];
            pIm^[t] := pIm^[t + t0];
          end;
          // No change to mDelT
          SetProperDelT := mDelT;
        end;
    end;
  end;
end;

procedure TReadIn.InitLog(pLog: TStringList);
begin
  with pLog do
  begin
    Add('Electric Field Read-in from ' + mFileName);
    if mShrunk then
      case mDownsizeType of
        dtInterpolate: Add('Number of points reduced to ' + IntToStr(mN) + ' by interpolation.');
        dtTruncate: Add('Number of points reduced to ' + IntToStr(mN) + ' by truncation.');
      end;
    Add('Temporal spacing: ' + FloatToStrF(mDelT, ffGeneral, 4, 0) + ' fs');
    Add('Center wavelength: ' + FloatToStrF(mLam0, ffGeneral, 4, 0) + ' nm');
  end;
end;


// THis stuff is for spectra - TADPOLE

// Caution - this function returns an object: possible memory leaks!
function TReadIn.LoadSpectrum: TSpectrum;
var
  F: TextFile;
  i, N, N2: integer;
  lam, oldlam, tDelLam, tDelT, junk1: double;
  tSpec: TSpectrum;
begin
  N := GetSpectrumFileSize;
  tSpec := TSpectrum.Create(N);
  N2 := N div 2;

    AssignFile(F, mFileName);
    try
      Reset(F);

  	  // read in data
  	  i := 0;
      while (not Eof(F)) and (i < N) do
      begin
    	  Read(F, lam, tSpec.Intensity^[i]);
        if i = N2 then oldlam := lam;
        if i = (N2 + 1) then tDelLam := lam - oldlam;
        Inc(i);
      end;

      // Check for leftover data - this shouldn't happen since N is filesize
      Read(F, junk1);
      if not Eof(F) then MessageDlg('Leftover data!', mtWarning, [mbOK], 0);
    finally
  	  CloseFile(F);
    end;
    tDelT := -oldlam*oldlam/(300.0*N*tDelLam);
    tSpec.SetMyUnits(tDelT, oldlam);
    // Note that this spectrum has weird features:
    //   1. It's not a power of two
    //   2. It's still in constant wavelength spacing and scaling
    //   3. Not normed to 1.
    LoadSpectrum := tSpec;
end;

function TReadIn.GetSpectrumFileSize: integer;
var
  F: TextFile;
  i: integer;
  s: string;
  junk1: double;
begin
  AssignFile(F, mFileName);
  try
    try
      Reset(F);
      // read in data
      i := 0;
      while (not Eof(F)) do
      begin
        ReadLn(F, s);
        if not (s = '') then Inc(i);
      end;

    finally
      CloseFile(F);
    end;
  except
    ShowMessage('Error reading file ' + mFileName);
  end;
  GetSpectrumFileSize := i;
end;

// Caution - this function returns an object: possible memory leaks!
function TReadIn.LoadAutocorrelation: TAutocorrelation;
var
  F: TextFile;
  i, N, N2: integer;
  t, oldT, tDelT, junk1: double;
  tAuto: TAutocorrelation;
begin
  N := GetSpectrumFileSize;      // Works ok for autocors too
  tAuto := TAutocorrelation.Create(N);
  N2 := N div 2;

    AssignFile(F, mFileName);
    try
      try
        Reset(F);

  	    // read in data
  	    i := 0;
        while (not Eof(F)) and (i < N) do
        begin
    	    Read(F, t, tAuto.Intensity^[i]);
          if i = N2 then oldT := t;
          if i = N2 + 1 then tDelT := t - oldT;
          Inc(i);
        end;

        // Check for leftover data - this shouldn't happen since N is filesize
        Read(F, junk1);
        if not Eof(F) then MessageDlg('Leftover data!', mtWarning, [mbOK], 0);
      finally
  	    CloseFile(F);
      end;
    except
      ShowMessage('Error reading file ' + mFileName);
    end;
    tAuto.SetMyUnits(tDelT, 800.0);
    // Autocorrelations, being inherently real, don't have a center wavelength

    // Now, a chilling and dangerous fact is that autocorrelations need to be
    //  centered on t = 0.
    tAuto.Center;
    LoadAutocorrelation := tAuto;
end;

// Often experimental data needs to be gridded.  These routines do it.
// All of these require the calibrations of the destination to be set
//   BEFORE the function is called.

procedure TReadIn.GridSpectrum(pSource, pDest: TSpectrum);
var
  lam, f0s, f0d, wp, Lam0, r, scaler, delT, f: double;
  w, N, N2: integer;
begin
  N := pDest.N;
  N2 := N div 2;
  f0d := 300.0/pDest.Lam0;
  f0s := 300.0/pSource.Lam0;
  r := pDest.DelF/pSource.DelF;
  Lam0 := pDest.Lam0;

  // We need to put the data on the proper FFT spacing.
  // Use this on a spectrum read in with LoadSpectrum, it reads in as a
  //   constant wavelength.
  for w := 0 to N - 1 do
  begin

    // This is the lambda that corresponds to the mFFTData points
    f := (w - N2)*pDest.DelF + f0d;
    if (f <> 0 ) then lam := 300.0/f else lam := 1e10;

    // Now figure out where that corresponds in the source data
    // Source data is constant wavelength!
    //wp := (f - f0s)/pSource.DelF + (pSource.N div 2);
    wp := (lam - pSource.Lam0)/pSource.DelLam + (pSource.N div 2);

    // This next line converts from I(lam) to I(f)
    scaler := (lam/Lam0)*(lam/Lam0);

    if (wp < 0) or (wp >= pSource.N - 1) then
      pDest.Intensity^[w] := 0
    else
      pDest.Intensity^[w] := scaler*Window(wp, r, pSource.Intensity, pSource.N);
  end;
end;

procedure TReadIn.GridAutocorrelation(pSource, pDest: TAutocorrelation);
var
  tt, tp, r: double;
  t, N, N2: integer;
begin
  N := pDest.N;
  N2 := N div 2;
	r := pDest.DelT/pSource.DelT;

  // We need to put the data on the proper FFT spacing.
  for t := 0 to N - 1 do
  begin

    // This is the time that corresponds to the mFFTData points
    tt := (t - N2)*pDest.DelT;

    // Now figure out where that corresponds in the source data
    tp := tt/pSource.DelT + (pSource.N div 2);

    if (tp < 0) or (tp >= pSource.N - 1) then
      pDest.Intensity^[t] := 0
    else
      pDest.Intensity^[t] := Window(tp, r, pSource.Intensity, pSource.N);
  end;
end;

procedure TReadIn.GridEField(pSource, pDest: TEField);
var
  lam, f0s, f0d, wp, Lam0, r, scaler, delT, f: double;
  w, N, N2: integer;
begin
  // It's best to do this in the frequency domain, to take care of frequency
  //  offsets. . .
  // It's a PRECONDITION to have the two EFields in the spectral domain.
  N := pDest.N;
  N2 := N div 2;
  f0d := 300.0/pDest.Lam0;
  f0s := 300.0/pSource.Lam0;
	r := pDest.DelF/pSource.DelF;
  Lam0 := pDest.Lam0;

  // We need to put the data on the proper FFT spacing.
  for w := 0 to N - 1 do
  begin

    // This is the lambda that corresponds to the mFFTData points
    f := (w - N2)*pDest.DelF + f0d;
    if (f <> 0 ) then lam := 300.0/f else lam := 1e10;

    // Now figure out where that corresponds in the source data
    wp := (f - f0s)/pSource.DelF + (pSource.N div 2);

    // This next line converts from I(lam) to I(f)
    scaler := (lam/Lam0)*(lam/Lam0);

    if (wp < 0) or (wp >= pSource.N - 1) then
    begin
      pDest.Re^[w] := 0;
      pDest.Im^[w] := 0;
    end
    else
    begin
      pDest.Re^[w] := scaler*Window(wp, r, pSource.Re, pSource.N);
      pDest.Im^[w] := scaler*Window(wp, r, pSource.Im, pSource.N);
    end;
  end;
end;

// This sets the DelT of dest to be equal to that of source by doing it in the
// time domain.  No spectral shifting or FFT scaling involved.
procedure TReadIn.SetEFieldDelT(pSource, pDest: TEField);
var
  tt, tp, r: double;
  t, N, N2: integer;
begin
  N := pDest.N;
  N2 := N div 2;
	 r := pDest.DelT/pSource.DelT;

  for t := 0 to N - 1 do
  begin

    // This is the time that corresponds to the pDest points
    tt := (t - N2)*pDest.DelT;

    // Now figure out where that corresponds in the source data
    tp := tt/pSource.DelT + (pSource.N div 2);

    if (tp < 0) or (tp >= pSource.N - 1) then
    begin
      pDest.Re^[t] := 0;
      pDest.Im^[t] := 0;
    end
    else
    begin
      pDest.Re^[t] := Window(tp, r, pSource.Re, pSource.N);
      pDest.Im^[t] := Window(tp, r, pSource.Im, pSource.N);
    end;
  end;
end;

function TReadIn.Window(wp, r: double; pSource: PArray; pN: integer): double;
var
  i, start, stop: integer;
  hw, sum, csum, height: double;
begin
	hw := Abs(r)/2.0;
	if (hw <= 1.0) then hw := 0.9999;  // See RawData.Window

	sum := 0.0;
	csum := 0.0;
	start := 1 + Trunc(wp - hw);
	stop := Trunc(wp + hw);
	if(start > stop) then stop := start;
	for i := start to stop do
  begin
		if (i >= 0) and (i < pN) then
    begin
			height := 1.0 - Abs(i - wp)/hw;
			if (height < 0.0) then Continue;
			sum := sum + height*pSource^[i];
			csum := csum + height;
		end;
	end;

	//if(sum < 0.0) then sum := 0.0;    // No negative FROG pixels
	if (csum = 0.0) then csum := 1.0;
	Window := (sum/csum);
end;

function TReadIn.IsNull: Boolean;
begin
  if mFileName = mNullFile then
    IsNull := True
  else
    IsNull := False;
end;

end.
