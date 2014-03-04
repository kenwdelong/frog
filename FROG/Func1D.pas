unit Func1D;

interface

uses
	FrogObj, Numerics;

type

  TDomainType = (dtTime, dtFreq);

	// 1Dfunction abstracts some of the basic functionality of vectors
	T1DFunction = class(TFrogObject)
  	protected
    	 mN: integer;
      mDelT: double;
      mLam0: double;
      mHalf1: double; // These two are for FWHM, so that we can give the
      mHalf2: double; // true FWHM in lambda units
      function FindMax: double;
      function CalcDelF: double;
      function CalcDelLam: double;
    public
    	 Intensity: PArray;
      procedure PlotIntensity(pTitle: string);
      procedure SetMyUnits(pDelT, pLam0: double);
      property IntenMax: double read FindMax;
      //procedure RescaleXAxis(pNewDelT: double); virtual; abstract;
      procedure Center; virtual; abstract;
      procedure CreateAutocorrelationFrom(pSource: T1DFunction); virtual;
      procedure ScaleForNonlinearWavelength; virtual;
      procedure UnscaleForNonlinearWavelength; virtual;
      procedure Norm; virtual;
      procedure Transform; virtual; abstract;
      procedure InverseTransform; virtual; abstract;
      function FWHM: double; virtual;
      function RMSWidth: double; virtual;
      function LaplacianWidth: double; virtual;
      function GetTAt(i: integer): double;
      function GetFAt(i: double): double;
      function GetLamAt(i: double): double;
      property N: integer read mN;
      property DelT: double read mDelT;
      property DelF: double read CalcDelF;
      property DelLam: double read CalcDelLam;
      property Lam0: double read mLam0;
      constructor Create(pN: integer);
      destructor Destroy; override;
  end;

  // This one  is for spectra, autocorrelations, etc.
  TReal1DFunction = class(T1DFunction)
  	 public
      //procedure RescaleXAxis(pNewDelT: double); override;
      procedure Transform; override;
      procedure InverseTransform; override;
      procedure Center; override;
      function InterpolateIntensityAtF(f: double): double;
      //procedure CopyFrom(pSource: TReal1DFunction);
  end;

  TAutocorrelation = TReal1DFunction;
  TSpectrum = TReal1DFunction;
  TMarginal = TReal1DFunction;

  // This one is for E fields
  TComplex1DFunction = class(T1DFunction)
  	protected
    	mDomain: TDomainType;
      mPhaseMax: double;
      mIntenMax: double;
      mTemporalFWHM: double;
      mSpectralFWHM: double;
      mRealSpecFWHM: double;
      mAutocorrelationFWHM: double;
      mFWHM_TBP: double;
      mRMS_TBP: double;
      mSL_TBP: double;
      mTL_TBP: double;
  	public
      // I know this breaks encapsulation but I need speed on these babies.
      Re: PArray;
      Im: PArray;
      Phase: PArray;
      //procedure RescaleXAxis(pNewDelT: double); override;
      procedure ExpandT;
      procedure ExpandS;
      procedure Center; override;
      procedure RescaleHeight(pR: double);
      procedure CopyFrom(pSource: TComplex1DFunction);
      procedure Add(pSource: TComplex1DFunction);
      procedure Transform; override;
      procedure InverseTransform; override;
      procedure Norm; override;
      function FWHM: double; override;
      function RMSWidth: double; override;
      function LaplacianWidth: double; override;
      procedure SetIntensityAndPhase;
      procedure ClipPhase(pFloor: double);
      procedure ScaleForNonlinearWavelength; override;
      procedure Perturb(pEps: double);
      procedure CalculateTimeBandwidthProducts;
      procedure PlotReAndIm(pTitle:string);
      procedure FlattenPhase;
      property Domain: TDomainType read mDomain;
      property IntenMax: double read mIntenMax;
      property PhaseMax: double read mPhaseMax;
      property TemporalFWHM: double read mTemporalFWHM;
      property SpectralFWHM: double read mSpectralFWHM;
      property RealSpectralFWHM: double read mRealSpecFWHM;
      property AutocorrelationFWHM: double read mAutocorrelationFWHM;
      property FWHM_TBP: double read mFWHM_TBP;
      property RMS_TBP: double read mRMS_TBP;
      property SpectralLaplacianTBP: double read mSL_TBP;
      property TemporalLaplacianTBP: double read mTL_TBP;
      constructor Create(pN: integer);
      destructor Destroy; override;
  end;

  TEField = TComplex1DFunction;


implementation

uses Math, TestPlot;

const
  SMALL_F = 1e-10;

//  *********************************************************
//  T1DFunction
//     Base class for EFields, Autocorrelations, etc;

constructor T1DFunction.Create(pN: integer);
begin
	 inherited Create;
	 GetMem(Intensity, pN*SizeOf(double));
  mN := pN;
  mDelT := 1.0;
  mLam0 := 800;
end;

destructor T1DFunction.Destroy;
begin
	 FreeMem(Intensity);
  inherited Destroy;
end;

procedure T1DFunction.PlotIntensity(pTitle: string);
var
	xTestPlot: TfrmTestPlot;
begin
  xTestPlot := TfrmTestPlot.Create;
  xTestPlot.SetTitle(pTitle);
  xTestPlot.PlotIntensity(self);
  xTestPlot.ShowModal;
  xTestPlot.Free;
end;

function T1DFunction.FindMax: double;
var
	i: integer;
  max: double;
begin
	max := 0.0;
  for i := 0 to mN - 1 do
  	if Intensity^[i] > max then max := Intensity^[i];
  FindMax := max;
end;

function T1DFunction.CalcDelF: double;
begin
  CalcDelF := 1.0/(mN*mDelT);
end;

function T1DFunction.CalcDelLam: double;
begin
  CalcDelLam := -mLam0*mLam0/(300.0*mN*mDelT);
end;

function T1DFunction.GetTAt(i: integer): double;
begin
	GetTAt := (i - (mN div 2))*mDelT;
end;

function T1DFunction.GetFAt(i: double): double;
var
	f0: double;
begin
  f0 := 300.0/mLam0;
  GetFAt := f0 + (i - (N div 2))*DelF;
end;

function T1DFunction.GetLamAt(i: double): double;
begin
  GetLamAt := 300.0/GetFAt(i);
end;

procedure T1DFunction.SetMyUnits(pDelT, pLam0: double);
begin
  mDelT := pDelT;
  mLam0 := pLam0;
end;

function T1DFunction.FWHM: double;
var
  i: integer;
  max: double;
begin
	// Find the maximum
	max := FindMax;

	// Step forward to find the half-max point
	for i := 1 to mN - 1 do
    if Intensity^[i]/max > 0.5 then Break;
	mHalf1 := (0.5*max - Intensity^[i-1])/(Intensity^[i] - Intensity^[i-1]) + i - 1;

	// Step backward to find the half-max point
	for i := mN - 2 downto 0 do
    if Intensity^[i]/max > 0.5 then Break;
	mHalf2 := (0.5*max - Intensity^[i])/(Intensity^[i+1] - Intensity^[i]) + i;

	FWHM := Abs((mHalf2 - mHalf1)*mDelT);
end;

function T1DFunction.RMSWidth: double;
var
  t: integer;
  sum, tsum, t2sum, tbar, width: double;
begin
  sum := 0; tsum := 0; t2sum := 0;
	for t := 0 to mN - 1 do
  begin
		sum := sum + Intensity^[t];
		tsum := tsum + t*Intensity^[t];
		t2sum := t2sum + t*t*Intensity^[t];
	end;
	tbar := tsum/sum;
	width := t2sum/sum - tbar*tbar;

	RMSWidth := sqrt(width)*mDelT;
end;

//This one finds the Laplacian width of the function.  Using this defn, all transform
//limited pulses should have a TBP of 0.5! TL is defined as no chirp in the domain where
//the Laplacian width is calculated.  Eg, t_lap*w_rms = 0.5 if phi(t) is constant. */
function T1DFunction.LaplacianWidth: double;
var
  dsum, sum, derv: double;
  t: integer;
begin
	// Make intensity
  sum := 0;
	for t := 0 to mN - 1 do
    sum := sum + Intensity^[t];

	// Make derivative
  dsum := 0;
	for t := 0 to mN - 2 do
  begin
		derv := sqrt(Intensity^[t+1]) - sqrt(Intensity^[t]);
		dsum := dsum + derv*derv;
  end;

	LaplacianWidth := (0.5*sqrt(sum/dsum))*mDelT;
end;

procedure T1DFunction.CreateAutocorrelationFrom(pSource: T1DFunction);
var
  t, tau, tp, N2: integer;
begin
  mDelT := pSource.DelT;
  mLam0 := pSource.Lam0;

  N2 := mN div 2;
  for tau := 0 to mN - 1 do
  begin
    Intensity^[tau] := 0;
    for t := 0 to mN - 1 do
    begin
      tp := t - tau + N2;
      if (tp >= 0) and (tp < mN) then
        Intensity^[tau] := Intensity^[tau] + pSource.Intensity^[t]*pSource.Intensity^[tp];
    end;
  end;
end;

procedure T1DFunction.ScaleForNonlinearWavelength;
var
  center, scaler, f0, f, max, lam: double;
  i, N2: integer;
begin
  // This scales for the nonlinear frequency
  // Precondition: must be in frequency domain already

  center := mLam0;
  f0 := 300.0/mLam0;
  N2 := mN div 2;
  max := 0.0;

  for i := 0 to mN - 1 do
  begin
    f := f0 + DelF*(i - N2);
    if f = 0.0 then f := SMALL_F;
    lam := 300.0/f;
    scaler := center*center/(lam*lam);
    Intensity^[i] := Intensity^[i]*scaler;
    if Intensity^[i] > max then max := Intensity^[i];
  end;

  for i := 0 to mN - 1 do
    Intensity^[i] := Intensity^[i]/max;
end;

procedure T1DFunction.UnscaleForNonlinearWavelength;
var
  center, scaler, f0, f, max, lam: double;
  i, N2: integer;
begin
  // This scales for the nonlinear frequency
  // Precondition: must be in frequency domain already
  // This turns a normal experimental spectrum into one scaled
  //   for FFT

  center := mLam0;
  f0 := 300.0/mLam0;
  N2 := mN div 2;
  max := 0.0;

  for i := 0 to mN - 1 do
  begin
    f := f0 + DelF*(i - N2);
    if f = 0.0 then f := SMALL_F;
    lam := 300.0/f;
    scaler := (lam*lam)/(center*center);
    Intensity^[i] := Intensity^[i]*scaler;
    if Intensity^[i] > max then max := Intensity^[i];
  end;

  for i := 0 to mN - 1 do
    Intensity^[i] := Intensity^[i]/max;
end;

procedure T1DFunction.Norm;
var
  max: double;
  i: integer;
begin
  max := FindMax;
  for i := 0 to mN - 1 do
  begin
    Intensity^[i] := Intensity^[i]/max;
  end;
end;


// ***********************************************
//  TReal1DFunction
//  base class for spectra and autocorrelations

procedure TReal1DFunction.Center;
var
	Tr: PArray;
  sum, sumt, max, temp: double;
  tmax, t, tprime: integer;
begin
	GetMem(Tr, mN*SizeOf(double));

  try
  	// Find the max
  	Sum := 0.0;
  	SumT := 0.0;
  	Max := 0.0;
  	for t := 0 to mN - 1 do
  	begin
  		Tr^[t] := Intensity^[t];
    	temp := Intensity^[t];
    	Sum := Sum + temp;
    	SumT := SumT + temp*t;
    	if (temp > Max) then
    	begin
    		Max := temp;
      	//tmax := t;
    	end;
  	end;

  	tmax := Trunc(sumt/sum + 0.49);
    Max := Sqrt(Max);

  	// Center field
  	for t := 0 to mN - 1 do
  	begin
  		tprime := t - (mN div 2 - tmax);
    	if (tprime < 0) or (tprime >= mN) then
    	begin
    		Intensity^[t] := 0.0;
      	Continue;
    	end;
    	Intensity^[t] := Tr^[tprime]/max;
  	end;
  finally
  	FreeMem(Tr);
  end;
end;

procedure TReal1DFunction.Transform;
var
  Im: PArray;
  i: integer;
begin
  GetMem(Im, mN*Sizeof(double));
  for i := 0 to mN - 1 do
    Im^[i] := 0.0;
  try
  	TNumerics.FFT(Intensity, Im, mN, 1);
    //mDomain := dtFreq;
  finally
    FreeMem(Im);
  end;
end;

procedure TReal1DFunction.InverseTransform;
var
  Im: PArray;
  i: integer;
begin
  GetMem(Im, mN*Sizeof(double));
  for i := 0 to mN - 1 do
    Im^[i] := 0.0;
  try
  	TNumerics.FFT(Intensity, Im, mN, -1);
    //mDomain := dtTime;
  finally
    FreeMem(Im);
  end;
end;

function TReal1DFunction.InterpolateIntensityAtF(f: double): double;
var
   x: double;
   i, xl, xh: integer;
begin
     x := (f - 300.0/mLam0)/DelF + (mN div 2);
     if x < 0 then
       InterpolateIntensityAtF := 0
     else if x >= (N - 1) then
       InterpolateIntensityAtF := 0
     else
     begin
       xl := Trunc(x);
       xh := xl + 1;
       InterpolateIntensityAtF := Intensity^[xl] + (Intensity^[xh] - Intensity^[xl])*(x - xl);
     end;
end;


// ************************************************
//  TComplex1DFunction
//  base class for TEField
// ************************************************
constructor TComplex1DFunction.Create(pN: integer);
begin
  inherited Create(pN);
  GetMem(Re, mN*SizeOf(double));
  GetMem(Im, mN*SizeOf(double));
  GetMem(Phase, mN*SizeOf(double));
  mDomain := dtTime;
end;

destructor TComplex1DFunction.Destroy;
begin
  FreeMem(Phase);
  FreeMem(Im);
  FreeMem(Re);
  inherited Destroy;
end;

procedure TComplex1DFunction.RescaleHeight(pR: double);
var
	i: integer;
begin
  for i := 0 to mN - 1 do
  begin
  	Re^[i] := Re^[i]*pR;
    Im^[i] := Im^[i]*pR;
  end;
end;

procedure TComplex1DFunction.Center;
var
	Tr, Ti: PArray;
  sum, sumt, max, temp: double;
  tmax, t, tprime: integer;
begin
	GetMem(Tr, mN*SizeOf(double));
  GetMem(Ti, mN*SizeOf(double));

  try
  	// Find the max
  	Sum := 0.0;
  	SumT := 0.0;
  	Max := 0.0;
  	for t := 0 to mN - 1 do
  	begin
  		Tr^[t] := Re^[t];
    	Ti^[t] := Im^[t];
    	temp := Re^[t]*Re^[t] + Im^[t]*Im^[t];
    	Sum := Sum + temp;
    	SumT := SumT + temp*t;
    	if (temp > Max) then
    	begin
    		Max := temp;
      	//tmax := t;
    	end;
  	end;

  	tmax := Trunc(sumt/sum + 0.49);
    Max := Sqrt(Max);

  	// Center field
  	for t := 0 to mN - 1 do
  	begin
  		tprime := t - (mN div 2 - tmax);
    	if (tprime < 0) or (tprime >= mN) then
    	begin
    		Re^[t] := 0.0;
      	Im^[t] := 0.0;
      	Continue;
    	end;
    	Re^[t] := Tr^[tprime]/max;
    	Im^[t] := Ti^[tprime]/max;
  	end;
  finally
		FreeMem(Ti);
  	FreeMem(Tr);
  end;
end;

procedure TComplex1DFunction.CopyFrom(pSource: TComplex1DFunction);
var
	i: integer;
begin
	// Copy the member vars
  mN := pSource.N;
  mDelT := pSource.DelT;
  mLam0 := pSource.Lam0;
  mDomain := pSource.Domain;

  // Only copy real and imaginary
	for i := 0 to mN - 1 do
  begin
    Re^[i] := pSource.Re^[i];
    Im^[i] := pSource.Im^[i];
  end;
end;

procedure TComplex1DFunction.Transform;
begin
  	TNumerics.FFT(Re, Im, mN, 1);
    mDomain := dtFreq;
end;

procedure TComplex1DFunction.InverseTransform;
begin
  	TNumerics.FFT(Re, Im, mN, -1);
    mDomain := dtTime;
end;

procedure TComplex1DFunction.Perturb(pEps: double);
var
	t: integer;
  max, temp, pertFieldInten: double;
begin
	max := 0.0;
	for t := 0 to mN - 1 do
  begin
    temp := Re^[t]*Re^[t] + Im^[t]*Im^[t];
    if temp > max then max := temp;
  end;

  Randomize;
  pertFieldInten := max*pEps;
  max := sqrt(pertFieldInten);
  for t := 0 to mN - 1 do
  begin
		Re^[t] := Re^[t] + 2*(Random - 0.5)*max;
		Im^[t] := Im^[t] + 2*(Random - 0.5)*max;
  end;
end;

procedure TComplex1DFunction.SetIntensityAndPhase;
var
  i: integer;
  intenmax, den, tr, ti: double;
begin
	// Find the intensity
  intenmax := 0;
  for i := 0 to N - 1 do
  begin
  	Intensity^[i] := Re^[i]*Re^[i] + Im^[i]*Im^[i];
    if Intensity^[i] > intenmax then intenmax := Intensity^[i];
    Phase^[i] := 0.0;
  end;
  mIntenMax := intenmax;

	Phase^[0] := 0.0;
	for i := 1 to mN - 1 do
  begin
		den := Intensity^[i-1];
		if (den = 0.0) or (Intensity^[i] = 0.0) then
    begin
			Phase^[i] := Phase^[i-1];
			Continue;
		end;
		tr := Re^[i]*Re^[i-1] + Im^[i]*Im^[i-1];
		ti := Im^[i]*Re^[i-1] - Re^[i]*Im^[i-1];
		Phase^[i] := Phase^[i-1] + ArcTan2(ti/den, tr/den);
	end;

  ClipPhase(0.0);
end;

procedure TComplex1DFunction.ClipPhase(pFloor: double);
var
  i: integer;
  phmax, phmin, clipLevel: double;
begin
  // Precondition: SetIntensityAndPhase was just called!!

	phmin := 1.0e20;  phmax := -1.0e20;
  clipLevel := pFloor*mIntenMax;
	// Find the min and max of the phase
	for i := 0 to mN - 1 do
  begin
		if(Intensity^[i] < clipLevel) then Continue;
		if(Phase^[i] < phmin) then phmin := Phase^[i];
		if(Phase^[i] > phmax) then phmax := Phase^[i];
	end;

	// Set baseline to zero, and zero the wings
	for i := 0 to mN - 1 do
  begin
		Phase^[i] := Phase^[i] - phmin;
		if Intensity^[i] < clipLevel then Phase^[i] := 0.0;
	end;
	mPhaseMax := phmax - phmin;
end;

procedure TComplex1DFunction.Add(pSource: TComplex1DFunction);
var
  i: integer;
begin
  // Precondition pSource.N = mN
  for i := 0 to mN - 1 do
  begin
    Re^[i] := Re^[i] + pSource.Re^[i];
    Im^[i] := Im^[i] + pSource.Im^[i];
  end;
end;

procedure TComplex1DFunction.Norm;
var
  max: double;
  i: integer;
begin
  SetIntensityAndPhase;
  max := sqrt(mIntenMax);
  for i := 0 to mN - 1 do
  begin
    Re^[i] := Re^[i]/max;
    Im^[i] := Im^[i]/max;
  end;
end;

function TComplex1DFunction.FWHM: double;
var
  res: double;
begin
  SetIntensityAndPhase;
  res := inherited FWHM;
  if mDomain = dtFreq then
  begin
    mRealSpecFWHM := Abs(GetLamAt(mHalf1) - GetLamAt(mHalf2));
    res := res*DelF/mDelT;
  end;
  FWHM := Abs(res);
end;

function TComplex1DFunction.RMSWidth: double;
var
  res: double;
begin
  SetIntensityAndPhase;
  res := inherited RMSWidth;
  if mDomain = dtFreq then
    res := res*DelF/mDelT;
  RMSWidth := res;
end;

function TComplex1DFunction.LaplacianWidth: double;
var
  res: double;
begin
  SetIntensityAndPhase;
  res := inherited LaplacianWidth;
  if mDomain = dtFreq then
    res := res*DelF/mDelT;
  LaplacianWidth := res;
end;

procedure TComplex1DFunction.ExpandT;
var
  FWHMWidth: double;
  i, newN, pad: integer;
  newRe, newIm: PArray;
begin
	FWHMWidth := FWHM/mDelT;
  i := 1;
  // 20 points under the FWHM
  while i < 20.0/FWHMWidth do
    i := i*2;
	newN := i*mN;

	pad := (newN - mN) div 2;
	// Go to Freq. domain
  Transform;

  GetMem(newRe, newN*SizeOf(double));
  GetMem(newIm, newN*SizeOf(double));

	// Pad
	for i := 0 to pad - 1 do
  begin
    newRe^[i] := 0.0;
    newIm^[i] := 0.0;
  end;
	for i := pad to pad + mN - 1 do
  begin
		newRe^[i] := Re^[i - pad];
		newIm^[i] := Im^[i - pad];
	end;
	for i := pad + mN to newN - 1 do
  begin
    newRe^[i] := 0.0;
    newIm^[i] := 0.0;
  end;

  // Free the old arrays
  FreeMem(Im);
  FreeMem(Re);
  Re := newRe;
  Im := newIm;
  FreeMem(Intensity);
  FreeMem(Phase);
  GetMem(Intensity, newN*SizeOf(double));
  GetMem(Phase, newN*SizeOf(double));
  mDelT := mDelT/(newN/mN);
  mN := newN;

	// Back to time domain
  InverseTransform;
  Norm;
end;

procedure TComplex1DFunction.ExpandS;
var
  FWHMWidth: double;
  i, newN, pad: integer;
  newRe, newIm: PArray;
begin
	FWHMWidth := FWHM/DelF;
  i := 1;
  // 20 points under the FWHM
  while i < 20.0/FWHMWidth do
    i := i*2;
	newN := i*mN;

	pad := (newN - mN) div 2;
	// Go to Time. domain
  InverseTransform;

  GetMem(newRe, newN*SizeOf(double));
  GetMem(newIm, newN*SizeOf(double));

	// Pad
	for i := 0 to pad - 1 do
  begin
    newRe^[i] := 0.0;
    newIm^[i] := 0.0;
  end;
	for i := pad to pad + mN - 1 do
  begin
		newRe^[i] := Re^[i - pad];
		newIm^[i] := Im^[i - pad];
	end;
	for i := pad + mN to newN - 1 do
  begin
    newRe^[i] := 0.0;
    newIm^[i] := 0.0;
  end;

  // Free the old arrays
  FreeMem(Im);
  FreeMem(Re);
  Re := newRe;
  Im := newIm;
  FreeMem(Intensity);
  FreeMem(Phase);
  GetMem(Intensity, newN*SizeOf(double));
  GetMem(Phase, newN*SizeOf(double));
  // Expanding in the freq domain doesn't change DelT!
  mN := newN;

	// Back to time domain
  Transform;
  Norm;
end;

procedure TComplex1DFunction.ScaleForNonlinearWavelength;
begin
  // This scales for the nonlinear frequency
  // Precondition: must be in frequency domain already
  SetIntensityAndPhase;
  inherited;
end;

procedure TComplex1DFunction.CalculateTimeBandwidthProducts;
var
  tempE: TEField;
  tempAuto: TAutocorrelation;
  tFWHM, tAuto, tRMS, tLap, sFWHM, sRMS, sLap, LamScale, sRealFWHM: double;
begin
  // Here is how you do Time-Bandwidth calculations of all sorts!
  tempE := TEField.Create(mN);
  try
  tempE.CopyFrom(Self);
  tempE.ExpandT;
  tFWHM := tempE.FWHM;
  tRMS := tempE.RMSWidth;
  tLap := tempE.LaplacianWidth;
  tempAuto := TAutocorrelation.Create(tempE.N);
    try
      tempAuto.CreateAutocorrelationFrom(tempE);
      tAuto := tempAuto.FWHM;
    finally
      tempAuto.Free;
    end;
  finally
  tempE.Free;
  end;

  tempE := TEField.Create(mN);
  try
  tempE.CopyFrom(Self);
  tempE.Transform;
  tempE.ExpandS;
  sFWHM := tempE.FWHM;
  sRealFWHM := tempE.RealSpectralFWHM;
  sRMS := tempE.RMSWidth;
  sLap := tempE.LaplacianWidth;
  LamScale := tempE.DelLam/tempE.DelF;
  finally
  tempE.Free;
  end;

  mTemporalFWHM := tFWHM;
  mSpectralFWHM := sRealFWHM;
  //mSpectralFWHM := Abs(LamScale*sFWHM);
  mAutocorrelationFWHM := tAuto;
  mFWHM_TBP := tFWHM*sFWHM;
  mRMS_TBP := tRMS*sRMS*2*Pi;
  mTL_TBP := tLap*sRMS*2*Pi;
  mSL_TBP := tRMS*sLap*2*Pi;
end;

procedure TComplex1DFunction.PlotReAndIm(pTitle: string);
var
	xTestPlot: TfrmTestPlot;
begin
  xTestPlot := TfrmTestPlot.Create;
  xTestPlot.SetTitle(pTitle);
  xTestPlot.PlotRealAndIm(self);
  xTestPlot.ShowModal;
  xTestPlot.Free;
end;

procedure TComplex1DFunction.FlattenPhase;
var
	i: integer;
  inten: double;
begin
  for i := 0 to mN - 1 do
  begin
  	inten := Re^[i]*Re^[i] + Im^[i]*Im^[i];
    Re^[i] := sqrt(inten);
    Im^[i] := 0.0;
  end;
end;

end.
