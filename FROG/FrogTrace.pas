unit FrogTrace;

interface

uses
	FrogObj, Numerics, SignalField, Func1D, TimeFreq;

type
	TFROGTrace = class(TFrogObject)
    private
      mN: integer;
      mDelT: double;
      mDelLam: double;
      mLam0: double;
      mDelF: double;
    protected
      function GetMax: double;
  	public
    	Vals: PArray;
  		function Max: double; virtual; abstract;
     procedure MakeTraceFrom(pEsig: TSignalField);
     function ZeroOrderFreqMarginal: TMarginal;
     function ZeroOrderDelayMarginal: TMarginal;
     procedure SynchronizeUnits(pE: TEField);
			procedure SynchronizeUnitsFromTF(pTF: TTimeFreqCalibration);
     procedure ForceAgreementWithMarginal(pExpMarg: TMarginal);
     procedure CopyFrom(pTrace: TFROGTrace);
     property DelT: double read mDelT;
     property DelF: double read mDelF;
     property Lam0: double read mLam0;
     property DelLam: double read mDelLam;
     property N: integer read mN;
     constructor Create(pN: integer);
     destructor Destroy; override;
  end;

  TOriginalFrogTrace = class(TFrogTrace)
  	private
    	mMax: double;
  	public
    	function Max: double; override;
     constructor Create(pN: integer);
  end;

  TRetrievedFrogTrace = class(TFrogTrace)
  	public
    	function Max: double; override;
  end;

implementation

constructor TFROGTrace.Create(pN: integer);
begin
	inherited Create;
  mN := pN;
	GetMem(Vals, mN*mN*SizeOf(double));
end;

function TFrogTrace.GetMax: double;
var
	i: integer;
  max: double;
begin
	{ Find the peak of the trace }
	max := 0;
	for i:= 0 to (mN*mN-1) do
		if(Vals^[i] > max) then max := Vals^[i];
  GetMax := max;
end;

procedure TFROGTrace.SynchronizeUnits(pE: TEField);
begin
  //mE := pE;
  mDelT := pE.DelT;
  mDelF := pE.DelF;
  mDelLam := pE.DelLam;
  mLam0 := pE.Lam0;
end;

procedure TFROGTrace.SynchronizeUnitsFromTF(pTF: TTimeFreqCalibration);
begin
  mDelT := pTF.DelT;
  mDelF := pTF.DelF;
  mDelLam := pTF.DelLam;
  mLam0 := pTF.Lam0;
end;

procedure TFROGTrace.MakeTraceFrom(pEsig: TSignalField);
var
	i, j :integer;
begin
  for i := 0 to mN - 1 do
  	for j := 0 to mN - 1 do
    	Vals^[i*mN + j] := pEsig.Re^[i*mN + j]*pEsig.Re^[i*mN + j] +
                         pEsig.Im^[i*mN + j]*pEsig.Im^[i*mN + j];
end;

destructor TFROGTrace.Destroy;
begin
	FreeMem(Vals);
  Vals := nil;
  inherited Destroy;
end;

procedure TFROGTrace.CopyFrom(pTrace: TFROGTrace);
var
   i: integer;
begin
     if not (pTrace.N = mN) then
     begin
          mN := pTrace.N;
          FreeMem(Vals);
          GetMem(Vals, mN*mN*SizeOf(double));
     end;

     for i := 0 to (mN*mN - 1) do
         Vals^[i] := pTrace.Vals^[i];

end;


// NOTE: for the marginal functions, the client is responsible for freeing the memory
function TFrogTrace.ZeroOrderFreqMarginal: TMarginal;
var
  tau, w: integer;
  ZOFM: TReal1DFunction;
begin
  ZOFM := TMarginal.Create(mN);
  for w := 0 to mN - 1 do
  begin
    ZOFM.Intensity^[w] := 0.0;
    for tau := 0 to mN - 1 do
      ZOFM.Intensity^[w] := ZOFM.Intensity^[w] + Vals^[tau*mN + w];
  end;
  ZeroOrderFreqMarginal := ZOFM;
  ZOFM.SetMyUnits(DelT, Lam0);
end;

function TFrogTrace.ZeroOrderDelayMarginal: TMarginal;
var
  tau, w: integer;
  ZODM: TMarginal;
begin
  ZODM := TMarginal.Create(mN);
  for tau := 0 to mN - 1 do
  begin
    ZODM.Intensity^[tau] := 0.0;
    for w := 0 to mN - 1 do
      ZODM.Intensity^[tau] := ZODM.Intensity^[tau] + Vals^[tau*mN + w];
  end;
  ZeroOrderDelayMarginal := ZODM;
  ZODM.SetMyUnits(DelT, Lam0);
end;

procedure TFrogTrace.ForceAgreementWithMarginal(pExpMarg: TMarginal);
var
   tau, w: integer;
   scaler: double;
   frogZOFM: TMarginal;
   f, inten: double;
begin
  frogZOFM := Self.ZeroOrderFreqMarginal;
  frogZOFM.Norm;
  pExpMarg.Norm;
  for w := 0 to mN - 1 do
  begin
    f := frogZOFM.GetFAt(w);
    inten := pExpMarg.InterpolateIntensityAtF(f);
    if frogZOFM.Intensity^[w] = 0 then continue;
    scaler := inten/frogZOFM.Intensity^[w];
    if scaler < 0.0 then scaler := 0.0;
    for tau := 0 to mN - 1 do
      Vals^[tau*mN + w] := scaler*Vals^[tau*mN + w];
  end;
  frogZOFM.Free;
end;


// ******* TOriginalFrogTrace ******************

constructor TOriginalFrogTrace.Create(pN: integer);
begin
	inherited Create(pN);
  mMax := 0.0;
end;

// Only find the max once here!
function TOriginalFrogTrace.Max: double;
begin
	if mMax = 0.0 then
  	mMax := GetMax;
	Max := mMax;
end;

// ************** TRetrievedFrogTrace ************8
function TRetrievedFrogTrace.Max: double;
begin
	Max := GetMax;
end;

end.
