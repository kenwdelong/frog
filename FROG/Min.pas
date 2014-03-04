unit Min;

interface

uses Strategy, Func1D, SignalField, NLO, Numerics, FrogTrace;

type
  TMinimization = class(TStrategy)
    private
      mMinIsSetup: Boolean;
      mNumerics: TNumerics;
      mP, mH: PArray;
      mNLO: TNLOInteraction;
      mEk: TEField;
      mFrogI: TFrogTrace;
      mEsig: TSignalField;
      function GErrorFunction(pE: PArray): double;
    protected
      mXi: PArray;
      mG: PArray;
      function GetDGG(pN: integer): double; virtual; abstract;
    public
      procedure Reset; override;
      procedure GetNewEk(pEk: TEField; pEsig: TSignalField;
      		pFrogI: TFrogTrace; pNLO: TNLOInteraction); override;
      constructor Create(pSwitchAfter: integer; pNewField: double;
          pSpecialPurpose: integer; pName: string); override;
      destructor Destroy; override;

  end;

implementation


constructor TMinimization.Create(pSwitchAfter: integer; pNewField: double;
      				pSpecialPurpose: integer; pName: string);
begin
  inherited Create(pSwitchAfter, pNewField, pSpecialPurpose, pName);
  mMinIsSetup := False;
  mNumerics := TNumerics.Create;
end;

procedure TMinimization.GetNewEk(pEk: TEField; pEsig: TSignalField;
      		pFrogI: TFrogTrace; pNLO: TNLOInteraction);
var
  NDim, j, N: integer;
  gg, dgg, gam, err: double;
begin
  N := pEk.N;
  NDim := 2*N;
  mNLO := pNLO;
  mFrogI := pFrogI;
  mEk := pEk;
  mEsig := pEsig;
  // Make sure that the signal field is in synch for GDerivative
  //   seems to work ok without it tho. . .
  mNLO.MakeSignalField(pEk,pEsig);

  if not mMinIsSetup then
  begin
    GetMem(mP, NDim*SizeOf(double));
    GetMem(mG, NDim*SizeOf(double));
    GetMem(mH, NDim*SizeOf(double));
    GetMem(mXi, NDim*SizeOf(double));
    mMinIsSetup := True;

    pNLO.GDerivative(pEk, pEsig, pFrogI, mXi);
    for j := 0 to NDim - 1 do
    begin
      mG^[j] := -mXi^[j];
      mH^[j] := mG^[j];
      mXi^[j] := mG^[j];
    end;
  end
  else
  begin
    pNLO.GDerivative(pEk, pEsig, pFrogI, mXi);
    gg := 0.0;
    for j := 0 to NDim - 1 do
      gg := gg + mG^[j]*mG^[j];
    dgg := GetDGG(NDim);        // This distinguishes FR from PR
    gam := dgg/gg;
    for j := 0 to NDim - 1 do
    begin
      mG^[j] := -mXi^[j];
      mH^[j] := mG^[j] + gam*mH^[j];
      mXi^[j] := mH^[j];
    end;
  end;

  for j := 0 to N - 1 do
  begin
    mP^[2*j] := pEk.Re^[j];
    mP^[2*j + 1] := pEk.Im^[j];
  end;
  mNumerics.LinMin(mP, mXi, NDim, err, GErrorFunction);
  for j := 0 to N - 1 do
  begin
    pEk.Re^[j] := mP^[2*j];
    pEk.Im^[j] := mP^[2*j + 1];
  end;
end;

function TMinimization.GErrorFunction(pE: PArray): double;
var
  N, t, tau, w, i: integer;
  temp, ans: double;
begin
  N := mEk.N;

  // Use Ek as a temp holding spot.  I think this is ok.
  for t := 0 to N - 1 do
  begin
    mEk.Re^[t] := pE^[2*t];
    mEk.Im^[t] := pE^[2*t + 1];
  end;

  mNLO.MakeSignalField(mEk, mEsig);

  // I think we shouldn't use Retrieval.EvalError here, because it changes then
  //  value of pE, and that might upset linmin, which is counting on knowing
  //  where it's vector is.
  ans := 0.0;
	for tau := 0 to N - 1 do
  	for w := 0 to N - 1 do
    begin
      i := tau*N + w;
      temp := mEsig.Re^[i]*mEsig.Re^[i] + mEsig.Im^[i]*mEsig.Im^[i];
    	temp := mFrogI.Vals^[i] - temp;
      ans := ans + temp*temp;
    end;
  GErrorFunction := ans;
end;

procedure TMinimization.Reset;
begin
  FreeMem(mXi);
  FreeMem(mH);
  FreeMem(mG);
  FreeMem(mP);
  mXi := nil;
  mH := nil;
  mG := nil;
  mP := nil;
  mMinIsSetup := False;
end;

destructor TMinimization.Destroy;
begin
  mNumerics.Free;
  if mMinIsSetup then
    Reset;
  inherited Destroy;
end;

end.
