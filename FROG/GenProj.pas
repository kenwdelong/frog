unit GenProj;

interface

uses
	Strategy, FrogObj, SignalField, Func1D, NLO, FrogTrace, Numerics, Shortcut;

type
	TGeneralizedProjectionBasedStrategy = class(TStrategy)
    protected
      mNumerics: TNumerics;
      mNLO: TNLOInteraction;
      mEsig: TSignalField;
      mFrogI: TFrogTrace;
      mShortcut: TShortcut;
      procedure CreateNewEk(pEk: TEField; pEx: PArray); virtual; abstract;
      function ProjectionError(pEx: PArray): double; virtual;
      procedure CalcDerivatives(pEk: TEField; pEsig: TSignalField; Derivs: PArray); virtual;
      function ShortcutError(pEx: PArray): double;
      function ShortcutESigError(pEx: PArray): double;
      procedure ApplyShortcut(pEk: TEField);
      function FrogErrorMinFunc(pX: double): double;
      function CalcError(pFrogk: TFrogTrace): double;
  	public
    	procedure Reset; override;
    	procedure GetNewEk(pEk: TEField; pEsig: TSignalField;
      		pFrogI: TFrogTrace; pNLO: TNLOInteraction); override;
      procedure ErrorIs(pError: double); override;
      constructor Create(pSwitchAfter: integer; pNewField: double;
      				pSpecialPurpose: integer; pName: string); override;
      destructor Destroy; override;
  end;


implementation

uses SysUtils;

constructor TGeneralizedProjectionBasedStrategy.Create(pSwitchAfter: integer; pNewField: double;
      				pSpecialPurpose: integer; pName: string);
begin
  inherited Create(pSwitchAfter, pNewField, pSpecialPurpose, pName);
  mNumerics := TNumerics.Create;
  mShortcut := TShortcut.Create(pSpecialPurpose);
end;

destructor TGeneralizedProjectionBasedStrategy.Destroy;
begin
  mShortcut.Free;
  mNumerics.Free;
  inherited;
end;


procedure TGeneralizedProjectionBasedStrategy.Reset;
begin
  mShortcut.Reset;
end;

procedure TGeneralizedProjectionBasedStrategy.GetNewEk(pEk: TEField; pEsig: TSignalField;
      		pFrogI: TFrogTrace; pNLO: TNLOInteraction);
var
  Ex, Derivs: PArray;
  fret: double;
  t: integer;
begin
  mNLO := pNLO;
  mEsig := pEsig;
  mFrogI := pFrogI;

  // Apply shortcut
  if mShortcut.ShortcutReady then
  begin
    ApplyShortcut(pEk);
    Exit;
  end;

  mShortcut.SetFirstSignalField(pEsig);
  Replace(pEsig, pFrogI);
  mShortcut.SetSecondSignalField(pEsig);

  pEsig.InverseTransformToTime;

  // Now find the field that minimizes Z!
  GetMem(Ex, 2*pEk.N*SizeOf(double));
  GetMem(Derivs, 2*pEk.N*SizeOf(double));
  try
    CalcDerivatives(pEk, pESig, Derivs);
	  //pNLO.ZDerivative(pEk, pEsig, Derivs);  // Template for XFrog
    for t := 0 to pEk.N - 1 do
    begin
      Ex^[2*t] := pEk.Re^[t];
      Ex^[2*t + 1] := pEk.Im^[t];
    end;

    // need to build a projectionerror function using pnlo.z
		mNumerics.Linmin(Ex, Derivs, 2*pEk.N, fret, ProjectionError);
    CreateNewEk(pEk, Ex);
  finally
    FreeMem(Derivs);
    FreeMem(Ex);
  end;
end;

procedure TGeneralizedProjectionBasedStrategy.CalcDerivatives(pEk: TEField; pEsig: TSignalField; Derivs: PArray);
begin
  mNLO.ZDerivative(pEk, pESig, Derivs);  // Template for XFrog
end;

function TGeneralizedProjectionBasedStrategy.ProjectionError(pEx: PArray): double;
begin
  ProjectionError := mNLO.Z(pEx, mEsig); // Template for XFrog
end;

procedure TGeneralizedProjectionBasedStrategy.ErrorIs(pError: double);
begin
  mShortcut.ErrorIs(pError);
end;

procedure TGeneralizedProjectionBasedStrategy.ApplyShortcut(pEk: TEField);
var
  tEgrad: TEField;
  Ex, Derivs: PArray;
  fret: double;
  t, n2: integer;
begin
  // Do this for linear interpolation
  {mShortcut.CalculateShortcutField;
  mNLO.BasicField(mShortcut.SignalField, pEk);}

  // for the minimization wrt E
  {mShortcut.CalculateShortcutField;
  tEgrad := TEField.Create(pEk.N);
  try
    mNLO.BasicField(mShortcut.SignalField, tEgrad);
    GetMem(Ex, 2*pEk.N*SizeOf(double));
    GetMem(Derivs, 2*pEk.N*SizeOf(double));
    try
      for t := 0 to pEk.N - 1 do
      begin
        Ex^[2*t] := pEk.Re^[t];
        Ex^[2*t + 1] := pEk.Im^[t];
        Derivs^[2*t] := 1*tEgrad.Re^[t];
        Derivs^[2*t + 1] := 1*tEgrad.Im^[t];
      end;
      // need to build a projectionerror function using pnlo.z
		  mNumerics.Linmin(Ex, Derivs, 2*pEk.N, fret, ShortcutError);
      for t := 0 to mESig.N - 1 do
      begin
        pEk.Re^[t] := Ex^[2*t];
        pEk.Im^[t] := Ex^[2*t + 1];
      end;
    finally
      FreeMem(Derivs);
      FreeMem(Ex);
    end;
  finally
    tEgrad.Free;
  end;}

  // for the minimization wrt Esig
    mShortcut.CalculateShortcutField;
    n2 := pEk.N*pEk.N;
    GetMem(Ex, 2*n2*SizeOf(double));
    GetMem(Derivs, 2*n2*SizeOf(double));
    try
      for t := 0 to n2 - 1 do
      begin
        Ex^[2*t] := mShortcut.StartField.Re^[t];
        Ex^[2*t + 1] := mShortcut.StartField.Im^[t];
        Derivs^[2*t] := mShortcut.SignalField.Re^[t];
        Derivs^[2*t + 1] := mShortcut.SignalField.Im^[t];
      end;
		  mNumerics.Linmin(Ex, Derivs, 2*n2, fret, ShortcutESigError);
      for t := 0 to n2 - 1 do
      begin
        mShortcut.SignalField.Re^[t] := Ex^[2*t];
        mShortcut.SignalField.Im^[t] := Ex^[2*t + 1];
      end;
      mNLO.BasicField(mShortcut.SignalField, pEk);
    finally
      FreeMem(Derivs);
      FreeMem(Ex);
    end;
end;

// Used when minimizeing the error wrt Ek
function TGeneralizedProjectionBasedStrategy.ShortcutError(pEx: PArray): double;
var
  t: integer;
  tEk: TEField;
  tFrog: TFrogTrace;
  tESig: TSignalField;
  error: double;
begin
  tEk := TEField.Create(mESig.N);
  tESig := TSignalField.Create(mESig.N);
  tFrog := TRetrievedFrogTrace.Create(mESig.N);
  try
    for t := 0 to mESig.N - 1 do
    begin
      tEk.Re^[t] := pEx^[2*t];
      tEk.Im^[t] := pEx^[2*t + 1];
    end;
    mNLO.MakeSignalField(tEk, tESig);
    tFrog.MakeTraceFrom(tESig);
    error := CalcError(tFrog);
  finally
    tFrog.Free;
    tESig.Free;
    tEk.Free;
  end;

  ShortcutError := error;
end;

// Used when minimizing hte error wrt Esig
function TGeneralizedProjectionBasedStrategy.ShortcutESigError(pEx: PArray): double;
var
  t, n2: integer;
  tESig: TSignalField;
  tFrog: TFrogTrace;
  error: double;
begin
  n2 := mESig.N*mESig.N;
  tESig := TSignalField.Create(mESig.N);
  tFrog := TRetrievedFrogTrace.Create(mESig.N);
  try
    for t := 0 to n2 - 1 do
    begin
      tESig.Re^[t] := pEx^[2*t];
      tESig.Im^[t] := pEx^[2*t + 1];
    end;
    tFrog.MakeTraceFrom(tESig);
    error := CalcError(tFrog);
  finally
    tFrog.Free;
    tESig.Free;
  end;

  ShortcutESigError := error;
end;

var
  gFrog: TFrogTrace;

function TGeneralizedProjectionBasedStrategy.CalcError(pFrogk: TFrogTrace): double;
var
  max, maxk, ax, xx, bx, xmin: double;
begin
    // Find the error
	  max := mFrogI.Max;
	  maxk := pFrogk.Max;
	  if maxk = 0.0 then
  	  raise Exception.Create('Retrieval.EvalError: divergent trace.');

	  ax := 0.0;
	  xx := max/maxk;
	  bx := 3.0*max/maxk;
    gFrog := pFrogk;
	  CalcError := TNumerics.Brent(ax, xx, bx, FrogErrorMinFunc, 5.0e-3, xmin);
end;

function TGeneralizedProjectionBasedStrategy.FrogErrorMinFunc(pX: double): double;
var
	t, N2: integer;
  temp, ans: double;
begin
	N2 := mFrogI.N*mFrogI.N;
  ans := 0.0;

	for t := 0 to N2 - 1 do
  begin
    temp := mFrogI.Vals^[t] - pX*gFrog.Vals^[t];
    ans := ans + temp*temp;
  end;

  FrogErrorMinFunc := Sqrt(ans)/(mFrogI.N*mFrogI.Max);
end;

end.
