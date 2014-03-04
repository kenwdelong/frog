unit Retrieval;

interface

uses
	FrogObj, Func1D, SignalField, FrogTrace, Strategy, NLO;

type
	TRetrieval = class(TFrogObject)
  	private
      mK: integer;
      mCurrentError: double;
      mFrogI: TFrogTrace;
      mFrogIk: TFrogTrace;
      mEsig: TSignalField;
      mEk: TEField;
      function GErrorMinFunc(pX: double): double;
      function EvalError: double;
    protected
    	mCurrentStrategy: TStrategy;
      mNLOInteraction: TNLOInteraction;
      procedure MakeSignalField(pEk: TEField; pESig: TSignalField); virtual;
      function GetRescaleFactor(pXmin: double): double; virtual;
      procedure SetCurrentStrategy(pCurrStrat: TStrategy); virtual;
      procedure CenterRetrievedField; virtual;
    public
    	procedure DoOneIteration;
      procedure MakeInitialFrogTrace(pE: TEField);
      procedure InitForRetrieval;
      procedure CalcBestTrace(pEBest: TEField);
      procedure ReInitializeSignalField;
      property CurrentStrategy: TStrategy read mCurrentStrategy write SetCurrentStrategy;
      property K: integer read mK;
      property CurrentError: double read mCurrentError;
      property Error: double read EvalError;
      constructor Create(pNLO: TNLOInteraction; pFrogI, pFrogIk: TFrogTrace; pEk: TEField);
      destructor Destroy; override;
  end;

implementation

uses Numerics, SysUtils, Math;

procedure TRetrieval.DoOneIteration;
begin
	mCurrentStrategy.GetNewEk(mEk, mEsig, mFrogI, mNLOInteraction);
  //mEk.Center;
  CenterRetrievedField;
  MakeSignalField(mEk, mESig);
  mFrogIk.MakeTraceFrom(mEsig);
  mCurrentError := EvalError;
  Inc(mK);
end;

procedure TRetrieval.CenterRetrievedField;
begin
	mEk.Center;
end;

procedure TRetrieval.MakeInitialFrogTrace(pE: TEField);
var
	tempESig: TSignalField;
begin
	tempESig := TSignalField.Create(pE.N);
  try
  	//mNLOInteraction.MakeSignalField(pE, tempESig);
    MakeSignalField(pE, tempESig);
  	mFrogI.MakeTraceFrom(tempEsig);
  finally
  	tempESig.Free;
  end;
end;


constructor TRetrieval.Create(pNLO: TNLOInteraction; pFrogI, pFrogIk: TFrogTrace;
															 pEk: TEField);
begin
  // Precondition: mEk has been init'ed with it's values
  // All input references are attached to objects: pNLO, pFrogI, pFrogIk, pEk
	inherited Create;

  mK := 0;
	mNLOInteraction := pNLO;
  mFrogI := pFrogI;
  mFrogIk := pFrogIk;
  mEk := pEk;

  mEsig := TSignalField.Create(mEk.N);

  // init the first guess field here
  InitForRetrieval;
end;

// When the strategy changes, a new Ek is generated and it needs to be init'ed
procedure TRetrieval.InitForRetrieval;
begin
  // Precondition : mEk has been init-ed
  MakeSignalField(mEk, mESig);
  mFrogIk.MakeTraceFrom(mEsig);
end;

// When the strategy changes, a new Ek is generated and it needs to be init'ed
procedure TRetrieval.CalcBestTrace(pEBest: TEField);
begin
  // Precondition : mEk has been init-ed
  MakeSignalField(pEBest, mESig);
  mFrogIk.MakeTraceFrom(mEsig);
end;

function TRetrieval.EvalError: double;
var
	MinVal, ax, bx, xx, xmin: double;
  max, maxk: double;
begin

	max := mFrogI.Max;
	maxk := mFrogIk.Max;
	if maxk = 0.0 then
  	raise Exception.Create('Retrieval.EvalError: divergent trace.');

	ax := 0.0;
	xx := max/maxk;
	bx := 3.0*max/maxk;

	MinVal := TNumerics.Brent(ax, xx, bx, GErrorMinFunc, 5.0e-3, xmin);
                                                             //^^^^ this is var

  mEk.RescaleHeight(GetRescaleFactor(xmin));
	EvalError := MinVal;

end;

function TRetrieval.GErrorMinFunc(pX: double): double;
var
	t, N2: integer;
  temp, ans: double;
begin
	N2 := mFrogI.N*mFrogI.N;
  ans := 0.0;

	for t := 0 to N2 - 1 do
    begin
    	temp := mFrogI.Vals^[t] - pX*mFrogIk.Vals^[t];
      ans := ans + temp*temp;
    end;

  GErrorMinFunc := Sqrt(ans)/(mFrogI.N*mFrogI.Max);
end;


destructor TRetrieval.Destroy;
begin
	mEsig.Free;
  mEsig := nil;
  inherited Destroy;
end;

// Singled out for override in XFrogRetrieval
procedure TRetrieval.MakeSignalField(pEk: TEField; pESig: TSignalField);
begin
  mNLOInteraction.MakeSignalField(pEk, pEsig);
end;

function TRetrieval.GetRescaleFactor(pXmin: double): double;
begin
  // Yeah, this should be in the NLO classes, I'm trying to avoid yet another
  // function call. . .anyway, it's easy to move there if necessary.
  if mNLOInteraction.NLOType in SecondOrderNLO then
    GetRescaleFactor := Power(pXmin, 0.25)
  else if mNLOInteraction.NLOType in ThirdOrderNLO then
    GetRescaleFactor := Power(pXmin, (1.0/6.0));
end;

procedure TRetrieval.SetCurrentStrategy(pCurrStrat: TStrategy);
begin
  mCurrentStrategy := pCurrStrat;
end;

// This is here to fix an XFROG bug.
// In FrogAlgo, first it does InitRetrieval, then SynchronizeUnits.
// When the retrievial is init'ed, it makes a signal field.
// In XFrog, you need the reference field to do that.
// But in XFROG, the reference field units aren't fixed up until SynchUnits
procedure TRetrieval.ReInitializeSignalField;
begin
  MakeSignalField(mEk, mESig);
end;

end.
