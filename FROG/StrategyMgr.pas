unit StrategyMgr;

interface

uses
	FrogObj, Strategy, Func1D, Grids, StratTypes;

type
	TStrategyManager = class(TFrogObject)
  	private
    	mStrategies: PStrategyArray;
      mNumStrategies: integer;
      mCurrentStrategyIndex: integer;
      mStrategyChanged: Boolean;
      mErrors: array[1..MAX_ITERS] of double;
      mCurrentBestError: double;   // Current refers to the current strategy
      mCurrentIterations: integer;
      function GetCurrentStrategy: TStrategy;
      procedure CreateStrategies(pStratCals: TStrategyCals);
    public
      procedure ChangeStrategy;
    	procedure UpdateStrategy(pErr, pGlobalBestError: double);
      function InitialStrategy: TStrategy;
      procedure NewStartField(pEk, pEBest: TEField);
    	property CurrentStrategy: TStrategy read GetCurrentStrategy;
      property StrategyChanged: Boolean read mStrategyChanged;
      constructor Create(pStratCals: TStrategyCals);
      destructor Destroy; override;
  end;

const
  A_BIG_ERROR: double = 1e10;

implementation

uses
	SysUtils;

constructor TStrategyManager.Create(pStratCals: TStrategyCals);
begin
	inherited Create;

	// See how many strategies are indicated on the list
  mNumStrategies := pStratCals.NumStrategies;
  GetMem(mStrategies, mNumStrategies*SizeOf(TStrategy));

  CreateStrategies(pStratCals);
  mCurrentStrategyIndex := 1;

  mCurrentIterations := 0;
  mCurrentBestError := A_BIG_ERROR;
end;

procedure TStrategyManager.ChangeStrategy;
begin
  mStrategyChanged := True;
  Inc(mCurrentStrategyIndex);
  mCurrentBestError := A_BIG_ERROR;
  mCurrentIterations := 0;

  // Check to see if we are recycling or not, if so set index := 1
  if mCurrentStrategyIndex > mNumStrategies then
  	mCurrentStrategyIndex := 1;

  CurrentStrategy.Reset;
end;

function TStrategyManager.GetCurrentStrategy: TStrategy;
begin
  GetCurrentStrategy := mStrategies^[mCurrentStrategyIndex];
end;

procedure TStrategyManager.UpDateStrategy(pErr, pGlobalBestError: double);
var
  i, numErrs: integer;
  prev, curr: double;
begin
  mStrategyChanged := False;

  // Check the value of the error
  if pErr < mCurrentBestError then mCurrentBestError := pErr;
  numErrs := GetCurrentStrategy.SwitchAfter;

  // The Error array
  Inc(mCurrentIterations);
  if mCurrentIterations <= numErrs then
  begin
    mErrors[mCurrentIterations] := pErr;
    Exit;
  end
  else
  begin
    mCurrentBestError := A_BIG_ERROR;
    for i := 1 to numErrs - 1 do
    begin
      mErrors[i] := mErrors[i + 1];
      if mErrors[i] < mCurrentBestError then mCurrentBestError := mErrors[i];
    end;
    mErrors[numErrs] := pErr;
    if mErrors[numErrs] < mCurrentBestError then mCurrentBestError := pErr;
  end;

  // Get the equivalent first and last errors for inten gate
  if GetCurrentStrategy.Name = 'IntenGate' then
  begin
    // Special permission for inten gate
    if pGlobalBestError = mCurrentBestError then Exit;
		prev := 0;
		for i := 1 to 3 do
      prev := prev + mErrors[i];
		prev := prev/3.0;
		curr := 0;
		for i := numErrs - 2 to numErrs do
      curr := curr + mErrors[i];
		curr := curr/3.0;
  end
	else
  begin
		prev := mErrors[1];
		curr := pErr;
  end;

  // If the best error occurred in this strategy, but not on the last iteration,
  //   then keep going.
  if (mCurrentBestError = pGlobalBestError) and (pErr <> pGlobalBestError) then
    Exit;

  // Otherwise, apply standard convergence criterion
  if (prev - curr)/prev > 0.005 then
    Exit;

  // If we made it here then we need to change!!
  ChangeStrategy;

end;

procedure TStrategyManager.NewStartField(pEk, pEBest: TEField);
begin
	if CurrentStrategy.NewField < -1 then
  	// Set phase flat
    pEk.FlattenPhase
  else if CurrentStrategy.NewField < 0 then
  	// do nothing, use previous Ek as new start field
  else if CurrentStrategy.NewField = 0 then
		// Use Ebest
  	pEk.CopyFrom(pEBest)
  else
  begin
  	// Perturb from EBest
  	pEk.CopyFrom(pEBest);
    pEk.Perturb(CurrentStrategy.NewField);
  end;
end;

procedure TStrategyManager.CreateStrategies(pStratCals: TStrategyCals);
var
	i, SwitchAfter: integer;
  NewField: double;
  Special: integer;
  Name: string;
begin
  	for i := 1 to mNumStrategies do
  	begin
      Name :=  pStratCals.Name[i];
      SwitchAfter := pStratCals.SwitchAfter[i];
      NewField := pStratCals.NewField[i];
      Special := pStratCals.Special[i];
      // We should check here the size of mNumStrategies :
      // SwitchAfter: less than 20 (error array size)

      mStrategies^[i] := pStratCals.ClassRef(Name).Create(SwitchAfter, NewField, Special, Name);
    end;
end;

function TStrategyManager.InitialStrategy: TStrategy;
begin
  InitialStrategy := mStrategies^[1];
end;

destructor TStrategyManager.Destroy;
var
	 i: integer;
begin
	for i := 1 to mNumStrategies do
  begin
    mStrategies^[i].Free;
    mStrategies^[i] := nil;
  end;

  FreeMem(mStrategies);

  inherited Destroy;
end;

end.
