unit StratTypes;

interface

uses
	FrogObj, Strategy, Classes;

const
  MAX_STRATS = 20;
  DEFAULT_SWITCH_AFTER = 9;
  MAX_ITERS = 20;

type
//  TStrategyType = (stBasic, stExpo, stIntenGate, stProjections, stOverstep,
  //									stFRMin, stPRMin, stXFrogProj, stModErrProj);

	TStrategyCals = class(TFrogObject)
  	private
      mAppPath: string;
      function GetName(i: integer): string;
      procedure SetName(i: integer; pName: string);
      function GetSwitch(i: integer): integer;
      procedure SetSwitch(i, pSwitch: integer);
      function GetNewField(i: integer): double;
      procedure SetNewField(i: integer; pNewField: double);
      function GetSpecial(i: integer): integer;
      procedure SetSpecial(i: integer; pSpecial: integer);
      procedure CheckIntenIters(pI: integer);
      function StratFileName: string;
    protected
    	mAvailableStrategies: TStringList;
      mNames: array[1..MAX_STRATS] of string;
      mSwitchAfter: array[1..MAX_STRATS] of integer;
      mNewField: array[1..MAX_STRATS] of double;
      mSpecial: array[1..MAX_STRATS] of integer;
      mNumStrats: integer;
      procedure CreateDefaultSchedule; virtual;
      procedure ReadFromFile; virtual;
      procedure SaveToFile; virtual;
      function AllowSpecial(pStrat: string): boolean; virtual;
    public
      function NameOfStrategy(i: integer): string;
      function TotalNumStrategies: integer;
      function ClassRef(pStratType: string): TStrategyClass;
      procedure InsertStrategy(pI: integer);
      procedure DeleteStrategy(pI: integer);
    	property NumStrategies: integer read mNumStrats;
      property Name[i: integer]: string read GetName write SetName;
      property SwitchAfter[i: integer]: integer read GetSwitch write SetSwitch;
      property Special[i: integer]: integer read GetSpecial write SetSpecial;
      property NewField[i: integer]: double read GetNewField write SetNewField;
    	constructor Create;
      destructor Destroy; override;
  end;

  TStratClassHolder = class
    private
      mClassRef: TStrategyClass;
    public
      property MyClassRef: TStrategyClass read mClassRef write mClassRef;
      constructor Create(pClassRef: TStrategyClass);
  end;

const
  FILE_NAME = 'strat.txt';

  stBasic = 'Basic';
  stExpo = 'Expo';
  stIntenGate = 'IntenGate';
  stProjections = 'Projections';
  stOverstep = 'Proj. - Overstep';
  stFRMin = 'Minimization - FR';
  stPRMin = 'Minimization - PR';


implementation

// In order to add a new Strategy Class to this class, do
//  1. Add the unit to the implementation uses clause
//  2. Add its name to the string and type lists in the constructor
//  3. Add its Class Type to the ClassRef function.

uses
	SysUtils, BasicFrog, Expo, IntenGate, Projection, Overstep, FRMin, PRMin;

constructor TStrategyCals.Create;
begin
	inherited Create;

  // This object gets instantiated at program startup, so it should have the
  //   app path as the starting path
  mAppPath := GetCurrentDir;

  mAvailableStrategies := TStringList.Create;

  mAvailableStrategies.AddObject(stBasic, TStratClassHolder.Create(TBasicStrategy));
  mAvailableStrategies.AddObject(stExpo, TStratClassHolder.Create(TExpoStrategy));
  mAvailableStrategies.AddObject(stIntenGate, TStratClassHolder.Create(TIntenGateStrategy));
  mAvailableStrategies.AddObject(stProjections, TStratClassHolder.Create(TProjection));
  mAvailableStrategies.AddObject(stOverstep, TStratClassHolder.Create(TOverstep));
  mAvailableStrategies.AddObject(stFRMin, TStratClassHolder.Create(TFRMin));
  mAvailableStrategies.AddObject(stPRMin, TStratClassHolder.Create(TPRMin));

  // Qualify the file name
  try
    ReadFromFile;
  except
    DeleteFile(StratFileName);
    CreateDefaultSchedule;
  end;
end;

function TStrategyCals.ClassRef(pStratType: string): TStrategyClass;
var
  i: integer;
begin
  i := mAvailableStrategies.IndexOf(pStratType);
  if i = (-1) then
  begin
    ClassRef := nil;
    raise Exception.Create('Strategy name not in list: ' + pStratType);
  end
  else
    ClassRef := TStratClassHolder(mAvailableStrategies.Objects[i]).MyClassRef;
end;

procedure TStrategyCals.CreateDefaultSchedule;
begin
  mNames[1] := stBasic;
  mSwitchAfter[1] := DEFAULT_SWITCH_AFTER;
  mNewField[1] := 0;
  mSpecial[1] := 0;

  mNames[2] := stProjections;
  mSwitchAfter[2] := DEFAULT_SWITCH_AFTER;
  mNewField[2] := 0;
  mSpecial[2] := 5;

  mNames[3] := stOverstep;
  mSwitchAfter[3] := DEFAULT_SWITCH_AFTER;
  mNewField[3] := 0;
  mSpecial[3] := 0;

  mNames[4] := stIntenGate;
  mSwitchAfter[4] := DEFAULT_SWITCH_AFTER;
  mNewField[4] := 0;
  mSpecial[4] := 0;

  mNames[5] := stExpo;
  mSwitchAfter[5] := DEFAULT_SWITCH_AFTER;
  mNewField[5] := 0;
  mSpecial[5] := 0;

  mNames[6] := stProjections;
  mSwitchAfter[6] := DEFAULT_SWITCH_AFTER;
  mNewField[6] := -1;
  mSpecial[6] := 0;

  mNames[7] := stProjections;
  mSwitchAfter[7] := DEFAULT_SWITCH_AFTER;
  mNewField[7] := 0.05;
  mSpecial[7] := 0;

  mNumStrats := 7;
end;

function TStrategyCals.TotalNumStrategies: integer;
begin
  TotalNumStrategies := mAvailableStrategies.Count;
end;

function TStrategyCals.NameOfStrategy(i: integer): string;
begin
  NameOfStrategy := mAvailableStrategies[i];
end;

// List manipulation
function TStrategyCals.GetName(i: integer): string;
begin
  GetName := mNames[i];
end;
procedure TStrategyCals.SetName(i: integer; pName: string);
begin
  mNames[i] := pName;
  CheckIntenIters(i);
end;
function TStrategyCals.GetSwitch(i: integer): integer;
begin
  GetSwitch := mSwitchAfter[i];
end;
procedure TStrategyCals.SetSwitch(i, pSwitch: integer);
begin
  mSwitchAfter[i] := pSwitch;
  if mSwitchAfter[i] > MAX_ITERS then
    mSwitchAfter[i] := MAX_ITERS;
  CheckIntenIters(i);
end;
function TStrategyCals.GetNewField(i: integer): double;
begin
  GetNewField := mNewField[i];
end;
procedure TStrategyCals.SetNewField(i: integer; pNewField: double);
begin
  mNewField[i] := pNewField;
end;
function TStrategyCals.GetSpecial(i: integer): integer;
begin
  GetSpecial := mSpecial[i];
end;
procedure TStrategyCals.SetSpecial(i: integer; pSpecial: integer);
begin
  // This is now used for shortcuts
  if (pSpecial < 0) then pSpecial := 0;
  if (pSpecial = 1) or (pSpecial = 2) then pSpecial := 3;
  if AllowSpecial(mNames[i]) then
    mSpecial[i] := pSpecial
  else
    mSpecial[i] := 0;
end;

function TStrategyCals.AllowSpecial(pStrat: string): boolean;
begin
  if (pStrat = stProjections) or (pStrat = stOverstep) then
    AllowSpecial := True
  else
    AllowSpecial := False;
end;

procedure TStrategyCals.CheckIntenIters(pI: integer);
begin
  if (mNames[pI] = stIntenGate) and (mSwitchAfter[pI] < 3) then
    mSwitchAfter[pI] := 3;
end;

procedure TStrategyCals.InsertStrategy(pI: integer);
var
  j: integer;
begin
  Inc(mNumStrats);
  for j := mNumStrats downto pI + 2 do
  begin
    mNames[j] := mNames[j - 1];
    mSwitchAfter[j] := mSwitchAfter[j - 1];
    mNewField[j] := mNewField[j - 1];
    mSpecial[j] := mSpecial[j - 1];
  end;
end;

procedure TStrategyCals.DeleteStrategy(pI: integer);
var
  j: integer;
begin
  // These arrays are 1-based.  But when you delete the first row on the form,
  // it comes in as zero based.  So without this next line you are overwriting
  // memory somewhere (I thought Object Pascal didn't let you do that?  Are we
  // writing C again?)
  pI := pI + 1;
  for j := pI to mNumStrats - 1 do
  begin
    mNames[j] := mNames[j + 1];
    mSwitchAfter[j] := mSwitchAfter[j + 1];
    mNewField[j] := mNewField[j + 1];
    mSpecial[j] := mSpecial[j + 1];
  end;
  Dec(mNumStrats);
end;

destructor TStrategyCals.Destroy;
var
  i, count: integer;
begin
  SaveToFile;

  count := mAvailableStrategies.Count;
  for i := 0 to count - 1 do
    mAvailableStrategies.Objects[i].Free;

  mAvailableStrategies.Free;
  mAvailableStrategies := nil;
  inherited Destroy;
end;

procedure TStrategyCals.SaveToFile;
var
  sList: TStringList;
  i: integer;
begin
  sList := TStringList.Create;
  try
    sList.Add(IntToStr(mNumStrats));
    for i := 1 to mNumStrats do
    begin
      sList.Add(mNames[i]);
      sList.Add(IntToStr(mSwitchAfter[i]));
      sList.Add(FloatToStr(mNewField[i]));
      sList.Add(FloatToStr(mSpecial[i]));
    end;
    sList.SaveToFile(StratFileName);
  finally
    sList.Free;
  end;
end;

procedure TStrategyCals.ReadFromFile;
var
  sList: TStringList;
  i, cursor: integer;
begin
  sList := TStringList.Create;
  cursor := 0;
  try
    sList.LoadFromFile(StratFileName);
    mNumStrats := StrToInt(sList[cursor]);
    if (mNumStrats < 1) or (mNumStrats > MAX_STRATS) then raise Exception.Create('Error');
    Inc(cursor);
    for i := 1 to mNumStrats do
    begin
      mNames[i] := sList[cursor];
      if mAvailableStrategies.IndexOf(mNames[i]) = -1 then raise Exception.Create('Error');
      Inc(cursor);
      mSwitchAfter[i] := StrToInt(sList[cursor]);
      if (mSwitchAfter[i] < 0) or (mSwitchAfter[i] > MAX_ITERS) then raise Exception.Create('Error');
      if (mSwitchAfter[i] = 0) then mSwitchAfter[i] := DEFAULT_SWITCH_AFTER;
      Inc(cursor);
      mNewField[i] := StrToFloat(sList[cursor]);
      Inc(cursor);
      mSpecial[i] := StrToInt(sList[cursor]);
      Inc(cursor);
    end;
  finally
    sList.Free;
  end;
end;

function TStrategyCals.StratFileName: string;
begin
  Result := mAppPath + '\' + FILE_NAME;
end;



constructor TStratClassHolder.Create(pClassRef: TStrategyClass);
begin
  mClassRef := pClassRef;
end;

end.
