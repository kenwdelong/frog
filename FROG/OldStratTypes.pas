unit StratTypes;

interface

uses
	FrogObj, Strategy, Classes;

const
  MAX_STRATS = 20;
  DEFAULT_SWITCH_AFTER = 9;
  MAX_ITERS = 20;

type
  TStrategyType = (stBasic, stExpo, stIntenGate, stProjections, stOverstep,
  									stFRMin, stPRMin, stXFrogProj, stModErrProj);

	TStrategyCals = class(TFrogObject)
  	private
      mAppPath: string;
    	mStrategyNames: TStringList;
      mStrategyTypes: array[Ord(Low(TStrategyType)).. Ord(High(TStrategyType))] of TStrategyType;
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
      mNames: array[1..MAX_STRATS] of string;
      mSwitchAfter: array[1..MAX_STRATS] of integer;
      mNewField: array[1..MAX_STRATS] of double;
      mSpecial: array[1..MAX_STRATS] of integer;
      mNumStrats: integer;
      procedure CreateDefaultSchedule; virtual;
      procedure ReadFromFile; virtual;
      procedure SaveToFile; virtual;
    public
      function NameOfStrategy(i: integer): string; virtual;
      function TotalNumStrategies: integer; virtual;
      function ClassRef(pStratType: TStrategyType): TStrategyClass;
      function NameOfType(pType: TStrategyType): string;
      function TypeOfName(pName: string): TStrategyType;
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

const
  FILE_NAME = 'strat.txt';


implementation

// In order to add a new Strategy Class to this class, do
//  1. Add the unit to the implementation uses clause
//  2. Add its name to the string and type lists in the constructor
//  3. Add its Class Type to the ClassRef function.

uses
	SysUtils, BasicFrog, Expo, IntenGate, Projection, Overstep, FRMin, PRMin, XProj;

constructor TStrategyCals.Create;
begin
	inherited Create;

  // This object gets instantiated at program startup, so it should have the
  //   app path as the starting path
  mAppPath := GetCurrentDir;

  mStrategyNames := TStringList.Create;

  // Fill out the name list
  mStrategyNames.Insert(Ord(stBasic), 'Basic');
  mStrategyNames.Insert(Ord(stExpo), 'Expo');
  mStrategyNames.Insert(Ord(stIntenGate), 'IntenGate');
  mStrategyNames.Insert(Ord(stProjections), 'Projections');
  mStrategyNames.Insert(Ord(stOverstep), 'Proj. - Overstep');
  mStrategyNames.Insert(Ord(stFRMin), 'Minimization - FR');
  mStrategyNames.Insert(Ord(stPRMin), 'Minimization - PR');
  mStrategyNames.Insert(Ord(stXFrogProj), 'XFrog Projections');

  // Fill out Type list
  mStrategyTypes[Ord(stBasic)] := stBasic;
  mStrategyTypes[Ord(stExpo)] := stExpo;
  mStrategyTypes[Ord(stIntenGate)] := stIntenGate;
  mStrategyTypes[Ord(stProjections)] := stProjections;
  mStrategyTypes[Ord(stOverstep)] := stOverstep;
  mStrategyTypes[Ord(stFRMin)] := stFRMin;
  mStrategyTypes[Ord(stPRMin)] := stPRMin;
  mStrategyTypes[Ord(stXFrogProj)] := stXFrogProj;

  // Qualify the file name

  try
    ReadFromFile;
  except
    DeleteFile(StratFileName);
    CreateDefaultSchedule;
  end;
end;

function TStrategyCals.ClassRef(pStratType: TStrategyType): TStrategyClass;
begin
  case pStratType of
  	stBasic: ClassRef := TBasicStrategy;
    stExpo: ClassRef := TExpoStrategy;
    stIntenGate: ClassRef := TIntenGateStrategy;
    stProjections: ClassRef := TProjection;
    stOverstep: ClassRef := TOverstep;
    stFRMin: ClassRef := TFRMin;
    stPRMin: ClassRef := TPRMin;
    stXFrogProj: ClassRef := TXFrogProjection;
  end;
end;

function TStrategyCals.TypeOfName(pName: string): TStrategyType;
var
	i: integer;
begin
  i := mStrategyNames.IndexOf(pName);
  if i = -1 then
  	raise Exception.Create('Strategy name not in list: ' + pName);

  TypeOfName := mStrategyTypes[i];
end;

function TStrategyCals.NameOfType(pType: TStrategyType): string;
begin
  NameOfType := mStrategyNames.Strings[Ord(pType)];
end;

procedure TStrategyCals.CreateDefaultSchedule;
begin
  mNames[1] := NameOfType(stBasic);
  mSwitchAfter[1] := DEFAULT_SWITCH_AFTER;
  mNewField[1] := 0;
  mSpecial[1] := 0;

  mNames[2] := NameOfType(stProjections);
  mSwitchAfter[2] := DEFAULT_SWITCH_AFTER;
  mNewField[2] := 0;
  mSpecial[2] := 5;

  mNames[3] := NameOfType(stOverstep);
  mSwitchAfter[3] := DEFAULT_SWITCH_AFTER;
  mNewField[3] := 0;
  mSpecial[3] := 0;

  mNames[4] := NameOfType(stIntenGate);
  mSwitchAfter[4] := DEFAULT_SWITCH_AFTER;
  mNewField[4] := 0;
  mSpecial[4] := 0;

  mNames[5] := NameOfType(stExpo);
  mSwitchAfter[5] := DEFAULT_SWITCH_AFTER;
  mNewField[5] := 0;
  mSpecial[5] := 0;

  mNames[6] := NameOfType(stProjections);
  mSwitchAfter[6] := DEFAULT_SWITCH_AFTER;
  mNewField[6] := -1;
  mSpecial[6] := 0;

  mNames[7] := NameOfType(stProjections);
  mSwitchAfter[7] := DEFAULT_SWITCH_AFTER;
  mNewField[7] := 0.05;
  mSpecial[7] := 0;

  mNumStrats := 7;
end;

function TStrategyCals.TotalNumStrategies: integer;
begin
  TotalNumStrategies := mStrategyNames.Count;
end;

function TStrategyCals.NameOfStrategy(i: integer): string;
begin
  NameOfStrategy := mStrategyNames[i];
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
  if (mNames[i] = NameOfType(stProjections)) or (mNames[i] = NameOfType(stOverstep)) then
    mSpecial[i] := pSpecial
  else
    mSpecial[i] := 0;
end;

procedure TStrategyCals.CheckIntenIters(pI: integer);
begin
  if (mNames[pI] = NameOfType(stIntenGate)) and (mSwitchAfter[pI] < 3) then
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
begin
  SaveToFile;
  mStrategyNames.Free;
  mStrategyNames := nil;
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
      if mStrategyNames.IndexOf(mNames[i]) = -1 then raise Exception.Create('Error');
      Inc(cursor);
      mSwitchAfter[i] := StrToInt(sList[cursor]);
      if (mSwitchAfter[i] < 0) or (mSwitchAfter[i] > MAX_ITERS) then raise Exception.Create('Error');
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

end.
