unit XStratCals;

interface

uses StratTypes, Classes, Strategy;

type
  TXStrategyCals = class(TStrategyCals)
    private
      procedure CreateDefaultSchedule; override;
      procedure ReadFromFile; override;
      procedure SaveToFile; override;
      function AllowSpecial(pStrat: string): boolean; override;
    public
      constructor Create;
  end;

const
  stXFrogProj = 'XFROG Projections';


implementation

uses SysUtils, BasicFrog, XProj, Expo;

// I had to butcher the parent class to make this work.  It's not elegant, but
// I couldn't figure out how to extend the enumeration type (TStrategyTypes).

constructor TXStrategyCals.Create;
begin
  inherited Create;

  // Fill out the name list
  mAvailableStrategies.Clear;
  mAvailableStrategies.AddObject(stBasic, TStratClassHolder.Create(TBasicStrategy));
  mAvailableStrategies.AddObject(stXFrogProj, TStratClassHolder.Create(TXFrogProjection));
  mAvailableStrategies.AddObject(stExpo, TStratClassHolder.Create(TExpoStrategy));

  CreateDefaultSchedule;
end;

procedure TXStrategyCals.CreateDefaultSchedule;
begin
  mNames[1] := stBasic;
  mSwitchAfter[1] := DEFAULT_SWITCH_AFTER;
  mNewField[1] := 0;
  mSpecial[1] := 0;

  mNames[2] := stXFrogProj;
  mSwitchAfter[2] := DEFAULT_SWITCH_AFTER;
  mNewField[2] := 0;
  mSpecial[2] := 5;

  mNames[3] := stExpo;
  mSwitchAfter[3] := DEFAULT_SWITCH_AFTER;
  mNewField[3] := 0;
  mSpecial[3] := 0;

  mNames[4] := stXFrogProj;
  mSwitchAfter[4] := DEFAULT_SWITCH_AFTER;
  mNewField[4] := 0;
  mSpecial[4] := 0;

  mNumStrats := 4;
end;

procedure TXStrategyCals.ReadFromFile;
begin
  // We always want it to use the default schedule
  raise Exception.Create('No file for XFrog');
end;

procedure TXStrategyCals.SaveToFile;
begin

end;

function TXStrategyCals.AllowSpecial(pStrat: string): boolean;
begin
  if pStrat = stXFrogProj then
    AllowSpecial := True
  else
    AllowSpecial := inherited AllowSpecial(pStrat);
end;

end.
