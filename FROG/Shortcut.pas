unit Shortcut;

interface

uses FrogObj, SignalField;

type TShortcut = class(TFrogObject)
  private
    mESig1: TSignalField;
    mESig2: TSignalField;
    mError1: double;
    mError2: double;
    mStep:integer;
    mCounter: integer;
    function FirstSigField: boolean;
    function SecondSigField: boolean;
  public
    procedure SetFirstSignalField(pESig: TSignalField);
    procedure SetSecondSignalField(pESig: TSignalField);
    procedure ErrorIs(pError: double);
    function ShortcutReady: boolean;
    procedure CalculateShortcutField;
    property SignalField: TSignalField read mESig1;
    property StartField: TSignalField read mESig2;
    procedure Reset;
    constructor Create(pStep:integer);
    destructor Destroy; override;
end;

implementation

constructor TShortcut.Create(pStep: integer);
begin
  inherited Create;
  mStep := pStep;
  mESig1 := nil;
  mEsig2 := nil;
  mCounter := 1;
end;

destructor TShortcut.Destroy;
begin
  mESig1.Free;
  mESig2.Free;
  inherited;
end;

procedure TShortcut.Reset;
begin
  mCounter := 1;
end;

procedure TShortcut.SetFirstSignalField(pESig: TSignalField);
begin
  if mStep = 0 then Exit;
  if mESig1 = nil then
    mEsig1 := TSignalField.Create(pESig.N);
  if mESig2 = nil then
    mEsig2 := TSignalField.Create(pESig.N);

  Inc(mCounter);
  if FirstSigField then mESig1.CopyFrom(pESig);
  if SecondSigField then mESig2.CopyFrom(pESig);
end;

procedure TShortcut.SetSecondSignalField(pESig: TSignalField);
begin
  if mStep = 0 then Exit;
  if FirstSigField then mESig1.AverageWith(pESig);
  if SecondSigField then mESig2.AverageWith(pESig);
end;

// This doesn't care if you have properly set the signal fields, it just
// is checking the iteration number.
function TShortcut.ShortcutReady: boolean;
begin
  if SecondSigField and (mStep <> 0) then
    ShortcutReady := True
  else
    ShortcutReady := False;
end;

procedure TShortcut.ErrorIs(pError: double);
begin
  if FirstSigField then mError1 := pError;
  if SecondSigField then mError2 := pError;
end;

function TShortcut.FirstSigField: boolean;
begin
  if mCounter = (mStep - 1) then
    FirstSigField := True
  else
    FirstSigField := False;
end;

function TShortcut.SecondSigField: boolean;
begin
  if mCounter = mStep then
    SecondSigField := True
  else
    SecondSigField := False;
end;


// This leaves the gradient direction in Esig1, and the field to start from
// in Esig2
procedure TShortcut.CalculateShortcutField;
var
  i, n2: integer;
  delError: double;
begin
  n2 := mEsig1.N*mESig2.N;
  delError := mError1 - mError2;

  // do a linear interpolation to zero error.
  {for i := 0 to n2 - 1 do
  begin
    mEsig1.Re^[i] := (mError1*mESig2.Re^[i] - mError2*mESig1.Re^[i])/delError;
    mEsig1.Im^[i] := (mError1*mESig2.Im^[i] - mError2*mESig1.Im^[i])/delError;
  end;}

  // calculate the gradient - direction to minimize in
  // Since pNLO.BasicField is linear, we can do the subtraction here
  for i := 0 to n2 - 1 do
  begin
    mEsig1.Re^[i] := mESig2.Re^[i] - mESig1.Re^[i];
    mEsig1.Im^[i] := mESig2.Im^[i] - mESig1.Im^[i];
  end;

  mCounter := 1;
end;

end.
