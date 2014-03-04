unit Overstep;

interface

uses Func1D, Numerics, GenProj;

type
  TOverstep = class(TGeneralizedProjectionBasedStrategy)
    protected
      procedure CreateNewEk(pEk: TEField; pEx: PArray); override;
  end;

implementation

procedure TOverstep.CreateNewEk(pEk: TEField; pEx: PArray);
var
  t, N: integer;
begin
  N := pEk.N;
  for t := 0 to N - 1 do
  begin
    pEk.Re^[t] := 2*pEx^[2*t] - pEk.Re^[t];
    pEk.Im^[t] := 2*pEx^[2*t + 1] - pEk.Im^[t];
    //pEk.Re^[t] := pEk.Re^[t] + beta*(pEx^[2*t] - pEk.Re^[t]);
    //pEk.Im^[t] := pEk.Im^[t] + beta*(pEx^[2*t + 1] - pEk.Im^[t]);
  end;
end;

end.
