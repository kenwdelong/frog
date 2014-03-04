unit Projection;

interface

uses Func1D, Numerics, GenProj;

type
  TProjection = class(TGeneralizedProjectionBasedStrategy)
    protected
      procedure CreateNewEk(pEk: TEField; pEx: PArray); override;
  end;

implementation

procedure TProjection.CreateNewEk(pEk: TEField; pEx: PArray);
var
  t, N: integer;
begin
  N := pEk.N;
  for t := 0 to N - 1 do
  begin
    pEk.Re^[t] := pEx^[2*t];
    pEk.Im^[t] := pEx^[2*t + 1];
  end;
end;

end.
