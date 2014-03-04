unit PRMin;

interface

uses Min;

type
  TPRMin = class(TMinimization)
    protected
      function GetDGG(pN: integer): double; override;
  end;

implementation

function TPRMin.GetDGG(pN: integer): double;
var
  t: integer;
  dgg: double;
begin
  dgg := 0.0;
  for t := 0 to pN - 1 do
    dgg := dgg + (mXi^[t] + mG^[t])*mXi^[t];
  GetDGG := dgg;
end;

end.
