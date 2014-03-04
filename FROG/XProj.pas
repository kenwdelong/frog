unit XProj;

interface

uses Projection, Func1D, SignalField, Numerics;

type
  TXFrogProjection = class(TProjection)
    protected
      function ProjectionError(pEx: PArray): double; override;
      procedure CalcDerivatives(pEk: TEField; pEsig: TSignalField; Derivs: PArray); override;
  end;

implementation

function TXFrogProjection.ProjectionError(pEx: PArray): double;
begin
  ProjectionError := mNLO.XFrogZ(pEx, mERef, mEsig)
end;

procedure TXFrogProjection.CalcDerivatives(pEk: TEField; pEsig: TSignalField; Derivs: PArray);
begin
  mNLO.XFrogZDerivative(pEk, mERef, pESig, Derivs); 
end;

end.
