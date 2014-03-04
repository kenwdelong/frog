unit BasicFrog;

interface

uses
	FrogObj, Vanilla, Strategy, Func1D, FrogTrace, NLO, SignalField;

type
	TBasicStrategy = class(TVanillaBasedStrategy)
  	public
    	procedure GetNewEk(pEk: TEField; pEsig: TSignalField;
      		pFrogI: TFrogTrace; pNLO: TNLOInteraction); override;
  end;

implementation

procedure TBasicStrategy.GetNewEk(pEk: TEField; pEsig: TSignalField;
      		pFrogI: TFrogTrace; pNLO: TNLOInteraction);
begin
  Replace(pEsig, pFrogI);
  pNLO.BasicField(pEsig, pEk);
end;

end.
