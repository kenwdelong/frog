unit IntenGate;

interface

uses
	FrogObj, Vanilla, Strategy, Func1D, FrogTrace, NLO, SignalField;

type
	TIntenGateStrategy = class(TVanillaBasedStrategy)
  	private
    	mCount: integer;
  	public
    	procedure Reset; override;
    	procedure GetNewEk(pEk: TEField; pEsig: TSignalField;
      		pFrogI: TFrogTrace; pNLO: TNLOInteraction); override;
      constructor Create(pSwitchAfter: integer; pNewField: double;
      				pSpecialPurpose: integer; pName: string); override;
  end;

implementation

constructor TIntenGateStrategy.Create(pSwitchAfter: integer; pNewField: double;
      				pSpecialPurpose: integer; pName: string);
begin
  inherited;
  if mSwitchAfter < 3 then mSwitchAfter := 3;
end;

procedure TIntenGateStrategy.GetNewEk(pEk: TEField; pEsig: TSignalField;
      		pFrogI: TFrogTrace; pNLO: TNLOInteraction);
begin
  Replace(pEsig, pFrogI);
  if mCount mod 3 = 0 then
  	pNLO.BasicField(pEsig, pEk)
  else
  	pNLO.GateField(pEsig, pEk);

  Inc(mCount);
end;

procedure TIntenGateStrategy.Reset;
begin
	// This is acutally unimportant, it's here for completeness. :-)
  mCount := 0;
end;

end.
