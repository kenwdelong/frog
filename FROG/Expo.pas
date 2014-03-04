unit Expo;

interface

uses
	FrogObj, Vanilla, Strategy, Func1D, FrogTrace, NLO, SignalField;

type
	TExpoStrategy = class(TVanillaBasedStrategy)
  	private
    	mExpo: double;
  	protected
    	procedure Replace(pEsig: TSignalField; pFrogI: TFrogTrace); override;
  	public
    	procedure Reset; override;
    	procedure GetNewEk(pEk: TEField; pEsig: TSignalField;
      		pFrogI: TFrogTrace; pNLO: TNLOInteraction); override;
      constructor Create(pSwitchAfter: integer; pNewField: double;
      				pSpecialPurpose: integer; pName: string); override;
  end;


implementation

uses
	Math;

constructor TExpoStrategy.Create(pSwitchAfter: integer; pNewField: double;
      				pSpecialPurpose: integer; pName: string);
begin
  inherited Create(pSwitchAfter, pNewField, pSpecialPurpose,pName);
  mExpo := 1.0;
end;

procedure TExpoStrategy.GetNewEk(pEk: TEField; pEsig: TSignalField;
      		pFrogI: TFrogTrace; pNLO: TNLOInteraction);
begin
  Replace(pEsig, pFrogI);
  pNLO.BasicField(pEsig, pEk);
end;

procedure TExpoStrategy.Reset;
begin
  mExpo := 1.0;
end;

procedure TExpoStrategy.Replace(pEsig: TSignalField; pFrogI: TFrogTrace);
var
	tExpo, re, im, magk, newmag: double;
  tau, w, N: integer;
begin
	mExpo := 1.08*mExpo;
  if mExpo > 3.0 then mExpo := 1.0;
  tExpo := mExpo/2.0;

	N := pFrogI.N;

	for tau := 0 to N - 1 do
		for w := 0 to N - 1 do
    begin
			re := pEsig.Re^[tau*N + w];
			im := pEsig.Im^[tau*N + w];
			magk := re*re + im*im;
			if magk = 0.0 then Continue;
			newmag := pFrogI.Vals^[tau*N + w]/magk;

			pEsig.Re^[tau*N + w] := Power(newmag, tExpo)*pEsig.Re^[tau*N + w];
			pEsig.Im^[tau*N + w] := Power(newmag, tExpo)*pEsig.Im^[tau*N + w];
    end;

end;

end.
