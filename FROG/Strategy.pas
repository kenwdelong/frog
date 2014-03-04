unit Strategy;

interface

uses
	FrogObj, Func1D, NLO, FrogTrace, SignalField;

type

  TNewFieldTypes = (nfUseOld, nfUseBest, nfPerturbBest);

	TStrategy = class(TFrogObject)
  	private
      mNewField: double;
      mSpecialPurpose: integer;
      mName: string;
    protected
      mERef: TEField;
    	mSwitchAfter: integer;    // Inten gate needs this one
    	procedure Replace(pEsig: TSignalField; pFrogI: TFrogTrace); virtual;
    public
    	procedure GetNewEk(pEk: TEField; pEsig: TSignalField;
      		pFrogI: TFrogTrace; pNLO: TNLOInteraction); virtual; abstract;
      procedure Reset; virtual; abstract;
      procedure ErrorIs(pError: double); virtual;
      property Name: string read mName;
      property SwitchAfter: integer read mSwitchAfter;
      property NewField: double read mNewField;
      property SpecialPurpose: integer read mSpecialPurpose;
      class function NewFieldTypeFromInt(pInt: integer): TNewFieldTypes;
      property ReferenceField: TEField write mERef;
      constructor Create(pSwitchAfter: integer; pNewField: double;
      				pSpecialPurpose: integer; pName: string); virtual;
  end;

  TStrategyArray = array[1..100] of TStrategy;
  PStrategyArray = ^TStrategyArray;
  TStrategyClass = class of TStrategy;

implementation

uses
	SysUtils, Numerics, Math;

constructor TStrategy.Create(pSwitchAfter: integer; pNewField: double;
      				pSpecialPurpose: integer; pName: string);
begin
	inherited Create;
	mSwitchAfter := pSwitchAfter;
  mNewField := pNewField;
  mSpecialPurpose := pSpecialPurpose;
  mName := pName;

  if mSwitchAfter = 0 then mSwitchAfter := 3;
end;

// Default procedure for replacing the magnitude.  Overridden by Expo types.
procedure TStrategy.Replace(pEsig: TSignalField; pFrogI: TFrogTrace);
var
	N, tau, w: integer;
  magk, newmag, re, im: double;
begin
	N := pFrogI.N;

	for tau := 0 to N - 1 do
		for w := 0 to N - 1 do
    begin
			re := pEsig.Re^[tau*N + w];
			im := pEsig.Im^[tau*N + w];
			magk := re*re + im*im;
			if magk = 0.0 then Continue;
			newmag := pFrogI.Vals^[tau*N + w]/magk;
			newmag := sqrt(newmag);

			pEsig.Re^[tau*N + w] := newmag*pEsig.Re^[tau*N + w];
			pEsig.Im^[tau*N + w] := newmag*pEsig.Im^[tau*N + w];
    end;

end;


class function TStrategy.NewFieldTypeFromInt(pInt: integer): TNewFieldTypes;
begin
  case pInt of
  	Ord(nfUseOld): NewFieldTypeFromInt := nfUseOld;
    Ord(nfUseBest): NewFieldTypeFromInt := nfUseBest;
    Ord(nfPerturbBest): NewFieldTypeFromInt := nfPerturbBest;
  end;
end;

procedure TStrategy.ErrorIs(pError: double);
begin
  // No implementation.  This is a necessary evil for the shortcut method
  // in projections to work.
end;

end.
