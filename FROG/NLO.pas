unit NLO;

interface

uses
	FrogObj, Func1D, FrogTrace, SignalField, Numerics;

type

	TNLOType = (nlPG, nlSHG, nlSD, nlTHG, nlDFG1, nlDFG2);

	TNLOInteraction = class(TFrogObject)
    private
      mMinIsSetup: Boolean;
      mN: integer;
      procedure FillArrays(pN: integer);
  	protected
    	mNLOType: TNLOType;
      mC: PArray;
      mS: PArray;
      procedure SetUpMinArrays(pN: integer);
  	public
    	procedure MakeSignalField(pEk: TEField; pEsig: TSignalField); virtual; abstract;
    	procedure MakeXSignalField(pEk, pERef: TEField; pEsig: TSignalField); virtual; abstract;
      procedure GDerivative(pEk: TEField; pEsig: TSignalField; pFrogI: TFrogTrace; Derivs: PArray); virtual; abstract;
      function Z(pEx: PArray; pEsig: TSignalField): double; virtual; abstract;
      procedure ZDerivative(pEk: TEField; pEsig: TSignalField; Derivs: PArray); virtual; abstract;
      function XFrogZ(pEx: PArray; pERef: TEField; pEsig: TSignalField): double; virtual; abstract;
      procedure XFrogZDerivative(pEk, pERef: TEField; pEsig: TSignalField; Derivs: PArray); virtual; abstract;
      procedure BasicField(pEsig: TSignalField; pEk: TEField); virtual; abstract;
      procedure GateField(pEsig: TSignalField; pEk: TEField); virtual; abstract;
      function CalculateMarginal(pSpec: TSpectrum; pAuto: TAutocorrelation; pSHGSpec: TSpectrum): TMarginal; virtual; abstract;
      function Name: string; virtual; abstract;
      function SpectrumMultiplier: double; virtual; abstract;
      function NLOType: TNLOType; virtual;
      function NeedSpectrum: Boolean; virtual; abstract;
      function NeedAutocorrelation: Boolean; virtual; abstract;
      function NeedSHGSpectrum: Boolean; virtual; abstract;
      function XTestLam(frogLam, refLam: double): double; virtual; abstract;
      function XFrogLam(refLam, testLam: double): double; virtual; abstract;
      constructor Create; virtual;
      destructor Destroy; override;
  end;

	TSecondOrderNLO = set of TNLOType;
	TThirdOrderNLO = set of TNLOType;
  TNLOClass = class of TNLOInteraction;

const
	SecondOrderNLO = [nlSHG];
  ThirdOrderNLO = [nlPG, nlSD, nlTHG];

implementation

constructor TNLOInteraction.Create;
begin
  inherited Create;
  mMinIsSetup := False;
end;

destructor TNLOInteraction.Destroy;
begin
  if mMinIsSetup then
  begin
    FreeMem(mS);
    FreeMem(mC);
  end;
end;

function TNLOInteraction.NLOType: TNLOType;
begin
  NLOType := mNLOType;
end;

procedure TNLOInteraction.SetUpMinArrays(pN: integer);
begin
  if mMinIsSetup then
  begin
    if pN <> mN then
    begin
      FreeMem(mS);
      FreeMem(mC);
      FillArrays(pN);
    end;
  end
  else
    FillArrays(pN);
  mMinIsSetUp := True;
end;
procedure TNLOInteraction.FillArrays(pN: integer);
var
  t, w: integer;
  a: double;
begin
  mN := pN;
  GetMem(mC, pN*pN*SizeOf(double));
  GetMem(mS, pN*pN*SizeOf(double));
  for t := 0 to pN - 1 do
    for w := 0 to pN - 1 do
    begin
      a := 2.0*Pi*(w-pN/2)*(t-pN/2)/pN;
      mC[t*pN + w] := cos(a);
      mS[t*pN + w] := sin(a);
    end;
end;

end.
