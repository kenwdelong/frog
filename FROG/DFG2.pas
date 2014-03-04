unit DFG2;

interface

uses
	NLO, SignalField, Func1D, Numerics, FrogTrace;

type
	TDFG2 = class(TNLOInteraction)
  	public
    	procedure MakeSignalField(pEk: TEField; pEsig: TSignalField); override;
    	procedure MakeXSignalField(pEk, pERef: TEField; pEsig: TSignalField); override;
      function Z(pEx: PArray; pEsig: TSignalField): double; override;
      procedure GDerivative(pEk: TEField; pEsig: TSignalField; pFrogI: TFrogTrace; Derivs: PArray); override;
      procedure ZDerivative(pEk: TEField; pEsig: TSignalField; Derivs: PArray); override;
      function XFrogZ(pEx: PArray; pERef: TEField; pEsig: TSignalField): double; override;
      procedure XFrogZDerivative(pEk, pERef: TEField; pEsig: TSignalField; Derivs: PArray); override;
      procedure BasicField(pEsig: TSignalField; pEk: TEField); override;
      procedure GateField(pEsig: TSignalField; pEk: TEField); override;
      function CalculateMarginal(pSpec: TSpectrum; pAuto: TAutocorrelation; pSHGSpec: TSpectrum): TMarginal; override;
      function Name: string; override;
      function SpectrumMultiplier: double; override;
      function NeedSpectrum: Boolean; override;
      function NeedAutocorrelation: Boolean; override;
      function NeedSHGSpectrum: Boolean; override;
      function XTestLam(frogLam, refLam: double): double; override;
      function XFrogLam(refLam, testLam: double): double; override;
      constructor Create; override;
  end;


implementation

uses sysutils;

// Difference frequency generation for XFrog only

constructor TDFG2.Create;
begin
  inherited;
  mNLOType := nlDFG2;
end;

function TDFG2.Name: string;
begin
  Name := 'DFG2: signal = reference - test (hi freq reference)';
end;

function TDFG2.SpectrumMultiplier: double;
begin
  // This really should be zero, but I think it's divided by somewhere.
  // As long as it's consistent. . .
  SpectrumMultiplier := 1.0;
end;

procedure TDFG2.MakeXSignalField(pEk, pERef: TEField; pEsig: TSignalField);
var
	tau, t, t0, N, tp: integer;
begin
	N := pEk.N;
  t0 := N div 2;
	for tau := 0 to N - 1 do
  begin
		for t := 0 to N - 1 do
		begin
      tp := t - (tau - t0);
      if (tp < 0) or (tp >= N) then
      begin
        pEsig.Re^[tau*N + t] := 0.0;
        pEsig.Im^[tau*N + t] := 0.0;
        Continue;
      end;
      pEsig.Re^[tau*N + t] := pEk.Re^[tp]*pERef.Re^[t] + pEk.Im^[tp]*pERef.Im^[t];
      pEsig.Im^[tau*N + t] := pEk.Re^[tp]*pERef.Im^[t] - pEk.Im^[tp]*pERef.Re^[t];
		end;
  end;

  // To the w domain
  pEsig.TransformToFreq;
end;

function TDFG2.XFrogZ(pEx: PArray; pERef: TEField; pEsig: TSignalField): double;
var
  t, tau, tp, N, N2, tr, ti: integer;
  sum, sigr, sigi, re, im: double;
begin
  N := pEsig.N;
  N2 := N div 2;
  sum := 0.0;

  for t := 0 to N - 1 do
  for tau := 0 to N - 1 do
  begin
    tp := t - (tau - N2);
    if (tp >= 0) and (tp < N) then
    begin
      tr := 2*tp;
      ti := tr + 1;
      sigr := pEx^[tr]*pERef.Re^[t] + pEx^[ti]*pERef.Im^[t];
      sigi := pEx^[tr]*pERef.Im^[t] - pEx^[ti]*pERef.Re^[t];
    end
    else
    begin
      sigr := 0.0;
      sigi := 0.0;
    end;
    re := pEsig.Re^[tau*N + t] - sigr;
    im := pEsig.Im^[tau*N + t] - sigi;
    sum := sum + re*re + im*im;
  end;
  XFrogZ := sum;
end;

procedure TDFG2.XFrogZDerivative(pEk, pERef: TEField; pEsig: TSignalField; Derivs: PArray);
var
  t, tp, tau, N, N2, vr, vi: integer;
  sigr, sigi, brakr, braki, re, im: double;
begin
  N := pEk.N;
  N2 := N div 2;

  for t := 0 to N - 1 do
  begin
    vr := t*2;
    vi := vr + 1;
    Derivs^[vr] := 0.0;
    Derivs^[vi] := 0.0;
    for tau := 0 to N - 1 do
    begin
      tp := t + tau - N2;
      if (tp < 0) or (tp >= N) then
      begin
         re := 0.0;
         im := 0.0;
         brakr := 0; braki := 0;
      end
      else
      begin
        re := pEref.Re^[tp];
        im := pEref.Im^[tp];
				sigr := pERef.Re^[tp]*pEk.Re^[t] + pERef.Im^[tp]*pEk.Im^[t];
				sigi := -pERef.Re^[tp]*pEk.Im^[t] + pERef.Im^[tp]*pEk.Re^[t];
				brakr := pEsig.Re^[tau*N + tp] - sigr;
				braki := pEsig.Im^[tau*N + tp] - sigi;
      end;

			Derivs^[vr] := Derivs^[vr] - 2*(re*brakr + im*braki);
			Derivs^[vi] := Derivs^[vi] - 2*(im*brakr - re*braki);
    end; // end of tau loop

    Derivs^[vr] := Derivs^[vr]/(1.0*N);
    Derivs^[vi] := Derivs^[vi]/(1.0*N);
  end; // end of t loop
end;

function TDFG2.XTestLam(frogLam, refLam: double): double;
var
  denom: double;
begin
  denom := 1.0/refLam - 1.0/frogLam;

  if denom = 0.0 then
     raise Exception.Create('Reference field at ' + FloatToStrF(refLam, ffGeneral, 4, 0) +
           ' nm and XFROG field at ' + FloatToStrF(frogLam, ffGeneral, 4, 0) +
           ' nm implies an infinite test beam wavelength.');
  if denom < 0.0 then
     raise Exception.Create('Reference field at ' + FloatToStrF(refLam, ffGeneral, 4, 0) +
           ' nm and XFROG field at ' + FloatToStrF(frogLam, ffGeneral, 4, 0) +
           ' nm implies a negative test beam wavelength.');

  XTestLam := 1.0/(denom);
end;

function TDFG2.XFrogLam(refLam, testLam: double): double;
var
  denom: double;
begin
  denom := 1.0/refLam - 1.0/testLam;

  if denom = 0.0 then
     raise Exception.Create('Reference field at ' + FloatToStrF(refLam, ffGeneral, 4, 0) +
           ' nm and test field at ' + FloatToStrF(testLam, ffGeneral, 4, 0) +
           ' nm implies an infinite XFROG Trace wavelength.');
  if denom < 0.0 then
     raise Exception.Create('Reference field at ' + FloatToStrF(refLam, ffGeneral, 4, 0) +
           ' nm and test field at ' + FloatToStrF(testLam, ffGeneral, 4, 0) +
           ' nm implies a negative XFROG Trace wavelength.');

    XFrogLam := 1.0/denom;
end;

procedure TDFG2.BasicField(pEsig: TSignalField; pEk: TEField);
var
	w, tau, N, minusT: integer;
  re, im: double;
begin
  // Since the test field is in the E*(t-tau), we need to use the gate field
  // function to get the right answer
  N := pEk.N;

  for tau := 0 to N - 1 do
  begin
    re := 0;
    im := 0;
    for w := 0 to N - 1 do
    begin
      re := re + pEsig.Re^[tau*N + w];
      im := im + pEsig.Im^[tau*N + w];
    end;
    minusT := N - 1 - tau;
    pEk.Re^[minusT] := re;
    pEk.Im^[minusT] := -im;
  end;
end;

procedure TDFG2.GateField(pEsig: TSignalField; pEk: TEField);
begin
end;

procedure TDFG2.MakeSignalField(pEk: TEField; pEsig: TSignalField);
begin
end;

function TDFG2.Z(pEx: PArray; pEsig: TSignalField): double;
begin
   Z := 0;
end;

procedure TDFG2.ZDerivative(pEk: TEField; pEsig: TSignalField; Derivs: PArray);
begin
end;

procedure TDFG2.GDerivative(pEk: TEField; pEsig: TSignalField; pFrogI: TFrogTrace; Derivs: PArray);
begin
end;

function TDFG2.NeedSpectrum: Boolean;
begin
  NeedSpectrum := True;
end;
function TDFG2.NeedAutocorrelation: Boolean;
begin
  NeedAutocorrelation := True;
end;
function TDFG2.NeedSHGSpectrum: Boolean;
begin
  NeedSHGSpectrum := True;
end;

function TDFG2.CalculateMarginal(pSpec: TSpectrum; pAuto: TAutocorrelation; pSHGSpec: TSpectrum): TMarginal;
begin
  CalculateMarginal := nil;
end;

end.