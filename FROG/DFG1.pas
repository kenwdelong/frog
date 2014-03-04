unit DFG1;

interface

uses
	NLO, SignalField, Func1D, Numerics, FrogTrace;

type
	TDFG1 = class(TNLOInteraction)
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

constructor TDFG1.Create;
begin
  inherited;
  mNLOType := nlDFG1;
end;

function TDFG1.Name: string;
begin
  Name := 'DFG1: signal = test - reference (low freq. reference)';
end;

function TDFG1.SpectrumMultiplier: double;
begin
  // This really should be zero, but I think it's divided by somewhere.
  // As long as it's consistent. . .
  SpectrumMultiplier := 1.0;
end;

procedure TDFG1.MakeXSignalField(pEk, pERef: TEField; pEsig: TSignalField);
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
      pEsig.Re^[tau*N + t] := pEk.Re^[t]*pERef.Re^[tp] + pEk.Im^[t]*pERef.Im^[tp];
      pEsig.Im^[tau*N + t] := -pEk.Re^[t]*pERef.Im^[tp] + pEk.Im^[t]*pERef.Re^[tp];
		end;
  end;

  // To the w domain
  pEsig.TransformToFreq;
end;

function TDFG1.XFrogZ(pEx: PArray; pERef: TEField; pEsig: TSignalField): double;
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
      tr := 2*t;
      ti := tr + 1;
      sigr := pEx^[tr]*pERef.Re^[tp] + pEx^[ti]*pERef.Im^[tp];
      sigi := -pEx^[tr]*pERef.Im^[tp] + pEx^[ti]*pERef.Re^[tp];
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

procedure TDFG1.XFrogZDerivative(pEk, pERef: TEField; pEsig: TSignalField; Derivs: PArray);
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
      tp := t - (tau - N2);
      if (tp >= 0) and (tp < N) then
      begin
        sigr := pEk.Re^[t]*pERef.Re^[tp] + pEk.Im^[t]*pERef.Im^[tp];
        sigi := -pEk.Re^[t]*pERef.Im^[tp] + pEk.Im^[t]*pERef.Re^[tp];
				re := pERef.Re^[tp];
        im := pERef.Im^[tp];
      end
      else
      begin
        sigr := 0.0;
        sigi := 0.0;
        re := 0.0;
        im := 0.0;
      end; // End of check tp

      brakr := pEsig.Re^[tau*N + t] - sigr;
      braki := pEsig.Im^[tau*N + t] - sigi;
      Derivs^[vr] := Derivs^[vr] - 2*(re*brakr + im*braki);
      Derivs^[vi] := Derivs^[vi] + 2*(im*brakr - re*braki);
    end; // end of tau loop

    Derivs^[vr] := Derivs^[vr]/(1.0*N);
    Derivs^[vi] := Derivs^[vi]/(1.0*N);
  end; // end of t loop
end;

function TDFG1.XTestLam(frogLam, refLam: double): double;
var
  testF: double;
begin
  testF := (1.0/frogLam + 1.0/refLam);
  if testF <> 0.0 then
    XTestLam := 1.0/testF
  else
    XTestLam := frogLam;
end;

function TDFG1.XFrogLam(refLam, testLam: double): double;
var
  denom: double;
begin
  denom := 1.0/testLam - 1.0/refLam;

  if denom = 0.0 then
     raise Exception.Create('Reference field at ' + FloatToStrF(refLam, ffGeneral, 4, 0) +
           ' nm and test field at ' + FloatToStrF(testLam, ffGeneral, 4, 0) +
           ' nm implies an infinite XFROG Trace wavelength.');
  if denom < 0.0 then
     raise Exception.Create('Reference field at ' + FloatToStrF(refLam, ffGeneral, 4, 0) +
           ' nm and test field at ' + FloatToStrF(testLam, ffGeneral, 4, 0) +
           ' nm implies a negative XFROG Trace wavelength.');
           
  XFrogLam := 1.0/(denom);
end;

procedure TDFG1.BasicField(pEsig: TSignalField; pEk: TEField);
var
	w, tau, N: integer;
begin
	N := pEsig.N;

  for w := 0 to N - 1 do
  begin
  	pEk.Re^[w] := 0.0;
    pEk.Im^[w] := 0.0;
  end;

  for tau := 0 to N - 1 do
  begin
  	for w := 0 to N - 1 do
  	begin
    	pEk.Re^[w] := pEk.Re^[w] + pEsig.Re^[tau*N + w];
      pEk.Im^[w] := pEk.Im^[w] + pEsig.Im^[tau*N + w];
  	end;
  end;

  // Back to the Time domain
  pEk.InverseTransform;
end;

procedure TDFG1.GateField(pEsig: TSignalField; pEk: TEField);
begin
end;

procedure TDFG1.MakeSignalField(pEk: TEField; pEsig: TSignalField);
begin
end;

function TDFG1.Z(pEx: PArray; pEsig: TSignalField): double;
begin
   Z := 0;
end;

procedure TDFG1.ZDerivative(pEk: TEField; pEsig: TSignalField; Derivs: PArray);
begin
end;

procedure TDFG1.GDerivative(pEk: TEField; pEsig: TSignalField; pFrogI: TFrogTrace; Derivs: PArray);
begin
end;

function TDFG1.NeedSpectrum: Boolean;
begin
  NeedSpectrum := True;
end;
function TDFG1.NeedAutocorrelation: Boolean;
begin
  NeedAutocorrelation := True;
end;
function TDFG1.NeedSHGSpectrum: Boolean;
begin
  NeedSHGSpectrum := True;
end;

function TDFG1.CalculateMarginal(pSpec: TSpectrum; pAuto: TAutocorrelation; pSHGSpec: TSpectrum): TMarginal;
begin
  CalculateMarginal := nil;
end;

end.