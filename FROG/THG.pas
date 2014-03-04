unit THG;

interface

uses
	NLO, SignalField, Func1D, Numerics, FrogTrace;

type
	TTHG = class(TNLOInteraction)
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
      function SpectrumMultiplier: double; override;
      function Name: string; override;
      function NeedSpectrum: Boolean; override;
      function NeedAutocorrelation: Boolean; override;
      function NeedSHGSpectrum: Boolean; override;
      function XTestLam(frogLam, refLam: double): double; override;
      function XFrogLam(refLam, testLam: double): double; override;
      constructor Create; override;
  end;


implementation

uses ReadIn, sysutils;

constructor TTHG.Create;
begin
  inherited;
  mNLOType := nlTHG;
end;

function TTHG.Name: string;
begin
  Name := 'THG';
end;

function TTHG.SpectrumMultiplier: double;
begin
  SpectrumMultiplier := 3.0;
end;

procedure TTHG.BasicField(pEsig: TSignalField; pEk: TEField);
var
	w, tau, N, minusT: integer;
  re, im: double;
begin
	N := pEsig.N;

  // We acutally use the Gate Field to get the basic Field in THG
  // The interaction is E^2(t)*E(t-tau)
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
    pEk.Im^[minusT] := im;
  end;

  // This one seems to drift in frequency, so let's try. . .
  //pEk.Transform;
  //pEk.Center;
  //pEk.InverseTransform;

end;

procedure TTHG.GateField(pEsig: TSignalField; pEk: TEField);
var
  w, N, tau: integer;
  phi, tInt: double;
begin
  N := pEk.N;

  for w := 0 to N - 1 do
  begin
  	pEk.Re^[w] := 0.0;
    pEk.Im^[w] := 0.0;
  end;

  // Sum over tau
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

  // But this is E^2 for THG
  pEk.SetIntensityAndPhase;
  for tau := 0 to N - 1 do
  begin
    tInt := sqrt(pEk.Intensity^[tau]);
    phi := pEk.Phase^[tau]/2;
    pEk.Re^[tau] := tInt*cos(phi);
    pEk.Im^[tau] := tInt*sin(phi);
  end;
end;

procedure TTHG.MakeSignalField(pEk: TEField; pEsig: TSignalField);
var
	tau, t, t0, N, tp: integer;
  u, v: double;
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
      u := pEk.Re^[t]*pEk.Re^[t] - pEk.Im^[t]*pEk.Im^[t];
      v := 2*pEk.Re^[t]*pEk.Im^[t];
      pEsig.Re^[tau*N + t] := u*pEk.Re^[tp] - v*pEk.Im^[tp];
      pEsig.Im^[tau*N + t] := v*pEk.Re^[tp] + u*pEk.Im^[tp];
		end;
  end;

  // To the w domain
  pEsig.TransformToFreq;
end;

procedure TTHG.MakeXSignalField(pEk, pERef: TEField; pEsig: TSignalField);
var
	tau, t, t0, N, tp: integer;
  u, v: double;
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
      u := pERef.Re^[t]*pERef.Re^[t] - pERef.Im^[t]*pERef.Im^[t];
      v := 2*pERef.Re^[t]*pERef.Im^[t];
      pEsig.Re^[tau*N + t] := u*pEk.Re^[tp] - v*pEk.Im^[tp];
      pEsig.Im^[tau*N + t] := v*pEk.Re^[tp] + u*pEk.Im^[tp];
		end;
  end;

  // To the w domain
  pEsig.TransformToFreq;
end;

function TTHG.Z(pEx: PArray; pEsig: TSignalField): double;
var
  t, tau, tp, N, N2, tr, ti, tpr, tpi: integer;
  sum, sigr, sigi, re, im, er2, ei2: double;
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
      tpr := 2*tp;
      tpi := tpr + 1;
      er2 := pEx^[tr]*pEx^[tr] - pEx^[ti]*pEx^[ti];
      ei2 := 2*pEx^[tr]*pEx^[ti];
      sigr := er2*pEx^[tpr] - ei2*pEx^[tpi];
      sigi := ei2*pEx^[tpr] + er2*pEx^[tpi];
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
  Z := sum;
end;

procedure TTHG.ZDerivative(pEk: TEField; pEsig: TSignalField; Derivs: PArray);
var
  t, tp, tau, N, N2, vr, vi: integer;
  sigr, sigi, brakr, braki, re, im, e2r, e2i: double;
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
        e2r := pEk.Re^[t]*pEk.Re^[t] - pEk.Im^[t]*pEk.Im^[t];
        e2i := 2*pEk.Re^[t]*pEk.Im^[t];
				re := pEk.Re^[tp];
        im := pEk.Im^[tp];
        sigr := e2r*re - e2i*im;
        sigi := e2i*re + e2r*im;
				re := pEk.Re^[t]*pEk.Re^[tp] - pEk.Im^[t]*pEk.Im^[tp];
				im := pEk.Re^[tp]*pEk.Im^[t] + pEk.Re^[t]*pEk.Im^[tp];
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
      Derivs^[vr] := Derivs^[vr] - 4*(re*brakr + im*braki);
      Derivs^[vi] := Derivs^[vi] + 4*(im*brakr - re*braki);

      tp := t + tau - N2;
      if (tp < 0) or (tp >= N) then
      begin
         e2r := 0.0;
         e2i := 0.0;
      end
      else
      begin
        e2r := pEk.Re^[tp]*pEk.Re^[tp] - pEk.Im^[tp]*pEk.Im^[tp];
        e2i := 2*pEk.Re^[tp]*pEk.Im^[tp];
				re := pEk.Re^[t];
        im := pEk.Im^[t];
        sigr := e2r*re - e2i*im;
        sigi := e2i*re + e2r*im;
        brakr := pEsig.Re^[tau*N + tp] - sigr;
        braki := pEsig.Im^[tau*N + tp] - sigi;
      end;

			Derivs^[vr] := Derivs^[vr] - 2*(e2r*brakr + e2i*braki);
			Derivs^[vi] := Derivs^[vi] + 2*(e2i*brakr - e2r*braki);
    end; // end of tau loop
    Derivs^[vr] := Derivs^[vr]/(1.0*N);
    Derivs^[vi] := Derivs^[vi]/(1.0*N);
  end; // end of t loop
end;

function TTHG.XFrogZ(pEx: PArray; pERef: TEField; pEsig: TSignalField): double;
var
  t, tau, tp, N, N2, tpr, tpi: integer;
  sum, sigr, sigi, re, im, er2, ei2: double;
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
      tpr := 2*tp;
      tpi := tpr + 1;
      er2 := pERef.Re^[t]*pERef.Re^[t] - pERef.Im^[t]*pERef.Im^[t];
      ei2 := 2*pERef.Re^[t]*pERef.Im^[t];
      sigr := er2*pEx^[tpr] - ei2*pEx^[tpi];
      sigi := ei2*pEx^[tpr] + er2*pEx^[tpi];
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

procedure TTHG.XFrogZDerivative(pEk, pERef: TEField; pEsig: TSignalField; Derivs: PArray);
var
  t, tp, tau, N, N2, vr, vi: integer;
  sigr, sigi, brakr, braki, re, im, e2r, e2i: double;
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
         e2r := 0.0;
         e2i := 0.0;
         brakr := 0.0;
         braki := 0.0;
      end
      else
      begin
        e2r := pERef.Re^[tp]*pERef.Re^[tp] - pERef.Im^[tp]*pERef.Im^[tp];
        e2i := 2*pERef.Re^[tp]*pERef.Im^[tp];
				re := pEk.Re^[t];
        im := pEk.Im^[t];
        sigr := e2r*re - e2i*im;
        sigi := e2i*re + e2r*im;
        brakr := pEsig.Re^[tau*N + tp] - sigr;
        braki := pEsig.Im^[tau*N + tp] - sigi;
      end;

			Derivs^[vr] := Derivs^[vr] - 2*(e2r*brakr + e2i*braki);
			Derivs^[vi] := Derivs^[vi] + 2*(e2i*brakr - e2r*braki);
    end; // end of tau loop
    Derivs^[vr] := Derivs^[vr]/(1.0*N);
    Derivs^[vi] := Derivs^[vi]/(1.0*N);
  end; // end of t loop
end;

procedure TTHG.GDerivative(pEk: TEField; pEsig: TSignalField; pFrogI: TFrogTrace; Derivs: PArray);
var
  N, t, tau, w, tp, N2, tpw, tw, tauw: integer;
  fact, fmax, fkmax, cc, ss, re, im, e2r, e2i: double;
  tFrogIk: TFrogTrace;
begin
  N := pEk.N;
  SetUpMinArrays(N);
  N2 := N div 2;
  pEk.SetIntensityAndPhase;

  tFrogIk := TRetrievedFrogTrace.Create(N);
  try

  tFrogIk.MakeTraceFrom(pEsig);
  fmax := pFrogI.Max;
  fkmax := tFrogIk.Max;

	for t := 0 to N - 1 do
  begin
		Derivs^[2*t] := 0.0;
    Derivs^[2*t + 1] := 0.0;
		for w := 0 to N - 1 do
		for tau := 0 to N - 1 do
    begin
      fact := pFrogI.Vals^[tau*N + w]/fmax - tFrogIk.Vals^[tau*N + w]/fkmax;
      tw := t*N + w;
      tauw := tau*N + w;

			// The deriv wrt the real part of the field
      tp := t - (tau - N2);
			if (tp < 0) or (tp >= N) then
      begin
        re := 0.0;
        im := 0.0;
      end
      else
      begin
 				cc := mC[tw]; ss := mS[tw];
				e2r := pEk.Re^[t]*pEk.Re^[tp] - pEk.Im^[t]*pEk.Im^[tp];
				e2i := pEk.Im^[t]*pEk.Re^[tp] + pEk.Re^[t]*pEk.Im^[tp];
				re := 2*(e2r*cc - e2i*ss);
				im := 2*(e2r*ss + e2i*cc);
				Derivs^[2*t] := Derivs^[2*t] - 4*fact*(re*pEsig.Re^[tauw]
						+ im*pEsig.Im^[tauw]);
				Derivs^[2*t+1] := Derivs^[2*t+1] - 4*fact*(re*pEsig.Im^[tauw]
						- im*pEsig.Re^[tauw]);
			end;

      tp := t + tau - N2;
			if (tp < 0) or (tp >= N) then
        re := 0.0
      else
      begin
        tpw := tp*N + w;
				cc := mC[tpw]; ss := mS[tpw];
				e2r := pEk.Re^[tp]*pEk.Re^[tp] - pEk.Im^[tp]*pEk.Im^[tp];
				e2i := 2*pEk.Im^[tp]*pEk.Re^[tp];
				re := e2r*cc - e2i*ss;
				im := e2r*ss + e2i*cc;
				Derivs^[2*t] := Derivs^[2*t] - 4*fact*(re*pEsig.Re^[tauw]
						+ im*pEsig.Im^[tauw]);
				Derivs^[2*t+1] := Derivs^[2*t+1] - 4*fact*(im*pEsig.Re^[tauw]
						- re*pEsig.Im^[tauw]);
			end;
		end;  // end of tau loop
	end;

  finally
    tFrogIk.Free;
  end;

  for t := 0 to N - 1 do
  begin
		Derivs^[2*t] := Derivs^[2*t]/(1.0*N*N);
		Derivs^[2*t+1] := Derivs^[2*t+1]/(1.0*N*N);
  end;
end;

function TTHG.NeedSpectrum: Boolean;
begin
  NeedSpectrum := True;
end;
function TTHG.NeedAutocorrelation: Boolean;
begin
  NeedAutocorrelation := False;
end;
function TTHG.NeedSHGSpectrum: Boolean;
begin
  NeedSHGSpectrum := True;
end;

function TTHG.CalculateMarginal(pSpec: TSpectrum; pAuto: TAutocorrelation; pSHGSpec: TSpectrum): TMarginal;
var
  Spectrum, SHGSpectrum: TEField;
  tempSpec: TSpectrum;
  Marginal: TMarginal;
  ReadIn: TReadIn;
  N, i, maxi: integer;
  DelF, Lam0, max, re, im, F, T: double;
begin
  // Find the maximum frequency range F
  if (pSpec.DelF*pSpec.N) > (pSHGSpec.DelF*pSHGSpec.N) then
    F := pSpec.DelF*pSpec.N
  else
    F := pSHGSpec.DelF*pSHGSpec.N;

  // Find the maximum time range T
  if (pSpec.DelT*pSpec.N) > (pSHGSpec.DelT*pSHGSpec.N) then
    T := pSpec.DelT*pSpec.N
  else
    T := pSHGSpec.DelT*pSHGSpec.N;

  i := Trunc(F*T);
  N := 2;
  while N < i do
    N := N*2;

  // Get the smallest DelF
  if pSpec.DelF < pSHGSpec.DelF then
    DelF := pSpec.DelF
  else
    DelF := pSHGSpec.DelF;

  max := 0;
  for i := 0 to pSpec.N - 1 do
    if pSpec.Intensity^[i] > max then
    begin
      max := pSpec.Intensity^[i];
      maxi := i;
    end;
  Lam0 := (maxi - pSpec.N div 2)*pSpec.DelLam + pSpec.Lam0;

  ReadIn := TReadIn.Create;
  tempSpec := TSpectrum.Create(N);
  Spectrum := TEField.Create(N);
  SHGSpectrum := TEField.Create(N);
  Marginal := TMarginal.Create(N);
  tempSpec.SetMyUnits(1.0/(N*DelF), Lam0);
  Spectrum.SetMyUnits(tempSpec.DelT, tempSpec.Lam0);
  SHGSpectrum.SetMyUnits(tempSpec.DelT, tempSpec.Lam0/2.0);
  Marginal.SetMyUnits(tempSpec.DelT, tempSpec.Lam0/3.0);
  try
    ReadIn.GridSpectrum(pSpec, tempSpec);
    for i := 0 to N - 1 do
    begin
      Spectrum.Re^[i] := tempSpec.Intensity^[i];
      Spectrum.Im^[i] := 0.0;
    end;

    tempSpec.SetMyUnits(Spectrum.DelT, Spectrum.Lam0/2.0);
    ReadIn.GridSpectrum(pSHGSpec, tempSpec);
    for i := 0 to N - 1 do
    begin
      SHGSpectrum.Re^[i] := tempSpec.Intensity^[i];
      SHGSpectrum.Im^[i] := 0.0;
    end;

    Spectrum.InverseTransform;
    SHGSpectrum.InverseTransform;

    for i := 0 to N - 1 do
    begin
      re := Spectrum.Re^[i]*SHGSpectrum.Re^[i] - Spectrum.Im^[i]*SHGSpectrum.Im^[i];
      im := Spectrum.Re^[i]*SHGSpectrum.Im^[i] + Spectrum.Im^[i]*SHGSpectrum.Re^[i];
      Spectrum.Re^[i] := re;
      Spectrum.Im^[i] := im;
    end;

    Spectrum.Transform;

    for i := 0 to N - 1 do
      Marginal.Intensity^[i] := Spectrum.Re^[i];

  finally
    tempSpec.Free;
    Spectrum.Free;
    SHGSpectrum.Free;
    ReadIn.Free;
  end;
  CalculateMarginal := Marginal;
end;

function TTHG.XTestLam(frogLam, refLam: double): double;
var
  denom: double;
begin
  denom := 1.0/frogLam - 2.0/refLam;

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

function TTHG.XFrogLam(refLam, testLam: double): double;
begin
  XFrogLam := 1.0/(1.0/testLam + 2.0/refLam);
end;

end.
