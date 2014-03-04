unit PG;

interface

uses
	NLO, SignalField, Func1D, Numerics, FrogTrace;

type
	TPG = class(TNLOInteraction)
  	public
    	procedure MakeSignalField(pEk: TEField; pEsig: TSignalField); override;
    	procedure MakeXSignalField(pEk, pERef: TEField; pEsig: TSignalField); override;
      procedure GDerivative(pEk: TEField; pEsig: TSignalField; pFrogI: TFrogTrace; Derivs: PArray); override;
      function Z(pEx: PArray; pEsig: TSignalField): double; override;
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

uses ReadIn;

constructor TPG.Create;
begin
  inherited;
  mNLOType := nlPG;
end;

function TPG.Name: string;
begin
  Name := 'PG';
end;

function TPG.SpectrumMultiplier: double;
begin
  SpectrumMultiplier := 1.0;
end;

procedure TPG.BasicField(pEsig: TSignalField; pEk: TEField);
var
	w, tau, N: integer;
begin
	N := pEsig.N;

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

end;

procedure TPG.GateField(pEsig: TSignalField; pEk: TEField);
var
  localIntensity, corr, re, im: double;
  w, N, tau, minusT: integer;
begin
  // Don't forget, for PG the gate is real so you have to mult by the complex field
  pEk.SetIntensityAndPhase;
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
    localIntensity := sqrt(re*re + im*im);
    minusT := N - 1 - tau;
    if pEk.Intensity^[minusT] = 0 then Continue;
    corr := sqrt(Abs(localIntensity)/pEk.Intensity^[minusT]);
    pEk.Re^[minusT] := pEk.Re^[minusT]*corr;
    pEk.Im^[minusT] := pEk.Im^[minusT]*corr;
  end;

end;

procedure TPG.MakeSignalField(pEk: TEField; pEsig: TSignalField);
var
	tau, t, t0, N, tp: integer;
  inten: double;
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
      inten := pEk.Re^[tp]*pEk.Re^[tp] + pEk.Im^[tp]*pEk.Im^[tp];
      pEsig.Re^[tau*N + t] := pEk.Re^[t]*inten;
      pEsig.Im^[tau*N + t] := pEk.Im^[t]*inten;
		end;
  end;

  // To the w domain
  pEsig.TransformToFreq;
end;

// For XFrog
procedure TPG.MakeXSignalField(pEk, pERef: TEField; pEsig: TSignalField);
var
	tau, t, t0, N, tp: integer;
  inten: double;
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
      inten := pERef.Re^[tp]*pERef.Re^[tp] + pERef.Im^[tp]*pERef.Im^[tp];
      pEsig.Re^[tau*N + t] := pEk.Re^[t]*inten;
      pEsig.Im^[tau*N + t] := pEk.Im^[t]*inten;
		end;
  end;

  // To the w domain
  pEsig.TransformToFreq;
end;

function TPG.Z(pEx: PArray; pEsig: TSignalField): double;
var
  t, tau, tp, N, N2, tr, ti, tpr, tpi: integer;
  sum, sigr, sigi, re, im, inten: double;
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
      inten := pEx^[tpr]*pEx^[tpr] + pEx^[tpi]*pEx^[tpi];
      sigr := inten*pEx^[tr];
      sigi := inten*pEx^[ti];
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

procedure TPG.ZDerivative(pEk: TEField; pEsig: TSignalField; Derivs: PArray);
var
  t, tp, tau, N, N2, vr, vi: integer;
  sigr, sigi, inten, brakr, braki, re, im: double;
begin
  N := pEk.N;
  N2 := N div 2;
  pEk.SetIntensityAndPhase;

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
        inten := pEk.Intensity^[tp];
        sigr := inten*pEk.Re^[t];
        sigi := inten*pEk.Im^[t];
      end
      else
      begin
        sigr := 0.0;
        sigi := 0.0;
        inten := 0.0;
      end; // End of check tp

      brakr := pEsig.Re^[tau*N + t] - sigr;
      braki := pEsig.Im^[tau*N + t] - sigi;
      Derivs^[vr] := Derivs^[vr] - 2*inten*brakr;
      Derivs^[vi] := Derivs^[vi] - 2*inten*braki;

      tp := t + tau - N2;
      if (tp < 0) or (tp >= N) then
      begin
         re := 0.0;
         im := 0.0;
      end
      else
      begin
        re := pEk.Re^[tp];
        im := pEk.Im^[tp];
				inten := pEk.Intensity^[t];
				brakr := pEsig.Re^[tau*N + tp] - pEk.Re^[tp]*inten;
				braki := pEsig.Im^[tau*N + tp] - pEk.Im^[tp]*inten;
      end;

			Derivs^[vr] := Derivs^[vr] - 4*pEk.Re^[t]*(re*brakr + im*braki);
			Derivs^[vi] := Derivs^[vi] - 4*pEk.Im^[t]*(re*brakr + im*braki);
    end; // end of tau loop
    Derivs^[vr] := Derivs^[vr]/(1.0*N);
    Derivs^[vi] := Derivs^[vi]/(1.0*N);
  end; // end of t loop
end;

function TPG.XFrogZ(pEx: PArray; pERef: TEField; pEsig: TSignalField): double;
var
  t, tau, tp, N, N2, tr, ti: integer;
  sum, sigr, sigi, re, im, inten: double;
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
      inten := pERef.Intensity^[tp];
      sigr := inten*pEx^[tr];
      sigi := inten*pEx^[ti];
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

procedure TPG.XFrogZDerivative(pEk, pERef: TEField; pEsig: TSignalField; Derivs: PArray);
var
  t, tp, tau, N, N2, vr, vi: integer;
  sigr, sigi, inten, brakr, braki: double;
begin
  N := pEk.N;
  N2 := N div 2;
  pEk.SetIntensityAndPhase;

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
        inten := pERef.Intensity^[tp];
        sigr := inten*pEk.Re^[t];
        sigi := inten*pEk.Im^[t];
      end
      else
      begin
        sigr := 0.0;
        sigi := 0.0;
        inten := 0.0;
      end; // End of check tp

      brakr := pEsig.Re^[tau*N + t] - sigr;
      braki := pEsig.Im^[tau*N + t] - sigi;
      Derivs^[vr] := Derivs^[vr] - 2*inten*brakr;
      Derivs^[vi] := Derivs^[vi] - 2*inten*braki;
    end; // end of tau loop

    Derivs^[vr] := Derivs^[vr]/(1.0*N);
    Derivs^[vi] := Derivs^[vi]/(1.0*N);
  end; // end of t loop
end;

procedure TPG.GDerivative(pEk: TEField; pEsig: TSignalField; pFrogI: TFrogTrace; Derivs: PArray);
var
  N, t, tau, w, tp, N2, jj, tw, tauw: integer;
  inten, fact, derv, fmax, fkmax, cc, ss, temp: double;
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
        derv := 0.0;
        inten := 0.0;
      end
      else
      begin
				inten := pEk.Intensity^[tp];
				derv := mC[tw]*pEsig.Re^[tauw] + mS[tw]*pEsig.Im^[tauw];
				derv := derv*inten;
			end;

      tp := t + tau - N2;
			if (tp < 0) or (tp >= N) then
        temp := 0.0
      else
      begin
        jj := tp*N + w;
				cc := mC[jj]; ss := mS[jj];
				temp := (pEk.Re^[tp]*cc - pEk.Im^[tp]*ss)*pEsig.Re^[tauw]
					  + (pEk.Re^[tp]*ss + pEk.Im^[tp]*cc)*pEsig.Im^[tauw];
			end;

			derv := derv + (temp*2.0*pEk.Re^[t]);
			Derivs^[2*t] := Derivs^[2*t] - 4*fact*derv;

			// Now the derivative wrt the imaginary part */
			// Can use the same "inten" as before */
			derv := mC[tw]*pEsig.Im^[tauw] - mS[tw]*pEsig.Re^[tauw];
			derv := derv*inten;

			// We can also use the same temp as before! */
			derv := derv + (temp*2.0*pEk.Im^[t]);
			Derivs^[2*t+1] := Derivs^[2*t + 1] - 4*fact*derv;
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

function TPG.NeedSpectrum: Boolean;
begin
  NeedSpectrum := True;
end;
function TPG.NeedAutocorrelation: Boolean;
begin
  NeedAutocorrelation := True;
end;
function TPG.NeedSHGSpectrum: Boolean;
begin
  NeedSHGSpectrum := False;
end;

function TPG.CalculateMarginal(pSpec: TSpectrum; pAuto: TAutocorrelation; pSHGSpec: TSpectrum): TMarginal;
var
  Spectrum: TEField;
  tempSpec: TSpectrum;
  tempAuto: TAutocorrelation;
  Marginal: TMarginal;
  ReadIn: TReadIn;
  N, i, maxi: integer;
  DelF, Lam0, max, re, im, F, T: double;
begin
  // Find the maximum frequency range F
  if (pSpec.DelF*pSpec.N) > (pAuto.DelF*pAuto.N) then
    F := pSpec.DelF*pSpec.N
  else
    F := pAuto.DelF*pAuto.N;

  // Find the maximum time range T
  if (pSpec.DelT*pSpec.N) > (pAuto.DelT*pAuto.N) then
    T := pSpec.DelT*pSpec.N
  else
    T := pAuto.DelT*pAuto.N;

  i := Trunc(F*T);
  N := 2;
  while N < i do
    N := N*2;

  // Get the smallest DelF
  if pSpec.DelF < pAuto.DelF then
    DelF := pSpec.DelF
  else
    DelF := pAuto.DelF;

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
  tempAuto := TAutocorrelation.Create(N);
  Spectrum := TEField.Create(N);
  Marginal := TMarginal.Create(N);
  tempSpec.SetMyUnits(1.0/(N*DelF), Lam0);
  tempAuto.SetMyUnits(tempSpec.DelT, tempSpec.Lam0);
  Spectrum.SetMyUnits(tempSpec.DelT, tempSpec.Lam0);
  Marginal.SetMyUnits(tempSpec.DelT, tempSpec.Lam0);
  try
    ReadIn.GridSpectrum(pSpec, tempSpec);
    for i := 0 to N - 1 do
    begin
      Spectrum.Re^[i] := tempSpec.Intensity^[i];
      Spectrum.Im^[i] := 0.0;
    end;

    tempAuto.SetMyUnits(Spectrum.DelT, Spectrum.Lam0);
    ReadIn.GridAutocorrelation(pAuto, tempAuto);

    Spectrum.InverseTransform;

    for i := 0 to N - 1 do
    begin
      re := Spectrum.Re^[i]*tempAuto.Intensity^[i];
      im := Spectrum.Im^[i]*tempAuto.Intensity^[i];
      Spectrum.Re^[i] := re;
      Spectrum.Im^[i] := im;
    end;

    Spectrum.Transform;

    for i := 0 to N - 1 do
      Marginal.Intensity^[i] := Spectrum.Re^[i];

  finally
    tempAuto.Free;
    tempSpec.Free;
    Spectrum.Free;
    ReadIn.Free;
  end;
  CalculateMarginal := Marginal;
end;

function TPG.XTestLam(frogLam, refLam: double): double;
begin
  XTestLam := frogLam;
end;

function TPG.XFrogLam(refLam, testLam: double): double;
begin
  XFrogLam := testLam;
end;


end.
