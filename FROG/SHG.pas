unit SHG;

interface

uses
	NLO, SignalField, Func1D, Numerics, FrogTrace;

type
	TSHG = class(TNLOInteraction)
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

uses TestPlot, ReadIn, sysutils;

constructor TSHG.Create;
begin
  inherited;
  mNLOType := nlSHG;
end;

function TSHG.Name: string;
begin
  Name := 'SHG';
end;

function TSHG.SpectrumMultiplier: double;
begin
  SpectrumMultiplier := 2.0;
end;

procedure TSHG.BasicField(pEsig: TSignalField; pEk: TEField);
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

  // Might want to do the Java-style normalization here.

end;

procedure TSHG.GateField(pEsig: TSignalField; pEk: TEField);
var
  w, N, tau, minusT: integer;
  re, im: double;
begin
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
    pEk.Im^[minusT] := im;
  end;
end;

procedure TSHG.MakeSignalField(pEk: TEField; pEsig: TSignalField);
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
      pEsig.Re^[tau*N + t] := pEk.Re^[t]*pEk.Re^[tp] - pEk.Im^[t]*pEk.Im^[tp];
      pEsig.Im^[tau*N + t] := pEk.Re^[t]*pEk.Im^[tp] + pEk.Im^[t]*pEk.Re^[tp];
		end;
  end;

  // To the w domain
  pEsig.TransformToFreq;

end;

procedure TSHG.MakeXSignalField(pEk, pERef: TEField; pEsig: TSignalField);
var
	tau, t, t0, N, tp: integer;
begin
	N := pEk.N;
 // Fix XFROG bug
 if not (N = pERef.N) then
 begin
      for tau := 0 to N*N - 1 do
      begin
            pESig.Re^[tau] := 0.1;
            pESig.Im^[tau] := 0.0;
      end;
      // To the w domain
      pEsig.TransformToFreq;
      Exit;
 end;

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
      pEsig.Re^[tau*N + t] := pEk.Re^[t]*pERef.Re^[tp] - pEk.Im^[t]*pERef.Im^[tp];
      pEsig.Im^[tau*N + t] := pEk.Re^[t]*pERef.Im^[tp] + pEk.Im^[t]*pERef.Re^[tp];
		end;
  end;

  // To the w domain
  pEsig.TransformToFreq;
end;

function TSHG.Z(pEx: PArray; pEsig: TSignalField): double;
var
  t, tau, tp, N, N2, tr, ti, tpr, tpi: integer;
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
      tpr := 2*tp;
      tpi := tpr + 1;
      sigr := pEx^[tr]*pEx^[tpr] - pEx^[ti]*pEx^[tpi];
      sigi := pEx^[tr]*pEx^[tpi] + pEx^[ti]*pEx^[tpr];
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

procedure TSHG.ZDerivative(pEk: TEField; pEsig: TSignalField; Derivs: PArray);
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
        sigr := pEk.Re^[t]*pEk.Re^[tp] - pEk.Im^[t]*pEk.Im^[tp];
        sigi := pEk.Re^[t]*pEk.Im^[tp] + pEk.Im^[t]*pEk.Re^[tp];
				re := pEk.Re^[tp];
        im := pEk.Im^[tp];
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
				sigr := pEk.Re^[t]*pEk.Re^[tp] - pEk.Im^[t]*pEk.Im^[tp];
				sigi := pEk.Re^[t]*pEk.Im^[tp] + pEk.Im^[t]*pEk.Re^[tp];
				brakr := pEsig.Re^[tau*N + tp] - sigr;
				braki := pEsig.Im^[tau*N + tp] - sigi;
      end;

			Derivs^[vr] := Derivs^[vr] - 2*(re*brakr + im*braki);
			Derivs^[vi] := Derivs^[vi] + 2*(im*brakr - re*braki);
    end; // end of tau loop
    Derivs^[vr] := Derivs^[vr]/(1.0*N);
    Derivs^[vi] := Derivs^[vi]/(1.0*N);
  end; // end of t loop
end;

function TSHG.XFrogZ(pEx: PArray; pERef: TEField; pEsig: TSignalField): double;
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
      sigr := pEx^[tr]*pERef.Re^[tp] - pEx^[ti]*pERef.Im^[tp];
      sigi := pEx^[tr]*pERef.Im^[tp] + pEx^[ti]*pERef.Re^[tp];
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

procedure TSHG.XFrogZDerivative(pEk, pERef: TEField; pEsig: TSignalField; Derivs: PArray);
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
        sigr := pEk.Re^[t]*pERef.Re^[tp] - pEk.Im^[t]*pERef.Im^[tp];
        sigi := pEk.Re^[t]*pERef.Im^[tp] + pEk.Im^[t]*pERef.Re^[tp];
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
      Derivs^[vr] := Derivs^[vr] - 2*(re*brakr - im*braki);
      Derivs^[vi] := Derivs^[vi] - 2*(im*brakr + re*braki);
    end; // end of tau loop
    
    Derivs^[vr] := Derivs^[vr]/(1.0*N);
    Derivs^[vi] := Derivs^[vi]/(1.0*N);
  end; // end of t loop
end;

procedure TSHG.GDerivative(pEk: TEField; pEsig: TSignalField; pFrogI: TFrogTrace; Derivs: PArray);
var
  N, t, tau, w, tp, N2, tpw, tw, tauw: integer;
  fact, fmax, fkmax, cc, ss, re, im: double;
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
				re := pEk.Re^[tp]*cc - pEk.Im^[tp]*ss;
				im := pEk.Re^[tp]*ss + pEk.Im^[tp]*cc;
			end;

      tp := t + tau - N2;
			if not ((tp < 0) or (tp >= N)) then
      begin
        tpw := tp*N + w;
				cc := mC[tpw]; ss := mS[tpw];
				re := re + pEk.Re^[tp]*cc - pEk.Im^[tp]*ss;
				im := im + pEk.Re^[tp]*ss + pEk.Im^[tp]*cc;
			end;

			Derivs^[2*t] := Derivs^[2*t] - fact*(re*pEsig.Re^[tauw]
					+ im*pEsig.Im^[tauw]);
			Derivs^[2*t+1] := Derivs^[2*t + 1] - fact*(re*pEsig.Im^[tauw]
					- im*pEsig.Re^[tauw]);
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

function TSHG.NeedSpectrum: Boolean;
begin
  NeedSpectrum := True;
end;
function TSHG.NeedAutocorrelation: Boolean;
begin
  NeedAutocorrelation := False;
end;
function TSHG.NeedSHGSpectrum: Boolean;
begin
  NeedSHGSpectrum := False;
end;

function TSHG.CalculateMarginal(pSpec: TSpectrum; pAuto: TAutocorrelation; pSHGSpec: TSpectrum): TMarginal;
var
  i, NSpec: integer;
  re, im, newDelT: double;
  Spectrum: TEField;
  Marginal: TMarginal;
  readIn: TReadIn;
  tempSpec: TSpectrum;
begin
  Spectrum := nil;
  Marginal := nil;

  NSpec := 2;
  while NSpec < pSpec.N do
    NSpec := NSpec*2;
  // This next line ensures that the marginal will fit on the grid - if the
  // original spectrum fills pSpec, then the autoconvolution will be about
  // root(2) bigger, and it aliases
  NSpec := NSpec*2;

  Marginal := TMarginal.Create(NSpec);

  try
  	readIn := TReadIn.Create;
   tempSpec := TSpectrum.Create(NSpec);
	  Spectrum := TEField.Create(NSpec);

	  newDelT := pSpec.DelT*(pSpec.N/NSpec);
  	Spectrum.SetMyUnits(newDelT, pSpec.Lam0);
  	tempSpec.SetMyUnits(newDelT, pSpec.Lam0);
  	Marginal.SetMyUnits(newDelT, pSpec.Lam0);

  	readIn.GridSpectrum(pSpec, tempSpec);


  for i := 0 to Spectrum.N - 1 do
  begin
    Spectrum.Re^[i] := tempSpec.Intensity^[i];
    Spectrum.Im^[i] := 0;
  end;

  Spectrum.InverseTransform;
  for i := 0 to NSpec - 1 do
  begin
    re := Spectrum.Re^[i]*Spectrum.Re^[i] - Spectrum.Im^[i]*Spectrum.Im^[i];
    im := 2.0*Spectrum.Re^[i]*Spectrum.Im^[i];
    Spectrum.Re^[i] := re;
    Spectrum.Im^[i] := im;
  end;

  Spectrum.Transform;
  //frmTestPlot.SetTitle('Afer transform');
  //frmTestPlot.PlotRealAndIm(Spectrum);

  for i := 0 to NSpec - 1 do
  begin
    Marginal.Intensity^[i] := Spectrum.Re^[i];
  end;

  finally
    Spectrum.Free;
    tempSpec.Free;
    readIn.Free;
  end;

  Marginal.SetMyUnits(Marginal.DelT, pSpec.Lam0/2.0);
  CalculateMarginal := Marginal;
end;

function TSHG.XTestLam(frogLam, refLam: double): double;
var
  denom: double;
begin
  denom := 1.0/frogLam - 1.0/refLam;

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

function TSHG.XFrogLam(refLam, testLam: double): double;
begin
  XFrogLam := 1.0/(1.0/testLam + 1.0/refLam);
end;

end.