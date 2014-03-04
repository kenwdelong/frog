unit EFieldCals;

interface

uses
  Numerics, FrogObj, Func1D, Classes, Math;

type
  TEFieldInitType = (eiNoise, eiPhaseNoise, eiIntensityNoise, eiCustom);
  TUnits = (unPixel, unReal);
  TEFieldShape = (fsGaussian, fsSech, fsSuperGaussian);

  TEFieldCals = class(TFrogObject)
    private
      mAutoDelT: boolean;
      mN: integer;

      // General Field parms
      mInitType: TEFieldInitType;
      mDomain: TDomainType;
      mUnits: TUnits;
      mLam0: double;

      // For Time Domain
      mTempShape: TEFieldShape;
      mTempWidth: double;
      mTempChirp: double;
      mSPM: double;
      mTCP: double;
      mDelT: double;
      // Double Pulse
      mTempHeight2: double;
      mTempSep2: double;
      mTempPhase2: double;
      mTempWidth2: double;
      mTempChirp2: double;
      mSPM2: double;
      mTCP2: double;


      // Freq Domain
      mSpecShape: TEFieldShape;
      mSpecWidth: double;  // This is in nm
      mSpecChirp: double;
      mSCP: double;
      mSQP: double;
      mDelLam: double;  // This is also in nm
      // Double Pulse
      mSpecHeight2: double;
      mSpecSep2: double;   // This is also in nm, dui bu dui?
      mSpecPhase2: double;
      mSpecWidth2: double;
      mSpecChirp2: double;
      mSCP2: double;
      mSQP2: double;

      // Access procedures
      procedure SetN(pN:integer);
      procedure SetDomain(pDomain: TDomainType);
      procedure SetUnits(pUnits: TUnits);
      procedure SetLam0(pLam0: double);
      procedure SetTempWidth(pWidth: double);
      procedure SetDelT(pDelT: double);
      procedure SetSpecWidth(pSpecWidth: double);
      procedure SetDelLam(pDelLam: double);

      // internal procedures
      procedure CalcDelT;
      procedure CalcDelLam;
      procedure GenTime(pE: TEField; pPulseNum: integer);
      procedure GenFreq(pE: TEField; pPulseNum: integer);

      // E Field initialzation
      procedure AllNoise(pE: TEField);
      procedure PhaseNoise(pE: TEField);
      procedure IntensityNoise(pE: TEField);
      procedure CustomField(pE: TEField);
    public
      // Functional interface
      procedure GenerateE(pE: TEField);

      // General properties
      property N: integer read mN write SetN;
      property InitType: TEFieldInitType read mInitType write mInitType;
      property Domain: TDomainType read mDomain write SetDomain;
      property Units: TUnits read mUnits write SetUnits;
      property Lam0: double read mLam0 write SetLam0;
      function ShapeName: string;

      // For Time Domain
      property TempShape: TEFieldShape read mTempShape write mTempShape;
      property TempWidth: double read mTempWidth write SetTempWidth;
      property TempChirp: double read mTempChirp write mTempChirp;
      property SPM: double read mSPM write mSPM;
      property TCP: double read mTCP write mTCP;
      property DelT: double read mDelT write SetDelT;
      // Double Pulse
      property TempHeight2: double read mTempHeight2 write mTempHeight2;
      property TempSep2: double read mTempSep2 write mTempSep2;
      property TempPhase2: double read mtempPhase2 write mTempPhase2;
      property TempWidth2: double read mTempWidth2 write mTempWidth2;
      property TempChirp2: double read mTempChirp2 write mTempChirp2;
      property SPM2: double read mSPM2 write mSPM2;
      property TCP2: double read mTCP2 write mTCP2;


      // Freq Domain
      property SpecShape: TEFieldShape read mSpecShape write mSpecShape;
      property SpecWidth: double read mSpecWidth write SetSpecWidth;
      property SpecChirp: double read mSpecChirp write mSpecChirp;
      property SCP: double read mSCP write mSCP;
      property SQP: double read mSQP write mSQP;
      property DelLam: double read mDelLam write SetDelLam;
      // Double Pulse
      property SpecHeight2: double read mSpecHeight2 write mSpecHeight2;
      property SpecSep2: double read mSpecSep2 write mSpecSep2;
      property SpecPhase2: double read mSpecPhase2 write mSpecPhase2;
      property SpecWidth2: double read mSpecWidth2 write mSpecWidth2;
      property SpecChirp2: double read mSpecChirp2 write mSpecChirp2;
      property SCP2: double read mSCP2 write mSCP2;
      property SQP2: double read mSQP2 write mSQP2;

      // The Log file
      procedure InitLog(pLog: TStringList);

      constructor Create;
  end;

const
  AUTO_DELT = 10.0;
  AUTO_DELF = 6.0;


implementation

uses SysUtils;

constructor TEFieldCals.Create;
begin
  inherited Create;

  // Set up the non-zero values
  mAutoDelT := True;
  mN := 64;

  // General Field parms
  mInitType := eiCustom;
  mDomain := dtTime;
  mUnits := unPixel;
  mLam0 := sqrt(300.0*mN);

  // For Time Domain
  mTempShape := fsGaussian;
  mTempWidth := 10.0;
  mDelT := mTempWidth/AUTO_DELT;
  // Double Pulse
  mTempSep2 := 6.0;
  mTempWidth2 := 10.0;

  // Freq Domain
  mSpecShape := fsGaussian;
  mSpecWidth := 10.0;
  mDelLam := mSpecWidth/AUTO_DELF;
  // Double Pulse
  mSpecSep2 := 6;
  mSpecWidth2 := 10;

  case mDomain of
    dtTime: CalcDelLam;
    dtFreq: CalcDelT;
  end;
end;

procedure TEFieldCals.SetN(pN: integer);
begin
  mN := pN;
  case mDomain of
    dtTime: CalcDelLam;
    dtFreq: CalcDelT;
  end;
  if mUnits = unPixel then
    mLam0 := sqrt(300.0*mN);
end;

procedure TEFieldCals.SetLam0(pLam0: double);
begin
  mLam0 := pLam0;
  case mDomain of
    dtTime: CalcDelLam;
    dtFreq: CalcDelT;
  end;
end;

procedure TEFieldCals.SetDelT(pDelT: double);
begin
  mDelT := pDelT;
  mAutoDelT := False;
  CalcDelLam;
end;

procedure TEFieldCals.SetDelLam(pDelLam: double);
begin
  mDelLam := pDelLam;
  mAutoDelT := False;
  CalcDelT;
end;

procedure TEFieldCals.CalcDelT;
begin
  mDelT := mLam0*mLam0/(mDelLam*mN*300.0);
end;

procedure TEFieldCals.CalcDelLam;
begin
  mDelLam := mLam0*mLam0/(mDelT*mN*300.0);
end;

procedure TEFieldCals.SetTempWidth(pWidth: double);
begin
  mTempWidth := pWidth;
  if mAutoDelT and (mUnits = unReal) then
  begin
    mDelT := mTempWidth/AUTO_DELT;
    CalcDelLam;
  end;
end;

procedure TEFieldCals.SetSpecWidth(pSpecWidth: double);
begin
  mSpecWidth := pSpecWidth;
  if mAutoDelT and (mUnits = unReal) then
  begin
    mDelLam := mSpecWidth/AUTO_DELF;
    CalcDelT;
  end;
end;

procedure TEFieldCals.SetDomain(pDomain: TDomainType);
begin
  mDomain := pDomain;
  case pDomain of
    dtTime:
      if mAutoDelT and (mUnits = unReal) then
      begin
  	    mDelT := mTempWidth/AUTO_DELT;
        CalcDelLam;
      end;
    dtFreq:
      if mAutoDelT and (mUnits = unReal) then
      begin
  	    mDelLam := mSpecWidth/AUTO_DELF;
        CalcDelT;
      end;
  end;
end;

procedure TEFieldCals.SetUnits(pUnits: TUnits);
begin
  if mUnits = pUnits then Exit;
  mUnits := pUnits;
  if pUnits = unReal then
  begin
    if mAutoDelT then
      if mDomain = dtTime then
        mDelT := mTempWidth/AUTO_DELT
      else if mDomain = dtFreq then
        mDelLam := mSpecWidth/AUTO_DELF;
  end;

  if pUnits = unPixel then
  begin
    mDelT := 1.0;
    mLam0 := Sqrt(300.0*mN);
    CalcDelLam;
  end
  else
    mLam0 := 800.0;
end;

function TEFieldCals.ShapeName: string;
begin
  if mDomain = dtTime then
    case mTempShape of
      fsGaussian: ShapeName := 'Gaussian';
      fsSech: ShapeName := 'Sech-squared';
      fsSuperGaussian: ShapeName := 'Super Gaussian';
    end
  else
    case mSpecShape of
      fsGaussian: ShapeName := 'Gaussian';
      fsSech: ShapeName := 'Sech-squared';
      fsSuperGaussian: ShapeName := 'Super Gaussian';
    end;
end;

procedure TEFieldCals.GenerateE(pE: TEField);
begin
  pE.SetMyUnits(mDelT, mLam0);
  case mInitType of
    eiNoise: AllNoise(pE);
    eiPhaseNoise: PhaseNoise(pE);
    eiIntensityNoise: IntensityNoise(pE);
    eiCustom: CustomField(pE);
  end;
	// Center in Time
	pE.Center;
  pE.Norm;
end;

procedure TEFieldCals.CustomField(pE: TEField);
var
  DoDouble: boolean;
  tempE: TEField;
begin

  DoDouble := False;
  case mDomain of
		dtTime: begin
            GenTime(pE, 1);
					  pE.Transform;
					  GenFreq(pE, 1);
					  pE.InverseTransform;
            if not (mTempHeight2 = 0.0) then DoDouble := True;
            end;
		dtFreq: begin
            GenFreq(pE, 1);
					  pE.InverseTransform;
					  GenTime(pE, 1);
            if not (mSpecHeight2 = 0.0) then DoDouble := True;
            end;
  end;

	// Double pulse
	if DoDouble then
  begin
    tempE := TEField.Create(mN);
    try
		  case mDomain of
			  dtTime: begin
                GenTime(tempE, 2);
				        tempE.Transform;
				        GenFreq(tempE, 2);
				        tempE.InverseTransform;
                end;
			  dtFreq: begin
                GenFreq(tempE, 2);
				        tempE.InverseTransform;
				        GenTime(tempE, 2);
                end;
		  end;

		  // Add coherently
      pE.Add(tempE);
    finally
      tempE.Free;
    end;
	end;

	// center in freq
  // Maybe this should only happen for pixel units, not real??
	pE.Transform;
	pE.Center;
	pE.InverseTransform;
end;

// This function adds the temporal distortion to a generated pulse.
procedure TEFieldCals.GenTime(pE: TEField; pPulseNum: integer);
var
	t: integer;
	t0, maxint, B2, a, r: double;
	width, chirp, Q, TCP, phi, B, D: double;
  amp, phase: double;
  re, im, cs, sn, arg: double;
begin

	// Normalize for real units here
	r := mDelT;
	if mUnits = unPixel then r := 1.0;

  case pPulseNum of
    1: begin
       width := mTempWidth;
       chirp := mTempChirp;
       Q := mSPM;
       TCP := mTCP;
       D := 0.0;
       phi := 0.0;
       B := 1.0;
       end;
    2: begin
       width := mTempWidth2;
       chirp := mTempChirp2;
       Q := mSPM2;
       TCP := mTCP2;
       D := mTempSep2;
       phi := mTempPhase2;
       B := mTempHeight2;
       end;
  end;
	t0 := mN div 2 + D/r;
	B2 := sqrt(B);

	// If this is the ruling domain, generate original pulse shape
	if mDomain = dtTime then
  begin
		case mTempShape of
			fsGaussian:
      begin
				a := 2.0*Ln(2.0)/(width*width);
				for t := 0 to mN - 1 do
        begin
					amp := B2*exp(-a*r*r*(t - t0)*(t - t0));
					phase := chirp*r*r*(t-t0)*(t-t0);
					pE.Re^[t] := amp*cos(phase);
					pE.Im^[t] := amp*sin(phase);
				end;
      end;
			fsSech:
      begin
				for t:= 0 to mN - 1 do
        begin
					arg := 1.762747174*r*(t - t0)/width;
					amp := B2/cosh(arg);
					phase := chirp*r*r*(t-t0)*(t-t0);
					pE.Re^[t] := amp*cos(phase);
					pE.Im^[t] := amp*sin(phase);
				end;
      end;
			fsSuperGaussian:
      begin
				a := 8.0*Ln(2.0)/Power(width, 4.0);
				for t := 0 to mN - 1 do
        begin
					amp := B2*exp(-a*Power(r*(t - t0), 4.0));
					phase := chirp*r*r*(t-t0)*(t-t0);
					pE.Re^[t] := amp*cos(phase);
					pE.Im^[t] := amp*sin(phase);
				end;
      end;
		end;
	end;

	// Set up intensity and find maximum
  pE.SetIntensityAndPhase;
  maxint := pE.IntenMax;

	// Add the temporal phase distortions
	for t := 0 to mN - 1 do
  begin
		re := pE.Re^[t]; im := pE.Im^[t];
		arg := TCP*Power(1.0*r*(t-t0), 3.0) + Q*pE.Intensity^[t]/maxint + phi;
		cs := cos(arg); sn := sin(arg);
		pE.Re^[t] := re*cs - im*sn; pE.Im^[t] := re*sn + im*cs;
	end;

end;

procedure TEFieldCals.GenFreq(pE: TEField; pPulseNum: integer);
var
	f: integer;
	f0, width, chirp, SCP, SQP, D, B, phi, B2, a, r: double;
  amp, phase, arg, re, im, cs, sn, norm: double;
begin

	// Normalize for real units here
	if mUnits = unPixel then
  begin
    r := 1.0;
    norm := 1.0;
  end
  else
  begin
	  r := (2.0*Pi)/(mN*mDelT);   // Angular freq
    norm := 2.0*Pi*300.0/(mLam0*mLam0);
  end;

  case pPulseNum of
    1: begin
       width := mSpecWidth*norm;
       chirp := mSpecChirp;
       SCP := mSCP;
       SQP := mSQP;
       D := 0.0;
       phi := 0.0;
       B := 1.0;
       end;
    2: begin
       width := mSpecWidth2*norm;
       chirp := mSpecChirp2;
       SCP := mSCP2;
       SQP := mSQP2;
       D := mSpecSep2*norm;
       phi := mSpecPhase2;
       B := mSpecHeight2;
       end;
  end;

	f0 := N div 2 + D/r;
	B2 := sqrt(B);

	// If this is the ruling domain, generate original pulse shape
	if mDomain = dtFreq then
  begin
		case mSpecShape of
			fsGaussian:
      begin
				a := 2.0*Ln(2.0)/(width*width);
				for f := 0 to mN - 1 do
        begin
					amp := B2*exp(-a*r*r*(f - f0)*(f - f0));
					phase := chirp*r*r*(f-f0)*(f-f0);
					pE.Re^[f] := amp*cos(phase);
					pE.Im^[f] := amp*sin(phase);
				end;
      end;
			fsSech:
      begin
				for f := 0 to mN - 1 do
        begin
					arg := 1.762747174*r*(f - f0)/width;
					amp := B2/cosh(arg);
					phase := chirp*r*r*(f-f0)*(f-f0);
					pE.Re^[f] := amp*cos(phase);
					pE.Im^[f] := amp*sin(phase);
				end;
      end;
			fsSuperGaussian:
      begin
				a := 8.0*Ln(2.0)/Power(width, 4.0);
				for f := 0 to mN - 1 do
        begin
					amp := B2*exp(-a*Power(r*(f - f0), 4.0));
					phase := chirp*r*r*(f-f0)*(f-f0);
					pE.Re^[f] := amp*cos(phase);
					pE.Im^[f] := amp*sin(phase);
				end;
      end;
		end;
	end;


	// Add the spectral phase distortions
	for f := 0 to mN - 1 do
  begin
		re := pE.Re^[f]; im := pE.Im^[f];
		arg := SCP*Power(r*(f-f0), 3.0) + SQP*Power(r*(f-f0), 4.0) + phi;
		cs := cos(arg); sn := sin(arg);
		pE.Re^[f] := re*cs - im*sn; pE.Im^[f] := re*sn + im*cs;
	end;
end;

procedure TEFieldCals.AllNoise(pE: TEField);
var
  t: integer;
begin
  Randomize;
  for t := 0 to pE.N - 1 do
  begin
    pE.Re^[t] := Random;
    pE.Im^[t] := Random;
  end;
end;

procedure TEFieldCals.PhaseNoise(pE: TEField);
var
  t, N2: integer;
  inten, phaseval, a: double;
begin
  Randomize;
  N2 := pE.N div 2;
  a := 4*Ln(2.0)/pE.N;  // This makes an intensity width of sqrt(N)
  for t := 0 to pE.N - 1 do
  begin
    inten := exp(-a*(t - N2)*(t - N2));
    phaseval := Random;
    pE.Re^[t] := inten*cos(phaseval);
    pE.Im^[t] := inten*sin(phaseval);
  end;
end;

procedure TEFieldCals.IntensityNoise(pE: TEField);
var
  t: integer;
begin
  Randomize;
  for t := 0 to pE.N - 1 do
  begin
    pE.Re^[t] := Random;
    pE.Im^[t] := 0.0;
  end;
end;

procedure TEFieldCals.InitLog(pLog: TStringList);
begin
      begin
        pLog.Add('Theoretical Run.');
        with pLog do
        begin
          if Units = unReal then
          begin
            Add('Real units, Time increment is ' + FloatToStr(DelT) + ' fs.');
            Add('Center wavelength is ' + FloatToStr(Lam0) + ' nm.');
          end
          else
            Add('Pixel units, Time increment equivalent to 1 fs.');
          if Domain = dtTime then
          begin
            Add('Pulse Shape: ' + ShapeName);
            Add('Width: ' + FloatToStr(TempWidth) + ' fs.');
            Add('Chirp: ' + FloatToStr(TempChirp) + ' fs^-2.');
            Add('SPM: ' + FloatToStr(SPM) + ' rad.');
            Add('TCP: ' + FloatToStr(TCP) + ' fs^-3.');
            Add('SCP: ' + FloatToStr(SCP) + ' fs^3.');
            Add('SQP: ' + FloatToStr(SQP) + ' fs^4.');
            if not (TempHeight2 = 0) then
            begin
              Add(' ');
              Add('Second pulse parameters:');
              Add('Height: ' + FloatToStr(TempHeight2));
              Add('Width: ' + FloatToStr(TempWidth2) + ' fs.');
              Add('Chirp: ' + FloatToStr(TempChirp2) + ' fs^-2.');
              Add('SPM: ' + FloatToStr(SPM2) + ' rad.');
              Add('TCP: ' + FloatToStr(TCP2) + ' fs^-3.');
              Add('SCP: ' + FloatToStr(SCP2) + ' fs^3.');
              Add('SQP: ' + FloatToStr(SQP2) + ' fs^4.');
            end;
          end;
          if Domain = dtFreq then
          begin
            Add('Pulse Shape: ' + ShapeName);
            Add('Width: ' + FloatToStr(SpecWidth) + ' nm.');
            Add('Chirp: ' + FloatToStr(SpecChirp) + ' fs^2.');
            Add('SCP: ' + FloatToStr(SCP) + ' fs^3.');
            Add('SQP: ' + FloatToStr(SQP) + ' fs^4.');
            Add('SPM: ' + FloatToStr(SPM) + ' rad.');
            Add('TCP: ' + FloatToStr(TCP) + ' fs^-3.');
            if not (SpecHeight2 = 0) then
            begin
              Add(' ');
              Add('Second pulse parameters:');
              Add('Height: ' + FloatToStr(SpecHeight2));
              Add('Width: ' + FloatToStr(SpecWidth2) + ' nm.');
              Add('Chirp: ' + FloatToStr(SpecChirp2) + ' fs^2.');
              Add('SCP: ' + FloatToStr(SCP2) + ' fs^3.');
              Add('SQP: ' + FloatToStr(SQP2) + ' fs^4.');
              Add('SPM: ' + FloatToStr(SPM2) + ' rad.');
              Add('TCP: ' + FloatToStr(TCP2) + ' fs^-3.');
            end;
          end;
        end;
      end;
end;

end.
