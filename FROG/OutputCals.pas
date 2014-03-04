unit OutputCals;

interface

uses FrogObj, Func1D, FrogTrace, NLO;

type
  TOutputFormat = (ofClassic, ofFemtosoft);

  TOutputCals = class(TFrogObject)
    private
      mFilePath: string;
      mPhaseBlank: double;
      mDisplayGraphics: boolean;  // controls whether graphics are displayed or not.
      mOverwrite: boolean;
      mOutputFormat: TOutputFormat;
    public
      property FilePath: string read mFilePath write mFilePath;
      property PhaseBlank: double read mPhaseBlank write mPhaseBlank;
      property DisplayGraphics: boolean read mDisplayGraphics write mDisplayGraphics;
      property Overwrite: boolean read mOverwrite write mOverwrite;
      property OutputFormat: TOutputFormat read mOutputFormat write mOutputFormat;
      procedure SaveEField(pE: TEField; pFileName: string);
      procedure SaveSpectrum(pE: TEField; pFileName: string; pPixelUnits: Boolean);
      procedure SaveAutocorrelation(pE: TEField; pFileName: string);
      procedure SaveFrogTrace(pFrog: TFrogTrace; pE: TEField; pFileName: string; pPixelUnits: Boolean; pNLOType: TNLOType);
      procedure SaveMarginals(pFrogI, pFrogIk: TFrogTrace; pE: TEField; pFileName: string; pPixelUnits: Boolean; pNLOType: TNLOType);
      procedure SaveExperimentalMarginal(pMarg: TMarginal; pFileName: string);
      procedure SaveFormat;
      constructor Create;
  end;

implementation

uses Dialogs, SysUtils, WindowMgr;

constructor TOutputCals.Create;
var
   myWinMgr: TWindowManager;
   format: integer;
begin
  mPhaseBlank := 1e-4;
  mDisplayGraphics := True;
  mOverwrite := True;
  myWinMgr := TWindowManager.Create;
  format := myWinMgr.GetOutputFormat;
  myWinMgr.Free;
  if format = Ord(ofClassic) then
     mOutputFormat := ofClassic
  else if format = Ord(ofFemtosoft) then
     mOutputFormat := ofFemtosoft;
end;

procedure TOutputCals.SaveFormat;
var
   myWinMgr: TWindowManager;
begin
     myWinMgr := TWindowManager.Create;
     myWinMgr.SaveOutputFormat(Ord(mOutputFormat));
     myWinMgr.Free;
end;

procedure TOutputCals.SaveEField(pE: TEField; pFileName: string);
var
  t, N: integer;
  F: textfile;
  tempE: TEField;
  theFileName, s: string;
begin
  // Save an EField to Disk
  tempE := TEField.Create(pE.N);

  try
  tempE.CopyFrom(pE);
  tempE.ExpandT;
  tempE.Norm;
  tempE.ClipPhase(PhaseBlank);

  N := tempE.N;
  theFileName := mFilePath + '\' + pFileName;
  AssignFile(F, theFileName);

  try
    ReWrite(F);
  except
    ShowMessage('Error opening file ' + theFileName);
    Exit;
  end;

  try
    try
      for t := 0 to N - 1 do
      begin
        s := FloatToStrF((t - (N div 2))*tempE.DelT, ffGeneral, 7, 7) + '  ' +
            FloatToStrF(tempE.Intensity^[t], ffGeneral, 7, 7) + '  ' +
            FloatToStrF(tempE.Phase^[t], ffGeneral, 7, 7) + '  ' +
            FloatToStrF(tempE.Re^[t], ffGeneral, 7, 7) + '  ' +
            FloatToStrF(tempE.Im^[t], ffGeneral, 7, 7);
        Writeln(F, s);
      end;
    finally
      CloseFile(F);
    end;
  except
    ShowMessage('Error writing to file ' + theFileName);
  end;

  finally
    tempE.Free;
  end;
end;

const
  SMALL_F = 1e-10;

procedure TOutputCals.SaveSpectrum(pE: TEField; pFileName: string; pPixelUnits: Boolean);
var
  t, N, N2: integer;
  F: textfile;
  tempE: TEField;
  theFileName, s: string;
  ff, f0, lam: double;
begin
  // Save an EField to Disk
  tempE := TEField.Create(pE.N);

  try
  tempE.CopyFrom(pE);
  tempE.Transform;
  tempE.ExpandS;

  if pPixelUnits then
    tempE.Norm
  else
    tempE.ScaleForNonlinearWavelength;
    // No Norm, it calls SetIntensityAndPhase

  tempE.ClipPhase(PhaseBlank);
  N := tempE.N;
  theFileName := mFilePath + '\' + pFileName;
  AssignFile(F, theFileName);

  try
    ReWrite(F);
  except
    ShowMessage('Error opening file ' + theFileName);
    Exit;
  end;

  f0 := 300.0/tempE.Lam0;
  N2 := N div 2;
  try
    try
      for t := 0 to N - 1 do
      begin
        if pPixelUnits then
          // Save it in pixel order. . .
          lam := -(t - N2)*tempE.DelLam
        else
        begin
          ff := f0 + tempE.DelF*(t - N2);
          if ff = 0.0 then ff := SMALL_F;
          lam := 300.0/ff;
        end;

        s := FloatToStrF(lam, ffGeneral, 7, 7) + '  ' +
            FloatToStrF(tempE.Intensity^[t], ffGeneral, 7, 7) + '  ' +
            FloatToStrF(tempE.Phase^[t], ffGeneral, 7, 7) + '  ' +
            FloatToStrF(tempE.Re^[t], ffGeneral, 7, 7) + '  ' +
            FloatToStrF(tempE.Im^[t], ffGeneral, 7, 7);
        Writeln(F, s);
      end;
    finally
      CloseFile(F);
    end;
  except
    ShowMessage('Error writing to file ' + theFileName);
  end;

  finally
    tempE.Free;
  end;
end;

procedure TOutputCals.SaveAutocorrelation(pE: TEField; pFileName: string);
var
  t, N: integer;
  F: textfile;
  tempE: TEField;
  tempAuto: TAutocorrelation;
  theFileName, s: string;
begin
  // Save an EField to Disk
  tempE := TEField.Create(pE.N);
  tempAuto := nil;

  try
  tempE.CopyFrom(pE);
  tempE.ExpandT;
  tempE.SetIntensityAndPhase;
  tempAuto := TAutocorrelation.Create(tempE.N);
  tempAuto.CreateAutocorrelationFrom(tempE);
  tempAuto.Norm;

  N := tempAuto.N;
  theFileName := mFilePath + '\' + pFileName;
  AssignFile(F, theFileName);

  try
    ReWrite(F);
  except
    ShowMessage('Error opening file ' + theFileName);
    Exit;
  end;

  try
    try
      for t := 0 to N - 1 do
      begin
        s := FloatToStrF((t - (N div 2))*tempAuto.DelT, ffGeneral, 7, 7) + '  ' +
            FloatToStrF(tempAuto.Intensity^[t], ffGeneral, 7, 7);
        Writeln(F, s);
      end;
    finally
      CloseFile(F);
    end;
  except
    ShowMessage('Error writing to file ' + theFileName);
  end;

  finally
    tempAuto.Free;
    tempE.Free;
  end;
end;

procedure TOutputCals.SaveFrogTrace(pFrog: TFrogTrace; pE: TEField;
        pFileName: string; pPixelUnits: Boolean; pNLOType: TNLOType);
var
  N, tau, w: integer;
  theFileName, s: string;
  F: TextFile;
  f0, Lam0, fr, DelF, DelT, DelLam, temp, newLam0, newDelLam: double;
begin

  N := pE.N;
  theFileName := mFilePath + '\' + pFileName;
  AssignFile(F, theFileName);
  DelF := pFrog.DelF;
  Lam0 := pFrog.Lam0;
  DelT := pFrog.DelT;
  DelLam := pFrog.DelLam;

  try
    ReWrite(F);
  except
    ShowMessage('Error opening file ' + theFileName);
    Exit;
  end;

  try
    try
      // The header - common
      s := IntToStr(pFrog.N) + '  ' + IntToStr(pFrog.N);
      WriteLn(F, s);

      if mOutputFormat = ofClassic then
      begin
           // The max value
            s := '0.0  ' + FloatToStrF(pFrog.Max, ffGeneral, 7, 7);
            WriteLn(F, s);

            // Wavelength Vals
  	         if pPixelUnits then
            begin
                 for w := 0 to N - 1 do
                 begin
                      s := FloatToStrF(-DelF*(w - (N div 2) + 1), ffGeneral, 7, 7);
                      WriteLn(F, s);
                 end;
            end
            else
            begin
                 f0 := 300.0/Lam0;
                 for w := 0 to N - 1 do
                 begin
			                 fr := (f0 - (w-(N div 2)+1)*DelF);
                      if fr = 0.0 then fr := SMALL_F;
                      s := FloatToStrF(300.0/fr, ffGeneral, 7, 7);
                      WriteLn(F, s );
                 end;
            end;

    	       // Columns: delay
	           for tau := 0 to N - 1 do
            begin
                 s := FloatToStrF(DelT*(tau - N div 2), ffGeneral, 7, 7);
                 WriteLn(F, s);
            end;
      end // of header
      else if mOutputFormat = ofFemtosoft then
      begin
           // I think we don't ever want to output negative time delays
           s := FloatToStrF(Abs(DelT), ffGeneral, 7, 7) + ' ';
           Write(F, s);
           // We could have left DelLam +ve and read in below as tau*N + (N - 1 - w), but then it's flipped.
           // There are two options here:
           // Option 1
           //    Use (N - 1 - w) below, use Lam0 and +DelLam.
           //    This leads to perfect read in but it's flipped in the RawData window
           // Option 2
           //    Use w below, and then use newLam0 and +newDelLam.
           //    This leads to the center being at N/2 -1, necessitating a shift, with some
           //     subtle round-off errors.  The result comes out with a slightly shifted center wavl.
           //temp := 300.0/Lam0 - DelF;
           //if not (temp = 0.0) then newLam0 := 300.0/temp else newLam0 := Lam0;
           //newDelLam := newLam0*newLam0*DelF/300.0;
           s := FloatToStrF(DelLam, ffGeneral, 7, 7) + ' ';
           Write(F, s);
           s := FloatToStrF(Lam0, ffGeneral, 7, 7) + ' ';
           WriteLn(F, s);
      end;

	    // The data
      for w := N - 1 downto 0 do
        for tau := 0 to N - 1 do
        begin
          if mOutputFormat = ofClassic then  // do it the same was as always
          begin
               s := FloatToStrF(pFrog.Vals^[tau*N + w], ffGeneral, 7, 7);
               WriteLn(F, s);
          end
          else if mOutputFormat = ofFemtosoft then  // write it as an array
          begin
               // Note that the loop is w = N - 1 downto 0
               s := FloatToStrF(pFrog.Vals^[tau*N + (N - 1 - w)], ffGeneral, 7, 7);
               if tau = (N - 1) then  // slap a CR/LF on it.
                  WriteLn(F, s)
               else
                   Write(F, s + ' ');
          end
        end;
    finally
      CloseFile(F);
    end;
  except
    ShowMessage('Error writing to file ' + theFileName);
  end;
end;

procedure TOutputCals.SaveMarginals(pFrogI, pFrogIk: TFrogTrace; pE: TEField;
                pFileName: string; pPixelUnits: Boolean; pNLOType: TNLOType);
var
  t, N, N2: integer;
  F: textfile;
  ZOFMo, ZODMo, ZOFMr, ZODMr: TMarginal;
  theFileName, s: string;
  ff, f0, tt, ww, Lam0: double;
begin

  ZOFMo := pFrogI.ZeroOrderFreqMarginal;
  ZODMo := pFrogI.ZeroOrderDelayMarginal;
  ZOFMr := pFrogIk.ZeroOrderFreqMarginal;
  ZODMr := pFrogIk.ZeroOrderDelayMarginal;

  try

  ZODMo.Norm;
  ZODMr.Norm;
  if pPixelUnits then
  begin
    ZOFMo.Norm;
    ZOFMr.Norm;
  end
  else
  begin
    ZOFMo.ScaleForNonlinearWavelength;
    ZOFMr.ScaleForNonlinearWavelength;
  end;

  N := pE.N;
  theFileName := mFilePath + '\' + pFileName;
  AssignFile(F, theFileName);

  try
    ReWrite(F);
  except
    ShowMessage('Error opening file ' + theFileName);
    Exit;
  end;

  Lam0 := pFrogI.Lam0;
  f0 := 300.0/Lam0;
  N2 := N div 2;
  try
    try
      for t := 0 to N - 1 do
      begin
        tt := (t - N2)*pE.DelT;
        if pPixelUnits then
          ww := -(t - N2)*pE.DelLam
        else
        begin
          ff := f0 + pE.DelF*(t - N2);
          if ff = 0.0 then ff := SMALL_F;
          ww := 300.0/ff;
        end;

        s := FloatToStrF(tt, ffGeneral, 7, 7) + '  ' +
            FloatToStrF(ZODMo.Intensity^[t], ffGeneral, 7, 7) + '  ' +
            FloatToStrF(ZODMr.Intensity^[t], ffGeneral, 7, 7) + '  ' +
            FloatToStrF(ZOFMo.Intensity^[t], ffGeneral, 7, 7) + '  ' +
            FloatToStrF(ZOFMr.Intensity^[t], ffGeneral, 7, 7) + '  ' +
            FloatToStrF(ww, ffGeneral, 7, 7);
        Writeln(F, s);
      end;
    finally
      CloseFile(F);
    end;
  except
    ShowMessage('Error writing to file ' + theFileName);
  end;

  finally
    ZODMr.Free;
    ZOFMr.Free;
    ZODMo.Free;
    ZOFMo.Free;
  end;
end;

procedure TOutputCals.SaveExperimentalMarginal(pMarg: TMarginal; pFileName: string);
var
  w, N: integer;
  F: textfile;
  theFileName, s: string;
  lam, ff, f0: double;
begin
  pMarg.Norm;
  N := pMarg.N;
  theFileName := mFilePath + '\' + pFileName;
  AssignFile(F, theFileName);

  try
    ReWrite(F);
  except
    ShowMessage('Error opening file ' + theFileName);
    Exit;
  end;

  // The FROG marginal is rescaled back to constant wavelength for output (see
  // above routine), so let's do it here too!
  pMarg.ScaleForNonlinearWavelength;

  try
    try
      f0 := 300.0/pMarg.Lam0;
      for w := 0 to N - 1 do
      begin
        ff := f0 + pMarg.DelF*(w - N div 2);
        if ff = 0.0 then ff := SMALL_F;
        lam := 300.0/ff;
        s := FloatToStrF(lam, ffGeneral, 7, 7) + '  ' +
            FloatToStrF(pMarg.Intensity^[w], ffGeneral, 7, 7);
        Writeln(F, s);
      end;
    finally
      CloseFile(F);
    end;
  except
    ShowMessage('Error writing to file ' + theFileName);
  end;
end;

end.
