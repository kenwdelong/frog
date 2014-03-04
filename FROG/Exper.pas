unit Exper;

interface

uses FrogObj, FrogTrace, Numerics, NLO, Classes, Windows, RawData;

type
  TDataOrder = (doWavelength, doDelay);
  TBinning = (bnWavelength, bnFrequency);
  TWavelengthCentering = (wcCenterOfMass, wcPeak);

  TExper = class(TFrogObject)
    private
      mNTau: integer;
      mNLam: integer;
      mDataOrder: TDataOrder;
      mUseHeader: Boolean;
      mFileName: string;
      mNullFile: string;
      mDelT: double;
      mDelLam: double;
      mLam0: double;
      mModDelT: double;
      mModLam0: double;
      mRawData: TRawData;
      mLog: TStringList;
      mBinning: TBinning;   // doWavl means read in as constant wavelength
      mWavelengthCentering: TWavelengthCentering;
      function GetLam1: double;
      procedure SetLam1(pLam1: double);
      procedure ChangeUseHeader(pUseHeader: Boolean);
      procedure ReadHeaders(var pF: TextFile);
      procedure FetchHeaders;
      procedure ReadDataFromFile;
      function GetFileName: string;
    public
      procedure LoadOriginalFrogTrace(pFrogI: TFrogTrace; pNLO: TNLOInteraction);
      procedure InitLog(pLog: TStringList);
      property NTau: integer read mNTau write mNTau;
      property NLam: integer read mNLam write mNLam;
      property DataOrder: TDataOrder read mDataOrder write mDataOrder;
      property UseHeader: Boolean read mUseHeader write ChangeUseHeader;
      property ExperFile: string read mFileName write mFileName;
      property FileName: string read GetFileName;
      property DelT: double read mDelT write mDelT;
      property DelLam: double read mDelLam write mDelLam;
      property Lam0: double read mLam0 write mLam0;
      property Lam1: double read GetLam1 write SetLam1;
      property ModLam0: double read mModLam0;
      property ModDelT: double Read mModDelT;
      property NullFile: string read mNullFile;
      property RawData: TRawData read mRawData;
      property Binning: TBinning read mBinning write mBinning;
      property WavelengthCentering: TWavelengthCentering read mWavelengthCentering write mWavelengthCentering;
      constructor Create;
      destructor Destroy; override;
  end;

implementation


uses Dialogs, SysUtils, RawImage, Forms, WindowMgr, Controls;

constructor TExper.Create;
begin
  inherited Create;
  mNTau := 0;
  mNLam := 0;
  mDelT := 0;
  mDelLam := 0;
  mLam0 := 0;
  mNullFile := 'None Selected';
  mFileName := mNullFile;
  mDataOrder := doWavelength;
  mBinning := bnWavelength;
  mWavelengthCentering := wcCenterOfMass;
  mLog := TStringList.Create;
end;

destructor TExper.Destroy;
begin
  mLog.Free;
  inherited;
end;

procedure TExper.LoadOriginalFrogTrace(pFrogI: TFrogTrace; pNLO: TNLOInteraction);
var
  centerWavelengthByPeak: boolean;
begin
  mLog.Clear;
  centerWavelengthByPeak := (WavelengthCentering = wcPeak);
  mRawData := TRawData.Create(mNTau, mNLam, mDelT, mDelLam, mLam0, pNLO, centerWavelengthByPeak);
  ReadDataFromFile;   // Puts data into mRawData
  // We should probably add the "reset log" fucntions here, for this class
  // and for RawData.
  frmRaw := TfrmRaw.Create(Application);
  WindowManager.InitializeThisWindow(frmRaw);
  try
    frmRaw.Rawdata := mRawData;
    frmRaw.ShowModal;
    if (frmRaw.ModalResult = mrCancel) then
    	raise Exception.Create('Experimental Retrieval Cancelled');
    mRawData.GridData(pFrogI, (mBinning = bnWavelength));
    mModLam0 := mRawData.Lam0*pNLO.SpectrumMultiplier;
    mModDelT := Abs(mRawData.DelT);  // We don't want negative DelT in the EFields
    mRawData.InitLog(mLog);
  finally
    mRawData.Free;
    mRawData := nil;
    frmRaw.RawData := nil;
    WindowManager.SaveAsDefaults(frmRaw);
    frmRaw.Free;
  end;
end;

procedure TExper.SetLam1(pLam1: double);
begin
  mLam0 := pLam1 + mDelLam*(mNLam div 2)
end;

function TExper.GetLam1: double;
begin
  GetLam1 := mLam0 - mDelLam*(mNLam div 2)
end;

procedure TExper.ChangeUseHeader(pUseHeader: Boolean);
begin
  mUseHeader := pUseHeader;
  if pUseHeader then
    FetchHeaders
  else
  begin
    // Reset 'em
    mNTau := 0;
    mNLam := 0;
    mDelT := 0;
    mDelLam := 0;
    mLam0 := 0;
  end;
end;

procedure TExper.FetchHeaders;
var
  F: TextFile;
begin
  AssignFile(F, mFileName);
  try
    Reset(F);
  except
    ShowMessage('Error opening file ' + ExtractFileName(mFileName));
    Exit;
  end;

  try
    ReadHeaders(F);
  except
    ShowMessage('Error reading header in ' + ExtractFileName(mFileName));
  end;
  CloseFile(F);
end;

procedure TExper.ReadHeaders(var pF: TextFile);
begin
  Read(pF, mNtau);
  Read(pF, mNLam);
  Read(pF, mDelT);
  Read(pF, mDelLam);
  Read(pF, mLam0);
end;

procedure TExper.ReadDataFromFile;
var
  F: TextFile;
  tau, w: integer;
  i, NTot: LongInt;
  junk: double;
  mssg, s: string;
begin
  AssignFile(F, mFileName);
  NTot := mNTau*mNLam;
  try
    Reset(F);
  except
    raise Exception.Create('Error opening file ' + ExtractFileName(mFileName));
  end;

  try
    // If there are headers, get rid of 'em
    if mUseHeader then
    begin
      Read(F, junk);
      Read(F, junk);
      Read(F, junk);
      Read(F, junk);
      Read(F, junk);
      // Took this out in v3.08a.  It caused trouble when the lam0 was followed
      //  by a space and then a linefeed.  Anyway, Read(F, double) consumes the
      //  preceeding space/linefeed anyway, this new version should work in all
      //  cases.
      // Gotta clear the linefeed
      //ReadLn(F, s);
      //This doesn't work either: ReadLn(F);
    end;

    // Read in the data
    i := 0;
    if mDataOrder = doWavelength then
    begin
	    while (not Eof(F)) and (i < NTot) do
  	  begin
 			  Read(F, mRawData.Vals^[i]);
         //mRawData.Vals^[i] := 255 - mRawData.Vals^[i] - 25;
    	  Inc(i);
  	  end;
    end
    else if mDataOrder = doDelay then
    begin
  	  for w := 0 to mNLam - 1 do
  		  for tau := 0 to mNTau - 1 do
        begin
      	  if Eof(F) then Break;
          Read(F, mRawData.Vals^[tau*mNLam + w]);
          Inc(i);
        end;
    end;

    // Ran out at too few points
    if i < NTot then
    begin
  	  w := (i-1) div mNLam;
  	  tau := i - w*mNLam - 1;
   	  mssg := 'Not enough data!  Ran out at wavelength ' + IntToStr(w) +
   					' and delay ' + IntToStr(tau) + '.';
 		  MessageDlg(mssg, mtWarning, [mbOK], 0);
      // raise exception
	  end;

    // Check to see that we are really at the end
 	  Read(F, junk);
	  if not Eof(F) then
   	  MessageDlg('Too much data!', mtWarning, [mbOK], 0);
  finally
    CloseFile(F);
  end;
end;

procedure TExper.InitLog(pLog: TStringList);
begin
  with pLog do
  begin
    Add('Experimental FROG trace loaded from ' + mFileName);
    Add('Number of delay points: ' + IntToStr(mNTau));
    Add('Number of wavelength points: ' + IntToStr(mNLam));
    Add('Delay spacing: ' + FloatToStrF(mDelT, ffGeneral, 4, 0) + ' fs');
    Add('Wavelength spacing: ' + FloatToStrF(mDelLam, ffGeneral, 4, 0) + ' nm');
    Add('Center wavelength: ' + FloatToStrF(mLam0, ffGeneral, 4, 0) + ' nm');
    case mDataOrder of
      doWavelength: Add('DataOrder: Wavelength');
      doDelay: Add('DataOrder: Delay');
    end;
    case mBinning of
      bnWavelength: Add('Read in as constant Wavelength bins');
      bnFrequency: Add('Read in as constant Frequency bins');
    end;
    Add(' ');
  end;
  pLog.AddStrings(mLog);
end;

function TExper.GetFileName: string;
begin
  GetFileName := ExtractFileName(mFileName);
end;

end.
