unit TimeFreq;

interface

uses FrogObj;

type
	TTimeFreqCalibration = class(TFrogObject)
  	private
    	 mN: integer;
      mN2: integer;
      mDelT: double;
      mLam0: double;
      mDelF: double;
      mF0: double;
			 function CalcDelF: double;
      function CalcDelLam: double;
      private constructor Create;
    public
	 		 function GetTAt(i: double): double;
			 function GetFAt(i: double): double;
			 function GetLamAt(i: double): double;
      function GetIForF(f: double): double;
      property N: integer read mN;
      property DelT: double read mDelT;
      property Lam0: double read mLam0;
      property DelF: double read mDelF;
      property DelLam: double read CalcDelLam;
    	 constructor CreateFromDelT(pDelT, pLam0: double; pN: integer);
	end;

implementation

// Don't use an uninitialized one.
constructor TTimeFreqCalibration.Create;
begin
end;

constructor TTimeFreqCalibration.CreateFromDelT(pDelT, pLam0: double; pN: integer);
begin
	 mN := pN;
  mDelT := pDelT;
  mLam0 := pLam0;

  // temps
  mN2 := mN div 2;
  mDelF := CalcDelF;
  mF0 := 300.0/mLam0;
end;

function TTimeFreqCalibration.CalcDelF: double;
begin
  CalcDelF := 1.0/(mN*mDelT);
end;

function TTimeFreqCalibration.CalcDelLam: double;
begin
  CalcDelLam := -mLam0*mLam0/(300.0*mN*mDelT);
end;

function TTimeFreqCalibration.GetTAt(i: double): double;
begin
	GetTAt := (i - mN2)*mDelT;
end;

function TTimeFreqCalibration.GetFAt(i: double): double;
begin
  GetFAt := mF0 + (i - mN2)*DelF;
end;

function TTimeFreqCalibration.GetLamAt(i: double): double;
begin
  GetLamAt := 300.0/GetFAt(i);
end;

function TTimeFreqCalibration.GetIForF(f: double): double;
begin
     GetIForF := (f - mF0)/DelF + mN2;
end;

end.
