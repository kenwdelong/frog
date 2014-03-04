unit AlgoCals;

interface

uses FrogObj, NLO, EFieldCals, OutputCals, ReadIn, Exper, StratTypes,
  MargCals, RunTypeU, RunTypeFactory;

type
  TAlgoCals = class(TFrogObject)
    private
      mN: integer;
      mRunType: TRunType;
      mNLOType: TNLOType;
      mRunTypeFactory: TRunTypeFactory;
      function GetRunTypeEnum: TRunTypeEnum;
      procedure SetRunTypeEnum(pEnum: TRunTypeEnum);
    public
      property N: integer read mN write mN;
      property RunTypeEnum: TRunTypeEnum read GetRunTypeEnum write SetRunTypeEnum;
      property RunType: TRunType read mRunType;
      property NLOType: TNLOType read mNLOType write mNLOType;
      function NLOClass: TNLOClass;
      constructor Create;
      destructor Destroy; override;
  end;


implementation

uses PG, SHG, SD, THG, DFG1, DFG2, WindowMgr, Classes, SysUtils;                          

constructor TAlgoCals.Create;
var
   initCals: TStringList;
   rt, nt, i: Integer;

begin
  inherited Create;

  // These initial values should be kept in synch with the default
  // vals programmed into the GUI.  The only other way to do it is to refresh
  // all the Cal objects with the GUI vals at CalsForm.Create.
  //mN := 64;
  //mNLOType := nlPG;
  //mRunType := rtTheory;
  mRunTypeFactory := TRunTypeFactory.Create;

  initCals := WindowManager.LoadDefaultAlgoCals;
  if initCals = nil then
  begin
       mN := 64;
       mNLOType := nlSHG;
       mRunType := mRunTypeFactory.getRunTypeForEnum(rteTheory);
       Exit;
  end;
  mN := StrToInt(initCals.Strings[0]);

  rt := StrToInt(initCals.Strings[1]);
//  mRunType := rtTheory;
//  for i := 0 to rt - 1 do                  // A way to turn an int back into an ordinal.
//      mRunType := Succ(mRunType);
  mRunType := mRunTypeFactory.getRunTypeForInt(rt);

  nt := StrToInt(initCals.Strings[2]);
  mNLOType := nlPG;
  // Start with PG, then loop thru nt times to get to the "real" NLO.
  for i := 0 to nt - 1 do
      mNLOType := Succ(mNLOType);

  initCals.Free;

end;

function TAlgoCals.NLOClass: TNLOClass;
begin
  case mNLOType of
    nlPG: NLOClass := TPG;
    nlSHG: NLOClass := TSHG;
    nlSD: NLOClass := TSD;
    nlTHG: NLOCLass := TTHG;
    nlDFG1: NLOClass := TDFG1;
    nlDFG2: NLOClass := TDFG2;
  end;
end;

destructor TAlgoCals.Destroy;
var
   currentCals: TStringList;
   myWinMgr: TWindowManager;
begin
     // Save the current values in the Registry before exiting.
     // The global variable WindowManager is controlled by the Main Form, and
     // deleted on exit.  The Cals form is auto-created and destructed on the way
     // out, so its lifecycle is longer than MainForm.  So forget about trying
     // to synch it up, and let's just create one for here.
     myWinMgr := TWindowManager.Create;
     currentCals := TStringList.Create;
     currentCals.Add(IntToStr(mN));
     currentCals.Add(IntToStr(Ord(mRunType.GetEnum)));
     currentCals.Add(IntToStr(Ord(mNLOType)));
     myWinMgr.SaveDefaultAlgoCals(currentCals);
     myWinMgr.Free;
     inherited Destroy;
end;

function TAlgoCals.GetRunTypeEnum: TRunTypeEnum;
begin
     GetRunTypeEnum := mRunType.GetEnum;
end;

procedure TAlgoCals.SetRunTypeEnum(pEnum: TRunTypeEnum);
begin
     mRunType := mRunTypeFactory.getRunTypeForEnum(pEnum);
end;

end.
