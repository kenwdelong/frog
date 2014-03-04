unit RunTypeFactory;

interface

uses FrogObj, RunTypeU;

type
   TRunTypeFactory = class(TFrogObject)
     private
       mTheory: TRunType;
       mExperimental: TRunType;
       mReadIn: TRunType;
     public
       function getRunTypeForEnum(pRTEnum: TRunTypeEnum): TRunType;
       function getRunTypeForInt(pRTInt: Integer): TRunType;
       constructor Create;
       destructor Destroy; override;
     end;


var
   RRunTypeFactory: TRunTypeFactory;

implementation

uses
  RTTheory, RTExperiment, RTReadIn;

constructor TRunTypeFactory.Create;
begin
  mTheory := TRTTheory.Create;
  mExperimental := TRTExperiment.Create;
  mReadIn := TRTReadIn.Create;
end;

destructor TRunTypeFactory.Destroy;
begin
  mTheory.Free;
  mExperimental.Free;
  mReadIn.Free;
  inherited Destroy;
end;

function TRunTypeFactory.getRunTypeForEnum(pRTEnum: TRunTypeEnum): TRunType;
begin
  case pRTEnum of
    rteTheory:
      getRunTypeForEnum := mTheory;
    rteExperimental:
      getRunTypeForEnum := mExperimental;
    rteReadIn:
      getRunTypeForEnum := mReadIn;
  end;
end;

function TRunTypeFactory.getRunTypeForInt(pRTInt: Integer): TRunType;
begin
  case pRTInt of
    Ord(rteTheory):
      getRunTypeForInt := mTheory;
    Ord(rteExperimental):
      getRunTypeForInt := mExperimental;
    Ord(rteReadIn):
      getRunTypeForInt := mReadIn;
  end;
end;

end.
