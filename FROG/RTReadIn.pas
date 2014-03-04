unit RTReadIn;

interface

uses RunTypeU;

type
  TRTReadin = class(TRunType)
    public
      function GetEnum: TRunTypeEnum; override;
      function HasInitialField: boolean; override;
  end;

implementation

function TRTReadIn.GetEnum: TRunTypeEnum;
begin
     GetEnum := rteReadIn;
end;

function TRTReadIn.HasInitialField: boolean;
begin
     HasInitialField := True;
end;

end.
