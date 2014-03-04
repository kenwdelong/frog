unit RTTheory;

interface

uses RunTypeU;

type
  TRTTheory = class(TRunType)
    public
      function GetEnum: TRunTypeEnum; override;
      function HasInitialField: boolean; override;
  end;


implementation

function TRTTheory.GetEnum: TRunTypeEnum;
begin
     GetEnum := rteTheory;
end;

function TRTTheory.HasInitialField: boolean;
begin
     HasInitialField := True;
end;

end.
