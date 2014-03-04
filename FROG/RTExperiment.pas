unit RTExperiment;

interface

uses RunTypeU;

type
  TRTExperiment = class(TRunType)
    public
      function GetEnum: TRunTypeEnum; override;
      function HasInitialField: boolean; override;
    end;

implementation

function TRTExperiment.GetEnum: TRunTypeEnum;
begin
     GetEnum := rteExperimental;
end;

function TRTExperiment.HasInitialField: boolean;
begin
     HasInitialField := False;
end;

end.
