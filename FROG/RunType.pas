unit RunType;

interface

uses FrogObj;

type
    TRunTypeEnum = (rteTheory, rteExperimental, rteReadIn);
    TRunType = class (TFrogObject)
       public
         function GetEnum: TRunTypeEnum; virtual; abstract;
    end;


implementation

end.
