unit RunTypeU;

interface

uses FrogObj, EFieldCals;


// This was kind of a bust.  I wanted to get rid of all the big case statements
// in FrogAlgo, but there's so much data that's there.  I could make callbacks
// with method pointers, I guess, but it's so much work. . .
// We'd add method pointer instance variables to this class, and then in
// FrogAlgo we'd have to assign the pointers to point at methods back in FrogAlgo.
// But since the RunTypes are singletons, we'd have to reassign them in XFROG
// as well.  Yuk. . .I wish I'd realized this before I wrote all these classes.
type
    TRunTypeEnum = (rteTheory, rteExperimental, rteReadIn);
    TRunType = class (TFrogObject)
       public
         function GetEnum: TRunTypeEnum; virtual; abstract;
         function HasInitialField: boolean; virtual; abstract;
    end;


implementation

end.
