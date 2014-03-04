unit Vanilla;

interface

uses
	Strategy, FrogObj;

type
	TVanillaBasedStrategy = class(TStrategy)
  	public
    	procedure Reset; override;
  end;

implementation

procedure TVanillaBasedStrategy.Reset;
begin
	// Do nothing
end;

end.
