unit FROGColorTable;

interface

uses FrogObj, Graphics;

type
	TFROGColorTable = class(TFrogObject)
  private
    procedure GenerateColorTable; // Makes the color table
  public
  	ColorTable: array[1 .. 100] of TColor;   // The FROG color table
 		constructor Create;
  end;

implementation

uses Math;

constructor TFROGColorTable.Create;
begin
	inherited Create;
  GenerateColorTable;
end;

procedure TFROGColorTable.GenerateColorTable;
var
   i: integer;
   Step1, Step2, Step3: integer;
   red, green, blue: double;
begin
     { Initialize Custom FROG Color Table! }
     {Memo1.Text := 'Starting FROG Color Table';}
     Step1 := 15;
     Step2 := 50;
     Step3 := 95;
     green := 0; red := 0; blue := 0;
     for i := 1 to 100 do
     begin
			{ A smaller exponent means a faster rise, slower fall }
			if(i <= Step1) then
     begin
				red := 1.0;
				green := (1.0*i)/Step1;
				if(green <> 0.0) then green := Power(green, 0.7);
				blue := 0.0;
			end
     else if(i > Step1) and (i <= Step2) then
     begin
				red := (Step2 - i)/(Step2 - Step1);
				if(red <> 0.0) then red := power(red, 0.7);
				green := 1.0;
				blue := 0.0;
			end
     else if(i > Step2) and (i <= Step3) then
     begin
				red := 0.0;
				green := (Step3 - i)/(Step3 - Step2);
				blue := (i - Step2)/(Step3 - Step2);
				if(green <> 0.0) then green := power(green, 0.8);
				if (blue <> 0.0) then blue := power(blue, 0.8);
			end
			else if(i > Step3) then
			begin
				red := (i - Step3)/(100 - Step3);
       green := red;
				red := power(red, 1.0);
       green := power(green, 1.0);
				blue := 1.0;
			end;
     ColorTable[i] :=  Trunc(255*red);
     ColorTable[i] := ColorTable[i] + 256*Trunc(255*green);
     ColorTable[i] := ColorTable[i] + 256*256*Trunc(255*blue);
     ColorTable[i] := ColorTable[i] + 256*256*256*0;
     end;
end;

end.
