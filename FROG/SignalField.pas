unit SignalField;

// Let's make the convention that the ordering is (tau, t) and (tau, w)
//   Use Re^[tau*N + t] etc.

interface

uses
	FrogObj, Numerics;

type
	TSignalField = class(TFrogObject)
  	private
    	mN: integer;
  	public
    	Re: PArray;
      Im: PArray;
      procedure TransformToFreq;
      procedure InverseTransformToTime;
      procedure CopyFrom(pESig: TSignalField);
      procedure AverageWith(pESig: TSignalField);
      property N: integer read mN;
      constructor Create(pN: integer);
      destructor Destroy; override;
  end;

implementation

constructor TSignalField.Create(pN: integer);
begin
	inherited Create;
  mN := pN;
  GetMem(Re, mN*mN*SizeOf(double));
  GetMem(Im, mN*mN*SizeOf(double));
end;

procedure TSignalField.TransformToFreq;
var
	t, tau: integer;
  xre, xim: PArray;
begin
  GetMem(xre, mN*SizeOf(Double));
  GetMem(xim, mN*SizeOf(double));
  try
  	for tau := 0 to mN - 1 do
    begin
    	for t := 0 to mN - 1 do
      begin
        xre^[t] := Re^[tau*mN + t];
        xim^[t] := Im^[tau*mN + t];
      end;
      TNumerics.FFT(xre, xim, mN, 1);
    	for t := 0 to mN - 1 do
      begin
      	Re^[tau*mN + t] := xre^[t];
        Im^[tau*mN + t] := xim^[t];
      end;
    end;
  finally
    FreeMem(xim);
    FreeMem(xre);
    xim := nil;
    xre := nil;
  end;
end;

procedure TSignalField.InverseTransformToTime;
var
	t, tau: integer;
  xre, xim: PArray;
begin
  GetMem(xre, mN*SizeOf(Double));
  GetMem(xim, mN*SizeOf(double));
  try
  	for tau := 0 to mN - 1 do
    begin
    	for t := 0 to mN - 1 do
      begin
        xre^[t] := Re^[tau*mN + t];
        xim^[t] := Im^[tau*mN + t];
      end;
      TNumerics.FFT(xre, xim, mN, -1);
    	for t := 0 to mN - 1 do
      begin
      	Re^[tau*mN + t] := xre^[t];
        Im^[tau*mN + t] := xim^[t];
      end;
    end;
  finally
    FreeMem(xim);
    FreeMem(xre);
    xim := nil;
    xre := nil;
  end;
end;

procedure TSignalField.CopyFrom(pESig: TSignalField);
var
  i: integer;
begin
  for i := 0 to mN*mN - 1 do
  begin
    Re^[i] := pESig.Re^[i];
    Im^[i] := pESig.Im^[i];
  end;
end;

procedure TSignalField.AverageWith(pESig: TSignalField);
var
  i: integer;
begin
  for i := 0 to mN*mN - 1 do
  begin
    Re^[i] := (Re^[i] + pESig.Re^[i])/2.0;
    Im^[i] := (Im^[i] + pESig.Im^[i])/2.0;
  end;
end;


destructor TSignalField.Destroy;
begin
  FreeMem(Im);
  FreeMem(Re);
  Im := nil;
  Re := nil;

  inherited Destroy;
end;

end.
