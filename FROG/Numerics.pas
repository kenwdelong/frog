unit Numerics;

interface

type
  TArray = Array [0..4194304] of double;
	PArray = ^TArray;
  TMinFunc = function(pX: double): double of object;
  TLinMinFunc = function(pX: PArray): double of object;

	TNumerics = class(TObject)
    private
      // Some private data for Linmin
      mNCom: integer;
      mNRFunc: TLinMinFunc;
      mPcom: PArray;
      mXicom: PArray;
      mF1Dim: TMinFunc;
      function F1Dim(x: double): double;
      procedure MnBrak(var ax, bx, cx, fa, fb, fc: double; pFunc: TMinFunc);
  	public
    	class procedure FFT(pRe, pIm: PArray; pN, pSign: integer);
      class procedure Four1(data: PArray; nn, isign: integer);
      class function Brent(ax, bx, cx: double; f: TMinFunc; tol: double; var xmin:double): double;
      procedure Linmin(p, xi: PArray; n: Integer; var fret: double; pFunc: TLinMinFunc);
      class function Interp(x, y: PArray; x0: double; numpts, m: integer): double;
      class function Polint(xa, ya: PArray; n: integer; x: double; var dy: double): double;
  end;

implementation

uses
	SysUtils, Math;

const
	// For Brent
	ITMAX = 100;
	CGOLD = 0.3819660;
	ZEPS = 1.0e-10;

  // For LinMin
  TOL = 1e-3;

  // For MnBrak
  GOLD = 1.618034;
  GLIMIT = 100.0;
  TINY = 1.0e-20;



class procedure TNumerics.FFT(pRe, pIm: PArray; pN, pSign: integer);
var
	i, i2: integer;
  x: PArray;
begin
	GetMem(x, 2*pN*SizeOf(double));
  try
  	// This piece was DLoadFFT
    //  Inc() is supposed to generate really tight code
  	i := 0;
		while i < pN do
    begin
    	i2 := i shl 1;
    	x^[i2] := pRe^[i];
      Inc(i2);
      x^[i2] := pIm^[i];
      Inc(i);
      Inc(i2);
      x^[i2] := -pRe^[i];
      Inc(i2);
      x^[i2] := -pIm^[i];
      Inc(i);
    end;

    Four1(x, pN, pSign);

  	// This piece was DUnLoadFFT
  	i := 0;
		while i < pN do
    begin
    	i2 := i shl 1;
    	pRe^[i] := x^[i2];
      Inc(i2);
      pIm^[i] := x^[i2];
      Inc(i);
      Inc(i2);
      pRe^[i] := -x^[i2];
      Inc(i2);
      pIm^[i] := -x^[i2];
      Inc(i);
    end;
  finally
  	FreeMem(x);
  end;
end;

class procedure TNumerics.Four1(data: PArray; nn, isign: integer);
var
	n, mmax, m, j, istep, i: integer;
  wtemp, wr, wpr, wpi, wi, theta: double;
  tempr, tempi, temp: double;
begin

	n := nn shl 1;
  j := 0;
  i := 0;

  while i < n do
  begin
		if j > i then
    begin
    	temp := data^[j]; data^[j] := data^[i]; data^[i] := temp;
    	temp := data^[j+1]; data^[j+1] := data^[i+1]; data^[i+1] := temp;
	end;
		m := n shr 1;
		while (m >= 2) and (j >= m) do
    begin
			j := j - m;
			m := m shr 1;
		end;
		j := j + m;
    i := i + 2;
  end;

	mmax := 2;
	while (n > mmax) do
  begin
		istep := 2*mmax;
		theta := 6.28318530717959/(isign*mmax);
		wtemp := sin(0.5*theta);
		wpr := -2.0*wtemp*wtemp;
		wpi := sin(theta);
		wr:=1.0;
		wi:=0.0;
    m := 1;
    while m < mmax do
    begin
      i := m - 1;
      while i < n do
      begin
				j := i + mmax;
				tempr := wr*data^[j] - wi*data^[j+1];
				tempi := wr*data^[j+1] + wi*data^[j];
				data^[j] := data^[i] - tempr;
				data^[j+1] := data^[i+1] - tempi;
				data^[i] := data^[i] + tempr;
				data^[i+1] := data^[i+1] + tempi;
        i := i + istep;
			end;
      wtemp := wr;
			wr := wtemp*wpr - wi*wpi + wr;
			wi := wi*wpr + wtemp*wpi + wi;
      m := m + 2;
  	end;
		mmax := istep;
	end;

	// Normalize the inverse transform */
	if isign = -1 then
  	for i := 0 to n - 1 do
    	data^[i] := data^[i]/nn;
end;


// Minimizes a 1D function.  See Numerical Recipes.
class function TNumerics.Brent(ax, bx, cx: double; f: TMinFunc; tol: double;
			var xmin: double): double;
var
	iter: integer;
  a, b, d, etemp, fu, fv, fw, fx, p, q, r: double;
  tol1, tol2, u, v, w, x, xm, e: double;
begin
  e := 0.0;

  if ax < cx then
  begin
  	a := ax;
    b := cx;
  end
  else begin
  	a:= cx;
    b := ax;
  end;
  x := bx;
  w := bx;
  v := bx;
  fx := f(x);
  fw := fx;
  fv := fx;
	for iter := 1 to ITMAX do
  begin
		xm := 0.5*(a+b);
    tol1 := tol*Abs(x)+ZEPS;
		tol2 := 2.0*tol1;
		if Abs(x-xm) <= (tol2- 0.5*(b-a)) then
    begin
			xmin := x;
      brent := fx;
			Exit;
		end;
		if (Abs(e) > tol1) then
    begin
			r := (x-w)*(fx-fv);
			q := (x-v)*(fx-fw);
			p := (x-v)*q-(x-w)*r;
			q := 2.0*(q-r);
			if (q > 0.0) then p := -p;
			q := Abs(q);
			etemp := e;
			e := d;
			if (Abs(p) >= Abs(0.5*q*etemp)) or (p <= q*(a-x)) or (p >= q*(b-x)) then
      begin
      	if x >= xm then e := a-x else e:= b-x;
				d := CGOLD*e;
      end
			else begin
				d := p/q;
				u := x+d;
				if ((u-a) < tol2) or ((b-u) < tol2) then
        	if (xm-x) > 0.0 then d := Abs(tol1) else d := -Abs(tol1);
      end;
    end
    else begin
    	if x >= xm then e := a-x else e := b-x;
			d := CGOLD*e;
    end;

    if Abs(d) >= tol1 then
    	u := x+d
    else
    	if d > 0.0 then u := x + Abs(tol1) else u := x - Abs(tol1);
		fu := f(u);
		if fu <= fx then
    begin
			if u >= x then a := x else b := x;
      v := w; w := x; x := u;
      fv := fw; fw := fx; fx := fu;
    end
    else
    begin
			if (u < x) then a := u else b := u;
			if (fu <= fw) or (w = x) then
      begin
				v := w;
				w := u;
				fv := fw;
				fw := fu;
      end
			else if (fu <= fv) or (v = x) or (v = w) then
      begin
				v := u;
				fv := fu;
			end;
		end;
	end;
  raise Exception.Create('Too Many Iterations in BRENT');
	xmin := x;
	brent := fx;
end;

procedure TNumerics.Linmin(p, xi: PArray; n: Integer; var fret: double; pFunc: TLinMinFunc);
var
  j: integer;
  xx, xmin, fx, fb, fa, bx, ax: double;
begin
	mNCom := n;
  mF1Dim := F1Dim;
  GetMem(mPcom, n*SizeOf(double));
  GetMem(mXicom, n*SizeOf(double));
  try
	  mNRFunc := pFunc;
    for j := 0 to N - 1 do
    begin
      mPcom^[j] := p^[j];
      mXicom^[j] := xi^[j];
    end;

	  ax := 0.0;
	  xx := 1.0;
	  bx := 2.0;
	  mnbrak(ax, xx, bx, fa, fx, fb, mF1Dim);
	  fret := Brent(ax, xx, bx, f1dim, TOL, xmin);
    for j := 0 to n - 1 do
    begin
      xi^[j] := xi^[j]*xmin;
      p^[j] := p^[j] + xi^[j];
    end;

  finally
    FreeMem(mXicom);
    FreeMem(mPcom);
  end;

end;

// This provides an effective 1D function that linmin sends to brent.
// See Numerical Recipes
function TNumerics.F1Dim(x: double): double;
var
	j: integer;
	f: double;
  xt: PArray;
begin
  GetMem(xt, mNCom*SizeOf(double));
  try
    for j := 0 to mNCom - 1 do
      xt^[j] := mPcom^[j] + x*mXicom^[j];
	f := mNRFunc(xt);
  finally
    FreeMem(xt);
  end;
	F1Dim := f;
end;

procedure TNumerics.MnBrak(var ax, bx, cx, fa, fb, fc: double; pFunc: TMinFunc);
var
  ulim, u, r, q, fu, dum: double;
  sign: integer;
begin
	fa := pFunc(ax);
	fb := pFunc(bx);
	if (fb > fa) then
  begin
    dum := ax; ax := bx; bx := dum;
    dum := fb; fb := fa; fa := dum;
	end;
	cx := bx + GOLD*(bx - ax);
	fc := pFunc(cx);
	while (fb > fc) do
  begin
		r := (bx-ax)*(fb-fc);
		q := (bx-cx)*(fb-fa);
    if (q-r)>0 then sign := 1 else sign := -1;
    if Abs(q-r) > TINY then dum := Abs(q-r) else dum := TINY;
		u := (bx)-((bx-cx)*q-(bx-ax)*r)/(2.0*dum*sign);
		ulim := (bx) + GLIMIT*(cx-bx);
		if ((bx-u)*(u-cx) > 0.0) then
    begin
			fu := pFunc(u);
			if (fu < fc) then
      begin
				ax:=(bx);
				bx:=u;
				fa:=(fb);
				fb:=fu;
				Exit;
      end
      else if (fu > fb) then
      begin
				cx:=u;
				fc:=fu;
				Exit;
      end;
			u:=(cx)+GOLD*(cx-bx);
			fu:= pFunc(u);
    end
		else if ((cx-u)*(u-ulim) > 0.0) then
    begin
			fu:= pFunc(u);
			if (fu < fc) then
      begin
        bx := cx; cx := u; u := cx + GOLD*(cx - bx);
        fb := fc; fc := fu; fu := pFunc(u);
			end
    end
		else if ((u-ulim)*(ulim-cx) >= 0.0) then
    begin
			u:=ulim;
			fu:= pFunc(u);
    end
		else
    begin
			u:=(cx)+GOLD*(cx-bx);
			fu:= pFunc(u);
		end;
    ax := bx; bx := cx; cx := u;
    fa := fb; fb := fc; fc := fu;
	end;
end;

class function TNumerics.Interp(x, y: PArray; x0: double; numpts, m: integer): double;
var
  i, m2, position: integer;
  y0, dy: double;
  xx, yy: PArray;
begin
{This serves as front end to polint. It selects which N points to pass
to the interpolating routine. The x and y arrays are the data, and x0 is the
point for which we desire a y value. numpts is the number of points in the
x and y arrays.  m is the number of points to pass to polint, and the
order of the interpolating polynomial is m-1.}

	for i := 0 to numpts - 1 do      // Find where x0 is in the array */
		if x0 < x^[i] then Break;    // i now contains the x[] point just above x0 */

	m2 := Trunc(m/2);

	if ( (i - m2) < 0 ) then
		position := 0
	else if ( (i - m2 + m) >= numpts) then
			position := numpts - m
	else
		position := i - m2;

  xx := @(x^[position]);
  yy := @(y^[position]);
	y0 := Polint(xx, yy, m, x0, dy);
	Interp := y0;
end;

class function TNumerics.Polint(xa, ya: PArray; n: integer; x: double; var dy: double): double;
var
  c, d: array [0..10] of double;
  y, dif, dift, den, ho, hp, w: double;
  m, ns, i: integer;
{This is a polynomial interpolation routine from Numerical Recipes, p.82.
It accepts an array of n x-y coords in xa and ya. It returns the value of
the interpolated y for the given x value. Dy is the estimated error in y.}
begin

	dif := Abs(x - xa^[0]);
  ns := 0;
	for i := 0 to n do      // Find the index ns of the closest table entry to x*/
  begin
    dift := Abs(x - xa^[i]);
		if ( dift < dif) then
    begin
			ns := i;
      dif := dift;
			end;
    c[i] := ya^[i];
    d[i] := ya^[i];  // initialize the c and d arrays*/
  end;

	y := ya^[ns];  // this is the initial approximation to y */

  Dec(ns);
	for m := 1 to n do
  begin
		for i := 0 to (n - m - 1) do
    begin
			ho := xa^[i] - x;
      hp := xa^[i + m] - x ;
			w := c[i + 1] - d[i];
      den := ho - hp;
			if (den = 0) then
        raise Exception.Create('den = 0 in TNumerics.Polint');
			den := w/den;
			d[i] := hp*den; c[i] := ho*den;
    end;

		if ( (2*ns + 2) < (n - m)) then
			dy := c[ns + 1]
		else
    begin
			dy := d[ns];
      Dec(ns);
    end;

		y := y + dy;
  end;
	Polint := y;
end;

end.
