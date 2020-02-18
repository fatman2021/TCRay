'============================================================================
' The Mandelbrot Dazibao - October 2003 - http://mandelbrot.dazibao.free.fr
'                 TC-Ray v2.1C - Polynomial shapes raytracing
'=============================================================================
'                 Copyright 2003 - The Mandelbrot Dazibao
'
'  This program is free software; you can redistribute it and/or modify
'  it under the terms of the GNU General Public License as published by
'  the Free Software Foundation; either version 2 of the License, or
'  any later version.
'
'  This program is distributed in the hope that it will be useful,
'  but WITHOUT ANY WARRANTY; without even the implied warranty of
'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'  GNU General Public License for more details.
'
'  You should have received a copy of the GNU General Public License
'  along with this program; if not, write to the Free Software
'  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'============================================================================
'Ported to FB by Antoni Gual 4/2005:
'.............................................................................
'  added defsng a-z at the top of every  file
'  added underscores to  variable names that clashed with arrays or FB keywords
'  converted TC-Lib to use FB's gfxlib functions
'  added a check for a keypress for at the end of every render line (to allow 
'    user to abort it)
'  added screenlock and screenunlock to every line (to increase speed)
'  removed switches to textmode and back, as text can be displayed in FB hi-res
'  added an erase in the menu to clear old objects before a new rendering
'  removed custom trig functions (FB functions are like C's, so reliable)
'  added Dumbledore's rnd2 QB-like RND replacement: got rid off grainy 3d textures
'  added conditional compiling of anti-alias: uncomment #DEFINE ANTIALIAS  
'  removed CLEAR, (1MB of stack should be enough)...
'  used a temp var in calls to HSVto_RGB in Draw.Space so Texture.Object can 
'   return its normals before Light-Specular uses them. Sky and ripples are back!
'  added if Shape(shapeshadow%).numtype=110 then flagshadow%=0  to Draw.space to
'   avoid sky projecting an ugly round shadow to the ground
'............................................................................
' To do:
'        Do a screen mode selector...
'        Create a new image for the moebius strip. The source is there....
'        Make it modular....
'============================================================================
' $INCLUDE: 'TcLib17.bi'
'#DEFINE ANTIALIAS
DEFSNG A-Z


' TC-Ray sample pics
DECLARE SUB Menu ()
DECLARE SUB Init.Example1 ()
DECLARE SUB Init.Example2 ()
DECLARE SUB Init.Example3 ()
DECLARE SUB Init.Example4 ()
DECLARE SUB Init.Example5 ()
DECLARE SUB Init.Example6 ()
DECLARE SUB Init.Example7 ()
DECLARE SUB Init.Example8 ()
DECLARE SUB Init.Example9 ()
DECLARE SUB Init.Example10 ()

' Raytracing modules
DECLARE FUNCTION Light.Specular (xp(), _xo!())
DECLARE SUB Calc.Normal (FlagHit%, xh!(), _xn!(), Ray!())


DECLARE SUB Init.Perlin ()

DECLARE SUB Cloud.BackGround ()
DECLARE SUB Draw.Axis ()
DECLARE SUB Draw.Space ()
DECLARE SUB Ray.Pixel (Nx%, Ny%, Ray!())
DECLARE SUB Ray.Reflect (xt!(), _xn!(), Ray!())
DECLARE SUB Ray.Shadow (xt!(), Ray!())
DECLARE SUB Pixel.Point (Nx%, Ny%, x!, y!, z!)

DECLARE SUB Init.Cubic (Id%, _Type$, HueRef!, x0!, y0!, z0!, dRef1!, dRef2!, Alpha!, Beta!)
DECLARE SUB Init.Object (_Type$, HueRef!, x0!, y0!, z0!, dRef1!, dRef2!, Alpha!, Beta!)
DECLARE SUB Init.Plan (Id%, _Type$, HueRef!, x0!, y0!, z0!, dRef1!, dRef2!, Alpha, Beta)
DECLARE SUB Init.Quadric (Id%, _Type$, HueRef!, x0!, y0!, z0!, dRef1!, dRef2!, Alpha, Beta)
DECLARE SUB Init.Quartic (Id%, _Type$, HueRef!, x0!, y0!, z0!, dRef1!, dRef2!, Alpha!, Beta!)
DECLARE SUB Init.Space (AlphaCam!, BetaCam!, DistCam!, DistScreen!, aLight!, bLight!)

DECLARE SUB Probe.Ball (Id%, Ray!(), Dist!, xh!(), xt!(), _xn!(), FlagPlot%)
DECLARE SUB Probe.Cone (Id%, Ray!(), Dist!, xh!(), xt!(), _xn!(), FlagPlot%)
DECLARE SUB Probe.Cubic (Id%, Ray!(), Dist!, xh!(), xt!(), _xn!(), FlagPlot%)
DECLARE SUB Probe.Cylinder (Id%, Ray!(), Dist!, xh!(), xt!(), _xn!(), FlagPlot%)
DECLARE SUB Probe.Floor (Id%, Ray(), Dist, xh!(), xt!(), _xo!(), FlagPlot%)
DECLARE SUB Probe.Mandelbrot (Id%, Ray!(), Dist!, xh!(), xt!(), _xn!(), FlagPlot%)
DECLARE SUB Probe.Object (Id%, Ray!(), Dist!, xh!(), xt!(), _xn!(), FlagPlot%)
DECLARE SUB Probe.Plan (Id%, Ray(), Dist, xh!(), xt!(), _xo!(), FlagPlot%)
DECLARE SUB Probe.Quadric (Id%, Ray(), Dist!, xh!(), xt!(), _xo!(), FlagPlot%)
DECLARE SUB Probe.Quartic (Id%, Ray!(), Dist!, xh!(), xt!(), _xn!(), FlagPlot%)
DECLARE SUB Probe.Ray (Ray!(), ShapeHit%, DistHit!, xhHit!(), xtHit!(), xnHit!(), FlagHit%)
DECLARE SUB Probe.Sky (Id%, Ray!(), Dist!, xh!(), xt!(), _xn!(), FlagPlot%)

' Texture routines
DECLARE FUNCTION Julia! (p!, q!)
DECLARE FUNCTION Mandelbrot! (p!, q!)
DECLARE FUNCTION Nephroid (x!, y!, Radius!)
DECLARE FUNCTION Texture.Object! (Id%, FlagPlot%, xh!(), _xn!())

' Mathematical utilities
DECLARE FUNCTION _ACOS! (x#)
DECLARE FUNCTION CosD! (A!)
DECLARE FUNCTION SinD! (A!)
DECLARE FUNCTION Sqr3! (x#)
DECLARE FUNCTION Noise3! (A%, B%, C%)
DECLARE FUNCTION Perlin! (p!, q!)
DECLARE FUNCTION Perlin3D (xp!, yp!, zp!)
DECLARE FUNCTION Gamma.Correction! (Value!)
DECLARE FUNCTION Exposure.Control! (Light!)

DECLARE SUB Equa1 (A1#, a0#, x1#, Err1%, nSol1%)
DECLARE SUB Equa2 (A2#, A1#, a0#, x1#, x2#, Err2%, nSol2%)
DECLARE SUB Equa3 (A3#, A2#, A1#, a0#, x1#, x2#, x3#, Err3%, nSol3%)
DECLARE SUB Equa4 (a4#, A3#, A2#, A1#, a0#, x1#, x2#, x3#, x4#, Err4%, nSol4%)

' ==============================================================

TYPE Shape
Degree AS INTEGER
ShapeType AS STRING * 2
NumType AS INTEGER
HueRef AS SINGLE
SatRef AS SINGLE
x0 AS SINGLE
y0 AS SINGLE
z0 AS SINGLE
dRef1 AS SINGLE
dRef2 AS SINGLE
Brilliance AS SINGLE
Reflect AS SINGLE
Texture AS INTEGER
Bump AS INTEGER
END TYPE

DIM SHARED NbObjects%
DIM SHARED Shape(64) AS Shape, Object(64, 35), Matrix(64, 3, 3)
DIM SHARED cReflect, cShadow, AntiAlias
DIM SHARED xCam(3), xLight(3), xCenter(3), UScreen(3), VScreen(3)

CONST Gamma = 1.3, Aperture = 1

' Shared variables for the Perlin noises
DIM SHARED Seed&, Noise%(129, 129), Octaves%, Persistence!, Span%, _Amplitude!(16)

setvga
do
  erase shape,Object,matrix,noise%,_Amplitude!
  nbobjects%=0:creflect=0:cshadow=0:antialias=0:persistence=0:span=0

  SetText
  print  "TC-Ray v2.1C - Polynomial shapes raytracing"
  print
  print "    Select a picture to render     "
  print "==================================="
  PRINT "Geometry                          1"
  PRINT "Birthday                          2"
  PRINT "ADN helix                         3"
  PRINT "Perlin Ring                       4"
  PRINT "Stormy Sea                        5"
  PRINT "Dolmen                            6"
  PRINT "Mandelchrist                      7"
  PRINT "Bike Mirrors                      8"
  PRINT "Water Effect                      9"
  PRINT "The Nephroid Cup                 10"
 ' print "Moebius strip                    11"  'need to create an image init for it
 '                                               the probe routine was left here by Jark
  print
  PRINT "End of Program                    0"
  PRINT
  INPUT "Select an option                : ", Choice

  CLS : PRINT "Loading space description..."
  SELECT CASE Choice
  CASE 0: CLS : EndProg
  CASE 1: call Init.Example1: _Name$ = "Geo"
  CASE 2: call Init.Example2: _Name$ = "Birthday"
  CASE 3: call Init.Example3: _Name$ = "ADN"
  CASE 4: call Init.Example4: _Name$ = "Ring"
  CASE 5: call Init.Example5: _Name$ = "Storm"
  CASE 6: call Init.Example6: _Name$ = "Dolmen"
  CASE 7: call Init.Example7: _Name$ = "Christ"
  CASE 8: call Init.Example8: _Name$ = "Bike"
  CASE 9: call Init.Example9: _Name$ = "Water"
  CASE 10: call Init.Example10: _Name$ = "Cup"
  'case 11: call Init.Example11: _Name$ = "Moebius"
  CASE ELSE
  END SELECT
  cls
  Draw.Space
  WaitKey
loop until choice=0

EndProg
end

FUNCTION _ACOS! (x#)
' Returns the ArcCosinus of a real number

_acos!=acos(x#)
'IF x# = 0 THEN _ACOS = PiOver2: EXIT FUNCTION

'IF x# > 0 THEN
'_ACOS = ATN(SQR(1 - x# * x#) / x#)
'ELSE
'_ACOS = Pi + ATN(SQR(1 - x# * x#) / x#)
'END IF
END FUNCTION

SUB Calc.Normal (Id%, xh!(), _xn!(), Ray!())
' Get the normal vector xn on object number Id%
' The hit point is xh is the object axis system
' The normal vector is returned in the main axis system for brilliance calculation
' xo is the same vector, but in the object axis system
' The ray is defined in the main axis system

DIM _xo!(3)

'=======================================================
' Calculate the normal vector in object axis system

SELECT CASE Shape(Id%).Degree
CASE 1
_xo!(1) = Object(Id%, 32)
_xo!(2) = Object(Id%, 33)
_xo!(3) = Object(Id%, 34)

CASE 2
_xo!(1) = (2 * Object(Id%, 26) * xh!(1) + Object(Id%, 29) * xh!(2) + Object(Id%, 31) * xh!(3) + Object(Id%, 32))
_xo!(2) = (2 * Object(Id%, 27) * xh!(2) + Object(Id%, 29) * xh!(1) + Object(Id%, 30) * xh!(3) + Object(Id%, 33))
_xo!(3) = (2 * Object(Id%, 28) * xh!(3) + Object(Id%, 30) * xh!(2) + Object(Id%, 31) * xh!(3) + Object(Id%, 34))

CASE 3
_xo!(1) = 3 * Object(Id%, 17) * xh!(1) ^ 2 + Object(Id%, 16) * xh!(2) * xh!(3) + 2 * Object(Id%, 20) * xh!(1) * xh!(2) + 2 * Object(Id%, 21) * xh!(1) * xh!(3) + Object(Id%, 23) * xh!(2) ^ 2 + Object(Id%, 24) * xh!(3) ^ 2
_xo!(1) = _xo!(1) + 2 * Object(Id%, 26) * xh!(1) + Object(Id%, 29) * xh!(2) + Object(Id%, 31) * xh!(3) + Object(Id%, 32)
_xo!(2) = 3 * Object(Id%, 18) * xh!(2) ^ 2 + Object(Id%, 16) * xh!(3) * xh!(1) + 2 * Object(Id%, 22) * xh!(2) * xh!(3) + 2 * Object(Id%, 23) * xh!(2) * xh!(1) + Object(Id%, 25) * xh!(3) ^ 2 + Object(Id%, 20) * xh!(1) ^ 2
_xo!(2) = _xo!(2) + 2 * Object(Id%, 27) * xh!(2) + Object(Id%, 30) * xh!(3) + KYX * xh!(1) + Object(Id%, 33)
_xo!(3) = 3 * Object(Id%, 19) * xh!(3) ^ 2 + Object(Id%, 16) * xh!(1) * xh!(2) + 2 * Object(Id%, 24) * xh!(3) * xh!(1) + 2 * Object(Id%, 25) * xh!(3) * xh!(2) + Object(Id%, 21) * xh!(1) ^ 2 + Object(Id%, 22) * xh!(2) ^ 2
_xo!(3)+=  2 * Object(Id%, 28) * xh!(3) + Object(Id%, 31) * xh!(1) + Object(Id%, 30) * xh!(2) + Object(Id%, 34)

CASE 4
_xo!(1) = 4 * Object(Id%, 1) * xh!(1) * xh!(1) * xh!(1)
_xo!(1) = _xo!(1) + 2 * Object(Id%, 10) * xh!(1) * xh!(2) * xh!(2)
_xo!(1) = _xo!(1) + 2 * Object(Id%, 11) * xh!(1) * xh!(3) * xh!(3)
_xo!(1) = _xo!(1) + 3 * Object(Id%, 4) * xh!(1) * xh!(1) * xh!(2)
_xo!(1) = _xo!(1) + 3 * Object(Id%, 5) * xh!(1) * xh!(1) * xh!(3)
_xo!(1) = _xo!(1) + Object(Id%, 7) * xh!(2) * xh!(2) * xh!(2)
_xo!(1) = _xo!(1) + Object(Id%, 8) * xh!(3) * xh!(3) * xh!(3)
_xo!(1) = _xo!(1) + 3 * Object(Id%, 17) * xh!(1) * xh!(1)
_xo!(1) = _xo!(1) + Object(Id%, 16) * xh!(2) * xh!(3)
_xo!(1) = _xo!(1) + 2 * Object(Id%, 20) * xh!(1) * xh!(2)
_xo!(1) = _xo!(1) + 2 * Object(Id%, 21) * xh!(1) * xh!(3)
_xo!(1) = _xo!(1) + Object(Id%, 23) * xh!(2) * xh!(2)
_xo!(1) = _xo!(1) + Object(Id%, 24) * xh!(3) * xh!(3)
_xo!(1) = _xo!(1) + 2 * Object(Id%, 26) * xh!(1)
_xo!(1) = _xo!(1) + Object(Id%, 29) * xh!(2)
_xo!(1) = _xo!(1) + Object(Id%, 31) * xh!(3)
_xo!(1) = _xo!(1) + Object(Id%, 32)

_xo!(2) = 4 * Object(Id%, 2) * xh!(2) * xh!(2) * xh!(2)
_xo!(2) = _xo!(2) + 2 * Object(Id%, 12) * xh!(2) * xh!(3) * xh!(3)
_xo!(2) = _xo!(2) + 2 * Object(Id%, 10) * xh!(2) * xh!(1) * xh!(1)
_xo!(2) = _xo!(2) + 3 * Object(Id%, 6) * xh!(2) * xh!(2) * xh!(3)
_xo!(2) = _xo!(2) + 3 * Object(Id%, 7) * xh!(2) * xh!(2) * xh!(1)
_xo!(2) = _xo!(2) + Object(Id%, 9) * xh!(3) * xh!(3) * xh!(3)
_xo!(2) = _xo!(2) + Object(Id%, 4) * xh!(1) * xh!(1) * xh!(1)
_xo!(2) = _xo!(2) + 3 * Object(Id%, 18) * xh!(2) * xh!(2)
_xo!(2) = _xo!(2) + Object(Id%, 16) * xh!(3) * xh!(1)
_xo!(2) = _xo!(2) + 2 * Object(Id%, 22) * xh!(2) * xh!(3)
_xo!(2) = _xo!(2) + 2 * Object(Id%, 23) * xh!(2) * xh!(1)
_xo!(2) = _xo!(2) + Object(Id%, 25) * xh!(3) * xh!(3)
_xo!(2) = _xo!(2) + Object(Id%, 20) * xh!(1) * xh!(1)
_xo!(2) = _xo!(2) + 2 * Object(Id%, 27) * xh!(2)
_xo!(2) = _xo!(2) + Object(Id%, 30) * xh!(3)
_xo!(2) = _xo!(2) + KYX * xh!(1)
_xo!(2) = _xo!(2) + Object(Id%, 33)

_xo!(3) = 4 * Object(Id%, 3) * xh!(3) * xh!(3) * xh!(3)
_xo!(3)+=  2 * Object(Id%, 11) * xh!(3) * xh!(1) * xh!(1)
_xo!(3)+=  2 * Object(Id%, 12) * xh!(3) * xh!(2) * xh!(2)
_xo!(3)+=  3 * Object(Id%, 8) * xh!(3) * xh!(3) * xh!(1)
_xo!(3)+=  3 * Object(Id%, 9) * xh!(3) * xh!(3) * xh!(2)
_xo!(3)+=  Object(Id%, 5) * xh!(1) * xh!(1) * xh!(1)
_xo!(3)+=  Object(Id%, 6) * xh!(2) * xh!(2) * xh!(2)
_xo!(3)+=  3 * Object(Id%, 19) * xh!(3) * xh!(3)
_xo!(3)+=  Object(Id%, 16) * xh!(1) * xh!(2)
_xo!(3)+=  2 * Object(Id%, 24) * xh!(3) * xh!(1)
_xo!(3) += 2*Object(Id%, 25)*xh!(3)*xh!(2)
_xo!(3)+=  Object(Id%, 21) * xh!(1) * xh!(1)
_xo!(3)+=  Object(Id%, 22) * xh!(2) * xh!(2)
_xo!(3)+=  2 * Object(Id%, 28) * xh!(3)
_xo!(3)+=  Object(Id%, 31) * xh!(1)
_xo!(3)+=  Object(Id%, 30) * xh!(2)
_xo!(3)+=  Object(Id%, 34)

CASE ELSE
END SELECT

' Normalise the vector
ModNorm = Module(_xo!(1), _xo!(2), _xo!(3))
IF ModNorm = 0 THEN EXIT SUB
FOR k% = 1 TO 3: _xo!(k%) = _xo!(k%) / ModNorm: NEXT k%

' Convert to main axis system
FOR k% = 1 TO 3
_xn!(k%) = _xo!(1) * Matrix(Id%, 1, k%) + _xo!(2) * Matrix(Id%, 2, k%) + _xo!(3) * Matrix(Id%, 3, k%)
NEXT k%

' Reverse the vector if necessary
Side = Ray!(4) * _xn!(1) + Ray!(5) * _xn!(2) + Ray!(6) * _xn!(3)
IF Side < 0 THEN
FOR k% = 1 TO 3: _xn!(k%) = -_xn!(k%): NEXT k%
END IF

END SUB

FUNCTION CosD (A)
' Corrects the wrong values provided by thea standard COS function
' a is in degrees

cosd=cos(a*piover180)
'A = A - INT(A / 360 + .5) * 360
'
'IF A >= 0 AND A <= 90 THEN
'x# = A / 180 * Pi
'    IF COS(x#) < .5 THEN
'    CosD = SIN(Pi / 2 - x#)
'    ELSE
'    CosD = COS(x#)
''    END IF
'EXIT FUNCTION
'END IF
'
'IF A < 0 AND A >= -90 THEN
'x# = -A / 180 * Pi
'    IF COS(x#) < .5 THEN
'    CosD = SIN(Pi / 2 - x#)
'    ELSE
'    CosD = COS(x#)
'    END IF
'EXIT FUNCTION
'END IF
'
'IF A > 90 AND A <= 180 THEN
'x# = (180 - A) / 180 * Pi
'    IF COS(x#) < .5 THEN
'    CosD = SIN(x# - Pi / 2)
'    ELSE
'    CosD = -COS(x#)
'    END IF
'EXIT FUNCTION
'END IF
'
'IF A < -90 AND A >= -180 THEN
'x# = (A - 180) / 180 * Pi
'    IF COS(x#) < .5 THEN
'    CosD = -SIN(Pi / 2 - x#)
'    ELSE
'    CosD = -COS(x#)
'    END IF
'EXIT FUNCTION
'END IF
END FUNCTION

'SUB Draw.Axis
'Pixel.Point x0%, y0%, 0, 0, 0
'Pixel.Point x1%, y1%, 2, 0, 0
'Pixel.Point x2%, y2%, 0, 2, 0
'Pixel.Point x3%, y3%, 0, 0, 2'

'_RGB 0, 255, 0
'Line24 x0%, y0%, x1%, y1%, 0
'Line24 x0%, y0%, x2%, y2%, 0
'Line24 x0%, y0%, x3%, y3%, 0
'
'dRef1 = .75: dRef2 = 1.45: dZ = .1
'Radius = SQR(dRef1 ^ 2 - (dRef1 / dRef2) ^ 2 * dZ ^ 2)
'x0! = .3
'y0! = 0
'z0! = .8

'FOR k% = 0 TO 360
'Angle! = k% * Pi / 180
'x! = x0! + Radius * COS(Angle!)
'y! = y0! + Radius * SIN(Angle!)
'z! = z0!
'Pixel.Point m%, p%, x!, y!, z!
'Pset24 m%, p%
'NEXT k%

'END SUB

SUB Draw.Space
' Main raytracing routine

DIM Ray(7)
DIM xhHit(3), xtHit(3), xnHit(3)
DIM xhReflect(3), xtReflect(3), xnReflect(3)
DIM xhShadow(3), xtShadow(3), xnShadow(3)

' Main loop: one calculation per pixel
FOR Ny% = 0 TO ScrHeight - 1
  screenlock
  FOR Nx% = 0 TO ScrWidth - 1

    ' Probe the objects with the pixel ray
    Probe% = 1: AntiAlias = 0
    HitAliasCheck:
    Ray.Pixel Nx%, Ny%, Ray(): ShapeHit% = 0
    Probe.Ray Ray(), ShapeHit%, Dist, xhHit(), xtHit(), xnHit(), FlagHit%

    ' AntiAlias checking: iterate until two similar hits in case of a change
    #IFDEF ANTIALIAS
     IF Shape(ShapeHit%).Degree = 4 OR Shape(LastShape%).Degree = 4 THEN
	    IF Probe% = 1 AND (ShapeHit% <> LastShape% OR FlagHit% <> LastFlag%) THEN
	      Probe% = 2: AntiAlias = 1
	      Hit1% = ShapeHit%: Flag1% = FlagHit%
	      GOTO HitAliasCheck:
	    END IF
	    IF Probe% = 2 AND (ShapeHit% <> Hit1% OR FlagHit% <> Flag1%) THEN
	      Probe% = 2: AntiAlias = 0
	      Hit1% = ShapeHit%: Flag1% = FlagHit%
	      GOTO HitAliasCheck:
	  	END IF
    END IF
    #ENDIF
    LastShape% = ShapeHit%: LastFlag% = FlagHit%

    ' Calculate the pixel colour in case a shape is hit
    IF FlagHit% <> 0 THEN

	    ' Check if a shadow must be applied
	    Probe% = 1: AntiAlias = 0
	    d1 = xtHit(1) - xLight(1)
	    d2 = xtHit(2) - xLight(2)
	    d3 = xtHit(3) - xLight(3)
	    DistToLight = Module(d1, d2, d3)
	    ShadowAliasCheck:
	    Ray.Shadow xtHit(), Ray(): ShapeShadow% = ShapeHit%
	    Probe.Ray Ray(), ShapeShadow%, DistShadow, xhShadow(), xtShadow(), xnShadow(), FlagShadow%
	    'IF DistShadow > DistToLight THEN FlagShadow% = 0
      'AGV  
      if Shape(shapeshadow%).numtype=110 then flagshadow%=0
	    IF DistShadow > 30 THEN FlagShadow% = 0
	
	    ' AntiAlias checking: iterate until two similar hits in case of a change
      #IFDEF ANTIALIAS 
	    iF Shape(ShapeShadow%).Degree = 4 OR Shape(LastShadow%).Degree = 4 THEN
	      IF Probe% = 1 AND FlagShadow% <> LastShadow% THEN
			    Probe% = 2: AntiAlias = 0
			    Flag1% = FlagShadow%
			    GOTO ShadowAliasCheck:
		    END IF
		    IF Probe% = 2 AND FlagShadow% <> Flag1% THEN
			    Probe% = 2: AntiAlias = 1
			    Flag1% = FlagShadow%
			    GOTO ShadowAliasCheck:
		    END IF
	    END IF
      #ENDIF
	    LastShadow% = FlagShadow%
	
	    IF FlagShadow% <> 0 THEN
	    	Shadow = cShadow / (1 + .5 * DistShadow) * Shape(ShapeHit%).Brilliance
	    ELSE
	    	Shadow = Shape(ShapeHit%).Brilliance
	    END IF
	
	    ' Calculate the pixel colour without reflection
      a=Texture.Object!(ShapeHit%, FlagHit%, xhHit!(), xnHit!())   
	    HSVto_RGB a, Shape(ShapeHit%).SatRef, Shadow * Light.Specular(xtHit!(), xnHit!())
	
	    ' Check if a reflection colour must be added
	    IF Shape(ShapeHit%).Reflect <> 0 THEN
	
	    Probe% = 1: AntiAlias = 0
	    ReflectAliasCheck:
	    Ray.Reflect xtHit!(), xnHit!(), Ray(): ShapeReflect% = ShapeHit%
	    Probe.Ray Ray(), ShapeReflect%, DistReflect, xhReflect(), xtReflect(), xnReflect(), FlagReflect%
	    #IFDEF ANTIALIAS 
	    ' AntiAlias checking: iterate until two similar hits in case of a change
	    IF Shape(ShapeReflect%).Degree = 4 OR Shape(LastReflect%).Degree = 4 THEN
		    IF Probe% = 1 AND FlagReflect% <> LastReflect% THEN
			    Probe% = 2: AntiAlias = 0
			    Flag1% = FlagReflect%
			    GOTO ReflectAliasCheck:
		    END IF
		    IF Probe% = 2 AND FlagReflect% <> Flag1% THEN
			    Probe% = 2: AntiAlias = 1
			    Flag1% = FlagReflect%
			    GOTO ReflectAliasCheck:
		    END IF
		   END IF
      #ENDIF  
	    LastReflect% = FlagReflect%
	
	    ' Merge the two colours in case of reflection
	    IF FlagReflect% <> 0 THEN
		    r1% = Red%: g1% = Green%: B1% = Blue%
        a=   Texture.Object!(ShapeReflect%, FlagReflect%, xtReflect(), xnReflect())
		    HSVto_RGB a, Shape(ShapeReflect%).SatRef, Shape(ShapeReflect%).Brilliance * Light.Specular(xtReflect(), xnReflect())
		    r2% = Red%: g2% = Green%: B2% = Blue%
		    Reflection = Shape(ShapeHit%).Reflect / (1 + .15 * DistReflect)
		    Red% = r1% + Reflection * r2%
		    Green% = g1% + Reflection * g2%
		    Blue% = B1% + Reflection * B2%
		    IF Red% > 255 THEN Red% = 255
		    IF Green% > 255 THEN Green% = 255
		    IF Blue% > 255 THEN Blue% = 255
	    END IF
	
    END IF

    ' Apply Gamma and exposure corrections if wanted
    IF Gamma <> 1 THEN
      _RGBtoHSV Hue, Sat, Value
      Value = Gamma.Correction(Value)
      Value = Exposure.Control(Value)
      HSVto_RGB Hue, Sat, Value
      END IF

      ' Plot the point...
      Pset24 Nx%, Ny%
    END IF

  NEXT Nx%
  screenunlock
  if len(inkey$) then exit for
NEXT Ny%

EndView  ' Close the file in case of direct bitmap generation

END SUB

SUB Equa1 (A1#, a0#, x1#, Err1%, nSol1%)
' First degree equation resolution
IF A1# = 0 AND a0# = 0 THEN Err1% = 1: nSol1% = 0: EXIT SUB
IF A1# = 0 AND a0# <> 0 THEN Err1% = 0: nSol1% = 0: EXIT SUB
IF A1# <> 0 THEN Err1% = 0: x1# = -a0# / A1#: nSol1% = 1: EXIT SUB
END SUB

SUB Equa2 (A2#, A1#, a0#, x1#, x2#, Err2%, nSol2%)
' Second degree equation resolution - Discriminant method
Err2% = 0
IF A2# = 0 AND A1# = 0 AND a0# = 0 THEN Err2% = 1: nSol2% = 0: EXIT SUB
IF A2# = 0 AND A1# = 0 AND a0# <> 0 THEN nSol2% = 0: EXIT SUB

IF A2# = 0 THEN
  Equa1 A1#, a0#, x1#, Err2%, nSol2%
ELSE
  Delta2# = A1# * A1# - 4 * A2# * a0#
  IF Delta2# >= 0 THEN
    nSol2% = 2
    x1# = -(A1# + SQR(Delta2#)) * .5 / A2#
    x2# = (SQR(Delta2#) - A1#) * .5 / A2#
  ELSE
    nSol2% = 0: EXIT SUB
  END IF
END IF
END SUB

SUB Equa3 (a30#, a20#, a10#, a00#, x1#, x2#, x3#, Err3%, nSol3%)
' Third degree equation resolution - Cardan method
Err3% = 0

IF a30# = 0 AND a20# = 0 AND a10# = 0 AND a00# = 0 THEN
nSol3% = 0
Err3% = 1
EXIT SUB
END IF

IF a30# = 0 AND a20# = 0 AND a10# = 0 AND a00# <> 0 THEN
nSol3% = 0
EXIT SUB
END IF

IF a30# = 0 AND a20# = 0 THEN
Equa1 a10#, a00#, x1#, Err3%, nSol3%
EXIT SUB
END IF

IF a30# = 0 THEN
Equa2 a20#, a10#, a00#, x1#, x2#, Err3%, nSol3%
EXIT SUB
END IF

A3# = 1
A2# = a20# / a30#
A1# = a10# / a30#
a0# = a00# / a30#

p# = A1# - 1 / 3 * (A2#) ^ 2
q# = 2 / 27 * (A2#) ^ 3 + a0# - A2# / 3 * A1#
Delta3# = q# ^ 2 + 4 / 27 * p# ^ 3

IF Delta3# > 0 THEN
x1# = Sqr3(-q# / 2 + SQR(Delta3#) / 2) + Sqr3(-q# / 2 - SQR(Delta3#) / 2) - A2# / 3
nSol3% = 1
EXIT SUB
END IF

nSol3% = 3

IF Delta3# = 0 THEN
IF p# <> 0 THEN
x1# = q# / p# * 3 - A2# / 3
x2# = -SQR(-p# / 3) - A2# / 3
x3# = x2#
IF x1# > x2# THEN SWAP x1#, x2#
IF x1# > x3# THEN SWAP x1#, x3#
IF x2# > x3# THEN SWAP x2#, x3#
ELSE
x1# = -A2# / 3: x2# = x1#: x3# = x1#
END IF
EXIT SUB
END IF

IF Delta3# < 0 THEN
r# = SQR(-p# ^ 3 / 27)
CosTeta3# = -q# / 2 / r#
Teta3# = _ACOS(CosTeta3#)

x1# = 2 * SQR(-p# / 3) * COS(Teta3# / 3) - A2# / 3
x2# = 2 * SQR(-p# / 3) * COS(Teta3# / 3 + 2 * Pi / 3) - A2# / 3
x3# = 2 * SQR(-p# / 3) * COS(Teta3# / 3 + 4 * Pi / 3) - A2# / 3
IF x1# > x2# THEN SWAP x1#, x2#
IF x1# > x3# THEN SWAP x1#, x3#
IF x2# > x3# THEN SWAP x2#, x3#
END IF

END SUB

SUB Equa4 (a40#, a30#, a20#, a10#, a00#, x1#, x2#, x3#, x4#, Err4%, nSol4%)
' 4th degree polynomial equations - Ferrari method

' If a40#=0, the equation is a cubic
IF a40# = 0 THEN
Equa3 a30#, a20#, a10#, a00#, x1#, x2#, x3#, Err4%, nSol4%
EXIT SUB
END IF

' Change the coefficients so that a4#=1
a4# = 1
A3# = a30# / a40#
A2# = a20# / a40#
A1# = a10# / a40#
a0# = a00# / a40#

' The equation to be solved is now
' x^4 + a3*x^3 + a2*x^2 + a1*x + a0 = 0  (1)

' Change variable to remove the cubic term
' X = x + a3#/4
' The original equation becomes:
' X^4 + A*X^2 + B*X + C = 0     (2)

A# = A2# - 3 / 8 * A3# ^ 2
B# = A3# ^ 3 / 8 - A3# * A2# / 2 + A1#
C# = A3# ^ 2 * A2# / 16 - 3 / 256 * A3# ^ 4 - A1# * A3# / 4 + a0#

' If B = 0 then (2) is a bi-quadric equation:
IF B# = 0 THEN
Equa2 1, A#, C#, x21#, x22#, Err2%, nSol2%
IF x21# < 0 AND x22# < 0 THEN nSol4% = 0: EXIT SUB
IF x21# >= 0 AND x22# >= 0 THEN
nSol4% = 4
x1# = SQR(x21#) - A3# / 4
x2# = -SQR(x21#) - A3# / 4
x3# = SQR(x22#) - A3# / 4
x4# = -SQR(x22#) - A3# / 4
EXIT SUB
END IF
IF x21# >= 0 AND x22# < 0 THEN
nSol4% = 2
x1# = SQR(x21#) - A3# / 4
x2# = -SQR(x21#) - A3# / 4
x3# = 0
x4# = 0
EXIT SUB
END IF
IF x21# < 0 AND x22# >= 0 THEN
nSol4% = 2
x1# = SQR(x22#) - A3# / 4
x2# = -SQR(x22#) - A3# / 4
x3# = 0
x4# = 0
EXIT SUB
END IF

END IF

' If B<>0, we must introduce an auxiliary variable "u".
' Let's calculate (X^2 + u/2)^2:
' (X^2 + u/2)^2 = (u-A)*X^2 - B*X + u^2/4 - C  (3)

' We want (3) to be equivalent to (2) for any "u"
' That means the second term of (3) is a double-root second degree polynom
' This condition is obtained if its discriminant is nul

' That leads to the cubic solvant equation:
' u^3 - A * u^2 - 4 * C * u + (4*A*C - B^2) = 0

p# = -A#
q# = -4 * C#
r# = 4 * A# * C# - B# * B#
Equa3 1, p#, q#, r#, u1#, u2#, u3#, Err3%, nSol3%

' This cubic equation has a root U0, with U0 > A
' If such a root does't exist, then B = 0, which means equation (2)
' is a bi-quadric equation.

IF u1# > A# THEN
u0# = u1#
ELSE
IF u2# > A# THEN
u0# = u2#
ELSE
IF u3# > A# THEN
u0# = u3#
ELSE
nSol4% = 0
END IF
END IF
END IF

' Equation (3) can now be re-written:
' (X^2 + u0/2)^2 = (u0 - A) * (X - B/2/(u0-A))^2
' This bi-quadric equation is equivalent to the two following quadrics:

' X^2 + u0/2 = +SQR(u0-A) * X - B/2/SQR(u0-A)
' X^2 + u0/2 = -SQR(u0-A) * X + B/2/SQR(u0-A)

SF# = SQR(u0# - A#)
Equa2 1, -SF#, (u0# / 2 + B# / 2 / SF#), xa1#, xa2#, Err2a%, nSol2a%
Equa2 1, SF#, (u0# / 2 - B# / 2 / SF#), xb1#, xb2#, Err2b%, nSol2b%

IF nSol2a% = 0 AND nSol2b% = 0 THEN nSol4% = 0: EXIT SUB

IF nSol2a% = 2 AND nSol2b% = 0 THEN
nSol4% = 2
x1# = xa1# - A3# / 4
x2# = xa2# - A3# / 4
x3# = 0
x4# = 0
EXIT SUB
END IF

IF nSol2a% = 0 AND nSol2b% = 2 THEN
nSol4% = 2
x1# = xb1# - A3# / 4
x2# = xb2# - A3# / 4
x3# = 0
x4# = 0
EXIT SUB
END IF

IF nSol2a% = 2 AND nSol2b% = 2 THEN
nSol4% = 4
x1# = xa1# - A3# / 4
x2# = xa2# - A3# / 4
x3# = xb1# - A3# / 4
x4# = xb2# - A3# / 4
EXIT SUB
END IF

END SUB

FUNCTION Exposure.Control (Light)
' Avoids over-exposure
Exposure.Control = (1 - EXP(-Light * .01 * Aperture)) * 100
END FUNCTION

FUNCTION Gamma.Correction (Value)
' Corrects the monitor rendering for dark pixels
' The Gamma correction coefficient should be set once and for all
' for you own computer monitor
' Gamma = 1 > no correction
Gamma.Correction = ((Value / 100) ^ (1 / Gamma)) * 100
END FUNCTION

SUB Init.Cubic (Id%, _Type$, HueRef, x0!, y0!, z0!, dRef1, dRef2, Alpha, Beta)
' The power of three... somehow dangerous!
' I'm not sure this routine is 100% safe

SELECT CASE _Type$
CASE "C0"
Shape(Id%).NumType = 300
KXYZ = 0: KX3 = 1: KY3 = 1: KZ3 = 1
KX2Y = 0: KX2Z = 0: KY2Z = 0
KY2X = 0: KZ2X = 0: KZ2Y = 0
KX2 = 0: KY2 = 0: KZ2 = 0
KXY = 0: KXZ = 0: KYZ = 0
kx = 0: ky = 0: KZ = 0
K0 = -dRef1 ^ 3

CASE "C1"
Shape(Id%).NumType = 301
KXYZ = 0: KX3 = 0: KY3 = 0: KZ3 = 1
KX2Y = 0: KX2Z = 1: KY2Z = 1
KY2X = 0: KZ2X = 0: KZ2Y = 0
KX2 = 0: KY2 = 0: KZ2 = 0
KXY = 0: KXZ = 0: KYZ = 0
kx = 0: ky = 0: KZ = -dRef1 ^ 2
K0 = 0
CASE ELSE: EXIT SUB
END SELECT

FOR k% = 1 TO 15: Object(Id%, k%) = 0: NEXT k%
Object(Id%, 16) = KXYZ
Object(Id%, 17) = KX3: Object(Id%, 18) = KY3: Object(Id%, 19) = KZ3
Object(Id%, 20) = KX2Y: Object(Id%, 21) = KX2Z: Object(Id%, 22) = KY2Z
Object(Id%, 23) = KY2X: Object(Id%, 24) = KZ2X: Object(Id%, 25) = KZ2Y
Object(Id%, 26) = KX2: Object(Id%, 27) = KY2: Object(Id%, 28) = KZ2
Object(Id%, 29) = KXY: Object(Id%, 30) = KYZ: Object(Id%, 31) = KXZ
Object(Id%, 32) = kx: Object(Id%, 33) = ky: Object(Id%, 34) = KZ
Object(Id%, 35) = K0

Shape(Id%).Degree = 3
Shape(Id%).ShapeType = _Type$
Shape(Id%).HueRef = HueRef: Shape(Id%).SatRef = 200 / 3
Shape(Id%).dRef1 = dRef1: Shape(Id%).dRef2 = dRef2
Shape(Id%).x0 = x0!: Shape(Id%).y0 = y0!: Shape(Id%).z0 = z0!
Shape(Id%).Brilliance = 1
Shape(Id%).Reflect = cReflect

Matrix(Id%, 1, 1) = CosD(Beta) * CosD(Alpha)
Matrix(Id%, 1, 2) = CosD(Beta) * SinD(Alpha)
Matrix(Id%, 1, 3) = SinD(Beta)
Matrix(Id%, 2, 1) = -SinD(Alpha)
Matrix(Id%, 2, 2) = CosD(Alpha)
Matrix(Id%, 2, 3) = 0
Matrix(Id%, 3, 1) = -SinD(Beta) * CosD(Alpha)
Matrix(Id%, 3, 2) = -SinD(Beta) * SinD(Alpha)
Matrix(Id%, 3, 3) = CosD(Beta)

END SUB

SUB Init.Example1
' Geometry
Init.Space 0, 15, 5, .8, 30, 60
Init.Object "Fl", 240, 0, 0, 0, .15, .15, 0, 0
Shape(1).Reflect = .1
Init.Object "H1", 200, 1, 1.2, 1, .8, .8, 20, -50
Init.Object "Sp", 120, -1, -2.3, .5, 1.8, 1.6, -10, -20
Init.Object "Q4", 30, 3.5, 0, .5, .4, .2, 30, -20
Shape(1).Texture = 17
END SUB

SUB Init.Example10
Init.Space 15, 35, 2.5, .8, 55, 30
'Init.Space 0, 90, 2.5, .8, 55, 30

' Table surface
Init.Object "Fl", 300, 0, 0, 0, 2, 2, 0, 0
Shape(1).Reflect = .9
Shape(1).Texture = 12

' External surface of the cup
Init.Object "Cp", 60, .3, 0, .9, .8, 1.45, 30, 0
Shape(2).Reflect = .1
Shape(2).Texture = 11

' Internal surface of the cup
Init.Object "Cp", 150, .3, 0, .9, .75, 1.45, 0, 0
Shape(3).Reflect = 0

' Close the cup with a torus ring
Init.Object "T0", 240, .3, 0, .9, (.8 + .75) / 2, .025, 0, 0
Shape(4).Reflect = 0

' Coffee surface
'x^2+y^2=-(dRef1/dRef2)^2*z^2 + dRef1^2
dRef1 = .75: dRef2 = 1.45: dZ = .1
Radius = SQR(dRef1 ^ 2 - (dRef1 / dRef2) ^ 2 * dZ ^ 2)
Init.Object "D1", 30, .3, 0, .8, Radius, Radius, 0, 0
Shape(5).Reflect = 0
Shape(5).SatRef = 50
Shape(5).Texture = 32

' Torus for the cup handle
Init.Object "T0", 240, .3, .7, .4, .3, .05, 0, 90
Shape(6).Reflect = 0
END SUB

SUB Init.Example2
' Birthday
Init.Space 0, 30, 5, .8, 30, 60
Init.Object "Fl", 240, 0, 0, 0, .15, .15, 0, 0
Shape(1).Texture = 17

r! = 2
FOR k% = 1 TO 16
A! = k% / 16 * 2 * Pi
Init.Object "Bl", k% * 24, r! * COS(A!), r! * SIN(A!), 1.5, .3, .3, 0, 0
NEXT k%
r! = 2.5
FOR k% = 1 TO 16
A! = k% / 16 * 2 * Pi
Init.Object "Cy", 180 + k% * 24, r! * COS(A!), r! * SIN(A!), .4, .3, .8, 0, 0
NEXT k%


END SUB

SUB Init.Example3
' ADN Helix
Init.Space 0, 40, 5, .8, 30, 60
DIM U(3), v(3), W(3)

Init.Object "Fl", 90, 0, 0, 0, .3, .3, 0, 0
Shape(1).Texture = 17
r! = 1.6
Alpha = Pi / 4: Beta = Pi / 3
U(1) = COS(Alpha) * COS(Beta): v(1) = -SIN(Alpha): W(1) = -SIN(Beta) * COS(Alpha)
U(2) = SIN(Alpha) * COS(Beta): v(2) = COS(Alpha): W(2) = -SIN(Beta) * SIN(Alpha)
U(3) = SIN(Beta): v(3) = 0: W(3) = COS(Beta)

FOR k% = 1 TO 48
A! = k% / 16 * 2 * Pi
x0r = r! * COS(A!)
y0r = r! * SIN(A!)
z0r = -4 + k% * .15
x0! = U(1) * x0r + v(1) * y0r + W(1) * z0r
y0! = U(2) * x0r + v(2) * y0r + W(2) * z0r
z0! = U(3) * x0r + v(3) * y0r + W(3) * z0r
z0! = z0! + r! / 3
Init.Object "Bl", k% * 24, x0!, y0!, z0!, .4, .4, 0, 0
NEXT k%

END SUB

SUB Init.Example4
' Perlin Ring
Init.Space 23, 30, 5, .8, 30, 60
cShadow = .7: cReflect = .2
Init.Object "T0", 120, 0, 0, .8, 2, .4, 0, 0
Shape(1).Texture = 16

FOR k% = 1 TO 4
A! = k% / 2 * Pi
Init.Object "Bl", k% * 90, 2 * COS(A!), 2 * SIN(A!), .8, .8, .8, 0, 0
NEXT k%
Shape(2).Texture = 13
Shape(3).Texture = 14
Shape(4).Texture = 15
Shape(5).Texture = 12

Init.Object "Fl", 0, 0, 0, 0, 1, 1, 0, 0
Shape(6).Texture = 13
END SUB

SUB Init.Example5
' Stormy Sea
Init.Space 5, 10, 5, .6, 40, 30
Init.Object "Sk", 260, 0, 0, 20, 0, 0, 0, 0
Init.Object "Fl", 190, 0, 0, 0, 1, 1, 0, 0
Init.Object "HC", 0, 0, 0, .7, .5, .2, -30, -15
Shape(2).Texture = 30
Shape(3).Texture = 20
END SUB

SUB Init.Example6
' Dolmen
Init.Space 10, 8, 4.9, .6, 40, 30
Init.Object "Fl", 120, 0, 0, 0, 2, 2, 0, 0
Shape(1).Texture = 30
Shape(1).Reflect = .3
Init.Object "Sk", 190, 0, 0, 20, 0, 0, 0, 0

Scale = 1.25
Init.Object "T1", 120, 0, 0, 1.6 * Scale, 1.2 * Scale, .2 * Scale, 0, 0
Init.Object "El", 120, Scale, Scale, .5 * Scale, .3 * Scale, .7 * Scale, 0, 0
Init.Object "El", 120, Scale, -Scale, .5 * Scale, .3 * Scale, .7 * Scale, 0, 0
Init.Object "El", 120, -Scale, Scale, .5 * Scale, .3 * Scale, .7 * Scale, 0, 0
Init.Object "El", 120, -Scale, -Scale, .5 * Scale, .3 * Scale, .7 * Scale, 0, 0

Scale = 2
Init.Object "El", 120, 2.5, -1.4, .5 * Scale, .2 * Scale, .7 * Scale, 0, 0
Scale = 1.25
Init.Object "El", 120, 3, 2, .5 * Scale, .3 * Scale, .7 * Scale, 0, 0

FOR k% = 3 TO NbObjects%
Shape(k%).SatRef = .5
Shape(k%).Bump = 1
Shape(k%).Reflect = .2
Shape(k%).Brilliance = .4
NEXT k%

END SUB

SUB Init.Example7
' MandelChrist
' Some comments are necessary
' This pic does not want to hurt nore shock anybody
' The one who would laugh or even smile after it can only be a stupid *#\
' Let's say I believe in Jesus, cause he was a man of faith in other men
' I just don't want this world to end in a sea of blood
' Hope this pic can help
' .

Init.Space -30, 20, 5, .8, 25, 40
Init.Object "Gm", 300, .4, 0, .81, 0, 0, 0, 0
Shape(1).Reflect = 0
Shape(1).Brilliance = .5
Init.Object "Sk", 240, 0, 0, 20, 0, 0, 0, 0
Init.Object "Fl", 0, 0, 0, 0, 1, 1, 0, 0
Shape(3).Texture = 30
CrossHue = 60
Init.Object "P2", CrossHue, .4, 0, 0, .4, 2.5, 0, 0    'Vertical bar, front
Init.Object "P2", CrossHue, .4, 0, 1, 2, .4, 0, 0      'Horizontal bar, front
Init.Object "P2", CrossHue, -.4, 0, 0, .4, 2.5, 0, 0   'Vertical bar, rear
Init.Object "P2", CrossHue, -.4, 0, 1, 2, .4, 0, 0     'Horizontal bar, rear
Init.Object "P1", CrossHue, 0, 0, 1.4, .4, 2, 0, 0     'Horizontal bar, top
Init.Object "P1", CrossHue, 0, 0, .6, .4, 2, 0, 0      'Horizontal bar, bottom
Init.Object "P3", CrossHue, 0, -.4, 0, .4, 2.5, 0, 0   'Vertical bar, left
Init.Object "P3", CrossHue, 0, .4, 0, .4, 2.5, 0, 0    'Vertical bar, right
Init.Object "P3", CrossHue, 0, -2, 1, .4, .4, 0, 0     'Horizontal bar, left
Init.Object "P3", CrossHue, 0, 2, 1, .4, .4, 0, 0      'Horizontal bar, right

FOR k% = 4 TO NbObjects%
Shape(k%).Bump = 2
Shape(k%).Reflect = 0
Shape(k%).Brilliance = .4
Shape(k%).SatRef = 200 / 3
NEXT k%
END SUB

SUB Init.Example8
' Bike Mirrors
Init.Space 0, 15, 5, .8, 40, 30
Init.Object "C0", 30, 0, 0, 0, 1, 8, -20, -20
Shape(1).Texture = 12
Init.Object "Q4", 240, 1.6, 0, .8, 1, .5, 20, -30
Shape(2).Texture = 16
END SUB

SUB Init.Example9
' Water Effect
Init.Space 0, 15, 5, .8, 30, 60
Init.Object "Fl", 240, 0, 0, 0, 1, 1, 0, 0
Init.Object "Bl", 240, 0, 0, 1, 1, 1, 0, 0
Shape(1).Bump = 3
END SUB

SUB Init.Moebius (Id%, _Type$, HueRef, x0!, y0!, z0!, dRef1, dRef2, Alpha, Beta)

Shape(Id%).Degree = 0
Shape(Id%).ShapeType = _Type$
Shape(Id%).HueRef = HueRef: Shape(Id%).SatRef = 200 / 3
Shape(Id%).dRef1 = dRef1: Shape(Id%).dRef2 = dRef2
Shape(Id%).x0 = x0!: Shape(Id%).y0 = y0!: Shape(Id%).z0 = z0!
Shape(Id%).Brilliance = 1
Shape(Id%).Reflect = cReflect

Matrix(Id%, 1, 1) = CosD(Beta) * CosD(Alpha)
Matrix(Id%, 1, 2) = CosD(Beta) * SinD(Alpha)
Matrix(Id%, 1, 3) = SinD(Beta)
Matrix(Id%, 2, 1) = -SinD(Alpha)
Matrix(Id%, 2, 2) = CosD(Alpha)
Matrix(Id%, 2, 3) = 0
Matrix(Id%, 3, 1) = -SinD(Beta) * CosD(Alpha)
Matrix(Id%, 3, 2) = -SinD(Beta) * SinD(Alpha)
Matrix(Id%, 3, 3) = CosD(Beta)

END SUB
'                  "Fl", 240,     0,    0,    0,       1,    1,    0,    0
SUB Init.Object (_Type$, HueRef, Px0!, Py0!, Pz0!, dRef1, dRef2, Alpha, Beta)
' Select the proper routine to create the object
' Increment number of objects at each call

NbObjects% = NbObjects% + 1
SELECT CASE _Type$
CASE "Fl", "P1", "P2", "P3", "D1", "D2", "D3", "Sk", "Se", "Gm"
Init.Plan NbObjects%, _Type$, HueRef, Px0!, Py0!, Pz0!, dRef1, dRef2, Alpha, Beta
CASE "Bl", "Sp", "El", "Cy", "Cn", "Co", "H1", "H2", "Hp", "Pa", "Cp"
Init.Quadric NbObjects%, _Type$, HueRef, Px0!, Py0!, Pz0!, dRef1, dRef2, Alpha, Beta
CASE "C0", "C1"
Init.Cubic NbObjects%, _Type$, HueRef, Px0!, Py0!, Pz0!, dRef1, dRef2, Alpha, Beta
CASE "T0", "T1", "HC", "Q0", "Q1", "Q2", "Q3", "Q4", "S2", "S3", "L0"
Init.Quartic NbObjects%, _Type$, HueRef, Px0!, Py0!, Pz0!, dRef1, dRef2, Alpha, Beta

CASE ELSE
SetText
PRINT "Non valid object type: "; _Type$
WaitKey
EndProg
END SELECT
END SUB

SUB Init.Perlin
' Inits the Noise% array for 2D perlin noises
' The pattern is perfectly tileable
' Based on squares division technique
' Fast, but can probably be optimised more:
' 2^n+1 can probably be replaced by plain 2^n via AND or MOD tricks...

RANDOMIZE TIMER: Seed& = 103476 * rnd2()    ' This number fell from my keyboard :-)
Octaves% = 7: Persistence! = 1.5: Span% = 2 ^ Octaves% + 1

FOR k% = 0 TO Octaves% + 1
_Amplitude!(k%) = (1 / Persistence!) ^ k%  ' You can try different logics!
NEXT k%

' Remark: the Init corners and edges lines make the pattern tileable

' Init the corners
Noise%(1, 1) = 128
Noise%(1, Span%) = 128
Noise%(Span%, 1) = 128
Noise%(Span%, Span%) = 128

' Init the edges
FOR Rank% = 1 TO Octaves%

Grid% = 2 ^ (Octaves% - Rank% + 1)
nStep% = 2 ^ (Rank% - 1) + 1

FOR kx% = 1 TO nStep% - 1
x% = (kx% - 1) * Grid% + 1: y% = 1
Alt% = (Noise%(x%, y%) + Noise%(x% + Grid%, y%)) / 2
zNew% = INT(Alt% * (1 + (rnd2() - .5) * _Amplitude!(Rank%)))
Noise%(x% + Grid% / 2, 1) = zNew%
Noise%(x% + Grid% / 2, Span%) = zNew%
NEXT kx%

FOR ky% = 1 TO nStep% - 1
x% = 1: y% = (ky% - 1) * Grid% + 1
Alt% = (Noise%(x%, y%) + Noise%(x%, y% + Grid%)) / 2
zNew% = INT(Alt% * (1 + (rnd2() - .5) * _Amplitude!(Rank%)))
Noise%(1, y% + Grid% / 2) = zNew%
Noise%(Span%, y% + Grid% / 2) = zNew%
NEXT ky%

NEXT Rank%

' Now, the routine really becomes random...
' Fill the array
FOR Rank% = 1 TO Octaves%

Grid% = 2 ^ (Octaves% - Rank% + 1)
nStep% = 2 ^ (Rank% - 1) + 1

FOR kx% = 1 TO nStep% - 1
x% = (kx% - 1) * Grid% + 1
FOR ky% = 1 TO nStep% - 1
y% = (ky% - 1) * Grid% + 1

Alt% = (Noise%(x%, y%) + Noise%(x% + Grid%, y%) + Noise%(x%, y% + Grid%) + Noise%(x% + Grid%, y% + Grid%)) / 4
Noise%(x% + Grid% / 2, y% + Grid% / 2) = INT(Alt% * (1 + (rnd2() - .5) * _Amplitude!(Rank%)))

Alt% = (Noise%(x%, y%) + Noise%(x% + Grid%, y%)) / 2
IF y% <> 1 THEN Noise%(x% + Grid% / 2, y%) = INT(Alt% * (1 + (rnd2() - .5) * _Amplitude!(Rank%)))

Alt% = (Noise%(x%, y%) + Noise%(x%, y% + Grid%)) / 2
IF x% <> 1 THEN Noise%(x%, y% + Grid% / 2) = INT(Alt% * (1 + (rnd2() - .5) * _Amplitude!(Rank%)))

Alt% = (Noise%(x% + Grid%, y%) + Noise%(x% + Grid%, y% + Grid%)) / 2
IF (x% + Grid%) <> Span% THEN Noise%(x% + Grid%, y% + Grid% / 2) = INT(Alt% * (1 + (rnd2() - .5) * _Amplitude!(Rank%)))

Alt% = (Noise%(x%, y% + Grid%) + Noise%(x% + Grid%, y% + Grid%)) / 2
IF (y% + Grid%) <> Span% THEN Noise%(x% + Grid% / 2, y% + Grid%) = INT(Alt% * (1 + (rnd2() - .5) * _Amplitude!(Rank%)))

NEXT ky%
NEXT kx%

NEXT Rank%

'for x%=0 to 255
'  for y%=0 to 255
'   z=perlin!((x%-128)/128,(y%-128)/128)*200
'   pset(x%,y%), rgb(z!,z!,z!)
'  next
'next
'sleep

END SUB

SUB Init.Plan (Id%, _Type$, HueRef, Px0!, Py0!, Pz0!, dRef1, dRef2, Alpha, Beta)
' A plan is nothing but 4 figures, i.e. a normal vector and a shift
' I found it handy to have vertical and horizontal plans,
' and then to rotate them at will
' Check an application at Example 8...
' and please don't be shocked by the result !

SELECT CASE _Type$
CASE "Fl": kx = 0: ky = 0: KZ = 1: K0 = 0: Shape(Id%).NumType = 100
CASE "P1": kx = 0: ky = 0: KZ = 1: K0 = 0: Shape(Id%).NumType = 101
CASE "P2": kx = 1: ky = 0: KZ = 0: K0 = 0: Shape(Id%).NumType = 102
CASE "P3": kx = 0: ky = 1: KZ = 0: K0 = 0: Shape(Id%).NumType = 103
CASE "D1": kx = 0: ky = 0: KZ = 1: K0 = 0: Shape(Id%).NumType = 104
CASE "D2": kx = 1: ky = 0: KZ = 0: K0 = 0: Shape(Id%).NumType = 105
CASE "D3": kx = 0: ky = 1: KZ = 0: K0 = 0: Shape(Id%).NumType = 106
CASE "Sk": kx = 0: ky = 0: KZ = 1: K0 = 0: Shape(Id%).NumType = 110
CASE "Gm": kx = 1: ky = 0: KZ = 0: K0 = 0: Shape(Id%).NumType = 120

CASE ELSE
EXIT SUB
END SELECT

FOR k% = 1 TO 31: Object(Id%, k%) = 0: NEXT k%
Object(Id%, 32) = kx: Object(Id%, 33) = ky: Object(Id%, 34) = KZ: Object(Id%, 35) = K0

Shape(Id%).Degree = 1
Shape(Id%).ShapeType = _Type$
Shape(Id%).HueRef = HueRef: Shape(Id%).SatRef = 200 / 3
Shape(Id%).dRef1 = dRef1: Shape(Id%).dRef2 = dRef2
Shape(Id%).x0 = Px0!: Shape(Id%).y0 = Py0!: Shape(Id%).z0 = Pz0!
Shape(Id%).Brilliance = 1
Shape(Id%).Reflect = cReflect

' Sky is such a strange buddy...
IF Shape(Id%).NumType = 110 THEN Shape(Id%).Reflect = 0
IF Shape(Id%).NumType = 110 THEN Shape(Id%).Texture = 31

' Now you can rotate the plan:
Matrix(Id%, 1, 1) = CosD(Beta) * CosD(Alpha)
Matrix(Id%, 1, 2) = CosD(Beta) * SinD(Alpha)
Matrix(Id%, 1, 3) = SinD(Beta)
Matrix(Id%, 2, 1) = -SinD(Alpha)
Matrix(Id%, 2, 2) = CosD(Alpha)
Matrix(Id%, 2, 3) = 0
Matrix(Id%, 3, 1) = -SinD(Beta) * CosD(Alpha)
Matrix(Id%, 3, 2) = -SinD(Beta) * SinD(Alpha)
Matrix(Id%, 3, 3) = CosD(Beta)
END SUB

SUB Init.Quadric (Id%, _Type$, HueRef, x0!, y0!, z0!, dRef1, dRef2, Alpha, Beta)
' Well, that's spheres, ellipsoids, hyperboloids, and all this kind of things!

SELECT CASE _Type$

CASE "Bl"                    ' Plain Ball
Shape(Id%).NumType = 200
KX2 = 1: KY2 = 1: KZ2 = 1
KXY = 0: KYZ = 0: KXZ = 0
kx = 0: ky = 0: KZ = 0
K0 = -dRef1 ^ 2

CASE "Sp"                    ' Sphere
Shape(Id%).NumType = 201
KX2 = 1: KY2 = 1: KZ2 = 1
KXY = 0: KYZ = 0: KXZ = 0
kx = 0: ky = 0: KZ = 0
K0 = -dRef1 ^ 2

CASE "El"                    ' Ellipso‹d
Shape(Id%).NumType = 202
KX2 = 1: KY2 = 1: KZ2 = (dRef1 / dRef2) ^ 2
KXY = 0: KYZ = 0: KXZ = 0
kx = 0: ky = 0: KZ = 0
K0 = -dRef1 ^ 2

CASE "Cp"                    ' Cup
Shape(Id%).NumType = 210
KX2 = 1: KY2 = 1: KZ2 = (dRef1 / dRef2) ^ 2
KXY = 0: KYZ = 0: KXZ = 0
kx = 0: ky = 0: KZ = 0
K0 = -dRef1 ^ 2

CASE "Cy"                    ' Cylinder
Shape(Id%).NumType = 203
KX2 = 1: KY2 = 1: KZ2 = 0
KXY = 0: KYZ = 0: KXZ = 0
kx = 0: ky = 0: KZ = 0
K0 = -dRef1 ^ 2

CASE "Co"                    ' Cone
Shape(Id%).NumType = 204
KX2 = 1: KY2 = 1: KZ2 = -(dRef1 / dRef2) ^ 2
KXY = 0: KYZ = 0: KXZ = 0
kx = 0: ky = 0: KZ = 0
K0 = 0

CASE "Cn"                    ' Half-Cone
Shape(Id%).NumType = 205
KX2 = 1: KY2 = 1: KZ2 = -(dRef1 / dRef2) ^ 2
KXY = 0: KYZ = 0: KXZ = 0
kx = 0: ky = 0: KZ = 0
K0 = 0

CASE "Hp"                    ' Hyperboloid paraboloid
Shape(Id%).NumType = 206
KX2 = 1: KY2 = -1: KZ2 = 0
KXY = 0: KYZ = 0: KXZ = 0
kx = 0: ky = 0: KZ = 2 * dRef1
K0 = 0

CASE "H1"                    ' Hyperbolo‹d, one sheet
Shape(Id%).NumType = 207
KX2 = 1: KY2 = 1: KZ2 = -(dRef1 / dRef2) ^ 2
KXY = 0: KYZ = 0: KXZ = 0
kx = 0: ky = 0: KZ = 0
K0 = -dRef1 ^ 2

CASE "H2"                    ' Hyperbolo‹d, two sheets
Shape(Id%).NumType = 208
KX2 = 1: KY2 = 1: KZ2 = -(dRef1 / dRef2) ^ 2
KXY = 0: KYZ = 0: KXZ = 0
kx = 0: ky = 0: KZ = 0
K0 = dRef1 ^ 2 / 4

CASE "Pa"                    ' Parabolo‹d
Shape(Id%).NumType = 209
KX2 = 1: KY2 = 1: KZ2 = 0
KXY = 0: KYZ = 0: KXZ = 0
kx = 0: ky = 0: KZ = -dRef2
K0 = dRef1

CASE ELSE
EXIT SUB
END SELECT

FOR k% = 1 TO 25: Object(Id%, k%) = 0: NEXT k%
Object(Id%, 26) = KX2: Object(Id%, 27) = KY2: Object(Id%, 28) = KZ2
Object(Id%, 29) = KXY: Object(Id%, 30) = KYZ: Object(Id%, 31) = KXZ
Object(Id%, 32) = kx: Object(Id%, 33) = ky: Object(Id%, 34) = KZ
Object(Id%, 35) = K0
Shape(Id%).Degree = 2
Shape(Id%).ShapeType = _Type$
Shape(Id%).HueRef = HueRef: Shape(Id%).SatRef = 200 / 3
Shape(Id%).dRef1 = dRef1: Shape(Id%).dRef2 = dRef2
Shape(Id%).x0 = x0!: Shape(Id%).y0 = y0!: Shape(Id%).z0 = z0!
Shape(Id%).Brilliance = 1
Shape(Id%).Reflect = cReflect

Matrix(Id%, 1, 1) = CosD(Beta) * CosD(Alpha)
Matrix(Id%, 1, 2) = CosD(Beta) * SinD(Alpha)
Matrix(Id%, 1, 3) = SinD(Beta)
Matrix(Id%, 2, 1) = -SinD(Alpha)
Matrix(Id%, 2, 2) = CosD(Alpha)
Matrix(Id%, 2, 3) = 0
Matrix(Id%, 3, 1) = -SinD(Beta) * CosD(Alpha)
Matrix(Id%, 3, 2) = -SinD(Beta) * SinD(Alpha)
Matrix(Id%, 3, 3) = CosD(Beta)

END SUB

SUB Init.Quartic (Id%, _Type$, HueRef, x0!, y0!, z0!, dRef1, dRef2, Alpha, Beta)
' That's the land of strange things...

SELECT CASE _Type$

CASE "HC"                             ' Hollow Cube
Shape(Id%).NumType = 444
KX4 = 1: KY4 = 1: KZ4 = 1
KX2 = -10 * dRef1 ^ 2
KY2 = KX2: KZ2 = KX2
K0 = 40 * dRef1 ^ 4

CASE "T0"                             ' Classical Torus
Shape(Id%).NumType = 400
KX4 = 1: KY4 = 1: KZ4 = 1
KX2Y2 = 2: KX2Z2 = 2: KY2Z2 = 2
KX2 = -2 * (dRef2 ^ 2 + dRef1 ^ 2)
KY2 = -2 * (dRef2 ^ 2 + dRef1 ^ 2)
KZ2 = 2 * (dRef1 ^ 2 - dRef2 ^ 2)
K0 = (dRef1 ^ 2 - dRef2 ^ 2) ^ 2

CASE "T1"                             ' Bulbeous donut
Shape(Id%).NumType = 401
KX4 = 1: KY4 = 1: KZ4 = 1
KX2Y2 = 1: KX2Z2 = 1: KY2Z2 = 1
KX2 = -2 * ((dRef1 / 5) ^ 2 + dRef1 ^ 2)
KY2 = -2 * ((dRef1 / 5) ^ 2 + dRef1 ^ 2)
KZ2 = 2 * (dRef1 ^ 2 - dRef2 ^ 2)
K0 = (dRef1 ^ 2 - dRef2 ^ 2) ^ 2

CASE "Q0"
Shape(Id%).NumType = 410
F1 = CQ1 * dRef2 ^ 2
F2 = CQ2 * dRef2 / dRef1
F3 = CQ3 * 4 * (dRef2 / dRef1) ^ 2

KX4 = 1 + F3 ^ 2
KY4 = 1 + F3 ^ 2
KZ4 = 1
KX3Y = -2 * F2 + 2 * F2 * F3
KY3X = -2 * F2 - 2 * F2 * F3
KZ2XY = -2 * F2
KX2Y2 = 2 + F2 ^ 2 - 2 * F3 ^ 2
KX2Z2 = 2
KY2Z2 = 2
KX2 = -2 * dRef1 ^ 2 - 2 * F1 + 2 * F1 * F3
KY2 = -2 * dRef1 ^ 2 - 2 * F1 - 2 * F1 * F3
KZ2 = 2 * dRef1 ^ 2 - 2 * F1
KXY = -2 * F2 * dRef1 ^ 2 + 2 * F1 * F2
K0 = dRef1 ^ 4 - 2 * F1 * dRef1 ^ 2 + F1 ^ 2


CASE "Q1"                             ' Orange slices
Shape(Id%).NumType = 411
F1 = 0
F2 = dRef2 / dRef1
F3 = 4 * (dRef2 / dRef1) ^ 2

KX4 = 1 + F3 ^ 2
KY4 = 1 + F3 ^ 2
KZ4 = 1
KX3Y = -2 * F2 + 2 * F2 * F3
KY3X = -2 * F2 - 2 * F2 * F3
'KZ2XY = -2 * F2
KX2Y2 = 2 + F2 ^ 2 - 2 * F3 ^ 2
KX2Z2 = 2
KY2Z2 = 2
KX2 = -2 * dRef1 ^ 2 - 2 * F1 + 2 * F1 * F3
KY2 = -2 * dRef1 ^ 2 - 2 * F1 - 2 * F1 * F3
KZ2 = 2 * dRef1 ^ 2 - 2 * F1
KXY = -2 * F2 * dRef1 ^ 2 + 2 * F1 * F2
K0 = dRef1 ^ 4 - 2 * F1 * dRef1 ^ 2 + F1 ^ 2

CASE "Q2"                             ' Two parts cushion
Shape(Id%).NumType = 412
F1 = dRef2 ^ 2
F2 = dRef2 / dRef1
F3 = 0

KX4 = 1 + F3 ^ 2
KY4 = 1 + F3 ^ 2
KZ4 = 1
KX3Y = -2 * F2 + 2 * F2 * F3
KY3X = -2 * F2 - 2 * F2 * F3
'KZ2XY = -2 * F2
KX2Y2 = 2 + F2 ^ 2 - 2 * F3 ^ 2
KX2Z2 = 2
KY2Z2 = 2
KX2 = -2 * dRef1 ^ 2 - 2 * F1 + 2 * F1 * F3
KY2 = -2 * dRef1 ^ 2 - 2 * F1 - 2 * F1 * F3
KZ2 = 2 * dRef1 ^ 2 - 2 * F1
KXY = -2 * F2 * dRef1 ^ 2 + 2 * F1 * F2
K0 = dRef1 ^ 4 - 2 * F1 * dRef1 ^ 2 + F1 ^ 2

CASE "Q3"                             ' Feminine breast
Shape(Id%).NumType = 413
F1 = dRef2 ^ 2
F2 = 0
F3 = 4 * (dRef2 / dRef1) ^ 2

KX4 = 1 + F3 ^ 2
KY4 = 1 + F3 ^ 2
KZ4 = 1
KX3Y = -2 * F2 + 2 * F2 * F3
KY3X = -2 * F2 - 2 * F2 * F3
'KZ2XY = -2 * F2
KX2Y2 = 2 + F2 ^ 2 - 2 * F3 ^ 2
KX2Z2 = 2
KY2Z2 = 2
KX2 = -2 * dRef1 ^ 2 - 2 * F1 + 2 * F1 * F3
KY2 = -2 * dRef1 ^ 2 - 2 * F1 - 2 * F1 * F3
KZ2 = 2 * dRef1 ^ 2 - 2 * F1
KXY = -2 * F2 * dRef1 ^ 2 + 2 * F1 * F2
K0 = dRef1 ^ 4 - 2 * F1 * dRef1 ^ 2 + F1 ^ 2

CASE "Q4"                             ' Bike mirrors
Shape(Id%).NumType = 414
F1 = dRef2 ^ 2
F2 = dRef2 / dRef1
F3 = 4 * (dRef2 / dRef1) ^ 2

KX4 = 1 + F3 ^ 2
KY4 = 1 + F3 ^ 2
KZ4 = 1
KX3Y = -2 * F2 + 2 * F2 * F3
KY3X = -2 * F2 - 2 * F2 * F3
'KZ2XY = -2 * F2
KX2Y2 = 2 + F2 ^ 2 - 2 * F3 ^ 2
KX2Z2 = 2
KY2Z2 = 2
KX2 = -2 * dRef1 ^ 2 - 2 * F1 + 2 * F1 * F3
KY2 = -2 * dRef1 ^ 2 - 2 * F1 - 2 * F1 * F3
KZ2 = 2 * dRef1 ^ 2 - 2 * F1
KXY = -2 * F2 * dRef1 ^ 2 + 2 * F1 * F2
K0 = dRef1 ^ 4 - 2 * F1 * dRef1 ^ 2 + F1 ^ 2


CASE "S2"                             ' Double sphere
Shape(Id%).NumType = 420
KX4 = 1: KY4 = 1: KZ4 = 1
KX2Y2 = 2: KX2Z2 = 2: KY2Z2 = 2
KX2 = -2 * (dRef1 ^ 2 + dRef2 ^ 2)
KY2 = 2 * (-dRef1 ^ 2 + dRef2 ^ 2)
KZ2 = 2 * (-dRef1 ^ 2 + dRef2 ^ 2)
K0 = (dRef1 ^ 2 - dRef2 ^ 2) ^ 2

CASE "S3"                             ' Pelote de laine
Shape(Id%).NumType = 421
KX4 = 1: KY4 = 1: KZ4 = 1
KX2Y2 = 1: KX2Z2 = 1: KY2Z2 = 1
KX2 = -2 * (dRef1 ^ 2 + dRef2 ^ 2)
KY2 = 2 * (-dRef1 ^ 2 + dRef2 ^ 2)
KZ2 = 2 * (-dRef1 ^ 2 + dRef2 ^ 2)
K0 = (dRef1 ^ 2 - dRef2 ^ 2) ^ 2

CASE "L0"                             ' Lemniscate
Shape(Id%).NumType = 422
KX4 = 1: KY4 = 1: KZ4 = 1
KX2Y2 = 2: KX2Z2 = 2: KY2Z2 = 2
KX2 = -2 * (dRef1 ^ 2 + dRef2 ^ 2)
KY2 = 4 * (dRef1 ^ 2 - dRef2 ^ 2)
KZ2 = 4 * (dRef1 ^ 2 - dRef2 ^ 2)
K0 = 0

CASE ELSE
EXIT SUB
END SELECT

Object(Id%, 1) = KX4: Object(Id%, 2) = KY4: Object(Id%, 3) = KZ4
Object(Id%, 4) = KX3Y: Object(Id%, 5) = KX3Z: Object(Id%, 6) = KY3Z
Object(Id%, 7) = KY3X: Object(Id%, 8) = KZ3X: Object(Id%, 9) = KZ3Y
Object(Id%, 10) = KX2Y2: Object(Id%, 11) = KX2Z2: Object(Id%, 12) = KY2Z2
Object(Id%, 13) = KX2YZ: Object(Id%, 14) = KY2XZ: Object(Id%, 15) = KZ2XY
Object(Id%, 16) = KXYZ
Object(Id%, 17) = KX3: Object(Id%, 18) = KY3: Object(Id%, 19) = KZ3
Object(Id%, 20) = KX2Y: Object(Id%, 21) = KX2Z: Object(Id%, 22) = KY2Z
Object(Id%, 23) = KY2X: Object(Id%, 24) = KZ2X: Object(Id%, 25) = KZ2Y
Object(Id%, 26) = KX2: Object(Id%, 27) = KY2: Object(Id%, 28) = KZ2
Object(Id%, 29) = KXY: Object(Id%, 30) = KYZ: Object(Id%, 31) = KXZ
Object(Id%, 32) = kx: Object(Id%, 33) = ky: Object(Id%, 34) = KZ
Object(Id%, 35) = K0

Shape(Id%).Degree = 4
Shape(Id%).ShapeType = _Type$
Shape(Id%).HueRef = HueRef: Shape(Id%).SatRef = 200 / 3
Shape(Id%).dRef1 = dRef1: Shape(Id%).dRef2 = dRef2
Shape(Id%).x0 = x0!: Shape(Id%).y0 = y0!: Shape(Id%).z0 = z0!
Shape(Id%).Brilliance = 1
Shape(Id%).Reflect = cReflect

Matrix(Id%, 1, 1) = CosD(Beta) * CosD(Alpha)
Matrix(Id%, 1, 2) = CosD(Beta) * SinD(Alpha)
Matrix(Id%, 1, 3) = SinD(Beta)
Matrix(Id%, 2, 1) = -SinD(Alpha)
Matrix(Id%, 2, 2) = CosD(Alpha)
Matrix(Id%, 2, 3) = 0
Matrix(Id%, 3, 1) = -SinD(Beta) * CosD(Alpha)
Matrix(Id%, 3, 2) = -SinD(Beta) * SinD(Alpha)
Matrix(Id%, 3, 3) = CosD(Beta)

END SUB
'
':::::::::::::::
SUB Init.Space (AlphaCam, BetaCam, DistCam, DistScreen, aLight, bLight)

' Init the 2D Perlin noise array
Init.Perlin

' Shadow and Reflection control coefficients
cShadow = .3: cReflect = .5

' Point of light
rLight = 10
xLight(1) = rLight * CosD(bLight) * CosD(aLight)
xLight(2) = rLight * CosD(bLight) * SinD(aLight)
xLight(3) = rLight * SinD(bLight)

' Camera
aCam = AlphaCam: bCam = BetaCam: rCam = DistCam
xCam(1) = rCam * CosD(bCam) * CosD(aCam)
xCam(2) = rCam * CosD(bCam) * SinD(aCam)
xCam(3) = rCam * SinD(bCam)

' Screen center spatial position
dScreen = DistScreen: aLook = 180 + aCam: bLook = -bCam
xCenter(1) = xCam(1) + dScreen * CosD(bLook) * CosD(aLook)
xCenter(2) = xCam(2) + dScreen * CosD(bLook) * SinD(aLook)
xCenter(3) = xCam(3) + dScreen * SinD(bLook)

' Screen horizontal axis (rightwards)
UScreen(1) = SinD(aLook): UScreen(2) = -CosD(aLook): UScreen(3) = 0
' Screen vertical axis (downwards)
VScreen(1) = SinD(bLook) * CosD(aLook): VScreen(2) = SinD(bLook) * SinD(aLook): VScreen(3) = -CosD(bLook)

END SUB
'
':::::::::::::::
FUNCTION Julia (p, q)
' Julia pattern
' p and q range from -1 to +1
Scale = 2
x = p * Scale: y = q * Scale
FOR n% = 1 TO 20
xn = x * x - y * y + .3
yn = 2 * x * y - .5
IF (xn * xn + yn * yn) > 4 THEN Julia = 0: EXIT FUNCTION
xo = x: yo = y: x = xn: y = yn
NEXT n%
dx = xo - x: dy = yo - y
Julia = 120 * LOG((dx * dx + dy * dy) + eps) - 240
END FUNCTION
'
':::::::::::::::
FUNCTION Light.Specular (xp(), _xo!())
' Returns the brilliance to apply to a given point on a surface
' The point of reflection is Xp, the normal vector to the surface is Xo
DIM xi(3), xr(3), xv(3)
FOR i = 1 TO 3
xi(i) = xp(i) - xLight(i)
xv(i) = xCam(i) - xp(i): NEXT i
Coeff = 2 * (xi(1) * _xo!(1) + xi(2) * _xo!(2) + xi(3) * _xo!(3))
FOR i = 1 TO 3: xr(i) = xi(i) - Coeff * _xo!(i): NEXT i
NormProd = SQR((xi(1) * xi(1) + xi(2) * xi(2) + xi(3) * xi(3)) * (xv(1) * xv(1) + xv(2) * xv(2) + xv(3) * xv(3)))
Light.Specular = 50 + 50 * (xr(1) * xv(1) + xr(2) * xv(2) + xr(3) * xv(3)) / NormProd
END FUNCTION
'
':::::::::::::::
FUNCTION Mandelbrot (p, q)
' M-Set Pattern
' p and q range from -1 to +1
Scale = 4
zx0 = p * Scale: zy0 = q * Scale
x = zx0: y = zy0
FOR NP% = 1 TO 20
xn = x * x - y * y + zx0
yn = 2 * x * y + zy0
IF (xn * xn + yn * yn) > 4 THEN Mandelbrot = 120: EXIT FUNCTION
xo = x: yo = y: x = xn: y = yn
NEXT NP%
dx = xo - x: dy = yo - y: VarMdb = dx * dx + dy * dy
IF VarMdb > 0 THEN : Mandelbrot = 270 - 5 * LOG(VarMdb):  ELSE : Mandelbrot = 0
END FUNCTION
'
':::::::::::::::
FUNCTION Nephroid (x!, y!, Radius!)
Nephroid = (x! ^ 2 + y! ^ 2 - 4 * (Radius! / 4) ^ 2) ^ 3 - 108 * (Radius! / 4) ^ 4 * y! ^ 2
END FUNCTION
'
':::::::::::::::
FUNCTION Noise3! (A%, B%, C%)
' This controlled noise function is the basis of the 3D Perlin noise

Noise3! = rnd2(-rnd2(-A% * B%) * rnd2(-C%) * Seed&) - .5
END FUNCTION
'
':::::::::::::::
FUNCTION Perlin! (xp!, xq!)
' 2D Perlin Noise - Based on the Noise% array (shared variable)
' p and q range from -1 to +1

p! = xp! / 2 + .5
q! = xq! / 2 + .5
IF p! > 1 THEN p! = 1
IF q! > 1 THEN q! = 1
IF p! < 0 THEN p! = 0
IF q! < 0 THEN q! = 0

' ============================================
' Cosine interpolation within the noise array
x0% = INT(p! * (Span% - 1)) + 1: y0% = INT(q! * (Span% - 1)) + 1
dx! = p! * (Span% - 1) + 1 - x0%: dy! = q! * (Span% - 1) + 1 - y0%
cx! = COS(dx! * Pi / 2)
Cy! = COS(dy! * Pi / 2)

IF x0% = Span% AND y0% = Span% THEN Perlin! = Noise%(x0%, y0%) / 255: EXIT FUNCTION

IF x0% = Span% AND y0% < Span% THEN
z1! = Noise%(x0%, y0%)
z2! = Noise%(x0%, y0% + 1)
Perlin! = (z1! * Cy! + z2! * (1 - Cy!)) / 255
EXIT FUNCTION
END IF

IF y0% = Span% AND x0% < Span% THEN
z1! = Noise%(x0%, y0%)
z2! = Noise%(x0% + 1, y0%)
Perlin! = (z1! * cx! + z2! * (1 - cx!)) / 255
EXIT FUNCTION
END IF

z1! = Noise%(x0%, y0%)
z2! = Noise%(x0% + 1, y0%)
Perlin1! = z1! * cx! + z2! * (1 - cx!)
z3! = Noise%(x0%, y0% + 1)
z4! = Noise%(x0% + 1, y0% + 1)
Perlin2! = z3! * cx! + z4! * (1 - cx!)
Perlin! = (Perlin1! * Cy! + Perlin2! * (1 - Cy!)) / 255
END FUNCTION
'
':::::::::::::::
FUNCTION Perlin3D (xt!, yt!, zt!)
' Perlin 3D noise. The Perlin value is calculated for every point
' without heavy array storage. Slow response time,
' but it's the only solution without EMS...

x! = (xt! + 1) / 2 * Power2%(Octaves%) + 1
y! = (yt! + 1) / 2 * Power2%(Octaves%) + 1
z! = (zt! + 1) / 2 * Power2%(Octaves%) + 1

Rank% = 0
ax% = 1: bx% = Span%
ay% = 1: by% = Span%
az% = 1: bz% = Span%
z1% = 128: z2% = 128: z3% = 128: z4% = 128
z5% = 128: z6% = 128: z7% = 128: z8% = 128

DO UNTIL ((bx% - x!) <= 1 AND (x! - ax%) <= 1) AND ((by% - y!) <= 1 AND (y! - ay%) <= 1) AND ((bz% - z!) <= 1 AND (z! - az%) <= 1)
Rank% = Rank% + 1
Grid% = Power2%(Octaves% - Rank%)

IF bx% - x! > x! - ax% THEN
bx% = bx% - Grid%
    IF by% - y! > y! - ay% THEN
    by% = by% - Grid%
        IF bz% - z! > z! - az% THEN
        bz% = bz% - Grid%
        z1n% = z1%
        z2n% = (z1% + z2%) / 2
        z3n% = (z1% + z2% + z3% + z4%) / 4
        z4n% = (z1% + z4%) / 2
        z5n% = (z1% + z5%) / 2
        z6n% = (z1% + z2% + z5% + z6%) / 4
        z7n% = (z1% + z2% + z3% + z4% + z5% + z6% + z7% + z8%) / 8
        z8n% = (z1% + z4% + z5% + z8%) / 4
z1% = z1n%
z2% = z2n% * (1 + Noise3!(bx%, ay%, az%) * _Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, by%, az%) * _Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, by%, az%) * _Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, ay%, bz%) * _Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, ay%, bz%) * _Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, by%, bz%) * _Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, by%, bz%) * _Amplitude!(Rank%))

        ELSE
        az% = az% + Grid%
        z1n% = (z1% + z5%) / 2
        z2n% = (z1% + z2% + z5% + z6%) / 4
        z3n% = (z1% + z2% + z3% + z4% + z5% + z6% + z7% + z8%) / 8
        z4n% = (z1% + z4% + z5% + z8%) / 4
        z5n% = z5%
        z6n% = (z5% + z6%) / 2
        z7n% = (z5% + z6% + z7% + z8%) / 4
        z8n% = (z5% + z8%) / 2
z1% = z1n% * (1 + Noise3!(ax%, ay%, az%) * _Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, ay%, az%) * _Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, by%, az%) * _Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, by%, az%) * _Amplitude!(Rank%))
z5% = z5n%
z6% = z6n% * (1 + Noise3!(bx%, ay%, bz%) * _Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, by%, bz%) * _Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, by%, bz%) * _Amplitude!(Rank%))

        END IF
    ELSE
    ay% = ay% + Grid%
        IF bz% - z! > z! - az% THEN
        bz% = bz% - Grid%
        z1n% = (z1% + z4%) / 2
        z2n% = (z1% + z2% + z3% + z4%) / 4
        z3n% = (z3% + z4%) / 2
        z4n% = z4%
        z5n% = (z1% + z4% + z5% + z8%) / 4
        z6n% = (z1% + z2% + z3% + z4% + z5% + z6% + z7% + z8%) / 8
        z7n% = (z4% + z3% + z7% + z8%) / 4
        z8n% = (z4% + z8%) / 2
z1% = z1n% * (1 + Noise3!(ax%, ay%, az%) * _Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, ay%, az%) * _Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, by%, az%) * _Amplitude!(Rank%))
z4% = z4n%
z5% = z5n% * (1 + Noise3!(ax%, ay%, bz%) * _Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, ay%, bz%) * _Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, by%, bz%) * _Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, by%, bz%) * _Amplitude!(Rank%))
        ELSE
        az% = az% + Grid%
        z1n% = (z1% + z4% + z5% + z8%) / 4
        z2n% = (z1% + z2% + z3% + z4% + z5% + z6% + z7% + z8%) / 8
        z3n% = (z4% + z3% + z7% + z8%) / 4
        z4n% = (z4% + z8%) / 2
        z5n% = (z5% + z8%) / 2
        z6n% = (z5% + z6% + z7% + z8%) / 4
        z7n% = (z7% + z8%) / 2
        z8n% = z8%
z1% = z1n% * (1 + Noise3!(ax%, ay%, az%) * _Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, ay%, az%) * _Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, by%, az%) * _Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, by%, az%) * _Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, ay%, bz%) * _Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, ay%, bz%) * _Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, by%, bz%) * _Amplitude!(Rank%))
z8% = z8n%
        END IF
    END IF

ELSE
ax% = ax% + Grid%
    IF by% - y! > y! - ay% THEN
    by% = by% - Grid%
        IF bz% - z! > z! - az% THEN
        bz% = bz% - Grid%
        z1n% = (z1% + z2%) / 2
        z2n% = z2%
        z3n% = (z3% + z2%) / 2
        z4n% = (z1% + z2% + z3% + z4%) / 4
        z5n% = (z1% + z2% + z5% + z6%) / 4
        z6n% = (z6% + z2%) / 2
        z7n% = (z2% + z3% + z6% + z7%) / 4
        z8n% = (z1% + z2% + z3% + z4% + z5% + z6% + z7% + z8%) / 8
z1% = z1n% * (1 + Noise3!(ax%, ay%, az%) * _Amplitude!(Rank%))
z2% = z2n%
z3% = z3n% * (1 + Noise3!(bx%, by%, az%) * _Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, by%, az%) * _Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, ay%, bz%) * _Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, ay%, bz%) * _Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, by%, bz%) * _Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, by%, bz%) * _Amplitude!(Rank%))
        ELSE
        az% = az% + Grid%
        z1n% = (z1% + z2% + z5% + z6%) / 4
        z2n% = (z2% + z6%) / 2
        z3n% = (z2% + z3% + z6% + z7%) / 4
        z4n% = (z1% + z2% + z3% + z4% + z5% + z6% + z7% + z8%) / 8
        z5n% = (z5% + z6%) / 2
        z6n% = z6%
        z7n% = (z6% + z7%) / 2
        z8n% = (z5% + z6% + z7% + z8%) / 4
z1% = z1n% * (1 + Noise3!(ax%, ay%, az%) * _Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, ay%, az%) * _Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, by%, az%) * _Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, by%, az%) * _Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, ay%, bz%) * _Amplitude!(Rank%))
z6% = z6n%
z7% = z7n% * (1 + Noise3!(bx%, by%, bz%) * _Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, by%, bz%) * _Amplitude!(Rank%))
        END IF
    ELSE
    ay% = ay% + Grid%
        IF bz% - z! > z! - az% THEN
        bz% = bz% - Grid%
        z1n% = (z1% + z2% + z3% + z4%) / 4
        z2n% = (z3% + z2%) / 2
        z3n% = z3%
        z4n% = (z4% + z3%) / 2
        z5n% = (z1% + z2% + z3% + z4% + z5% + z6% + z7% + z8%) / 8
        z6n% = (z2% + z3% + z6% + z7%) / 4
        z7n% = (z3% + z7%) / 2
        z8n% = (z3% + z4% + z7% + z8%) / 4
z1% = z1n% * (1 + Noise3!(ax%, ay%, az%) * _Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, ay%, az%) * _Amplitude!(Rank%))
z3% = z3n%
z4% = z4n% * (1 + Noise3!(ax%, by%, az%) * _Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, ay%, bz%) * _Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, ay%, bz%) * _Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, by%, bz%) * _Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, by%, bz%) * _Amplitude!(Rank%))
        ELSE
        az% = az% + Grid%
        z1n% = (z1% + z2% + z3% + z4% + z5% + z6% + z7% + z8%) / 8
        z2n% = (z2% + z3% + z6% + z7%) / 4
        z3n% = (z3% + z7%) / 2
        z4n% = (z3% + z4% + z7% + z8%) / 4
        z5n% = (z5% + z6% + z7% + z8%) / 4
        z6n% = (z6% + z7%) / 2
        z7n% = z7%
        z8n% = (z7% + z8%) / 2
z1% = z1n% * (1 + Noise3!(ax%, ay%, az%) * _Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, ay%, az%) * _Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, by%, az%) * _Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, by%, az%) * _Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, ay%, bz%) * _Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, ay%, bz%) * _Amplitude!(Rank%))
z7% = z7n%
z8% = z8n% * (1 + Noise3!(ax%, by%, bz%) * _Amplitude!(Rank%))
        END IF
END IF

END IF
LOOP

zp1! = z1% * (bx% - x!) + z2% * (x! - ax%)
zp2! = z4% * (bx% - x!) + z3% * (x! - ax%)
zp12! = zp1! * (by% - y!) + zp2! * (y! - ay%)

zp3! = z5% * (bx% - x!) + z6% * (x! - ax%)
zp4! = z8% * (bx% - x!) + z7% * (x! - ax%)
zp34! = zp3! * (by% - y!) + zp4! * (y! - ay%)

zp! = zp12! * (bz% - z!) + zp34! * (z! - az%)


Perlin3D! = zp! / 255

END FUNCTION
'
':::::::::::::::
SUB Pixel.Point (Nx%, Ny%, x!, y!, z!)
DIM U(3)

U(1) = x! - xCam(1): U(2) = y! - xCam(2): U(3) = z! - xCam(3)

A1 = xCenter(1) - xCam(1) - xScrCenter% * UScreen(1) / ScrHeight - yScrCenter% * VScreen(1) / ScrHeight
B1 = UScreen(1) / ScrHeight
C1 = VScreen(1) / ScrHeight
A2 = xCenter(2) - xCam(2) - xScrCenter% * UScreen(2) / ScrHeight - yScrCenter% * VScreen(2) / ScrHeight
B2 = UScreen(2) / ScrHeight
C2 = VScreen(2) / ScrHeight
A3 = xCenter(3) - xCam(3) - xScrCenter% * UScreen(3) / ScrHeight - yScrCenter% * VScreen(3) / ScrHeight
B3 = UScreen(3) / ScrHeight
C3 = VScreen(3) / ScrHeight

AlphaX1 = B1 * U(2) - B2 * U(1)
AlphaY1 = C1 * U(2) - C2 * U(1)
Gamma1 = -A1 * U(2) + A2 * U(1)

AlphaX2 = B2 * U(3) - B3 * U(2)
AlphaY2 = C2 * U(3) - C3 * U(2)
Gamma2 = -A2 * U(3) + A3 * U(2)

Det = AlphaX1 * AlphaY2 - AlphaX2 * AlphaY1
Nx% = INT((Gamma1 * AlphaY2 - Gamma2 * AlphaY1) / Det + .5)
Ny% = INT((AlphaX1 * Gamma2 - AlphaX2 * Gamma1) / Det + .5)

END SUB
'
':::::::::::::::
SUB Probe.Ball (Id%, Ray(),Dist,xh!(),xt!(),_xn!(),FlagPlot%)
' Probe a plain ball with a ray
' Dist is the distance from the ray origin
' xh is the hit point in the object axis system
' xt is the hit point in the main axis system
' xn is the normal vector in the main axis system
' FlagPlot% is the hit indicator (0 when no intersection)

Rx0 = Ray(1) - Shape(Id%).x0
Ry0 = Ray(2) - Shape(Id%).y0
Rz0 = Ray(3) - Shape(Id%).z0
DP = Ray(4) * Ray(4) + Ray(5) * Ray(5) + Ray(6) * Ray(6)
DQ = 2 * (Rx0 * Ray(4) + Ry0 * Ray(5) + Rz0 * Ray(6))
DR = Rx0 * Rx0 + Ry0 * Ry0 + Rz0 * Rz0 + Object(Id%, 35)
Delta = DQ * DQ - 4 * DP * DR
IF Delta < 0 THEN FlagPlot% = 0: EXIT SUB
Dist = -(DQ + SQR(Delta)) * .5 / DP
IF Dist < Ray(7) THEN FlagPlot% = 0: EXIT SUB
xh!(1) = Rx0 + Ray(4) * Dist
xh!(2) = Ry0 + Ray(5) * Dist
xh!(3) = Rz0 + Ray(6) * Dist

FOR k% = 1 TO 3
xt!(k%) = Ray(k%) + Dist * Ray(k% + 3)
_xn!(k%) = xh!(k%) / Shape(Id%).dRef1
NEXT k%
FlagPlot% = 1
END SUB
'
':::::::::::::::
SUB Probe.Cone (Id%, Ray(), Dist, xh(), xt(), _xn!(), FlagPlot%)
' Probe a half cone with a ray
' Dist is the distance from the ray origin
' xh is the hit point in the object axis system
' xt is the hit point in the main axis system
' xn is the normal vector in the main axis system
' FlagPlot% is the hit indicator (0 when no intersection)

DIM xp!(2, 3), FlagPoint%(2), _xo!(3)
FlagPlot% = 0

'=======================================================
' Ray conversion to object axis system
Rx0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 1, 1)
Rx0 = Rx0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 1, 2)
Rx0 = Rx0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 1, 3)

Ry0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 2, 1)
Ry0 = Ry0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 2, 2)
Ry0 = Ry0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 2, 3)

Rz0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 3, 1)
Rz0 = Rz0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 3, 2)
Rz0 = Rz0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 3, 3)

Rx = Ray(4) * Matrix(Id%, 1, 1) + Ray(5) * Matrix(Id%, 1, 2) + Ray(6) * Matrix(Id%, 1, 3)
Ry = Ray(4) * Matrix(Id%, 2, 1) + Ray(5) * Matrix(Id%, 2, 2) + Ray(6) * Matrix(Id%, 2, 3)
Rz = Ray(4) * Matrix(Id%, 3, 1) + Ray(5) * Matrix(Id%, 3, 2) + Ray(6) * Matrix(Id%, 3, 3)

'=======================================================
' Intersection calculation in the object axis system
DP = Rx * Rx + Ry * Ry + Object(Id%, 28) * Rz * Rz
DQ = 2 * (Rx0 * Rx + Ry0 * Ry + Object(Id%, 28) * Rz0 * Rz)
DR = Rx0 * Rx0 + Ry0 * Ry0 + Rz0 * Rz0 * Object(Id%, 28)
Delta = DQ * DQ - 4 * DP * DR
IF Delta < 0 THEN FlagPlot% = 0: EXIT SUB
Dist1 = -(DQ + SQR(Delta)) * .5 / DP
Dist2 = (SQR(Delta) - DQ) * .5 / DP

'=======================================================
' Check the intersections are not behind the screen
FlagPoint%(1) = 1: FlagPoint%(2) = 1
IF Dist1 < Ray(7) THEN FlagPoint%(1) = 0
IF Dist2 < Ray(7) THEN FlagPoint%(2) = 0
IF FlagPoint%(1) = 0 AND FlagPoint%(2) = 0 THEN FlagPlot% = 0: EXIT SUB

xp!(1, 1) = Rx0 + Rx * Dist1: xp!(1, 2) = Ry0 + Ry * Dist1: xp!(1, 3) = Rz0 + Rz * Dist1
xp!(2, 1) = Rx0 + Rx * Dist2: xp!(2, 2) = Ry0 + Ry * Dist2: xp!(2, 3) = Rz0 + Rz * Dist2

'=======================================================
' Set limits to the shape
FOR k% = 1 TO 2
IF xp!(k%, 3) < 0 OR xp!(k%, 3) > Shape(Id%).dRef2 THEN FlagPoint%(k%) = 0
NEXT k%
IF FlagPoint%(1) = 0 AND FlagPoint%(2) = 0 THEN FlagPlot% = 0: EXIT SUB

'=======================================================
' Select the intersection point (object axis system)
IF FlagPoint%(1) = 1 THEN
xh!(1) = xp!(1, 1): xh!(2) = xp!(1, 2): xh!(3) = xp!(1, 3): Dist = Dist1: FlagPlot% = 1
ELSE
xh!(1) = xp!(2, 1): xh!(2) = xp!(2, 2): xh!(3) = xp!(2, 3): Dist = Dist2: FlagPlot% = 2
END IF

'=======================================================
' Calculate the normal vector
_xo!(1) = xh!(1)
_xo!(2) = xh!(2)
_xo!(3) = Object(Id%, 28) * xh!(3)
Side = Rx * _xo!(1) + Ry * _xo!(2) + Rz * _xo!(3)
IF Side < 0 THEN Side = -1:  ELSE : Side = 1
ModNorm = Side * Module(_xo!(1), _xo!(2), _xo!(3))
IF ModNorm = 0 THEN EXIT SUB
FOR k% = 1 TO 3: _xo!(k%) = _xo!(k%) / ModNorm: NEXT k%

'=======================================================
' Back to main axis system
FOR k% = 1 TO 3
xt!(k%) = Ray(k%) + Dist * Ray(k% + 3)
_xn!(k%) = _xo!(1) * Matrix(Id%, 1, k%) + _xo!(2) * Matrix(Id%, 2, k%) + _xo!(3) * Matrix(Id%, 3, k%)
NEXT k%


END SUB
'
':::::::::::::::
SUB Probe.Cubic (Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%)
' Probe a cubic shape with a ray
' Dist is the distance from the ray origin
' xh is the hit point in the object axis system
' xt is the hit point in the main axis system
' xn is the normal vector in the main axis system
' FlagPlot% is the hit indicator (0 when no intersection)

DIM xp!(3, 3), FlagPoint%(3)
FlagPlot% = 0

'=======================================================
' Ray conversion to object axis system
Rx0# = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 1, 1)
Rx0# = Rx0# + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 1, 2)
Rx0# = Rx0# + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 1, 3)

Ry0# = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 2, 1)
Ry0# = Ry0# + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 2, 2)
Ry0# = Ry0# + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 2, 3)

Rz0# = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 3, 1)
Rz0# = Rz0# + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 3, 2)
Rz0# = Rz0# + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 3, 3)

Rx# = Ray(4) * Matrix(Id%, 1, 1) + Ray(5) * Matrix(Id%, 1, 2) + Ray(6) * Matrix(Id%, 1, 3)
Ry# = Ray(4) * Matrix(Id%, 2, 1) + Ray(5) * Matrix(Id%, 2, 2) + Ray(6) * Matrix(Id%, 2, 3)
Rz# = Ray(4) * Matrix(Id%, 3, 1) + Ray(5) * Matrix(Id%, 3, 2) + Ray(6) * Matrix(Id%, 3, 3)

'=======================================================
' Intersection calculation in the object axis system

DP# = Object(Id%, 17) * Rx# * Rx# * Rx#
DP# = DP# + Object(Id%, 18) * Ry# * Ry# * Ry#
DP# = DP# + Object(Id%, 19) * Rz# * Rz# * Rz#
DP# = DP# + Object(Id%, 20) * Rx# * Rx# * Ry#
DP# = DP# + Object(Id%, 21) * Rx# * Rx# * Rz#
DP# = DP# + Object(Id%, 23) * Ry# * Ry# * Rx#
DP# = DP# + Object(Id%, 22) * Ry# * Ry# * Rz#
DP# = DP# + Object(Id%, 24) * Rz# * Rz# * Rx#
DP# = DP# + Object(Id%, 25) * Rz# * Rz# * Ry#
DP# = DP# + Object(Id%, 16) * Rx# * Ry# * Rz#

DQ# = DQ# + Object(Id%, 17) * 3 * Rx0# * Rx# * Rx# + Object(Id%, 18) * 3 * Ry0# * Ry# * Ry# + Object(Id%, 19) * 3 * Rz0# * Rz# * Rz#
DQ# = DQ# + Object(Id%, 20) * Rx# * Rx# * Ry0# + Object(Id%, 21) * Rx# * Rx# * Rz0# + Object(Id%, 23) * Ry# * Ry# * Rx0#
DQ# = DQ# + Object(Id%, 22) * Ry# * Ry# * Rz0# + Object(Id%, 24) * Rz# * Rz# * Rx0# + Object(Id%, 25) * Rz# * Rz# * Ry0#
DQ# = DQ# + Object(Id%, 20) * 2 * Rx0# * Rx# * Ry# + Object(Id%, 21) * 2 * Rx0# * Rx# * Rz# + Object(Id%, 23) * 2 * Ry0# * Ry# * Rx#
DQ# = DQ# + Object(Id%, 22) * 2 * Ry0# * Ry# * Rz# + Object(Id%, 24) * 2 * Rz0# * Rz# * Rx# + Object(Id%, 25) * 2 * Rz0# * Rz# * Ry#
DQ# = DQ# + Object(Id%, 16) * Rx# * Ry# * Rz0# + Object(Id%, 16) * Rx# * Rz# * Ry0# + Object(Id%, 16) * Ry# * Rz# * Rx0#
DQ# = DQ# + Object(Id%, 26) * Rx# * Rx# + Object(Id%, 27) * Ry# * Ry# + Object(Id%, 28) * Rz# * Rz#
DQ# = DQ# + Object(Id%, 29) * Rx# * Ry# + Object(Id%, 30) * Ry# * Rz# + Object(Id%, 31) * Rx# * Rz#

DR# = DR# + Object(Id%, 17) * 3 * Rx0# * Rx0# * Rx# + Object(Id%, 18) * 3 * Ry0# * Ry0# * Ry# + Object(Id%, 19) * 3 * Rz0# * Rz0# * Rz#
DR# = DR# + Object(Id%, 20) * 2 * Rx0# * Rx# * Ry0# + Object(Id%, 21) * 2 * Rx0# * Rx# * Rz0# + Object(Id%, 23) * 2 * Rx0# * Ry0# * Ry#
DR# = DR# + Object(Id%, 22) * 2 * Ry0# * Ry# * Rz0# + Object(Id%, 24) * 2 * Rx0# * Rz0# * Rz# + Object(Id%, 25) * 2 * Ry0# * Rz0# * Rz#
DR# = DR# + Object(Id%, 20) * Rx0# * Rx0# * Ry# + Object(Id%, 21) * Rx0# * Rx0# * Rz# + Object(Id%, 23) * Ry0# * Ry0# * Rx#
DR# = DR# + Object(Id%, 22) * Ry0# * Ry0# * Rz# + Object(Id%, 24) * Rz0# * Rz0# * Rx# + Object(Id%, 25) * Rz0# * Rz0# * Ry#
DR# = DR# + Object(Id%, 16) * Rx0# * Rz0# * Ry# + Object(Id%, 16) * Ry0# * Rz0# * Rx# + Object(Id%, 16) * Rx0# * Ry0# * Rz#
DR# = DR# + Object(Id%, 26) * 2 * Rx0# * Rx# + Object(Id%, 27) * 2 * Ry0# * Ry# + Object(Id%, 28) * 2 * Rz0# * Rz#
DR# = DR# + Object(Id%, 29) * Ry0# * Rx# + Object(Id%, 30) * Ry0# * Rz# + Object(Id%, 31) * Rx0# * Rz# + Object(Id%, 29) * Rx0# * Ry#
DR# = DR# + Object(Id%, 30) * Rz0# * Ry# + Object(Id%, 31) * Rz0# * Rx#
DR# = DR# + Object(Id%, 32) * Rx# + Object(Id%, 33) * Ry# + Object(Id%, 34) * Rz#

DS# = DS# + Object(Id%, 17) * Rx0# * Rx0# * Rx0# + Object(Id%, 18) * Ry0# * Ry0# * Ry0# + Object(Id%, 19) * Rz0# * Rz0# * Rz0#
DS# = DS# + Object(Id%, 20) * Rx0# * Rx0# * Ry0# + Object(Id%, 21) * Rx0# * Rx0# * Rz0# + Object(Id%, 23) * Ry0# * Ry0# * Rx0#
DS# = DS# + Object(Id%, 22) * Ry0# * Ry0# * Rz0# + Object(Id%, 24) * Rz0# * Rz0# * Rx0# + Object(Id%, 25) * Rz0# * Rz0# * Ry0#
DS# = DS# + Object(Id%, 16) * Rx0# * Ry0# * Rz0#
DS# = DS# + Object(Id%, 26) * Rx0# * Rx0# + Object(Id%, 27) * Ry0# * Ry0# + Object(Id%, 28) * Rz0# * Rz0#
DS# = DS# + Object(Id%, 29) * Rx0# * Ry0# + Object(Id%, 30) * Ry0# * Rz0# + Object(Id%, 31) * Rx0# * Rz0#
DS# = DS# + Object(Id%, 32) * Rx0# + Object(Id%, 33) * Ry0# + Object(Id%, 34) * Rz0#
DS# = DS# + Object(Id%, 35)

Equa3 DP#, DQ#, DR#, DS#, Dist1#, Dist2#, Dist3#, Err3%, nSol3%
IF nSol3% = 0 THEN FlagPlot% = 0: EXIT SUB
IF nSol3% = 1 THEN Dist3# = Dist1#: Dist2# = Dist1#
IF nSol3% = 2 THEN Dist3# = Dist1#
IF Dist1# > Dist2# THEN SWAP Dist1#, Dist2#
IF Dist1# > Dist3# THEN SWAP Dist1#, Dist3#
IF Dist2# > Dist3# THEN SWAP Dist2#, Dist3#

'=======================================================
' Check the intersections are not behind the screen
FlagPoint%(1) = 1: FlagPoint%(2) = 1: FlagPoint%(3) = 1
IF Dist1# < Ray(7) THEN FlagPoint%(1) = 0
IF Dist2# < Ray(7) THEN FlagPoint%(2) = 0
IF Dist3# < Ray(7) THEN FlagPoint%(3) = 0
IF FlagPoint%(1) = 0 AND FlagPoint%(2) = 0 AND FlagPoint%(3) = 0 THEN FlagPlot% = 0: EXIT SUB

xp!(1, 1) = Rx0# + Rx# * Dist1#: xp!(1, 2) = Ry0# + Ry# * Dist1#: xp!(1, 3) = Rz0# + Rz# * Dist1#
xp!(2, 1) = Rx0# + Rx# * Dist2#: xp!(2, 2) = Ry0# + Ry# * Dist2#: xp!(2, 3) = Rz0# + Rz# * Dist2#
xp!(3, 1) = Rx0# + Rx# * Dist3#: xp!(3, 2) = Ry0# + Ry# * Dist3#: xp!(3, 3) = Rz0# + Rz# * Dist3#

'=======================================================
' Set limits to the shape
'FOR k% = 1 TO 3
'IF Module(xp!(k%, 1), xp!(k%, 2), xp!(k%, 3)) > Shape(Id%).dRef2 THEN FlagPoint%(k%) = 0
'NEXT k%
'IF FlagPoint%(1) = 0 AND FlagPoint%(2) = 0 AND FlagPoint%(3) = 0 THEN FlagPlot% = 0: EXIT SUB

'===================================================
' Select the intersection point (object axis system)
IF FlagPoint%(1) = 1 THEN
xh!(1) = xp!(1, 1): xh!(2) = xp!(1, 2): xh!(3) = xp!(1, 3): Dist = Dist1#
ELSE
IF FlagPoint%(2) = 1 THEN
xh!(1) = xp!(2, 1): xh!(2) = xp!(2, 2): xh!(3) = xp!(2, 3): Dist = Dist2#
ELSE
xh!(1) = xp!(3, 1): xh!(2) = xp!(3, 2): xh!(3) = xp!(3, 3): Dist = Dist3#
END IF
END IF

'=========================
' Back to main axis system
FOR k% = 1 TO 3
xt!(k%) = Ray(k%) + Dist * Ray(k% + 3)
NEXT k%
FlagPlot% = 1

END SUB
'
':::::::::::::::
SUB Probe.Cylinder (Id%, Ray(), Dist, xh(), xt(), _xn!(), FlagPlot%)
' Probe a cylinder with a ray
' Dist is the distance from the ray origin
' xh is the hit point in the object axis system
' xt is the hit point in the main axis system
' xn is the normal vector in the main axis system
' FlagPlot% is the hit indicator (0 when no intersection)

DIM xp!(2, 3), FlagPoint%(2), _xo!(3)
FlagPlot% = 0

'=======================================================
' Ray conversion to object axis system
Rx0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 1, 1)
Rx0 = Rx0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 1, 2)
Rx0 = Rx0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 1, 3)

Ry0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 2, 1)
Ry0 = Ry0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 2, 2)
Ry0 = Ry0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 2, 3)

Rz0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 3, 1)
Rz0 = Rz0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 3, 2)
Rz0 = Rz0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 3, 3)

Rx = Ray(4) * Matrix(Id%, 1, 1) + Ray(5) * Matrix(Id%, 1, 2) + Ray(6) * Matrix(Id%, 1, 3)
Ry = Ray(4) * Matrix(Id%, 2, 1) + Ray(5) * Matrix(Id%, 2, 2) + Ray(6) * Matrix(Id%, 2, 3)
Rz = Ray(4) * Matrix(Id%, 3, 1) + Ray(5) * Matrix(Id%, 3, 2) + Ray(6) * Matrix(Id%, 3, 3)

'=======================================================
' Intersection calculation in the object axis system
DP = Rx * Rx + Ry * Ry
DQ = 2 * (Rx0 * Rx + Ry0 * Ry)
DR = Rx0 * Rx0 + Ry0 * Ry0 + Object(Id%, 35)

Delta = DQ * DQ - 4 * DP * DR
IF Delta < 0 THEN FlagPlot% = 0: EXIT SUB
Dist1 = -(DQ + SQR(Delta)) * .5 / DP
Dist2 = (SQR(Delta) - DQ) * .5 / DP

'=======================================================
' Check the intersections are not behind the screen
FlagPoint%(1) = 1: FlagPoint%(2) = 1
IF Dist1 < Ray(7) THEN FlagPoint%(1) = 0
IF Dist2 < Ray(7) THEN FlagPoint%(2) = 0
IF FlagPoint%(1) = 0 AND FlagPoint%(2) = 0 THEN FlagPlot% = 0: EXIT SUB

xp!(1, 1) = Rx0 + Rx * Dist1: xp!(1, 2) = Ry0 + Ry * Dist1: xp!(1, 3) = Rz0 + Rz * Dist1
xp!(2, 1) = Rx0 + Rx * Dist2: xp!(2, 2) = Ry0 + Ry * Dist2: xp!(2, 3) = Rz0 + Rz * Dist2

'=======================================================
' Set limits to the shape
FOR k% = 1 TO 2
IF ABS(xp!(k%, 3)) > Shape(Id%).dRef2 THEN FlagPoint%(k%) = 0
NEXT k%
IF FlagPoint%(1) = 0 AND FlagPoint%(2) = 0 THEN FlagPlot% = 0: EXIT SUB

'=======================================================
' Select the intersection point (object axis system)
IF FlagPoint%(1) = 1 THEN
xh!(1) = xp!(1, 1): xh!(2) = xp!(1, 2): xh!(3) = xp!(1, 3): Dist = Dist1: FlagPlot% = 1
ELSE
xh!(1) = xp!(2, 1): xh!(2) = xp!(2, 2): xh!(3) = xp!(2, 3): Dist = Dist2: FlagPlot% = 2
END IF

'=======================================================
' Calculate the normal vector
Side = Rx * xh!(1) + Ry * xh!(2)
IF Side < 0 THEN Side = -1:  ELSE : Side = 1
InvModNorm = Side / Shape(Id%).dRef1
FOR k% = 1 TO 2: _xo!(k%) = xh!(k%) * InvModNorm: NEXT k%
_xo!(3) = 0

'=======================================================
' Back to main axis system
FOR k% = 1 TO 3
xt!(k%) = Ray(k%) + Dist * Ray(k% + 3)
_xn!(k%) = _xo!(1) * Matrix(Id%, 1, k%) + _xo!(2) * Matrix(Id%, 2, k%)
NEXT k%

END SUB
'
':::::::::::::::
SUB Probe.Floor (Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%)
' Probe the floor (altitude = 0) with a ray
' Dist is the distance from the ray origin
' xh is the hit point in the object axis system
' xt is the hit point in the main axis system
' xn is the normal vector in the main axis system
' FlagPlot% is the hit indicator (0 when no intersection)

IF Ray(6) <> 0 THEN
Dist = -Ray(3) / Ray(6)
ELSE
IF Ray(3) = 0 THEN : Dist = 0:  ELSE : FlagPlot% = 0: EXIT SUB
END IF
IF Dist < Ray(7) THEN FlagPlot% = 0: EXIT SUB
FOR k% = 1 TO 3
xh(k%) = Ray(k%) + Dist * Ray(k% + 3)
xt(k%) = xh(k%)
NEXT k%
_xn!(1) = 0: _xn!(2) = 0: _xn!(3)= 1
FlagPlot% = 1
END SUB
'
':::::::::::::::
SUB Probe.Mandelbrot (Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%)
' Probe a vertical M-Set with a ray
' Dist is the distance from the ray origin
' xh is the hit point in the object axis system
' xt is the hit point in the main axis system
' xn is the normal vector in the main axis system
' FlagPlot% is the hit indicator (0 when no intersection)

DIM _xo!(3)


'=======================================================
' Ray conversion to object axis system
Rx0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 1, 1)
Rx0 = Rx0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 1, 2)
Rx0 = Rx0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 1, 3)

Ry0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 2, 1)
Ry0 = Ry0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 2, 2)
Ry0 = Ry0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 2, 3)

Rz0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 3, 1)
Rz0 = Rz0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 3, 2)
Rz0 = Rz0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 3, 3)

Rx = Ray(4) * Matrix(Id%, 1, 1) + Ray(5) * Matrix(Id%, 1, 2) + Ray(6) * Matrix(Id%, 1, 3)
Ry = Ray(4) * Matrix(Id%, 2, 1) + Ray(5) * Matrix(Id%, 2, 2) + Ray(6) * Matrix(Id%, 2, 3)
Rz = Ray(4) * Matrix(Id%, 3, 1) + Ray(5) * Matrix(Id%, 3, 2) + Ray(6) * Matrix(Id%, 3, 3)

'=======================================================
' Intersection calculation in the object axis system
DQ = Object(Id%, 32) * Rx + Object(Id%, 33) * Ry + Object(Id%, 34) * Rz
DR = Object(Id%, 32) * Rx0 + Object(Id%, 33) * Ry0 + Object(Id%, 34) * Rz0 + Object(Id%, 35)
IF DQ <> 0 THEN
Dist = -DR / DQ
ELSE
IF DR = 0 THEN : Dist = 0:  ELSE : FlagPlot% = 0: EXIT SUB
END IF

'=======================================================
' Check the intersection is not behind the screen
IF Dist < Ray(7) THEN FlagPlot% = 0: EXIT SUB

xh!(1) = Rx0 + Rx * Dist: xh!(2) = Ry0 + Ry * Dist: xh!(3) = Rz0 + Rz * Dist: FlagPlot% = 1

'=======================================================
' Set limits to the shape
p = -xh(3): q = xh(2)
zx0 = p: zy0 = q
x = zx0: y = zy0
FOR NP% = 1 TO 20
xn = x * x - y * y + zx0
yn = 2 * x * y + zy0
IF (xn * xn + yn * yn) > 4 THEN FlagPlot% = 0: EXIT SUB
xo = x: yo = y: x = xn: y = yn
NEXT NP%
dx = xo - x: dy = yo - y: VarMdb = dx * dx + dy * dy
IF VarMdb > 0 THEN : FlagPlot% = (INT(ABS(LOG(VarMdb))) MOD (4) + 1):   ELSE : FlagPlot% = 1
IF FlagPlot% = 0 THEN EXIT SUB

'=======================================================
' Calculate the normal vector
_xo!(1) = Object(Id%, 32): _xo!(2) = Object(Id%, 33): _xo!(3) = Object(Id%, 34)
Side = Rx * _xo!(1) + Ry * _xo!(2) + Rz * _xo!(3)
IF Side < 0 THEN Side = -1:  ELSE : Side = 1
ModNorm = Side * Module(_xo!(1), _xo!(2), _xo!(3))
IF ModNorm = 0 THEN EXIT SUB
FOR k% = 1 TO 3: _xo!(k%) = _xo!(k%) / ModNorm: NEXT k%

'=======================================================
' Back to main axis system
FOR k% = 1 TO 3
xt(k%) = Ray(k%) + Dist * Ray(k% + 3)
_xn!(k%) = _xo!(1) * Matrix(Id%, 1, k%) + _xo!(2) * Matrix(Id%, 2, k%) + _xo!(3) * Matrix(Id%, 3, k%)
NEXT k%

END SUB
'
':::::::::::::::
SUB Probe.Moebius (Id%, Ray(), Dist, xh(), xt(), _xn!(), FlagPlot%)
DIM xp!(4, 3), FlagPoint%(4), _xo!(3)
FlagPlot% = 0

'=======================================================
' Ray conversion to object axis system
Rx0# = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 1, 1)
Rx0# = Rx0# + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 1, 2)
Rx0# = Rx0# + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 1, 3)

Ry0# = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 2, 1)
Ry0# = Ry0# + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 2, 2)
Ry0# = Ry0# + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 2, 3)

Rz0# = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 3, 1)
Rz0# = Rz0# + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 3, 2)
Rz0# = Rz0# + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 3, 3)

Rx# = Ray(4) * Matrix(Id%, 1, 1) + Ray(5) * Matrix(Id%, 1, 2) + Ray(6) * Matrix(Id%, 1, 3)
Ry# = Ray(4) * Matrix(Id%, 2, 1) + Ray(5) * Matrix(Id%, 2, 2) + Ray(6) * Matrix(Id%, 2, 3)
Rz# = Ray(4) * Matrix(Id%, 3, 1) + Ray(5) * Matrix(Id%, 3, 2) + Ray(6) * Matrix(Id%, 3, 3)

'=======================================================
' Intersection calculation in the object axis system


'The parametric definition of the moebius strip is:
'x = (r + l * Cos(Theta/2) * cos(Theta)
'y = (r + l * Cos(Theta/2) * sin(Theta)
'z= l * Sin (Theta)

'Let's define T:
'T = r + l * Cos(Theta/2),
'i.e.: T = r+ z /Tan(Theta/2)

'Since Tan (Theta) = 2 * Tan(Theta/2)/(1 - Tan(Theta/2)^2), then:
'y/x = 2 * t / (1-t^2) with Tan(Theta/2) = t
'and
'T^2 = r^2 + 2 * r * z/t + z^2/t^2 = x^2+ y^2

'That can be re-written:
't^2 * y + 2* x* t - y = 0
't^2*(x^2+y^2-r^2) - 2* r* z* t - z^2 = 0

'If Alpha = t^2 and Beta = t then:
'y * Alpha + 2* x * Beta = y
'(x^2+y^2-r^2) * Alpha - 2 * r * z* Beta = z^2

'That's a two equations linear system. So:
'Alpha = (x* z^2+r * z* y)/(x * (x^2 + y^2 - r^2) + r * y * z)
'Beta = (y - y* Alpha)/(2* x)


' We know that:
' x = Rx0 + Rx * Dist
' y = Ry0 + Ry * Dist
' z = Rz0 + Rz * Dist

'So we have:
'1) Alpha = Phi(Dist)
'2) Beta = Psi (Dist)
'and we are looking for Dist that matches Alpha = Beta^2.
'So we have Dist, so we have x, y, and z...

r# = Shape(Id%).dRef1
x# = Rx0# + Rx# * Dist#
y# = Ry0# + Ry# * Dist#
z# = Rz0# + Rz# * Dist#
Alpha# = (x# * z# * z# + r# * z# * y#) / (x# * (x# * x# + y# * y# - r# * r#) + r# * y# * z#)
Beta# = (y# - y# * Alpha#) / (2 * x#)

'Normal vector
'S = +/- SQR (Beta^2/(1+Beta^2))
'_xn!(1) = S (1-S^2)
'_xn!(2) = 1 * S^2 * SQR (1-S^2)
'_xn!(3) = +/- SQR(1-S^2)

'Set a limit to the strip width:
'Sin (Theta/2) = +/- SQR(Beta^2/(1-Beta^2))
'The limit is ABS(z) <= r * SIN (Theta/2) with Theta = Arg(x,y)


END SUB
'
':::::::::::::::
SUB Probe.Object (Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%)
' Selects the proper probe routine
SELECT CASE Shape(Id%).Degree
CASE 1
Probe.Plan Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%
CASE 2
Probe.Quadric Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%
CASE 3
Probe.Cubic Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%
CASE 4
Probe.Quartic Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%
CASE ELSE
END SELECT
END SUB
'
':::::::::::::::
SUB Probe.Plan (Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%)
' Probe a plan with a ray
' Dist is the distance from the ray origin
' xh is the hit point in the object axis system
' xt is the hit point in the main axis system
' xn is the normal vector in the main axis system
' FlagPlot% is the hit indicator (0 when no intersection)

DIM _xo!(3)

IF Shape(Id%).NumType = 100 THEN
Probe.Floor Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%
EXIT SUB
END IF

IF Shape(Id%).NumType = 110 THEN
Probe.Sky Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%
EXIT SUB
END IF

IF Shape(Id%).NumType = 120 THEN
Probe.Mandelbrot Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%
EXIT SUB
END IF

'=======================================================
' Ray conversion to object axis system
Rx0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 1, 1)
Rx0 = Rx0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 1, 2)
Rx0 = Rx0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 1, 3)

Ry0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 2, 1)
Ry0 = Ry0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 2, 2)
Ry0 = Ry0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 2, 3)

Rz0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 3, 1)
Rz0 = Rz0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 3, 2)
Rz0 = Rz0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 3, 3)

Rx = Ray(4) * Matrix(Id%, 1, 1) + Ray(5) * Matrix(Id%, 1, 2) + Ray(6) * Matrix(Id%, 1, 3)
Ry = Ray(4) * Matrix(Id%, 2, 1) + Ray(5) * Matrix(Id%, 2, 2) + Ray(6) * Matrix(Id%, 2, 3)
Rz = Ray(4) * Matrix(Id%, 3, 1) + Ray(5) * Matrix(Id%, 3, 2) + Ray(6) * Matrix(Id%, 3, 3)

'=======================================================
' Intersection calculation in the object axis system
DQ = Object(Id%, 32) * Rx + Object(Id%, 33) * Ry + Object(Id%, 34) * Rz
DR = Object(Id%, 32) * Rx0 + Object(Id%, 33) * Ry0 + Object(Id%, 34) * Rz0 + Object(Id%, 35)
IF DQ <> 0 THEN
Dist = -DR / DQ
ELSE
IF DR = 0 THEN : Dist = 0:  ELSE : FlagPlot% = 0: EXIT SUB
END IF

'=======================================================
' Check the intersection is not behind the screen
IF Dist < Ray(7) THEN FlagPlot% = 0: EXIT SUB

xh!(1) = Rx0 + Rx * Dist: xh!(2) = Ry0 + Ry * Dist: xh!(3) = Rz0 + Rz * Dist: FlagPlot% = 1

'=======================================================
' Set limits to the shape
SELECT CASE Shape(Id%).NumType
CASE 101
IF ABS(xh!(1)) > Shape(Id%).dRef1 OR ABS(xh!(2)) > Shape(Id%).dRef2 THEN FlagPlot% = 0
CASE 102
IF ABS(xh!(2)) > Shape(Id%).dRef1 OR ABS(xh!(3)) > Shape(Id%).dRef2 THEN FlagPlot% = 0
CASE 103
IF ABS(xh!(1)) > Shape(Id%).dRef1 OR ABS(xh!(3)) > Shape(Id%).dRef2 THEN FlagPlot% = 0
CASE 104
IF (xh!(1) * xh!(1) + xh!(2) * xh!(2)) > Shape(Id%).dRef1 * Shape(Id%).dRef1 THEN FlagPlot% = 0
CASE 105
IF (xh!(3) * xh!(3) + xh!(2) * xh!(2)) > Shape(Id%).dRef1 * Shape(Id%).dRef1 THEN FlagPlot% = 0
CASE 106
IF (xh!(1) * xh!(1) + xh!(3) * xh!(3)) > Shape(Id%).dRef1 * Shape(Id%).dRef1 THEN FlagPlot% = 0
CASE ELSE
END SELECT
IF FlagPlot% = 0 THEN EXIT SUB

'=========================
' Back to main axis system
FOR k% = 1 TO 3
xt(k%) = Ray(k%) + Dist * Ray(k% + 3)
NEXT k%


FlagPlot% = 1
END SUB
'
':::::::::::::::
SUB Probe.Quadric (Id%, Ray(), Dist, xh(), xt(), _xn!(), FlagPlot%)
' Probe a quadric shape with a ray
' Dist is the distance from the ray origin
' xh is the hit point in the object axis system
' xt is the hit point in the main axis system
' xn is the normal vector in the main axis system
' FlagPlot% is the hit indicator (0 when no intersection)

DIM xp!(2, 3), FlagPoint%(2), _xo!(3)

IF Shape(Id%).NumType = 200 THEN
Probe.Ball Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%
EXIT SUB
END IF

IF Shape(Id%).NumType = 203 THEN
Probe.Cylinder Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%
EXIT SUB
END IF

IF Shape(Id%).NumType = 205 THEN
Probe.Cone Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%
EXIT SUB
END IF

'IF Shape(Id%).NumType = 210 THEN
'Probe.Cup Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%
'EXIT SUB
'END IF

FlagPlot% = 0

'=======================================================
' Ray conversion to object axis system
Rx0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 1, 1)
Rx0 = Rx0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 1, 2)
Rx0 = Rx0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 1, 3)

Ry0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 2, 1)
Ry0 = Ry0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 2, 2)
Ry0 = Ry0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 2, 3)

Rz0 = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 3, 1)
Rz0 = Rz0 + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 3, 2)
Rz0 = Rz0 + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 3, 3)

Rx = Ray(4) * Matrix(Id%, 1, 1) + Ray(5) * Matrix(Id%, 1, 2) + Ray(6) * Matrix(Id%, 1, 3)
Ry = Ray(4) * Matrix(Id%, 2, 1) + Ray(5) * Matrix(Id%, 2, 2) + Ray(6) * Matrix(Id%, 2, 3)
Rz = Ray(4) * Matrix(Id%, 3, 1) + Ray(5) * Matrix(Id%, 3, 2) + Ray(6) * Matrix(Id%, 3, 3)

'=======================================================
' Intersection calculation in the object axis system
DP = Object(Id%, 26) * Rx * Rx + Object(Id%, 27) * Ry * Ry + Object(Id%, 28) * Rz * Rz
DP = DP + Object(Id%, 29) * Rx * Ry + Object(Id%, 30) * Ry * Rz + Object(Id%, 31) * Rx * Rz
DQ = 2 * (Object(Id%, 26) * Rx0 * Rx + Object(Id%, 27) * Ry0 * Ry + Object(Id%, 28) * Rz0 * Rz)
DQ = DQ + Object(Id%, 29) * (Rx0 * Ry + Rx * Ry0) + Object(Id%, 30) * (Ry0 * Rz + Ry * Rz0) + Object(Id%, 31) * (Rx0 * Rz + Rx * Rz0)
DQ = DQ + Object(Id%, 32) * Rx + Object(Id%, 33) * Ry + Object(Id%, 34) * Rz
DR = Rx0 * Rx0 * Object(Id%, 26) + Ry0 * Ry0 * Object(Id%, 27) + Rz0 * Rz0 * Object(Id%, 28) + Object(Id%, 29) * Rx0 * Ry0 + Object(Id%, 30) * Ry0 * Rz0 + Object(Id%, 31) * Rx0 * Rz0
DR = DR + Object(Id%, 32) * Rx0 + Object(Id%, 33) * Ry0 + Object(Id%, 34) * Rz0 + Object(Id%, 35)

IF DP <> 0 THEN
Delta = DQ * DQ - 4 * DP * DR
    IF Delta >= 0 THEN
    Dist1 = -(DQ + SQR(Delta)) * .5 / DP
    Dist2 = (SQR(Delta) - DQ) * .5 / DP
    ELSE
    FlagPlot% = 0: EXIT SUB
END IF
ELSE
    IF DQ <> 0 THEN
    Dist1 = -DR / DQ: Dist2 = Dist1
    ELSE
    IF DR = 0 THEN : Dist1 = 0: Dist2 = 0:  ELSE : FlagPlot% = 0: EXIT SUB
    END IF
END IF

'=======================================================
' Check the intersections are not behind the screen
FlagPoint%(1) = 1: FlagPoint%(2) = 1
IF Dist1 < Ray(7) THEN FlagPoint%(1) = 0
IF Dist2 < Ray(7) THEN FlagPoint%(2) = 0
IF FlagPoint%(1) = 0 AND FlagPoint%(2) = 0 THEN FlagPlot% = 0: EXIT SUB

xp!(1, 1) = Rx0 + Rx * Dist1: xp!(1, 2) = Ry0 + Ry * Dist1: xp!(1, 3) = Rz0 + Rz * Dist1
xp!(2, 1) = Rx0 + Rx * Dist2: xp!(2, 2) = Ry0 + Ry * Dist2: xp!(2, 3) = Rz0 + Rz * Dist2

'=======================================================
' Set limits to the shape
FOR k% = 1 TO 2
SELECT CASE Shape(Id%).NumType
CASE 201
IF ABS(xp!(k%, 1)) > Shape(Id%).dRef2 OR ABS(xp!(k%, 2)) > Shape(Id%).dRef2 OR ABS(xp!(k%, 3)) > Shape(Id%).dRef2 THEN FlagPoint%(k%) = 0
CASE 202
CASE 206
IF (xp!(k%, 1) ^ 2 + xp!(k%, 2) ^ 2) > Shape(Id%).dRef1 ^ 2 THEN FlagPoint%(k%) = 0
CASE 210
IF xp!(k%, 3) > 0 THEN FlagPoint%(k%) = 0
CASE ELSE
IF ABS(xp!(k%, 3)) > Shape(Id%).dRef2 THEN FlagPoint%(k%) = 0
END SELECT
NEXT k%
IF FlagPoint%(1) = 0 AND FlagPoint%(2) = 0 THEN FlagPlot% = 0: EXIT SUB

'===================================================
' Select the intersection point (object axis system)
IF FlagPoint%(1) = 1 THEN
xh!(1) = xp!(1, 1): xh!(2) = xp!(1, 2): xh!(3) = xp!(1, 3): Dist = Dist1: FlagPlot% = 1
ELSE
xh!(1) = xp!(2, 1): xh!(2) = xp!(2, 2): xh!(3) = xp!(2, 3): Dist = Dist2: FlagPlot% = 2
END IF

'=========================
' Back to main axis system
FOR k% = 1 TO 3
xt(k%) = Ray(k%) + Dist * Ray(k% + 3)
NEXT k%
END SUB
'
':::::::::::::::
SUB Probe.Quartic (Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%)
' Probe a quartic shape with a ray
' Dist is the distance from the ray origin
' xh is the hit point in the object axis system
' xt is the hit point in the main axis system
' xn is the normal vector in the main axis system
' FlagPlot% is the hit indicator (0 when no intersection)

DIM xp!(4, 3), FlagPoint%(4), _xo!(3)
FlagPlot% = 0

'=======================================================
' Ray conversion to object axis system
Rx0# = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 1, 1)
Rx0# = Rx0# + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 1, 2)
Rx0# = Rx0# + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 1, 3)

Ry0# = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 2, 1)
Ry0# = Ry0# + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 2, 2)
Ry0# = Ry0# + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 2, 3)

Rz0# = (Ray(1) - Shape(Id%).x0) * Matrix(Id%, 3, 1)
Rz0# = Rz0# + (Ray(2) - Shape(Id%).y0) * Matrix(Id%, 3, 2)
Rz0# = Rz0# + (Ray(3) - Shape(Id%).z0) * Matrix(Id%, 3, 3)

Rx# = Ray(4) * Matrix(Id%, 1, 1) + Ray(5) * Matrix(Id%, 1, 2) + Ray(6) * Matrix(Id%, 1, 3)
Ry# = Ray(4) * Matrix(Id%, 2, 1) + Ray(5) * Matrix(Id%, 2, 2) + Ray(6) * Matrix(Id%, 2, 3)
Rz# = Ray(4) * Matrix(Id%, 3, 1) + Ray(5) * Matrix(Id%, 3, 2) + Ray(6) * Matrix(Id%, 3, 3)

'=======================================================
' Intersection calculation in the object axis system


DP# = Object(Id%, 1) * Rx# * Rx# * Rx# * Rx#
DP# = DP# + Object(Id%, 2) * Ry# * Ry# * Ry# * Ry#
DP# = DP# + Object(Id%, 3) * Rz# * Rz# * Rz# * Rz#
DP# = DP# + Object(Id%, 13) * Rx# * Rx# * Ry# * Rz#
DP# = DP# + Object(Id%, 14) * Ry# * Ry# * Rx# * Rz#
DP# = DP# + Object(Id%, 15) * Rz# * Rz# * Rx# * Ry#
DP# = DP# + Object(Id%, 10) * Rx# * Rx# * Ry# * Ry#
DP# = DP# + Object(Id%, 11) * Rx# * Rx# * Rz# * Rz#
DP# = DP# + Object(Id%, 12) * Ry# * Ry# * Rz# * Rz#
DP# = DP# + Object(Id%, 4) * Rx# * Rx# * Rx# * Ry# + Object(Id%, 5) * Rx# * Rx# * Rx# * Rz#
DP# = DP# + Object(Id%, 6) * Ry# * Ry# * Ry# * Rz# + Object(Id%, 7) * Ry# * Ry# * Ry# * Rx#
DP# = DP# + Object(Id%, 8) * Rz# * Rz# * Rz# * Rx# + Object(Id%, 9) * Rz# * Rz# * Rz# * Ry#

DQ# = Object(Id%, 1) * 4 * Rx0# * Rx# * Rx# * Rx#
DQ# = DQ# + Object(Id%, 2) * 4 * Ry0# * Ry# * Ry# * Ry#
DQ# = DQ# + Object(Id%, 3) * 4 * Rz0# * Rz# * Rz# * Rz#
DQ# = DQ# + Object(Id%, 13) * Rx# * Rx# * Ry# * Rz0#
DQ# = DQ# + Object(Id%, 14) * Ry# * Ry# * Rx# * Rz0#
DQ# = DQ# + Object(Id%, 15) * Rz# * Rz# * Rx# * Ry0#
DQ# = DQ# + Object(Id%, 13) * Rx# * Rx# * Ry0# * Rz#
DQ# = DQ# + Object(Id%, 14) * Ry# * Ry# * Rx0# * Rz#
DQ# = DQ# + Object(Id%, 15) * Rz# * Rz# * Rx0# * Ry#
DQ# = DQ# + Object(Id%, 13) * 2 * Rx0# * Rx# * Ry# * Rz#
DQ# = DQ# + Object(Id%, 14) * 2 * Ry0# * Ry# * Rx# * Rz#
DQ# = DQ# + Object(Id%, 15) * 2 * Rz0# * Rz# * Rx# * Ry#
DQ# = DQ# + Object(Id%, 10) * 2 * Rx# * Rx# * Ry0# * Ry#
DQ# = DQ# + Object(Id%, 11) * 2 * Rx# * Rx# * Rz0# * Rz#
DQ# = DQ# + Object(Id%, 12) * 2 * Ry# * Ry# * Rz0# * Rz#
DQ# = DQ# + Object(Id%, 10) * 2 * Ry# * Ry# * Rx0# * Rx#
DQ# = DQ# + Object(Id%, 11) * 2 * Rz# * Rz# * Rx0# * Rx#
DQ# = DQ# + Object(Id%, 12) * 2 * Rz# * Rz# * Ry0# * Ry#
DQ# = DQ# + Object(Id%, 4) * Rx# * Rx# * Rx# * Ry0#
DQ# = DQ# + Object(Id%, 5) * Rx# * Rx# * Rx# * Rz0#
DQ# = DQ# + Object(Id%, 6) * Ry# * Ry# * Ry# * Rz0#
DQ# = DQ# + Object(Id%, 7) * Ry# * Ry# * Ry# * Rx0#
DQ# = DQ# + Object(Id%, 9) * Rz# * Rz# * Rz# * Rx0#
DQ# = DQ# + Object(Id%, 9) * Rz# * Rz# * Rz# * Ry0#
DQ# = DQ# + Object(Id%, 4) * 3 * Rx0# * Rx# * Rx# * Ry#
DQ# = DQ# + Object(Id%, 5) * 3 * Rx0# * Rx# * Rx# * Rz#
DQ# = DQ# + Object(Id%, 6) * 3 * Ry0# * Ry# * Ry# * Rz#
DQ# = DQ# + Object(Id%, 7) * 3 * Ry0# * Ry# * Ry# * Rx#
DQ# = DQ# + Object(Id%, 8) * 3 * Rz0# * Rz# * Rz# * Rx#
DQ# = DQ# + Object(Id%, 9) * 3 * Rz0# * Rz# * Rz# * Ry#
DQ# = DQ# + Object(Id%, 17) * Rx# * Rx# * Rx#
DQ# = DQ# + Object(Id%, 18) * Ry# * Ry# * Ry#
DQ# = DQ# + Object(Id%, 19) * Rz# * Rz# * Rz#
DQ# = DQ# + Object(Id%, 20) * Rx# * Rx# * Ry#
DQ# = DQ# + Object(Id%, 21) * Rx# * Rx# * Rz#
DQ# = DQ# + Object(Id%, 23) * Ry# * Ry# * Rx#
DQ# = DQ# + Object(Id%, 22) * Ry# * Ry# * Rz#
DQ# = DQ# + Object(Id%, 24) * Rz# * Rz# * Rx#
DQ# = DQ# + Object(Id%, 25) * Rz# * Rz# * Ry#
DQ# = DQ# + Object(Id%, 16) * Rx# * Ry# * Rz#

DR# = Object(Id%, 1) * 6 * Rx0# * Rx0# * Rx# * Rx#
DR# = DR# + Object(Id%, 2) * 6 * Ry0# * Ry0# * Ry# * Ry#
DR# = DR# + Object(Id%, 3) * 6 * Rz0# * Rz0# * Rz# * Rz#
DR# = DR# + Object(Id%, 13) * 2 * Rx0# * Rx# * Ry# * Rz0#
DR# = DR# + Object(Id%, 14) * 2 * Ry0# * Ry# * Rx# * Rz0#
DR# = DR# + Object(Id%, 15) * 2 * Rz0# * Rz# * Rx# * Ry0#
DR# = DR# + Object(Id%, 13) * 2 * Rx0# * Rx# * Ry0# * Rz#
DR# = DR# + Object(Id%, 14) * 2 * Ry0# * Ry# * Rx0# * Rz#
DR# = DR# + Object(Id%, 15) * 2 * Rz0# * Rz# * Rx0# * Ry#
DR# = DR# + Object(Id%, 13) * Rx# * Rx# * Ry0# * Rz0#
DR# = DR# + Object(Id%, 14) * Ry# * Ry# * Rx0# * Rz0#
DR# = DR# + Object(Id%, 15) * Rz# * Rz# * Rx0# * Ry0#
DR# = DR# + Object(Id%, 13) * Rx0# * Rx0# * Ry# * Rz#
DR# = DR# + Object(Id%, 14) * Ry0# * Ry0# * Rx# * Rz#
DR# = DR# + Object(Id%, 15) * Rz0# * Rz0# * Rx# * Ry#
DR# = DR# + Object(Id%, 10) * Rx# * Rx# * Ry0# * Ry0#
DR# = DR# + Object(Id%, 11) * Rx# * Rx# * Rz0# * Rz0#
DR# = DR# + Object(Id%, 12) * Ry# * Ry# * Rz0# * Rz0#
DR# = DR# + Object(Id%, 10) * Rx0# * Rx0# * Ry# * Ry#
DR# = DR# + Object(Id%, 11) * Rx0# * Rx0# * Rz# * Rz#
DR# = DR# + Object(Id%, 12) * Ry0# * Ry0# * Rz# * Rz#
DR# = DR# + Object(Id%, 10) * 4 * Rx0# * Rx# * Ry0# * Ry#
DR# = DR# + Object(Id%, 11) * 4 * Rx0# * Rx# * Rz0# * Rz#
DR# = DR# + Object(Id%, 12) * 4 * Ry0# * Ry# * Rz0# * Rz#
DR# = DR# + Object(Id%, 4) * 3 * Rx0# * Rx# * Rx# * Ry0#
DR# = DR# + Object(Id%, 5) * 3 * Rx0# * Rx# * Rx# * Rz0#
DR# = DR# + Object(Id%, 6) * 3 * Ry0# * Ry# * Ry# * Rz0#
DR# = DR# + Object(Id%, 7) * 3 * Ry0# * Ry# * Ry# * Rx0#
DR# = DR# + Object(Id%, 8) * 3 * Rz0# * Rz# * Rz# * Rx0#
DR# = DR# + Object(Id%, 9) * 3 * Rz0# * Rz# * Rz# * Ry0#
DR# = DR# + Object(Id%, 4) * 3 * Rx0# * Rx0# * Rx# * Ry#
DR# = DR# + Object(Id%, 5) * 3 * Rx0# * Rx0# * Rx# * Rz#
DR# = DR# + Object(Id%, 6) * 3 * Ry0# * Ry0# * Ry# * Rz#
DR# = DR# + Object(Id%, 7) * 3 * Ry0# * Ry0# * Ry# * Rx#
DR# = DR# + Object(Id%, 8) * 3 * Rz0# * Rz0# * Rz# * Rx#
DR# = DR# + Object(Id%, 9) * 3 * Rz0# * Rz0# * Rz# * Ry#
DR# = DR# + Object(Id%, 17) * 3 * Rx0# * Rx# * Rx#
DR# = DR# + Object(Id%, 18) * 3 * Ry0# * Ry# * Ry#
DR# = DR# + Object(Id%, 19) * 3 * Rz0# * Rz# * Rz#
DR# = DR# + Object(Id%, 20) * Rx# * Rx# * Ry0#
DR# = DR# + Object(Id%, 21) * Rx# * Rx# * Rz0#
DR# = DR# + Object(Id%, 23) * Ry# * Ry# * Rx0#
DR# = DR# + Object(Id%, 22) * Ry# * Ry# * Rz0#
DR# = DR# + Object(Id%, 24) * Rz# * Rz# * Rx0#
DR# = DR# + Object(Id%, 25) * Rz# * Rz# * Ry0#
DR# = DR# + Object(Id%, 20) * 2 * Rx0# * Rx# * Ry#
DR# = DR# + Object(Id%, 21) * 2 * Rx0# * Rx# * Rz#
DR# = DR# + Object(Id%, 23) * 2 * Ry0# * Ry# * Rx#
DR# = DR# + Object(Id%, 22) * 2 * Ry0# * Ry# * Rz#
DR# = DR# + Object(Id%, 24) * 2 * Rz0# * Rz# * Rx#
DR# = DR# + Object(Id%, 25) * 2 * Rz0# * Rz# * Ry#
DR# = DR# + Object(Id%, 16) * Rx# * Ry# * Rz0#
DR# = DR# + Object(Id%, 16) * Rx# * Rz# * Ry0#
DR# = DR# + Object(Id%, 16) * Ry# * Rz# * Rx0#
DR# = DR# + Object(Id%, 26) * Rx# * Rx#
DR# = DR# + Object(Id%, 27) * Ry# * Ry#
DR# = DR# + Object(Id%, 28) * Rz# * Rz#
DR# = DR# + Object(Id%, 29) * Rx# * Ry#
DR# = DR# + Object(Id%, 30) * Ry# * Rz#
DR# = DR# + Object(Id%, 31) * Rx# * Rz#

DS# = Object(Id%, 1) * 4 * Rx0# * Rx0# * Rx0# * Rx#
DS# = DS# + Object(Id%, 2) * 4 * Ry0# * Ry0# * Ry0# * Ry#
DS# = DS# + Object(Id%, 3) * 4 * Rz0# * Rz0# * Rz0# * Rz#
DS# = DS# + Object(Id%, 13) * 2 * Rx0# * Rx# * Ry0# * Rz0#
DS# = DS# + Object(Id%, 14) * 2 * Ry0# * Ry# * Rx0# * Rz0#
DS# = DS# + Object(Id%, 15) * 2 * Rz0# * Rz# * Rx0# * Ry0#
DS# = DS# + Object(Id%, 13) * Rx0# * Rx0# * Ry# * Rz0#
DS# = DS# + Object(Id%, 14) * 2 * Ry0# * Ry0# * Rx# * Rz0#
DS# = DS# + Object(Id%, 15) * 2 * Rz0# * Rz0# * Rx# * Ry0#
DS# = DS# + Object(Id%, 13) * Rx0# * Rx0# * Ry0# * Rz#
DS# = DS# + Object(Id%, 14) * 2 * Ry0# * Ry0# * Rx0# * Rz#
DS# = DS# + Object(Id%, 15) * 2 * Rz0# * Rz0# * Rx0# * Ry#
DS# = DS# + Object(Id%, 10) * 2 * Rx0# * Rx# * Ry0# * Ry0#
DS# = DS# + Object(Id%, 11) * 2 * Rx0# * Rx# * Rz0# * Rz0#
DS# = DS# + Object(Id%, 12) * 2 * Ry0# * Ry# * Rz0# * Rz0#
DS# = DS# + Object(Id%, 10) * 2 * Ry0# * Ry# * Rx0# * Rx0#
DS# = DS# + Object(Id%, 11) * 2 * Rz0# * Rz# * Rx0# * Rx0#
DS# = DS# + Object(Id%, 12) * 2 * Rz0# * Rz# * Ry0# * Ry0#
DS# = DS# + Object(Id%, 4) * 3 * Rx0# * Rx0# * Rx# * Ry0#
DS# = DS# + Object(Id%, 5) * 3 * Rx0# * Rx0# * Rx# * Rz0#
DS# = DS# + Object(Id%, 6) * 3 * Ry0# * Ry0# * Ry# * Rz0#
DS# = DS# + Object(Id%, 7) * 3 * Ry0# * Ry0# * Rx0# * Ry#
DS# = DS# + Object(Id%, 8) * 3 * Rz0# * Rz0# * Rx0# * Rz#
DS# = DS# + Object(Id%, 9) * 3 * Rz0# * Rz0# * Ry0# * Rz#
DS# = DS# + Object(Id%, 4) * Rx0# * Rx0# * Rx0# * Ry#
DS# = DS# + Object(Id%, 5) * Rx0# * Rx0# * Rx0# * Rz#
DS# = DS# + Object(Id%, 6) * Ry0# * Ry0# * Ry0# * Rz#
DS# = DS# + Object(Id%, 7) * Ry0# * Ry0# * Ry0# * Rx#
DS# = DS# + Object(Id%, 8) * Rz0# * Rz0# * Rz0# * Rx#
DS# = DS# + Object(Id%, 9) * Rz0# * Rz0# * Rz0# * Ry#
DS# = DS# + Object(Id%, 17) * 3 * Rx0# * Rx0# * Rx#
DS# = DS# + Object(Id%, 18) * 3 * Ry0# * Ry0# * Ry#
DS# = DS# + Object(Id%, 19) * 3 * Rz0# * Rz0# * Rz#
DS# = DS# + Object(Id%, 20) * 2 * Rx0# * Rx# * Ry0#
DS# = DS# + Object(Id%, 21) * 2 * Rx0# * Rx# * Rz0#
DS# = DS# + Object(Id%, 23) * 2 * Rx0# * Ry0# * Ry#
DS# = DS# + Object(Id%, 22) * 2 * Ry0# * Ry# * Rz0#
DS# = DS# + Object(Id%, 24) * 2 * Rx0# * Rz0# * Rz#
DS# = DS# + Object(Id%, 25) * 2 * Ry0# * Rz0# * Rz#
DS# = DS# + Object(Id%, 20) * Rx0# * Rx0# * Ry#
DS# = DS# + Object(Id%, 21) * Rx0# * Rx0# * Rz#
DS# = DS# + Object(Id%, 23) * Ry0# * Ry0# * Rx#
DS# = DS# + Object(Id%, 22) * Ry0# * Ry0# * Rz#
DS# = DS# + Object(Id%, 24) * Rz0# * Rz0# * Rx#
DS# = DS# + Object(Id%, 25) * Rz0# * Rz0# * Ry#
DS# = DS# + Object(Id%, 16) * Rx0# * Rz0# * Ry#
DS# = DS# + Object(Id%, 16) * Ry0# * Rz0# * Rx#
DS# = DS# + Object(Id%, 16) * Rx0# * Ry0# * Rz#
DS# = DS# + Object(Id%, 26) * 2 * Rx0# * Rx#
DS# = DS# + Object(Id%, 27) * 2 * Ry0# * Ry#
DS# = DS# + Object(Id%, 28) * 2 * Rz0# * Rz#
DS# = DS# + Object(Id%, 29) * Ry0# * Rx#
DS# = DS# + Object(Id%, 30) * Ry0# * Rz#
DS# = DS# + Object(Id%, 31) * Rx0# * Rz#
DS# = DS# + Object(Id%, 29) * Rx0# * Ry#
DS# = DS# + Object(Id%, 30) * Rz0# * Ry#
DS# = DS# + Object(Id%, 31) * Rz0# * Rx#
DS# = DS# + Object(Id%, 32) * Rx#
DS# = DS# + Object(Id%, 33) * Ry#
DS# = DS# + Object(Id%, 34) * Rz#

dt# = Object(Id%, 1) * Rx0# * Rx0# * Rx0# * Rx0#
dt# = dt# + Object(Id%, 2) * Ry0# * Ry0# * Ry0# * Ry0#
dt# = dt# + Object(Id%, 3) * Rz0# * Rz0# * Rz0# * Rz0#
dt# = dt# + Object(Id%, 13) * Rx0# * Rx0# * Ry0# * Rz0#
dt# = dt# + Object(Id%, 14) * Ry0# * Ry0# * Rx0# * Rz0#
dt# = dt# + Object(Id%, 15) * Rz0# * Rz0# * Rx0# * Ry0#
dt# = dt# + Object(Id%, 10) * Rx0# * Rx0# * Ry0# * Ry0#
dt# = dt# + Object(Id%, 11) * Rx0# * Rx0# * Rz0# * Rz0#
dt# = dt# + Object(Id%, 12) * Ry0# * Ry0# * Rz0# * Rz0#
dt# = dt# + Object(Id%, 4) * Rx0# * Rx0# * Rx0# * Ry0#
dt# = dt# + Object(Id%, 5) * Rx0# * Rx0# * Rx0# * Rz0#
dt# = dt# + Object(Id%, 6) * Ry0# * Ry0# * Ry0# * Rz0#
dt# = dt# + Object(Id%, 7) * Ry0# * Ry0# * Ry0# * Rx0#
dt# = dt# + Object(Id%, 8) * Rz0# * Rz0# * Rz0# * Rx0#
dt# = dt# + Object(Id%, 9) * Rz0# * Rz0# * Rz0# * Ry0#
dt# = dt# + Object(Id%, 17) * Rx0# * Rx0# * Rx0#
dt# = dt# + Object(Id%, 18) * Ry0# * Ry0# * Ry0#
dt# = dt# + Object(Id%, 19) * Rz0# * Rz0# * Rz0#
dt# = dt# + Object(Id%, 20) * Rx0# * Rx0# * Ry0#
dt# = dt# + Object(Id%, 21) * Rx0# * Rx0# * Rz0#
dt# = dt# + Object(Id%, 23) * Ry0# * Ry0# * Rx0#
dt# = dt# + Object(Id%, 22) * Ry0# * Ry0# * Rz0#
dt# = dt# + Object(Id%, 24) * Rz0# * Rz0# * Rx0#
dt# = dt# + Object(Id%, 25) * Rz0# * Rz0# * Ry0#
dt# = dt# + Object(Id%, 16) * Rx0# * Ry0# * Rz0#
dt# = dt# + Object(Id%, 26) * Rx0# * Rx0#
dt# = dt# + Object(Id%, 27) * Ry0# * Ry0#
dt# = dt# + Object(Id%, 28) * Rz0# * Rz0#
dt# = dt# + Object(Id%, 29) * Rx0# * Ry0#
dt# = dt# + Object(Id%, 30) * Ry0# * Rz0#
dt# = dt# + Object(Id%, 31) * Rx0# * Rz0#
dt# = dt# + Object(Id%, 32) * Rx0#
dt# = dt# + Object(Id%, 33) * Ry0#
dt# = dt# + Object(Id%, 34) * Rz0#
dt# = dt# + Object(Id%, 35)

Equa4 DP#, DQ#, DR#, DS#, dt#, Dist1#, Dist2#, Dist3#, Dist4#, Err4%, nSol4%

IF nSol4% = 0 THEN FlagPlot% = 0: EXIT SUB
IF nSol4% = 1 THEN Dist4# = Dist1#: Dist3# = Dist1#: Dist2# = Dist1#
IF nSol4% = 2 THEN Dist4# = Dist2#: Dist3# = Dist1#
IF nSol4% = 3 THEN Dist4# = Dist3#
IF Dist1# > Dist2# THEN SWAP Dist1#, Dist2#
IF Dist1# > Dist3# THEN SWAP Dist1#, Dist3#
IF Dist1# > Dist4# THEN SWAP Dist1#, Dist4#
IF Dist2# > Dist3# THEN SWAP Dist2#, Dist3#
IF Dist2# > Dist4# THEN SWAP Dist2#, Dist4#
IF Dist3# > Dist4# THEN SWAP Dist3#, Dist4#

'=======================================================
' Check the intersections are not behind the screen
FlagPoint%(1) = 1: FlagPoint%(2) = 1: FlagPoint%(3) = 1: FlagPoint%(4) = 1
IF Dist1# < Ray(7) THEN FlagPoint%(1) = 0
IF Dist2# < Ray(7) THEN FlagPoint%(2) = 0
IF Dist3# < Ray(7) THEN FlagPoint%(3) = 0
IF Dist4# < Ray(7) THEN FlagPoint%(4) = 0

IF (FlagPoint%(1) = 0 AND FlagPoint%(2) = 0 AND FlagPoint%(3) = 0 AND FlagPoint%(4) = 0) THEN FlagPlot% = 0: EXIT SUB
IF FlagPoint%(1) = 0 THEN FlagPlot% = 0: EXIT SUB

xp!(1, 1) = Rx0# + Rx# * Dist1#: xp!(1, 2) = Ry0# + Ry# * Dist1#: xp!(1, 3) = Rz0# + Rz# * Dist1#
xp!(2, 1) = Rx0# + Rx# * Dist2#: xp!(2, 2) = Ry0# + Ry# * Dist2#: xp!(2, 3) = Rz0# + Rz# * Dist2#
xp!(3, 1) = Rx0# + Rx# * Dist3#: xp!(3, 2) = Ry0# + Ry# * Dist3#: xp!(3, 3) = Rz0# + Rz# * Dist3#
xp!(4, 1) = Rx0# + Rx# * Dist4#: xp!(4, 2) = Ry0# + Ry# * Dist4#: xp!(4, 3) = Rz0# + Rz# * Dist4#

'=======================================================
' Select the intersection point (object axis system)
IF FlagPoint%(1) = 1 THEN
xh!(1) = xp!(1, 1): xh!(2) = xp!(1, 2): xh!(3) = xp!(1, 3): Dist = Dist1#
ELSE
IF FlagPoint%(2) = 1 THEN
xh!(1) = xp!(2, 1): xh!(2) = xp!(2, 2): xh!(3) = xp!(2, 3): Dist = Dist2#
ELSE
IF FlagPoint%(3) = 1 THEN
xh!(1) = xp!(3, 1): xh!(2) = xp!(3, 2): xh!(3) = xp!(3, 3): Dist = Dist3#
ELSE
xh!(1) = xp!(4, 1): xh!(2) = xp!(4, 2): xh!(3) = xp!(4, 3): Dist = Dist4#
END IF
END IF
END IF

'=========================
' Back to main axis system
FOR k% = 1 TO 3
xt(k%) = Ray(k%) + Dist * Ray(k% + 3)
NEXT k%
FlagPlot% = 1
END SUB
'
':::::::::::::::
SUB Probe.Ray (Ray(), ShapeHit%, DistHit, xhHit(), xtHit(), xnHit(), FlagHit%)
' Probe space with a ray
' DistHit is the distance to the closest object
' xhHit is the hit point in the object axis system
' xtHit is the hit point in the main axis system
' xnHit is the normal vector in the main axis system
' FlagHit% is the hit indicator (0 when no intersection)

	DIM xh(3), xt(3), _xn!(3), RayHit(7)
	FlagHit% = 0     ' Reset hit flag
	DistHit = 1E+12  ' Start from faraway
	
	' Probe all objects with the ray
	FOR Id% = 1 TO NbObjects%
		Probe.Object Id%, Ray(), Dist, xh(), xt(), _xn!(), FlagPlot%
		IF FlagPlot% <> 0 AND Dist < DistHit AND Dist >= 0 THEN
			FlagHit% = FlagPlot%: ShapeHit% = Id%: DistHit = Dist
			FOR k% = 1 TO 3: xhHit(k%) = xh(k%): xtHit(k%) = xt(k%): xnHit(k%) = _xn!(k%): NEXT k%
			FOR k% = 1 TO 7: RayHit(k%) = Ray(k%): NEXT k%
		END IF
	NEXT Id%
	
	' Quit if nothing was hit
	IF FlagHit% = 0 THEN EXIT SUB
	
	' Get the normal vector
	SELECT CASE Shape(ShapeHit%).NumType
	CASE 100, 110, 120, 200, 203, 205
	' Optimised routines : don't recalculate normal
	CASE ELSE
	' Get the normal vector for the rest
	Calc.Normal ShapeHit%, xhHit(), xnHit(), RayHit()
	END SELECT

END SUB
'
':::::::::::::::
SUB Probe.Sky (Id%, Ray(), Dist, xh!(), xt!(), _xn!(), FlagPlot%)
' Probe the sky (Altitude = z0) with a ray
' Dist is the distance from the ray origin
' xh is the hit point in the object axis system
' xt is the hit point in the main axis system
' xn is the normal vector in the main axis system
' FlagPlot% is the hit indicator (0 when no intersection)

'Rx0 = Ray(1): Ry0 = Ray(2):
Rz0 = Ray(3) - Shape(Id%).z0

IF Ray(6) <> 0 THEN
Dist = -Rz0 / Ray(6)
ELSE
IF Ray(3) = 0 THEN : Dist = 0:  ELSE : FlagPlot% = 0: EXIT SUB
END IF
IF Dist < Ray(7) THEN FlagPlot% = 0: EXIT SUB
FOR k% = 1 TO 3
xh(k%) = Ray(k%) + Dist * Ray(k% + 3)
xt(k%) = xh(k%)
NEXT k%

_xn!(1) = 0: _xn!(2) = 0: _xn!(3) = -1
FlagPlot% = 1

END SUB
'
':::::::::::::::
SUB Ray.Pixel (Nx%, Ny%, Ray())
' Calculates the ray from camera through a given pixel
AliasX = AntiAlias * (rnd2 * 1 - .5)
AliasY = AntiAlias * (rnd2 * 1 - .5)
FOR k% = 1 TO 3
Ray(k%) = xCam(k%)
Ray(k% + 3) = xCenter(k%) - xCam(k%) + ((Nx% + AliasX - xScrCenter%) * UScreen(k%) + (Ny% + AliasY - yScrCenter%) * VScreen(k%)) / ScrHeight
NEXT k%
Ray(7) = Module(Ray(4), Ray(5), Ray(6))
FOR k% = 4 TO 6: Ray(k%) = Ray(k%) / Ray(7): NEXT k%
END SUB
'
':::::::::::::::
SUB Ray.Reflect (xt!(), _xn!(), Ray())
' Calculate the refelcted ray from a hit point
DIM xi!(3), Bias!(3)
FOR k% = 1 TO 3: xi!(k%) = xt!(k%) - xCam(k%): NEXT k%

Coeff = 2 * (xi!(1) * _xn!(1) + xi!(2) * _xn!(2) + xi!(3) * _xn!(3))

FOR k% = 1 TO 3
Bias!(k%) = 1 + AntiAlias * (rnd2 - .5) * .01
Ray(k%) = xt!(k%)
Ray(k% + 3) = (xi!(k%) - Coeff * _xn!(k%)) * Bias!(k%)
NEXT k%

Ray(7) = Module(Ray(4), Ray(5), Ray(6))
FOR k% = 4 TO 6: Ray(k%) = Ray(k%) / Ray(7): NEXT k%
Ray(7) = .001
END SUB
'
':::::::::::::::
SUB Ray.Shadow (xt!(), Ray())
' Calculate the ray from a hit point to the point of light
DIM Bias!(3)
FOR k% = 1 TO 3
Bias!(k%) = 1 + AntiAlias * (rnd2 - .5) * .01
Ray(k%) = xt(k%) 'xLight(k%)
Ray(k% + 3) = -(xt!(k%) - xLight(k%)) * Bias!(k%)
NEXT k%
Ray(7) = Module(Ray(4), Ray(5), Ray(6))
FOR k% = 4 TO 6: Ray(k%) = Ray(k%) / Ray(7): NEXT k%
Ray(7) = .01
END SUB
'
':::::::::::::::
FUNCTION SinD (A)
' Correct Sine value for an angle in degrees
sinD=sin(a*piover180)
'SinD = CosD(90 - A)
END FUNCTION
'
':::::::::::::::
FUNCTION Sqr3 (x#)
' Cubic root of a double length real number
IF x# = 0 THEN Sqr3 = 0: EXIT FUNCTION
IF x# > 0 THEN
Sqr3 = EXP(LOG(x#) / 3)
ELSE
Sqr3 = -EXP(LOG(-x#) / 3)
END IF
END FUNCTION
'
':::::::::::::::
FUNCTION Texture.Object! (Id%, FlagPlot%, xh(), _xn!())
' Textures and Bummapping applied to the shapes

' Wood : v = 20*P3D, W= v - INt(v)
' Marble = M = Cos(x+P3D)

'==============================
' Default Texture: Plain colour
IF Shape(Id%).Texture = 0 THEN Texture.Object! = Shape(Id%).HueRef + 90 * (FlagPlot% - 1)

'==============================
' 2D textures
IF Shape(Id%).Texture < 20 THEN

	IF Shape(Id%).Degree = 1 THEN
	SELECT CASE Shape(Id%).NumType
	CASE 100, 101, 104, 201, 204
	p = 2 * (xh(1) - INT(xh(1))) - 1
	q = 2 * (xh(2) - INT(xh(2))) - 1
	CASE 102, 105, 202, 205
	p = 2 * (xh(2) - INT(xh(2))) - 1
	q = 2 * (xh(3) - INT(xh(3))) - 1
	CASE 103, 106, 203, 206
	p = 2 * (xh(1) - INT(xh(1))) - 1
	q = 2 * (xh(3) - INT(xh(3))) - 1
	CASE ELSE
	END SELECT
	END IF
	
	IF Shape(Id%).Degree = 2 THEN
		p = Arg(xh(1), xh(2)) / Pi
		q = Arg(SQR(xh(1) ^ 2 + xh(2) ^ 2), xh(3)) / Pi
	END IF
	
	IF Shape(Id%).Degree = 3 THEN
		p! = xh(1) / Shape(Id%).dRef2 / 2
		q! = xh(2) / Shape(Id%).dRef2
	END IF
	
	IF Shape(Id%).Degree = 4 THEN
		p = -Arg(SQR(xh(1) ^ 2 + xh(2) ^ 2), xh(3)) / Pi
		q = Arg(xh(1), xh(2)) / Pi
	END IF
	
	SELECT CASE Shape(Id%).Texture
	CASE 11
	 Texture.Object! = Shape(Id%).HueRef + Mandelbrot(p, q)
	CASE 12
	 Texture.Object! = Shape(Id%).HueRef + Julia(p, q)
	CASE 13
	 Texture.Object! = 720 * Perlin!(p, q)
	CASE 14
	 Noise = COS(p * 10 + q * 7 + 6 * Perlin!(p, q))
	 Texture.Object! = 200 + 320 * Noise
	CASE 15
	 Texture.Object! = 30 * COS(10 * p + 10 * Perlin!(p, q))
	CASE 16  ' Mandelbrot Ring
	 nMdb% = 8
	 q = q * nMdb% / 2
	 q = (q - INT(q + .5)) / 4
	 Texture.Object! = Mandelbrot(2 * p, 2 * q)
	CASE 17  ' ChessBoard
	 Flagx% = ABS(INT(xh(1) / Shape(Id%).dRef1) MOD (2))
	 Flagy% = ABS(INT(xh(2) / Shape(Id%).dRef2) MOD (2))
	 IF Flagx% = Flagy% THEN
	  Texture.Object! = Shape(Id%).HueRef + 90
	 ELSE
	  Texture.Object! = Shape(Id%).HueRef
	 END IF
	CASE ELSE
	 Texture.Object! = Shape(Id%).HueRef + 90 * (FlagPlot% - 1)
	END SELECT

END IF

'========================================
' Perlin 3D Hue based texture. For non flat shapes
IF Shape(Id%).Texture = 20 THEN
	p! = _xn!(1)
	q! = _xn!(2)
	r! = _xn!(3)
	Texture.Object! = 720 * COS(Pi * (p! + q! + Perlin3D!(p!, q!, r!)))
END IF

' ========================================
' Sea texture (designed for an horizontal plan)
IF Shape(Id%).Texture = 30 THEN
	e = 1 / 90 * (1 + .25 * rnd2)
	p! = 2 * (xh(1) / 7 - INT(xh(1) / 7)) - 1
	q! = 2 * (xh(2) / 7 - INT(xh(2) / 7)) - 1
	dx! = Perlin!(p! - e, q!) - Perlin!(p! + e, q!)
	dy! = Perlin!(p!, q! - e) - Perlin!(p!, q! + e)
	_xn!(1) = 15 * dx! * COS((xh(1) + xh(2)) * Pi / 2) ^ 2
	_xn!(2) = 15 * dy!
	_xn!(3) = 1
	Norm = Module(_xn!(1), _xn!(2), _xn!(3))
	'print norm
	FOR k% = 1 TO 3: _xn!(k%) = _xn!(k%) / Norm: NEXT k%
	Texture.Object! = Shape(Id%).HueRef
END IF

' ========================================
' Sky texture (designed for an horizontal plan)
IF Shape(Id%).Texture = 31 THEN
	p = 2 * (xh(1) / 100 - INT(xh(1) / 100)) - 1
	q = 2 * (xh(2) / 100 - INT(xh(2) / 100)) - 1
	'print p,q
	_xn!(1) = 5 * Perlin!(p, q): _xn!(2) = 5 * Perlin!(q, p): _xn!(3) = -1
	Norm = Module(_xn!(1), _xn!(2), _xn!(3))
	'print norm
	FOR k% = 1 TO 3: _xn!(k%) = _xn!(k%) / Norm: NEXT k%
	Texture.Object! = Shape(Id%).HueRef
END IF

' ========================================
' Nephroid Texture
IF Shape(Id%).Texture = 32 THEN
	AlphaRef = 55 * Pi / 180
	xp0! = xh(1) * COS(AlphaRef) + xh(2) * SIN(AlphaRef)
	yp0! = -xh(1) * SIN(AlphaRef) + xh(2) * COS(AlphaRef)
	IF xp0! < 0 THEN
		Temp! = Nephroid(xp0!, yp0!, Shape(Id%).dRef1)
		IF Temp! > 0 THEN
			Shape(Id%).Brilliance = 1 / (1 + Temp! * 100)
		ELSE
			Shape(Id%).Brilliance = 1 / (1 - Temp! * 1000)
		END IF
	ELSE
		Temp! = Nephroid(0, yp0!, Shape(Id%).dRef1)
		Shape(Id%).Brilliance = 1 / (1 - Temp! * 1000)
	END IF
END IF

' ========================================
' Bumpmapping (based on Perlin Noises)
' ========================================
' Bumpmapping for non flat shapes
IF Shape(Id%).Bump = 1 THEN
	e = .01
	p! = _xn!(1) * .9
	q! = _xn!(2) * .9
	r! = _xn!(3) * .9
	dx! = Perlin3D!(p! - e, q!, r!) - Perlin3D!(p! + e, q!, r!)
	dy! = Perlin3D!(p!, q! - e, r!) - Perlin3D!(p!, q! + e, r!)
	dZ! = Perlin3D!(p!, q!, r! - e) - Perlin3D!(p!, q!, r! + e)
	_xn!(1) = _xn!(1) + 5 * dx!
	_xn!(2) = _xn!(2) + 5 * dy!
	_xn!(3) = _xn!(3) + 5 * dZ!
	Norm = Module(_xn!(1), _xn!(2), _xn!(3))
	FOR k% = 1 TO 3: _xn!(k%) = _xn!(k%) / Norm: NEXT k%
	Texture.Object! = Shape(Id%).HueRef + 90 * (FlagPlot% - 1)
END IF

' Bumpmapping for flat shapes
IF Shape(Id%).Bump = 2 THEN

	SELECT CASE Shape(Id%).NumType
	CASE 101, 104, 201, 204
		p! = xh(1) / Shape(Id%).dRef1
		q! = xh(2) / Shape(Id%).dRef2
		e = .01
		dx! = Perlin!(p! - e, q!) - Perlin!(p! + e, q!)
		dy! = Perlin!(p!, q! - e) - Perlin!(p!, q! + e)
		_xn!(1) = _xn!(1) + 5 * dx!
		_xn!(2) = _xn!(2) + 5 * dy!
		_xn!(3) = _xn!(3)
	
	CASE 102, 105, 202, 205
		p! = xh(2) / Shape(Id%).dRef1
		q! = xh(3) / Shape(Id%).dRef2
		e = .01
		dx! = Perlin!(p! - e, q!) - Perlin!(p! + e, q!)
		dy! = Perlin!(p!, q! - e) - Perlin!(p!, q! + e)
		
		_xn!(2) = _xn!(2) + 5 * dx!
		_xn!(3) = _xn!(3) + 5 * dy!
		_xn!(1) = _xn!(1)
	
	CASE 103, 106, 203, 206
		p! = xh(1) / Shape(Id%).dRef1
		q! = xh(3) / Shape(Id%).dRef2
		e = .01
		dx! = Perlin!(p! - e, q!) - Perlin!(p! + e, q!)
		dy! = Perlin!(p!, q! - e) - Perlin!(p!, q! + e)
		_xn!(1) = _xn!(1) + 5 * dx!
		_xn!(3) = _xn!(3) + 5 * dy!
		_xn!(2) = _xn!(2)
	CASE ELSE
END SELECT

Norm = Module(_xn!(1), _xn!(2), _xn!(3))
FOR k% = 1 TO 3: _xn!(k%) = _xn!(k%) / Norm: NEXT k%
END IF

IF Shape(Id%).Bump = 3 THEN
	' Water effects
	Amplitude! = .1
	Fade! = .2
	Omega! = 8
	r! = SQR(xh(1) ^2 + xh(2) ^2)
	
	dAdR! = -Fade! / (1 + Fade! * r!) * SIN(Omega! * r!) + Omega! / (1 + Fade! * r!) * COS(Omega! * r!)
	
	IF r! <> 0 THEN
		dRdX! = xh(1) / r!
		drdY! = xh(2) / r!
	ELSE
		dRdX! = 0: drdY! = 0
	END IF
	
	dadX! = Amplitude! * dAdR! * dRdX!
	dadY! = Amplitude! * dAdR! * drdY!
  _xn!(1) = -dadX!
  _xn!(2) = -dadY!
  _xn!(3) = 1
	Norm = Module(_xn!(1), _xn!(2), _xn!(3))
  FOR k% = 1 TO 3: _xn!(k%) = _xn!(k%) / Norm: NEXT k%
END IF

END FUNCTION

'---------------------------------end of TCRay21C bas------------------------------------------