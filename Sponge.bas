'============================================================================
' The Mandelbrot Dazibao - November 2003 - http://mandelbrot.dazibao.free.fr
'============================================================================
'            Raytracing rendering of the Sponge of Sierpinsky
'============================================================================
' Copyright 2003 - The Mandelbrot Dazibao - http://mandelbrot.dazibao.free.fr
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
' $INCLUDE: 'TcLib17.bi'
defsng a-z
DECLARE SUB Init.Space (AlphaCam!, BetaCam!, DistCam!, DistScreen!)
DECLARE SUB Draw.Space ()
DECLARE SUB Ray.Pixel (Nx%, Ny%, Ray!())

DECLARE SUB Init.Arrays ()
DECLARE SUB Probe.Cube (Ray!(), x0!, y0!, z0!, HalfSide!, Dist!, FlagPlot%)
DECLARE SUB Probe.Ray (Ray!(), Dist!, FlagPlot%)
DECLARE SUB Probe.Square (Ray!(), x0!, y0!, z0!, HalfSide!, Normal%, Dist!, FlagPlot%)
DECLARE SUB Probe.Shape (Ray!(), x0!, y0!, z0!, dRef!, Face%, Dist!, FlagPlot%, xh!, yh!, zh!)

DECLARE SUB Cloud.Background ()
DECLARE SUB Init.Perlin ()
DECLARE FUNCTION Cloud.Value! (p!, q!)
DECLARE FUNCTION Perlin! (xp!, xq!)
DECLARE FUNCTION Noise3! (A%, b%, C%)
DECLARE FUNCTION Perlin3D! (xt!, yt!, zt!)

DECLARE FUNCTION Light.Specular! (xp!(), xo!())
DECLARE FUNCTION CosD! (A!)
DECLARE FUNCTION SinD! (A!)

' Shared raytracing variables
DIM SHARED xCam(3), xLight(3), xCenter(3), UScreen(3), VScreen(3)

CONST CubeSize! = 3

' Shared variables for the Perlin noises
DIM SHARED Seed&, Noise%(129, 129), Octaves%, Persistence!, Span%, Amplitude!(16)

' Shared objects definition arrays
DIM SHARED NormVect%(6, 3), CubeFace%(6, 3), Sh%(28, 6, 4)

DIM SHARED RankMax%

'Ffix   ' Uncomment this line if you use Ffix (3 times speed booster)

RankMax% = 5
SizeScreen = 4
_name$ = "Cubes" + RIGHT$(STR$(RankMax%), 1)
SetVGA

Init.Perlin
Init.Space 5, -10, 1.3, .8
'Init.Space 25, 30, 5, .8
Init.Arrays
T0 = TIMER
Cloud.Background
Draw.Space
EndView
dt = TIMER - T0
PRINT dt
WaitKey
EndProg

SUB Cloud.Background
Init.Perlin
FOR Ny% = 0 TO ScrHeight - 1
SCREENLOCK
FOR Nx% = 0 TO ScrWidth - 1
p! = 2 * (Nx% - xScrCenter%) / ScrWidth
q! = 2 * (Ny% - yScrCenter%) / ScrHeight
HSVto_RGB 240, 100, Cloud.Value(p!, q!)
Pset24 Nx%, Ny%
NEXT Nx%
SCREENUNLOCK
NEXT Ny%
END SUB

FUNCTION Cloud.Value! (p!, q!)
CloudCover = .1        ' 0 = Blue sky, 1 = White sky
CloudSharpness = .4    ' 0 = Flat, 1 = Sharp
C = Perlin!(p!, q!) - CloudCover
IF C < 0 THEN C = 0
Cloud.Value! = (CloudSharpness ^ C) * 100
END FUNCTION

FUNCTION CosD (A)
COSD=COS(A*PIOVER180)
EXIT FUNCTION

A = A - INT(A / 360 + .5) * 360

IF A >= 0 AND A <= 90 THEN
x# = A / 180 * Pi
    IF COS(x#) < .5 THEN
    CosD = SIN(Pi / 2 - x#)
    ELSE
    CosD = COS(x#)
    END IF
EXIT FUNCTION
END IF

IF A < 0 AND A >= -90 THEN
x# = -A / 180 * Pi
    IF COS(x#) < .5 THEN
    CosD = SIN(Pi / 2 - x#)
    ELSE
    CosD = COS(x#)
    END IF
EXIT FUNCTION
END IF

IF A > 90 AND A <= 180 THEN
x# = (180 - A) / 180 * Pi
    IF COS(x#) < .5 THEN
    CosD = SIN(x# - Pi / 2)
    ELSE
    CosD = -COS(x#)
    END IF
EXIT FUNCTION
END IF

IF A < -90 AND A >= -180 THEN
x# = (A - 180) / 180 * Pi
    IF COS(x#) < .5 THEN
    CosD = -SIN(Pi / 2 - x#)
    ELSE
    CosD = -COS(x#)
    END IF
EXIT FUNCTION
END IF

END FUNCTION

SUB Draw.Space
DIM Ray(7), xtHit!(3), xnHit!(3)

FOR Ny% = 0 TO ScrHeight - 1
SCREENLOCK
FOR Nx% = 0 TO ScrWidth - 1
Ray.Pixel Nx%, Ny%, Ray()
Probe.Ray Ray(), Dist!, FlagHit%

IF FlagHit% <> 0 THEN
FOR k% = 1 TO 3
xtHit!(k%) = Ray(k%) + Dist! * Ray(k% + 3)
xnHit!(k%) = NormVect%(FlagHit%, k%)
NEXT k%


p! = xtHit(1) / CubeSize!
q! = xtHit(2) / CubeSize!
r! = xtHit(3) / CubeSize!
Hue = 8 * 360 * COS(Pi * (p! + q! + Perlin3D!(p!, q!, r!)))

HSVto_RGB Hue, 200 / 3, Light.Specular(xtHit!(), xnHit!())
Pset24 Nx%, Ny%
END IF

NEXT Nx%
SCREENUNLOCK
IF LEN(INKEY$) THEN EXIT SUB
NEXT Ny%
END SUB

SUB Init.Arrays

' 6 possible normal vectors
NormVect%(1, 1) = 1: NormVect%(1, 2) = 0: NormVect%(1, 3) = 0
NormVect%(2, 1) = 0: NormVect%(2, 2) = 1: NormVect%(2, 3) = 0
NormVect%(3, 1) = 0: NormVect%(3, 2) = 0: NormVect%(3, 3) = 1
NormVect%(4, 1) = -1: NormVect%(4, 2) = 0: NormVect%(4, 3) = 0
NormVect%(5, 1) = 0: NormVect%(5, 2) = -1: NormVect%(5, 3) = 0
NormVect%(6, 1) = 0: NormVect%(6, 2) = 0: NormVect%(6, 3) = -1

' 6 squares for each cube
CubeFace%(1, 1) = 1: CubeFace%(1, 2) = 0: CubeFace%(1, 3) = 0
CubeFace%(2, 1) = 0: CubeFace%(2, 2) = 1: CubeFace%(2, 3) = 0
CubeFace%(3, 1) = 0: CubeFace%(3, 2) = 0: CubeFace%(3, 3) = 1
CubeFace%(4, 1) = -1: CubeFace%(4, 2) = 0: CubeFace%(4, 3) = 0
CubeFace%(5, 1) = 0: CubeFace%(5, 2) = -1: CubeFace%(5, 3) = 0
CubeFace%(6, 1) = 0: CubeFace%(6, 2) = 0: CubeFace%(6, 3) = -1

' 28 squares for the fractal generator

' 1st face

' Main face = 8 squares
Id% = 1: Sh%(Id%, 1, 1) = 0: Sh%(Id%, 1, 2) = -2: Sh%(Id%, 1, 3) = 2: Sh%(Id%, 1, 4) = 1
Id% = 2: Sh%(Id%, 1, 1) = 0: Sh%(Id%, 1, 2) = 0: Sh%(Id%, 1, 3) = 2: Sh%(Id%, 1, 4) = 1
Id% = 3: Sh%(Id%, 1, 1) = 0: Sh%(Id%, 1, 2) = 2: Sh%(Id%, 1, 3) = 2: Sh%(Id%, 1, 4) = 1
Id% = 4: Sh%(Id%, 1, 1) = 0: Sh%(Id%, 1, 2) = -2: Sh%(Id%, 1, 3) = 0: Sh%(Id%, 1, 4) = 1
Id% = 5: Sh%(Id%, 1, 1) = 0: Sh%(Id%, 1, 2) = 2: Sh%(Id%, 1, 3) = 0: Sh%(Id%, 1, 4) = 1
Id% = 6: Sh%(Id%, 1, 1) = 0: Sh%(Id%, 1, 2) = -2: Sh%(Id%, 1, 3) = -2: Sh%(Id%, 1, 4) = 1
Id% = 7: Sh%(Id%, 1, 1) = 0: Sh%(Id%, 1, 2) = 0: Sh%(Id%, 1, 3) = -2: Sh%(Id%, 1, 4) = 1
Id% = 8: Sh%(Id%, 1, 1) = 0: Sh%(Id%, 1, 2) = 2: Sh%(Id%, 1, 3) = -2: Sh%(Id%, 1, 4) = 1

' 1st tunnel = 4 squares
Id% = 9: Sh%(Id%, 1, 1) = -1: Sh%(Id%, 1, 2) = 1: Sh%(Id%, 1, 3) = 0: Sh%(Id%, 1, 4) = 5
Id% = 10: Sh%(Id%, 1, 1) = -1: Sh%(Id%, 1, 2) = -1: Sh%(Id%, 1, 3) = 0: Sh%(Id%, 1, 4) = 2
Id% = 11: Sh%(Id%, 1, 1) = -1: Sh%(Id%, 1, 2) = 0: Sh%(Id%, 1, 3) = 1: Sh%(Id%, 1, 4) = 6
Id% = 12: Sh%(Id%, 1, 1) = -1: Sh%(Id%, 1, 2) = 0: Sh%(Id%, 1, 3) = -1: Sh%(Id%, 1, 4) = 3

' 2nd tunnel = 4 squares
Id% = 13: Sh%(Id%, 1, 1) = -5: Sh%(Id%, 1, 2) = 1: Sh%(Id%, 1, 3) = 0: Sh%(Id%, 1, 4) = 5
Id% = 14: Sh%(Id%, 1, 1) = -5: Sh%(Id%, 1, 2) = -1: Sh%(Id%, 1, 3) = 0: Sh%(Id%, 1, 4) = 2
Id% = 15: Sh%(Id%, 1, 1) = -5: Sh%(Id%, 1, 2) = 0: Sh%(Id%, 1, 3) = 1: Sh%(Id%, 1, 4) = 6
Id% = 16: Sh%(Id%, 1, 1) = -5: Sh%(Id%, 1, 2) = 0: Sh%(Id%, 1, 3) = -1: Sh%(Id%, 1, 4) = 3

' Rear pannels of the four side tunnels
Id% = 17: Sh%(Id%, 1, 1) = -4: Sh%(Id%, 1, 2) = 0: Sh%(Id%, 1, 3) = 2: Sh%(Id%, 1, 4) = 1
Id% = 18: Sh%(Id%, 1, 1) = -4: Sh%(Id%, 1, 2) = 0: Sh%(Id%, 1, 3) = -2: Sh%(Id%, 1, 4) = 1
Id% = 19: Sh%(Id%, 1, 1) = -4: Sh%(Id%, 1, 2) = 2: Sh%(Id%, 1, 3) = 0: Sh%(Id%, 1, 4) = 1
Id% = 20: Sh%(Id%, 1, 1) = -4: Sh%(Id%, 1, 2) = -2: Sh%(Id%, 1, 3) = 0: Sh%(Id%, 1, 4) = 1

' Horizontal pannels of the two lateral tunnels
Id% = 21: Sh%(Id%, 1, 1) = -3: Sh%(Id%, 1, 2) = 2: Sh%(Id%, 1, 3) = 1: Sh%(Id%, 1, 4) = 6
Id% = 22: Sh%(Id%, 1, 1) = -3: Sh%(Id%, 1, 2) = -2: Sh%(Id%, 1, 3) = 1: Sh%(Id%, 1, 4) = 6
Id% = 23: Sh%(Id%, 1, 1) = -3: Sh%(Id%, 1, 2) = 2: Sh%(Id%, 1, 3) = -1: Sh%(Id%, 1, 4) = 3
Id% = 24: Sh%(Id%, 1, 1) = -3: Sh%(Id%, 1, 2) = -2: Sh%(Id%, 1, 3) = -1: Sh%(Id%, 1, 4) = 3

Id% = 25: Sh%(Id%, 1, 1) = -3: Sh%(Id%, 1, 2) = 1: Sh%(Id%, 1, 3) = 2: Sh%(Id%, 1, 4) = 5
Id% = 26: Sh%(Id%, 1, 1) = -3: Sh%(Id%, 1, 2) = 1: Sh%(Id%, 1, 3) = -2: Sh%(Id%, 1, 4) = 5
Id% = 27: Sh%(Id%, 1, 1) = -3: Sh%(Id%, 1, 2) = -1: Sh%(Id%, 1, 3) = 2: Sh%(Id%, 1, 4) = 2
Id% = 28: Sh%(Id%, 1, 1) = -3: Sh%(Id%, 1, 2) = -1: Sh%(Id%, 1, 3) = -2: Sh%(Id%, 1, 4) = 2

' 2nd face
FOR Id% = 1 TO 28
Sh%(Id%, 2, 1) = Sh%(Id%, 1, 3)
Sh%(Id%, 2, 2) = Sh%(Id%, 1, 1)
Sh%(Id%, 2, 3) = Sh%(Id%, 1, 2)
IF Sh%(Id%, 1, 4) = 1 THEN Sh%(Id%, 2, 4) = 2
IF Sh%(Id%, 1, 4) = 2 THEN Sh%(Id%, 2, 4) = 3
IF Sh%(Id%, 1, 4) = 3 THEN Sh%(Id%, 2, 4) = 1
IF Sh%(Id%, 1, 4) = 4 THEN Sh%(Id%, 2, 4) = 5
IF Sh%(Id%, 1, 4) = 5 THEN Sh%(Id%, 2, 4) = 6
IF Sh%(Id%, 1, 4) = 6 THEN Sh%(Id%, 2, 4) = 4
NEXT Id%

' 3rd face
FOR Id% = 1 TO 28
Sh%(Id%, 3, 1) = Sh%(Id%, 1, 2)
Sh%(Id%, 3, 2) = Sh%(Id%, 1, 3)
Sh%(Id%, 3, 3) = Sh%(Id%, 1, 1)
IF Sh%(Id%, 1, 4) = 1 THEN Sh%(Id%, 3, 4) = 3
IF Sh%(Id%, 1, 4) = 4 THEN Sh%(Id%, 3, 4) = 6
IF Sh%(Id%, 1, 4) = 2 THEN Sh%(Id%, 3, 4) = 1
IF Sh%(Id%, 1, 4) = 5 THEN Sh%(Id%, 3, 4) = 4
IF Sh%(Id%, 1, 4) = 3 THEN Sh%(Id%, 3, 4) = 2
IF Sh%(Id%, 1, 4) = 6 THEN Sh%(Id%, 3, 4) = 5
NEXT Id%


' 4th face
FOR Id% = 1 TO 28
FOR k% = 1 TO 3
Sh%(Id%, 4, k%) = -Sh%(Id%, 1, k%)
NEXT k%
Sh%(Id%, 4, 4) = (Sh%(Id%, 1, 4) + 2) MOD (6) + 1
NEXT Id%

' 5th face
FOR Id% = 1 TO 28
FOR k% = 1 TO 3
Sh%(Id%, 5, k%) = -Sh%(Id%, 2, k%)
NEXT k%
Sh%(Id%, 5, 4) = (Sh%(Id%, 2, 4) + 2) MOD (6) + 1
NEXT Id%

' 6th face
FOR Id% = 1 TO 28
FOR k% = 1 TO 3
Sh%(Id%, 6, k%) = -Sh%(Id%, 3, k%)
NEXT k%
Sh%(Id%, 6, 4) = (Sh%(Id%, 3, 4) + 2) MOD (6) + 1
NEXT Id%


END SUB

SUB Init.Perlin
' Inits the Noise% array for 2D perlin noises
' The pattern is perfectly tileable
' Based on squares division technique
' Fast, but can probably be optimised more:
' 2^n+1 can probably be replaced by plain 2^n via AND or MOD tricks...

RANDOMIZE TIMER
Seed& = 103476 * rnd2    ' This number fell from my keyboard :-)
'RANDOMIZE 76850
'Seed& = 103476 * .5
Octaves% = 7: Persistence! = 1.5: Span% = 2 ^ Octaves% + 1

FOR k% = 0 TO Octaves% + 1
Amplitude!(k%) = (1 / Persistence!) ^ k%  ' You can try different logics!
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
zNew% = INT(Alt% * (1 + (rnd2 - .5) * Amplitude!(Rank%)))
Noise%(x% + Grid% / 2, 1) = zNew%
Noise%(x% + Grid% / 2, Span%) = zNew%
NEXT kx%

FOR ky% = 1 TO nStep% - 1
x% = 1: y% = (ky% - 1) * Grid% + 1
Alt% = (Noise%(x%, y%) + Noise%(x%, y% + Grid%)) / 2
zNew% = INT(Alt% * (1 + (rnd2 - .5) * Amplitude!(Rank%)))
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
Noise%(x% + Grid% / 2, y% + Grid% / 2) = INT(Alt% * (1 + (rnd2 - .5) * Amplitude!(Rank%)))

Alt% = (Noise%(x%, y%) + Noise%(x% + Grid%, y%)) / 2
IF y% <> 1 THEN Noise%(x% + Grid% / 2, y%) = INT(Alt% * (1 + (rnd2 - .5) * Amplitude!(Rank%)))

Alt% = (Noise%(x%, y%) + Noise%(x%, y% + Grid%)) / 2
IF x% <> 1 THEN Noise%(x%, y% + Grid% / 2) = INT(Alt% * (1 + (rnd2 - .5) * Amplitude!(Rank%)))

Alt% = (Noise%(x% + Grid%, y%) + Noise%(x% + Grid%, y% + Grid%)) / 2
IF (x% + Grid%) <> Span% THEN Noise%(x% + Grid%, y% + Grid% / 2) = INT(Alt% * (1 + (rnd2 - .5) * Amplitude!(Rank%)))

Alt% = (Noise%(x%, y% + Grid%) + Noise%(x% + Grid%, y% + Grid%)) / 2
IF (y% + Grid%) <> Span% THEN Noise%(x% + Grid% / 2, y% + Grid%) = INT(Alt% * (1 + (rnd2 - .5) * Amplitude!(Rank%)))

NEXT ky%
NEXT kx%

NEXT Rank%


END SUB

SUB Init.Space (AlphaCam, BetaCam, DistCam, DistScreen)

' Point of light
'aLight = 30: bLight = 60: rLight = 10
aLight = 90: bLight = 0: rLight = 1

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
UScreen(1) = -SinD(aCam): UScreen(2) = CosD(aCam): UScreen(3) = 0
' Screen vertical axis (downwards)
VScreen(1) = SinD(bCam) * CosD(aCam): VScreen(2) = SinD(bCam) * SinD(aCam): VScreen(3) = -CosD(bCam)

END SUB

FUNCTION Light.Specular (xp(), xo())
' Returns the Light.Specular to apply to a given point on a surface
' The point of reflection is Xp, the normal vector to the surface is Xo
DIM xi(3), xr(3), xv(3)
FOR i = 1 TO 3
xi(i) = xp(i) - xLight(i)
xv(i) = xCam(i) - xp(i): NEXT i
Coeff = 2 * (xi(1) * xo(1) + xi(2) * xo(2) + xi(3) * xo(3))
FOR i = 1 TO 3: xr(i) = xi(i) - Coeff * xo(i): NEXT i
NormProd = SQR((xi(1) * xi(1) + xi(2) * xi(2) + xi(3) * xi(3)) * (xv(1) * xv(1) + xv(2) * xv(2) + xv(3) * xv(3)))
Light.Specular = 50 + 50 * (xr(1) * xv(1) + xr(2) * xv(2) + xr(3) * xv(3)) / NormProd
END FUNCTION

FUNCTION Noise3! (A%, b%, C%)
' This controlled noise function is the basis of the 3D Perlin noise
Noise3! = rnd2(-rnd2(-A% * b%) * rnd2(-C%) * Seed&) - .5
END FUNCTION

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

FUNCTION Perlin3D (xt!, yt!, zt!)
' Perlin 3D noise. The Perlin value is calculated for every point
' without heavy array storage. Slow response time,
' but it's the only solution without EMS...

x! = (xt! + 1) / 2 * Power2%(Octaves%) + 1
y! = (yt! + 1) / 2 * Power2%(Octaves%) + 1
z! = (zt! + 1) / 2 * Power2%(Octaves%) + 1

Rank% = 0
ax% = 1: bx% = Span%
Ay% = 1: By% = Span%
az% = 1: bz% = Span%
z1% = 128: z2% = 128: z3% = 128: z4% = 128
z5% = 128: z6% = 128: z7% = 128: z8% = 128

DO UNTIL ((bx% - x!) <= 1 AND (x! - ax%) <= 1) AND ((By% - y!) <= 1 AND (y! - Ay%) <= 1) AND ((bz% - z!) <= 1 AND (z! - az%) <= 1)
Rank% = Rank% + 1
Grid% = Power2%(Octaves% - Rank%)

IF bx% - x! > x! - ax% THEN
bx% = bx% - Grid%
    IF By% - y! > y! - Ay% THEN
    By% = By% - Grid%
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
z2% = z2n% * (1 + Noise3!(bx%, Ay%, az%) * Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, By%, az%) * Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, By%, az%) * Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, Ay%, bz%) * Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, Ay%, bz%) * Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, By%, bz%) * Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, By%, bz%) * Amplitude!(Rank%))
   
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
z1% = z1n% * (1 + Noise3!(ax%, Ay%, az%) * Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, Ay%, az%) * Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, By%, az%) * Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, By%, az%) * Amplitude!(Rank%))
z5% = z5n%
z6% = z6n% * (1 + Noise3!(bx%, Ay%, bz%) * Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, By%, bz%) * Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, By%, bz%) * Amplitude!(Rank%))
   
        END IF
    ELSE
    Ay% = Ay% + Grid%
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
z1% = z1n% * (1 + Noise3!(ax%, Ay%, az%) * Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, Ay%, az%) * Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, By%, az%) * Amplitude!(Rank%))
z4% = z4n%
z5% = z5n% * (1 + Noise3!(ax%, Ay%, bz%) * Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, Ay%, bz%) * Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, By%, bz%) * Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, By%, bz%) * Amplitude!(Rank%))
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
z1% = z1n% * (1 + Noise3!(ax%, Ay%, az%) * Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, Ay%, az%) * Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, By%, az%) * Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, By%, az%) * Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, Ay%, bz%) * Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, Ay%, bz%) * Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, By%, bz%) * Amplitude!(Rank%))
z8% = z8n%
        END IF
    END IF

ELSE
ax% = ax% + Grid%
    IF By% - y! > y! - Ay% THEN
    By% = By% - Grid%
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
z1% = z1n% * (1 + Noise3!(ax%, Ay%, az%) * Amplitude!(Rank%))
z2% = z2n%
z3% = z3n% * (1 + Noise3!(bx%, By%, az%) * Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, By%, az%) * Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, Ay%, bz%) * Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, Ay%, bz%) * Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, By%, bz%) * Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, By%, bz%) * Amplitude!(Rank%))
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
z1% = z1n% * (1 + Noise3!(ax%, Ay%, az%) * Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, Ay%, az%) * Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, By%, az%) * Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, By%, az%) * Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, Ay%, bz%) * Amplitude!(Rank%))
z6% = z6n%
z7% = z7n% * (1 + Noise3!(bx%, By%, bz%) * Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, By%, bz%) * Amplitude!(Rank%))
        END IF
    ELSE
    Ay% = Ay% + Grid%
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
z1% = z1n% * (1 + Noise3!(ax%, Ay%, az%) * Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, Ay%, az%) * Amplitude!(Rank%))
z3% = z3n%
z4% = z4n% * (1 + Noise3!(ax%, By%, az%) * Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, Ay%, bz%) * Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, Ay%, bz%) * Amplitude!(Rank%))
z7% = z7n% * (1 + Noise3!(bx%, By%, bz%) * Amplitude!(Rank%))
z8% = z8n% * (1 + Noise3!(ax%, By%, bz%) * Amplitude!(Rank%))
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
z1% = z1n% * (1 + Noise3!(ax%, Ay%, az%) * Amplitude!(Rank%))
z2% = z2n% * (1 + Noise3!(bx%, Ay%, az%) * Amplitude!(Rank%))
z3% = z3n% * (1 + Noise3!(bx%, By%, az%) * Amplitude!(Rank%))
z4% = z4n% * (1 + Noise3!(ax%, By%, az%) * Amplitude!(Rank%))
z5% = z5n% * (1 + Noise3!(ax%, Ay%, bz%) * Amplitude!(Rank%))
z6% = z6n% * (1 + Noise3!(bx%, Ay%, bz%) * Amplitude!(Rank%))
z7% = z7n%
z8% = z8n% * (1 + Noise3!(ax%, By%, bz%) * Amplitude!(Rank%))
        END IF
END IF

END IF
LOOP

zp1! = z1% * (bx% - x!) + z2% * (x! - ax%)
zp2! = z4% * (bx% - x!) + z3% * (x! - ax%)
zp12! = zp1! * (By% - y!) + zp2! * (y! - Ay%)

zp3! = z5% * (bx% - x!) + z6% * (x! - ax%)
zp4! = z8% * (bx% - x!) + z7% * (x! - ax%)
zp34! = zp3! * (By% - y!) + zp4! * (y! - Ay%)

Zp! = zp12! * (bz% - z!) + zp34! * (z! - az%)


Perlin3D! = Zp! / 255

END FUNCTION

SUB Probe.Cube (Ray(), x0!, y0!, z0!, HalfSide!, Dist!, FlagPlot%)
Dist! = 1E+12: FlagPlot% = 0

FOR k% = 1 TO 6
xf0! = CubeFace%(k%, 1) * HalfSide!
yf0! = CubeFace%(k%, 2) * HalfSide!
zf0! = CubeFace%(k%, 3) * HalfSide!
Normal% = k%
FlagHit% = 0
Probe.Square Ray(), xf0!, yf0!, zf0!, HalfSide!, Normal%, DistHit!, FlagHit%
IF FlagHit% <> 0 AND DistHit! < Dist! THEN
Dist! = DistHit!
FlagPlot% = FlagHit%
END IF

NEXT k%

END SUB

SUB Probe.Ray (Ray(), Dist!, FlagPlot%)
FlagPlot% = 0: Dist! = 1E+12

HalfSide! = CubeSize! / 2
x0! = 0: y0! = 0: z0! = 0
Probe.Cube Ray(), x0!, y0!, z0!, HalfSide!, DistHit!, FlagFace%
dRef! = HalfSide!: Face% = FlagFace%
xf0! = CubeFace%(Face%, 1) * HalfSide!
yf0! = CubeFace%(Face%, 2) * HalfSide!
zf0! = CubeFace%(Face%, 3) * HalfSide!

IF RankMax% = 0 AND FlagFace% <> 0 THEN Dist! = DistHit!: FlagPlot% = FlagFace%: EXIT SUB

FOR Rank% = 1 TO RankMax%
dRef! = dRef! / 3
NextDepth:
Dist! = 1E+12: FlagPlot% = 0
Probe.Shape Ray(), xf0!, yf0!, zf0!, dRef!, Face%, DistHit!, FlagHit%, xh!, yh!, zh!
IF (FlagHit% <> 0 AND DistHit! < Dist! AND DistHit! >= 0) THEN
FlagPlot% = FlagHit%: Dist! = DistHit!: Face% = FlagHit%
xf0! = xh!: yf0! = yh!: zf0! = zh!
END IF

IF FlagPlot% = 0 AND Rank% > 1 THEN
SELECT CASE Face%
CASE 1
xf0! = xf0! - 6 * dRef!
IF ABS(xf0!) > CubeSize! / 2 THEN FlagPlot% = 0: EXIT SUB
CASE 2
yf0! = yf0! - 6 * dRef!
IF ABS(yf0!) > CubeSize! / 2 THEN FlagPlot% = 0: EXIT SUB
CASE 3
zf0! = zf0! - 6 * dRef!
IF ABS(zf0!) > CubeSize! / 2 THEN FlagPlot% = 0: EXIT SUB
CASE 4
xf0! = xf0! + 6 * dRef!
IF ABS(xf0!) > CubeSize! / 2 THEN FlagPlot% = 0: EXIT SUB
CASE 5
yf0! = yf0! + 6 * dRef!
IF ABS(yf0!) > CubeSize! / 2 THEN FlagPlot% = 0: EXIT SUB
CASE 6
zf0! = zf0! + 6 * dRef!
IF ABS(zf0!) > CubeSize! / 2 THEN FlagPlot% = 0: EXIT SUB
CASE ELSE
END SELECT
GOTO NextDepth:
END IF

IF FlagPlot% = 0 THEN EXIT SUB
NEXT Rank%
END SUB

SUB Probe.Shape (Ray(), x0!, y0!, z0!, dRef!, Face%, Dist!, FlagPlot%, xh!, yh!, zh!)
Dist! = 1E+12: FlagPlot% = 0

FOR k% = 1 TO 28
xf0! = x0! + Sh%(k%, Face%, 1) * dRef!
yf0! = y0! + Sh%(k%, Face%, 2) * dRef!
zf0! = z0! + Sh%(k%, Face%, 3) * dRef!
Normal% = Sh%(k%, Face%, 4)
FlagHit% = 0
Probe.Square Ray(), xf0!, yf0!, zf0!, dRef!, Normal%, DistHit!, FlagHit%
IF FlagHit% <> 0 AND DistHit! < Dist! THEN
Dist! = DistHit!
FlagPlot% = FlagHit%
xh! = xf0!: yh! = yf0!: zh! = zf0!
END IF

NEXT k%


END SUB

SUB Probe.Square (Ray(), x0!, y0!, z0!, HalfSide!, Normal%, Dist!, FlagPlot%)
FlagPlot% = Normal%

SELECT CASE Normal%
CASE 1, 4
Dist! = (x0! - Ray(1)) / Ray(4)
yp! = Ray(2) + Dist! * Ray(5) - y0!
Zp! = Ray(3) + Dist! * Ray(6) - z0!
IF ABS(yp!) > HalfSide! OR ABS(Zp!) > HalfSide! THEN FlagPlot% = 0
CASE 2, 5
Dist! = (y0! - Ray(2)) / Ray(5)
xp! = Ray(1) + Dist! * Ray(4) - x0!
Zp! = Ray(3) + Dist! * Ray(6) - z0!
IF ABS(xp!) > HalfSide! OR ABS(Zp!) > HalfSide! THEN FlagPlot% = 0
CASE 3, 6
Dist! = (z0! - Ray(3)) / Ray(6)
xp! = Ray(1) + Dist! * Ray(4) - x0!
yp! = Ray(2) + Dist! * Ray(5) - y0!
IF ABS(xp!) > HalfSide! OR ABS(yp!) > HalfSide! THEN FlagPlot% = 0
CASE ELSE
END SELECT
END SUB

SUB Ray.Pixel (Nx%, Ny%, Ray())
FOR k% = 1 TO 3
Ray(k%) = xCam(k%)
Ray(k% + 3) = xCenter(k%) - xCam(k%) + ((Nx% - xScrCenter%) * UScreen(k%) + (Ny% - yScrCenter%) * VScreen(k%)) / ScrHeight
NEXT k%
Ray(7) = Module(Ray(4), Ray(5), Ray(6))
FOR k% = 4 TO 6: Ray(k%) = Ray(k%) / Ray(7): NEXT k%
END SUB

FUNCTION SinD (A)
SinD = CosD(90 - A)
END FUNCTION

