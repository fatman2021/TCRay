
'============================================================================
' The Mandelbrot Dazibao - September 2003 - http://mandelbrot.dazibao.free.fr
' True Colour Library for Quick-Basic - Version 1.7L - 24 September 2003
'============================================================================
'This lib has been emptied of most of its content by Antoni Gual, to be able to port
'TCRAY to FB. Most of the functions here are done by using plain keywords in FB's
'gfxlib.
'The only remining functions are the conversions RGB <=> HSV...
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
DEFSNG A-Z
' $INCLUDE: 'Tclib17.bi'

'REDIM SHARED xBpp&(0)
'REDIM SHARED yLut(0) AS ySVGA

FUNCTION Arg (xa, ya)
' Returns the argument (in radians) of a point in the (x,y) plan
' Ranges from -Pi to Pi

function=atan2(ya,xa)
'IF xa = 0 AND ya = 0 THEN Arg = 0: EXIT FUNCTION
'IF xa = 0 AND ya >= 0 THEN Arg = Pi / 2: EXIT FUNCTION
'IF ya = 0 AND xa < 0 THEN Arg = Pi: EXIT FUNCTION
'IF xa = 0 AND ya < 0 THEN Arg = -Pi / 2: EXIT FUNCTION
'IF xa > 0 THEN Arg = ATN(ya / xa): EXIT FUNCTION
'IF xa < 0 AND ya >= 0 THEN Arg = Pi - ATN(-ya / xa): EXIT FUNCTION
'IF xa < 0 AND ya < 0 THEN Arg = -Pi + ATN(ya / xa): EXIT FUNCTION

END FUNCTION

SUB ClearScreen
cls
' Clears the screen by reseting the current SVGA mode
'SetVGA
END SUB

SUB EndProg

' Quit the program
SetText
PRINT
PRINT
PRINT
PRINT
PRINT "               ษออออออออออออออออออออออออออออออออออออออออออออออออป"
PRINT "               บ         Thanks for using the TC-Lib            บ"
PRINT "               บ  Please report any bug, remark, comment... to  บ"
PRINT "               บ          mandelbrot.dazibao@free.fr            บ"
PRINT "               บ    and feel free to leave your footprint on    บ"
PRINT "               บ       http://mandelbrot.dazibao.free.fr        บ"
PRINT "               ศออออออออออออออออออออออออออออออออออออออออออออออออผ"
sleep
end
END SUB

SUB EndView

END SUB

SUB FullScreen
' The Pset24 routine can plot any pixel on the screen

xPlotMin% = 0
xPlotMax% = ScrWidth - 1
yPlotMin% = 0
yPlotMax% = ScrHeight - 1
xScrCenter% = INT((xPlotMin% + xPlotMax%) / 2 + .5)
yScrCenter% = INT((yPlotMin% + yPlotMax%) / 2 + .5)
END SUB

SUB HSVto_RGB (Hue, Sat, Value)
' ======================================================================
' Optimised version (35% faster than the previous version of the TC-Lib)
' ======================================================================

' Converts a HSV colour definition into _RGB values via exact
' trigonometry calculations. Refer to HSVto_RGB1 for a simplified,
' faster but poorer conversion algorithm.
' Hue ranges from 0 to 360 degrees. Red corresponds to Hue = 0
' Saturation ranges from 0 to 100
' Value ranges from 0 to 100
' Saturation = 0 provides grey tones, from black (Value = 0)
' to white (Value = 100).
' Red%, Green% and Blue% are shared variables
' The routine uses 4 constants:
'    SQR(1/2) = .7071068
'    SQR(1/6) = .4082483
'    ATN(SQR(6)/100 = 1.183199E-02
'    Pi/180 = .0174533

Angle = (Hue - 150) * PiOver180
Ur = Value * 2.55
Radius = Ur * TAN(Sat * SatCoef)
Vr = Radius * COS(Angle) * SQR12
Wr = Radius * SIN(Angle) * SQR16

Red% = Ur - Vr - Wr
Green% = Ur + Vr - Wr
Blue% = Ur + Wr + Wr

IF Red% < 0 THEN
Rdim = Ur / (Vr + Wr)
Red% = 0
Green% = Ur + (Vr - Wr) * Rdim
Blue% = Ur + 2 * Wr * Rdim
GOTO Ctrl255
END IF

IF Green% < 0 THEN
Rdim = -Ur / (Vr - Wr)
Red% = Ur - (Vr + Wr) * Rdim
Green% = 0
Blue% = Ur + 2 * Wr * Rdim
GOTO Ctrl255
END IF

IF Blue% < 0 THEN
Rdim = -Ur / (Wr + Wr)
Red% = Ur - (Vr + Wr) * Rdim
Green% = Ur + (Vr - Wr) * Rdim
Blue% = 0
GOTO Ctrl255
END IF

Ctrl255:
IF Red% > 255 THEN
Rdim = (Ur - 255) / (Vr + Wr)
Red% = 255
Green% = Ur + (Vr - Wr) * Rdim
Blue% = Ur + 2 * Wr * Rdim
END IF

IF Green% > 255 THEN
Rdim = (255 - Ur) / (Vr - Wr)
Red% = Ur - (Vr + Wr) * Rdim
Green% = 255
Blue% = Ur + 2 * Wr * Rdim
END IF

IF Blue% > 255 THEN
Rdim = (255 - Ur) / (Wr + Wr)
Red% = Ur - (Vr + Wr) * Rdim
Green% = Ur + (Vr - Wr) * Rdim
Blue% = 255
END IF

END SUB

FUNCTION Module (x, y, z)
' Returns the module of a 3D vector
Module = SQR(x * x + y * y + z * z)
END FUNCTION

SUB Point24 (x%, y%)

R_G_B%=point (X%,y%)
red%=R_G_b% shr 16
green%=(R_G_b% shr 8) and &hff
blue%=R_G_b%  and &hff

END SUB

SUB Pset24 (x%, y%)

pset(x%,y%),rgb(Red%,Green%,blue%)

END SUB

SUB _RGB (r%, g%, b%)
' Sets the _RGB current values
Red% = r%: Green% = g%: Blue% = b%
END SUB

SUB _RGBtoHSV (Hue, Saturation, Value)
' Returns the HSV codes of the current _RGB combination
Temp = (Red% + Green% + Blue%) / 3
xa = (Green% - Red%) * SQR12
ya = (Blue% + Blue% - Red% - Green%) * SQR16
Hue = Arg(xa, ya) / PiOver180 + 150
Saturation = Arg(Temp, Module(Red% - Temp, Green% - Temp, Blue% - Temp)) * SatCoeff
Value = Temp / 2.55

IF Saturation = 0 OR Value = 0 THEN Hue = 0
IF Hue < 0 THEN Hue = Hue + 360
IF Hue >= 360 THEN Hue = Hue - 360
END SUB

FUNCTION ScalProd (u(), v())
' Returns the scalar product of two vectors
ScalProd = u(1) * v(1) + u(2) * v(2) + u(3) * v(3)
END FUNCTION

SUB Screenshot (_Name$)
' Generates a 24 bits bitmap screenshot.
' The BMP file is placed in the DOS directory defined by SaveDir$

' Bitmap structure parameters
Bw& = ScrWidth
Bh& = ScrHeight
IF (3 * Bw& MOD (4)) <> 0 THEN PadBytes% = 4 - ((3 * Bw&) MOD (4))
OS& = 54
Fs& = (3 * Bw& + PadBytes%) * Bh& + OS&
Ps& = (3 * Bw& + PadBytes%) * Bh&

Pic$ = SaveDir$ + _Name$ + ".bmp"
ff% = FREEFILE
OPEN Pic$ FOR BINARY AS #ff%

' Header
Buffer$ = "BM"
Buffer$ = Buffer$ + MKL$(Fs&)                      'File Size
Buffer$ = Buffer$ + CHR$(0) + CHR$(0)              'Reserved 1
Buffer$ = Buffer$ + CHR$(0) + CHR$(0)              'Reserved 2
Buffer$ = Buffer$ + MKL$(OS&)                      'Offset
Buffer$ = Buffer$ + MKL$(40)                       'File Info Size
Buffer$ = Buffer$ + MKL$(Bw&)                      'Pic Width
Buffer$ = Buffer$ + MKL$(Bh&)                      'Pic Height
Buffer$ = Buffer$ + CHR$(1) + CHR$(0)              'Number of planes
Buffer$ = Buffer$ + CHR$(24) + CHR$(0)             'Number of bits per pixel
Buffer$ = Buffer$ + MKL$(0)                        'No compression
Buffer$ = Buffer$ + MKL$(Ps&)                      'Image Size
Buffer$ = Buffer$ + MKL$(0)                        'X Size (pixel/meter)
Buffer$ = Buffer$ + MKL$(0)                        'Y Size (pixel/meter)
Buffer$ = Buffer$ + MKL$(0)                        'Colors used
Buffer$ = Buffer$ + MKL$(0)                        'Important colors
PUT #ff%, ,Buffer$

' Pixels
FOR y% = (Bh& - 1) TO 0 STEP -1
Buffer$ = ""
FOR x% = 0 TO (Bw& - 1)
Point24 x%, y%
Col$ = CHR$(Blue%) + CHR$(Green%) + CHR$(Red%)
Buffer$ = Buffer$ + Col$
NEXT x%
IF PadBytes% > 0 THEN
FOR j = 1 TO PadBytes%
Buffer$ = Buffer$ + CHR$(0)
NEXT j
END IF
PUT #ff%, , Buffer$
NEXT y%

CLOSE ff%

END SUB

SUB SelectVGA
SetVGA
END SUB

SUB SetText
' Re-assigns the text mode of the graphic card

'screen 0
CLS
END SUB

SUB SetVGA

screen 20,32
scrwidth%=1024
scrheight%=768
xScrCenter%=scrwidth% \2
yScrCenter%=scrheight%\2



' Powers of two precalculation
REDIM Power2%(15)
FOR k% = 0 TO 14: Power2%(k%) = 2 ^ k%: NEXT k%

END SUB

SUB WaitKey
' Pause until a key is hit
sleep
'DO WHILE INKEY$ = ""
'LOOP
END SUB

function rnd2#(a as integer=0) ' ***** ***** This is the number produced by zombie 
'
'Dumbledore's QB-like RND
'
static seed#,ft as byte 
if ft=0 then seed#=327680:ft=1 
if a<>0 then 
   if a=abs(a) then seed#=(seed#+a) mod 16777216 else seed#=-a 
end if 
Temp# = (16598013# * Seed# + 12820163) 
Seed# = Temp# - INT(Temp# / 16777216) * 16777216 
rnd2#=seed#/16777216 
end function


