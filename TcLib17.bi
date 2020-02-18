'============================================================================
' The Mandelbrot Dazibao - September 2003 - http://mandelbrot.dazibao.free.fr
' True Colour Library for Quick-Basic - Version 1.7 - 07 July 2003
'============================================================================
' This graphic Library uses the VESA standard modes. Though it checks the
' presence and the capacities of the installed graphic cards, it cannot check
' the actual complience of the monitor with these modes.
'
'             NEVER USE THE TC-LIB WITH A MONOCHROME DISPLAY
'             AVOID USING A MODE THAT THE CARD CANNOT HANDLE
'       DO NOT MODIFY THE SOURCE CODE TO BY-PASS THE MODES CONTROL
' The author of the present code declines any liability in case these rules
'                       are trespassed by the user
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
' SVGA Routines
DECLARE SUB SelectVGA ()
DECLARE SUB SetVGA ()
DECLARE SUB SetText ()
DECLARE SUB ClearScreen ()
DECLARE SUB FullScreen ()
DECLARE SUB ClearBox (x1%, y1%, x2%, y2%)
DECLARE SUB SetView (x1%, y1%, x2%, y2%)

' Graphic modules
'DECLARE SUB Box24 (x1%, y1%, x2%, y2%, Pattern%)
'DECLARE SUB Circle24 (x0%, y0%, Rad%, Pattern%)
'DECLARE SUB Ellipse24 (x1%, y1%, x2%, y2%, Pattern%)
'DECLARE SUB Fill24 (x%, y%)
DECLARE SUB Pset24 (x%, y%)
'DECLARE SUB Paint24 (x1%, y1%, x2%, y2%)
'DECLARE SUB Photon24 (x%, y%)
DECLARE SUB Point24 (x%, y%)
'DECLARE SUB Line24 (x1%, y1%, x2%, y2%, Pattern%)
'DECLARE SUB Ray24 (x1%, y1%, x2%, y2%)
DECLARE SUB _RGB (r%, g%, b%)

' Colour management modules
DECLARE SUB HSVto_RGB (Hue, Saturation, Value)
DECLARE SUB HSVto_RGB1 (Hue, Saturation, Value)
DECLARE SUB _RGBtoHSV (Hue, Saturation, Value)
DECLARE SUB ShiftHue (x1%, y1%, x2%, y2%, Shift)
DECLARE SUB Satu24 (x1%, y1%, x2%, y2%, SatFactor)
DECLARE SUB Bright24 (x1%, y1%, x2%, y2%, BrightFactor)
DECLARE SUB Grey24 (x1%, y1%, x2%, y2%)
DECLARE SUB Invert24 (x1%, y1%, x2%, y2%)
DECLARE FUNCTION CompCol% (x%, y%, r%, g%, b%)

' Bitmaps management modules
DECLARE SUB Screenshot (_NAME$)
DECLARE SUB ViewBmp24 (_NAME$, x1%, y1%, x2%, y2%)
DECLARE SUB WriteBmp24 (_NAME$, x1%, y1%, x2%, y2%)

' Text management modules
DECLARE SUB Input24 (x%, y%, Text$, Prompt$)
DECLARE SUB Print24 (x%, y%, Text$)
DECLARE SUB PrintTC (Size%, Hue, x%, y%, Text$)

' Mathematical utilities
DECLARE SUB Ffix ()
DECLARE FUNCTION Arg (xa, ya)
DECLARE FUNCTION Module (x, y, z)
DECLARE FUNCTION ScalProd (u(), v())

' Program control utilities
DECLARE SUB EndProg ()
DECLARE SUB EndView ()
DECLARE SUB WaitKey ()
DECLARE SUB Tempo (Time)
DECLARE function rnd2#(a as integer=0)

' Data type for interrupt management
'TYPE RegType
'ax AS INTEGER
'bx AS INTEGER
'cx AS INTEGER
'dx AS INTEGER
'BP AS INTEGER
'si AS INTEGER
'di AS INTEGER
'FLAGS AS INTEGER
'DS AS INTEGER
'es AS INTEGER
'END TYPE

' Data type for graphic modes management
'TYPE ModeInfo
'ModeAttributes AS INTEGER
'WinAAttributes AS STRING * 1
'WinBAttributes AS STRING * 1
'WinGranularity AS INTEGER
'WinSize AS INTEGER
'WinASegment AS INTEGER
'WinBSegment AS INTEGER
'WinFuncPtr AS LONG
'BytesPerScanLine AS INTEGER
'xResolution AS INTEGER
'yResolution AS INTEGER
'xCharSize AS STRING * 1
'yCharSize AS STRING * 1
'NumberOfPlanes AS STRING * 1
'BitsPerPixel AS STRING * 1
'NumberOfBanks AS STRING * 1
'MemoryModel AS STRING * 1
'Banksize AS STRING * 1
'NumberOfImagePages AS STRING * 1
'Rsvd AS STRING * 1
'RedMaskSize AS STRING * 1
'RedFieldPosition AS STRING * 1
'GreenMaskSize AS STRING * 1
'GreenFieldPosition AS STRING * 1
'BlueMaskSize AS STRING * 1
'BlueFieldPosition AS STRING * 1
'Rsvdmasksize AS STRING * 1
'DirectColorModeInfo AS STRING * 1
'Reserved AS STRING * 216
'END TYPE

'TYPE ySVGA
'Bank AS INTEGER
'Offset AS LONG
'END TYPE

' Shared variables for SVGA management
'DIM SHARED Regs AS RegType
'DIM SHARED CurBank%, Bpl AS LONG, Bpp AS LONG
'DIM SHARED xPlotMin%, xPlotMax%
'DIM SHARED yPlotMin%, yPlotMax%
'DIM SHARED FontSeg%, FontOffset%

COMMON SHARED Power2%()
COMMON SHARED SizeScreen AS INTEGER
COMMON SHARED _sCREENRES$, _NAME$
COMMON SHARED ScrWidth AS LONG, ScrHeight AS LONG
COMMON SHARED Red%, Green%, Blue%
COMMON SHARED xScrCenter%, yScrCenter%

CONST eps = 9.98012604599318D-322    ' See Antoni's version
'CONST eps = 1.401298E-45             ' Avoids logarithm overflow

' Constants for the TC-Lib programs
CONST SaveDir$ = ""                  ' For files management
CONST Pi = 3.14159265358979#         ' For angles management
CONST SQR12 = .7071068  'SQR(1/2)
CONST SQR16 = .4082483  'SQR(1/6)
CONST SQR23 = .8164966  'SQR(2/3)
CONST SatCoeff = 84.51663 '100/ATN(SQR(6))
CONST SatCoef = 1.183199E-02 '1/SatCoeff
CONST PiOver180 = .0174533
CONST PiOver2 = 1.5707963267949#

