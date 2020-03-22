{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2020, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit t_GeometryPointSML;

interface

{$IFNDEF WIN32}

// The Extended data type is not supported on some platforms (such as Win64 and iOS),
// but the $EXTENDEDCOMPATIBILITY directive enables you to use the Extended type
// in your code on all platforms. This directive provides 10 bytes of storage
// for compatibility, but does not provide the higher floating-point precision
// that is available with the Extended data type on the Win32 platform.

// http://docwiki.embarcadero.com/RADStudio/Rio/en/Extended_type_compatibility_(Delphi)

{$EXTENDEDCOMPATIBILITY ON}

{$ENDIF WIN32}

type
  TGeometryPointSML = packed record
    X: Extended;
    Y: Extended;
    Reserved: LongWord; // proper record aligment for backward compatibility
  end;
  PGeometryPointSML = ^TGeometryPointSML;

implementation

end.
