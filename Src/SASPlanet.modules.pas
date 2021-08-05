{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit SASPlanet.modules;

// This unit must be first unit in dpr file

interface

{$I SASPlanet.inc}

uses
  {$IFDEF USE_FAST_MM}
  FastMM4,
  {$ENDIF}
  {$IFDEF USE_FAST_MOVE}
  FastMove,
  {$ENDIF}
  {$IFDEF USE_FAST_CODE}
  FastCode,
  {$ENDIF}
  {$IF CompilerVersion < 21.0} // CompilerVersion < Delphi 2010
  MidasSpeedFix,
  {$IFEND}
  {$IF (CompilerVersion < 23) or (CompilerVersion >= 27)}
  MidasLib, // bug in MidasLib.dcu in XE2..XE5 http://qc.embarcadero.com/wc/qcmain.aspx?d=109476
  {$IFEND}
  WinInetFix,
  XPMan;

implementation

{$IF DEFINED(RELEASE) and DEFINED(USE_FAST_MM)}
  {$IF CompilerVersion >= 32}
    {$I FastMM4Options.inc}
    {$IFDEF DetectMMOperationsAfterUninstall}
      // bug in Delphi 10.2 and up cause FastMM error:
      // "FastMM has detected a FreeMem call after FastMM was uninstalled"
      // Delphi bug-report: https://quality.embarcadero.com/browse/RSP-22897
      // SASPlanet issue: http://www.sasgis.org/mantis/view.php?id=3466
      {$MESSAGE ERROR 'DetectMMOperationsAfterUninstall (FastMM4Options.inc) must be undefined!'}
    {$ENDIF}
  {$IFEND}
{$IFEND}

end.
