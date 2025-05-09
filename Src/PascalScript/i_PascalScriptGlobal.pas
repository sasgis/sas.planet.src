{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit i_PascalScriptGlobal;

interface

type
  IPascalScriptGlobal = interface
    ['{62EF0D51-E01E-4AFD-8072-887C47B7D782}']
    procedure Lock;
    procedure Unlock;

    procedure LockRead;
    procedure UnlockRead;

    procedure SetVar(const AVarID: Integer; const AValue: Variant);
    procedure SetVarTS(const AVarID: Integer; const AValue: Variant);

    function GetVar(const AVarID: Integer): Variant;
    function GetVarTS(const AVarID: Integer): Variant;

    function Exists(const AVarID: Integer): Boolean;
    function ExistsTS(const AVarID: Integer): Boolean;
  end;

implementation

end.
