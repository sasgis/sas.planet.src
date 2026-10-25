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

unit i_CurlImpersonateConfig;

interface

uses
  i_ConfigDataElement,
  i_StringListStatic;

type
  ICurlImpersonateConfigStatic = interface
    ['{0E0BF026-0C4D-4651-B462-61D14E3BBD6D}']
    function GetTarget: AnsiString;
    property Target: AnsiString read GetTarget;
  end;

  ICurlImpersonateConfig = interface(IConfigDataElement)
    ['{9AC7F12F-CBA3-4725-89C1-B6B48F248C39}']
    function GetTargets: IStringListStatic;
    property Targets: IStringListStatic read GetTargets;

    function GetTarget: AnsiString;
    procedure SetTarget(const AValue: AnsiString);
    property Target: AnsiString read GetTarget write SetTarget;

    function GetStatic: ICurlImpersonateConfigStatic;
  end;

implementation

end.
