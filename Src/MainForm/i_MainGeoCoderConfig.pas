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

unit i_MainGeoCoderConfig;

interface

uses
  i_ConfigDataElement;

type
  IMainGeoCoderConfig = interface(IConfigDataElement)
    ['{9C11F955-42C4-4AED-A398-796DC5ABC936}']
    function GetActiveGeoCoderGUID: TGUID;
    procedure SetActiveGeoCoderGUID(const AValue: TGUID);
    property ActiveGeoCoderGUID: TGUID read GetActiveGeoCoderGUID write SetActiveGeoCoderGUID;
  end;

implementation

end.
