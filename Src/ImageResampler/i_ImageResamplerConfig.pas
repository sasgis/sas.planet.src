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

unit i_ImageResamplerConfig;

interface

uses
  i_ConfigDataElement;

type
  IImageResamplerConfig = interface(IConfigDataElement)
    ['{6E7E207D-F684-41E4-93C9-EBEE584F4510}']
    function GetDefaultGUID: TGUID;
    property DefaultGUID: TGUID read GetDefaultGUID;

    function GetActiveGUID: TGUID;
    procedure SetActiveGUID(const AValue: TGUID);
    property ActiveGUID: TGUID read GetActiveGUID write SetActiveGUID;
  end;

implementation

end.
