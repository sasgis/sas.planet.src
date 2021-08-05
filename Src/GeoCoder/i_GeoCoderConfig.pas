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

unit i_GeoCoderConfig;

interface

uses
  i_ConfigDataElement;

type
  IGeoCoderConfig = interface(IConfigDataElement)
    ['{8774AA07-9960-4BD1-9CA2-125BD994CF91}']
    function GetDataPath: string;
    property DataPath: string read GetDataPath;

    function GetGoogleApiKey: string;
    procedure SetGoogleApiKey(const AValue: string);
    property GoogleApiKey: string read GetGoogleApiKey write SetGoogleApiKey;

    function GetYandexApiKey: string;
    procedure SetYandexApiKey(const AValue: string);
    property YandexApiKey: string read GetYandexApiKey write SetYandexApiKey;
  end;

implementation

end.
