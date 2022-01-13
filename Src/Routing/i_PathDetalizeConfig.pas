{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit i_PathDetalizeConfig;

interface

uses
  i_ConfigDataElement;

type
  TProjectOSRM = record
    Address: string;
    Guid: array [0..2] of TGUID;
  end;
  TArrayOfProjectOSRM = array of TProjectOSRM;

  IPathDetalizeConfig = interface(IConfigDataElement)
    ['{BD3A7204-23C6-4DA5-AB16-35996256F5D6}']

    function GetEnableZlzk: Boolean;
    property EnableZlzk: Boolean read GetEnableZlzk;

    function GetEnableProjectOSRM: Boolean;
    property EnableProjectOSRM: Boolean read GetEnableProjectOSRM;

    function GetArrayOfProjectOSRM: TArrayOfProjectOSRM;
    property ArrayOfProjectOSRM: TArrayOfProjectOSRM read GetArrayOfProjectOSRM;

    function GetEnableAutomaticRouting: Boolean;
    procedure SetEnableAutomaticRouting(const AValue: Boolean);
    property EnableAutomaticRouting: Boolean read GetEnableAutomaticRouting write SetEnableAutomaticRouting;

    function GetDefaultProvider: TGUID;
    procedure SetDefaultProvider(const AValue: TGUID);
    property DefaultProvider: TGUID read GetDefaultProvider write SetDefaultProvider;

    function GetGarbageCollectionTimeOut: Integer;
    property GarbageCollectionTimeOut: Integer read GetGarbageCollectionTimeOut;
  end;

implementation

end.
