{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit i_TerrainConfig;

interface

uses
  i_ConfigDataElement;

type
  ITerrainConfig = interface(IConfigDataElement)
    ['{B89AD0B1-8DB6-4C2B-AF6B-E9F05EA5AC3D}']
    function GetShowInStatusBar: Boolean;
    procedure SetShowInStatusBar(const AValue: Boolean);
    property ShowInStatusBar: Boolean read GetShowInStatusBar write SetShowInStatusBar;

    function GetElevationInfoAvailable: Boolean;
    procedure SetElevationInfoAvailable(const AValue: Boolean);
    property ElevationInfoAvailable: Boolean read GetElevationInfoAvailable write SetElevationInfoAvailable;

    function GetElevationPrimaryProvider: TGUID;
    procedure SetElevationPrimaryProvider(const AValue: TGUID);
    property ElevationPrimaryProvider: TGUID read GetElevationPrimaryProvider write SetElevationPrimaryProvider;

    function GetLastActualProviderWithElevationData: TGUID;
    procedure SetLastActualProviderWithElevationData(const AValue: TGUID);
    property LastActualProviderWithElevationData: TGUID read GetLastActualProviderWithElevationData write SetLastActualProviderWithElevationData;

    function GetTrySecondaryElevationProviders: Boolean;
    procedure SetTrySecondaryElevationProviders(const AValue: Boolean);
    property TrySecondaryElevationProviders: Boolean read GetTrySecondaryElevationProviders write SetTrySecondaryElevationProviders;
  end;

implementation

end.
