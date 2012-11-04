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

unit u_TerrainInfo;

interface

uses
  t_GeoTypes,
  i_TerrainInfo,
  i_TerrainConfig,
  i_TerrainProvider,
  i_TerrainProviderList;

type
  TTerrainInfo = class(TInterfacedObject, ITerrainInfo)
  private
    FTerrainConfig: ITerrainConfig;
    FPrimaryTerrainProviderGUID: TGUID;
    FPrimaryTerrainProvider: ITerrainProvider;
    FTerrainProviderList: ITerrainProviderList;
  public
    constructor Create(
      const ATerrainConfig: ITerrainConfig;
      const ATerrainProviderList: ITerrainProviderList
    );
    destructor Destroy; override;

    function GetElevationInfoStr(
      const APoint: TDoublePoint;
      const AZoom: Byte
    ): string;

    function GetElevationInfoFloat(
      const APoint: TDoublePoint;
      const AZoom: Byte
    ): Single;
  end;

implementation

uses
  SysUtils;

{ TTerrainInfo }

constructor TTerrainInfo.Create(
  const ATerrainConfig: ITerrainConfig;
  const ATerrainProviderList: ITerrainProviderList
);
begin
  inherited Create;
  FTerrainConfig := ATerrainConfig;
  FTerrainProviderList := ATerrainProviderList;
  FPrimaryTerrainProviderGUID := FTerrainConfig.ElevationPrimaryProvider;
  FPrimaryTerrainProvider := FTerrainProviderList.Get(FPrimaryTerrainProviderGUID).Provider;
end;

destructor TTerrainInfo.Destroy;
begin
  inherited Destroy;
end;

function TTerrainInfo.GetElevationInfoFloat(
  const APoint: TDoublePoint;
  const AZoom: Byte
): Single;
begin
  Result := FPrimaryTerrainProvider.GetPointElevation(APoint, AZoom);
  { ToDo: if Result = 0 then try get elevation from Secondary providers  }
end;

function TTerrainInfo.GetElevationInfoStr(
  const APoint: TDoublePoint;
  const AZoom: Byte
): string;
var
  VElevation: Single;
begin
  VElevation := GetElevationInfoFloat(APoint, AZoom);
  Result := Format('%0.f m', [VElevation]);
end;

end.
