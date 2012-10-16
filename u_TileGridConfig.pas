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

unit u_TileGridConfig;

interface

uses
  GR32,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapLayerGridsConfig,
  u_BaseGridConfig;

type
  TTileGridConfig = class(TBaseGridConfig, ITileGridConfig)
  private
    FUseRelativeZoom: Boolean;
    FZoom: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetPointStickToGrid(
      const ALocalConverter: ILocalCoordConverter;
      const ASourceLonLat: TDoublePoint
    ): TDoublePoint; override;
    function GetRectStickToGrid(
      const ALocalConverter: ILocalCoordConverter;
      const ASourceRect: TDoubleRect
    ): TDoubleRect; override;
  private
    function GetUseRelativeZoom: Boolean;
    procedure SetUseRelativeZoom(AValue: Boolean);

    function GetZoom: Integer;
    procedure SetZoom(AValue: Integer);

    function GetActualZoom(const ALocalConverter: ILocalCoordConverter): Byte;
  public
    constructor Create;
  end;

implementation

uses
  i_CoordConverter,
  u_GeoFun;

{ TTileGridConfig }

constructor TTileGridConfig.Create;
begin
  inherited;
  FUseRelativeZoom := True;
  FZoom := 0;
end;

procedure TTileGridConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUseRelativeZoom := AConfigData.ReadBool('UseRelativeZoom', FUseRelativeZoom);
    FZoom := AConfigData.ReadInteger('Zoom', FZoom);
    SetChanged;
  end;
end;

procedure TTileGridConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UseRelativeZoom', FUseRelativeZoom);
  AConfigData.WriteInteger('Zoom', FZoom);
end;

function TTileGridConfig.GetActualZoom(
  const ALocalConverter: ILocalCoordConverter
): Byte;
var
  VZoom: Integer;
  VRelative: Boolean;
begin
  LockRead;
  try
    VZoom := FZoom;
    VRelative := FUseRelativeZoom;
  finally
    UnlockRead;
  end;
  if VRelative then begin
    VZoom := VZoom + ALocalConverter.GetZoom;
  end;
  if VZoom < 0 then begin
    Result := 0;
  end else begin
    Result := VZoom;
    ALocalConverter.GetGeoConverter.CheckZoom(Result);
  end;
end;

function TTileGridConfig.GetPointStickToGrid(
  const ALocalConverter: ILocalCoordConverter;
  const ASourceLonLat: TDoublePoint): TDoublePoint;
var
  VZoom: Byte;
  VZoomCurr: Byte;
  VSelectedTileFloat: TDoublePoint;
  VSelectedTile: TPoint;
  VConverter: ICoordConverter;
begin
  VZoomCurr := ALocalConverter.GetZoom;
  VConverter := ALocalConverter.GetGeoConverter;
  LockRead;
  try
    if GetVisible then begin
      VZoom := GetActualZoom(ALocalConverter);
    end else begin
      VZoom := VZoomCurr;
    end;
  finally
    UnlockRead;
  end;
  VSelectedTileFloat := VConverter.LonLat2TilePosFloat(ASourceLonLat, VZoom);
  VSelectedTile := PointFromDoublePoint(VSelectedTileFloat, prClosest);
  Result := VConverter.TilePos2LonLat(VSelectedTile, VZoom);
end;

function TTileGridConfig.GetRectStickToGrid(
  const ALocalConverter: ILocalCoordConverter;
  const ASourceRect: TDoubleRect
): TDoubleRect;
var
  VZoom: Byte;
  VZoomCurr: Byte;
  VSelectedTilesFloat: TDoubleRect;
  VSelectedTiles: TRect;
  VConverter: ICoordConverter;
begin
  VZoomCurr := ALocalConverter.GetZoom;
  VConverter := ALocalConverter.GetGeoConverter;
  LockRead;
  try
    if GetVisible then begin
      VZoom := GetActualZoom(ALocalConverter);
    end else begin
      VZoom := VZoomCurr;
    end;
  finally
    UnlockRead;
  end;
  VSelectedTilesFloat := VConverter.LonLatRect2TileRectFloat(ASourceRect, VZoom);
  VSelectedTiles := RectFromDoubleRect(VSelectedTilesFloat, rrOutside);
  Result := VConverter.TileRect2LonLatRect(VSelectedTiles, VZoom);
end;

function TTileGridConfig.GetUseRelativeZoom: Boolean;
begin
  LockRead;
  try
    Result := FUseRelativeZoom;
  finally
    UnlockRead;
  end;
end;

function TTileGridConfig.GetZoom: Integer;
begin
  LockRead;
  try
    Result := FZoom;
  finally
    UnlockRead;
  end;
end;

procedure TTileGridConfig.SetUseRelativeZoom(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseRelativeZoom <> AValue then begin
      FUseRelativeZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTileGridConfig.SetZoom(AValue: Integer);
begin
  LockWrite;
  try
    if FZoom <> AValue then begin
      FZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
