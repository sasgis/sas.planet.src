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

unit u_TerrainProviderByGE;

interface

uses
  Types,
  t_GeoTypes,
  i_Listener,
  i_Notifier,
  i_CoordConverter,
  i_TerrainStorage,
  i_TerrainProvider,
  u_GlobalCahceConfig,
  u_GoogleEarthTerrainParser,
  u_TerrainProviderByGEMemCache;

type
  TTerrainProviderByDLL = class(TInterfacedObject, ITerrainProvider)
  private
    FAvailable: Boolean;
    FStorage: ITerrainStorage;
    FCoordConverter: ICoordConverter;
    FTerrainPerser: TGoogleEarthTerrainParser;
    FMemCache: TTerrainProviderByGEMemCache;
    FCacheConfigChangeListener: IListener;
    FCacheConfig: TGlobalCahceConfig;
    procedure TryLoadTileToMemCache(const ATile: TPoint; const AZoom: Byte);
    procedure CheckTileZoom(var AZoom: Byte);
    procedure OnCloseTerrainTile(const ATile: PTerrainTile);
  protected
    function GetPointElevation(const ALonLat: TDoublePoint; const AZoom: Byte): Single;
    procedure OnCacheConfigChange; virtual; abstract;
  public
    constructor Create(
      const ACacheConfig: TGlobalCahceConfig;
      const AStorage: ITerrainStorage;
      const ACoordConverter: ICoordConverter
    );
    destructor Destroy; override;
  end;

  TTerrainProviderByGoogleEarth = class(TTerrainProviderByDLL)
  public
    constructor Create(const ACacheConfig: TGlobalCahceConfig);
    procedure OnCacheConfigChange; override;
  end;

const
  cUndefElevationValue = 0;

implementation

uses
  SysUtils,
  c_CoordConverter,
  i_TileInfoBasic,
  i_CoordConverterFactory,
  u_GeoFun,
  u_ListenerByEvent,
  u_CoordConverterFactorySimple,
  u_TileStorageGETerrain;

const
  cMemCacheCapacity = 500;

{ TTerrainProviderByDLL }

constructor TTerrainProviderByDLL.Create(
  const ACacheConfig: TGlobalCahceConfig;
  const AStorage: ITerrainStorage;
  const ACoordConverter: ICoordConverter
);
begin
  inherited Create;
  FCacheConfig := ACacheConfig;
  FStorage := AStorage;
  FCoordConverter := ACoordConverter;
  FTerrainPerser := TGoogleEarthTerrainParser.Create;
  FMemCache := TTerrainProviderByGEMemCache.Create(cMemCacheCapacity, Self.OnCloseTerrainTile);
  FAvailable := FStorage.Available and FTerrainPerser.Available;
  FCacheConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnCacheConfigChange);
  FCacheConfig.CacheChangeNotifier.Add(FCacheConfigChangeListener);
end;

destructor TTerrainProviderByDLL.Destroy;
begin
  if FCacheConfig <> nil then begin
    FCacheConfig.CacheChangeNotifier.Remove(FCacheConfigChangeListener);
    FCacheConfig := nil;
    FCacheConfigChangeListener := nil;
  end;
  FCoordConverter := nil;
  FStorage := nil;
  FMemCache.Free;
  FTerrainPerser.Free;
  inherited Destroy;
end;

procedure TTerrainProviderByDLL.CheckTileZoom(var AZoom: Byte);
begin
  // GE terrain's zooms mast be in values: [3,5,7,9,11,13,15,17,19,23,25]
  if AZoom <= 3 then begin
    AZoom := 2;
  end else if (AZoom mod 2) > 0 then begin
    AZoom := AZoom - 1;
  end;
end;

procedure TTerrainProviderByDLL.OnCloseTerrainTile(const ATile: PTerrainTile);
begin
  if (ATile <> nil) then begin
    FTerrainPerser.Close(@ATile.ParserContext);
  end;
end;

procedure TTerrainProviderByDLL.TryLoadTileToMemCache(
  const ATile: TPoint;
  const AZoom: Byte
);
var
  VTileInfo: ITileInfoBasic;
  VTileInfoWithData: ITileInfoWithData;
  VContext: Pointer;
  VTneFound: Boolean;
begin
  VTileInfo := FStorage.GetTileInfo(ATile, AZoom);

  if VTileInfo <> nil then begin
    VTneFound := (not VTileInfo.IsExists or VTileInfo.IsExistsTNE);

    if (not VTneFound and Supports(VTileInfo, ITileInfoWithData, VTileInfoWithData)) then begin
      if FTerrainPerser.Open(@VContext, VTileInfoWithData.TileData) then begin
        FMemCache.Add(ATile, AZoom, VContext);
      end else begin
        VTneFound := True;
      end;
    end else begin
      VTneFound := True;
    end;

    if VTneFound then begin
      FMemCache.AddTne(ATile, AZoom);
    end;
  end else begin
    FAvailable := False;
  end;
end;

function TTerrainProviderByDLL.GetPointElevation(
 const ALonLat: TDoublePoint;
 const AZoom: Byte
): Single;
var
  VLonLat: TDoublePoint;
  VTilePoint: TPoint;
  VZoom: Byte;
  VTerrain: PTerrainTile;
  VFound: Boolean;
  VElevation: Single;
begin
  Result := cUndefElevationValue;

  if FAvailable then begin
    VFound := False;
    VLonLat := ALonLat;
    VZoom := AZoom;
    repeat
      CheckTileZoom(VZoom);

      VTilePoint := PointFromDoublePoint(
        FCoordConverter.LonLat2TilePosFloat(VLonLat, VZoom),
        prToTopLeft
      );

      VTerrain := FMemCache.Get(VTilePoint, VZoom);

      if (VTerrain = nil) then begin
        TryLoadTileToMemCache(VTilePoint, VZoom);
        VTerrain := FMemCache.Get(VTilePoint, VZoom);
      end;

      if (VTerrain <> nil) and (VTerrain.Exists) then begin
        VFound :=
          FTerrainPerser.GetElevation(
            @VTerrain.ParserContext,
            VLonLat,
            VElevation
          );
        if VFound then begin
          Result := VElevation;
        end;
      end;

      if (not VFound) and (VZoom > 2) then begin
        VZoom := VZoom - 1;
      end else begin
        Break;
      end;

    until False;
  end;  
end;

{ TTerrainProviderByGoogleEarth }

constructor TTerrainProviderByGoogleEarth.Create(
  const ACacheConfig: TGlobalCahceConfig
);
var
  VStrorage: ITerrainStorage;
  VConverter: ICoordConverter;
begin
  VStrorage :=
    TTileStorageGETerrain.Create(ACacheConfig.GECachePath.Path) as ITerrainStorage;

  VConverter :=
    (TCoordConverterFactorySimple.Create as ICoordConverterFactory).GetCoordConverterByCode(
      CGELonLatProjectionEPSG,
      CTileSplitQuadrate256x256
    );

  inherited Create(ACacheConfig, VStrorage, VConverter);
end;

procedure TTerrainProviderByGoogleEarth.OnCacheConfigChange;
begin
  FStorage.SetPath(FCacheConfig.GECachePath.Path);
  FAvailable := (FStorage.Available and FTerrainPerser.Available);
end;

end.
