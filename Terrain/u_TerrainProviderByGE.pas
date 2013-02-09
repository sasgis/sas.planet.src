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
  i_PathConfig,
  i_CoordConverter,
  i_TerrainStorage,
  i_TerrainProvider,
  i_GlobalCacheConfig,
  u_GoogleEarthLibrary,
  u_TerrainProviderByGEMemCache,
  u_BaseInterfacedObject;

type
  TTerrainProviderByDLL = class(TBaseInterfacedObject, ITerrainProvider)
  private
    FAvailable: Boolean;
    FStorage: ITerrainStorage;
    FCoordConverter: ICoordConverter;
    FGoogleEarthLib: TGoogleEarthLibrary;
    FMemCache: TTerrainProviderByGEMemCache;
    FCacheStateChangeListner: IListener;
    FCacheConfigChangeListener: IListener;
    FPathConfig: IPathConfig;
    FStateChangeNotifier: INotifier;
    FStateChangeNotifierInternal: INotifierInternal;
    procedure TryLoadTileToMemCache(const ATile: TPoint; const AZoom: Byte);
    procedure CheckTileZoom(var AZoom: Byte);
  protected
    function GetPointElevation(const ALonLat: TDoublePoint; const AZoom: Byte): Single;
    procedure OnCacheConfigChange;
    procedure OnCacheStateChange;
    function GetAvailable: Boolean;
    function GetStateChangeNotifier: INotifier;
  public
    constructor Create(
      const APathConfig: IPathConfig;
      const AStorage: ITerrainStorage;
      const ACoordConverter: ICoordConverter
    );
    destructor Destroy; override;
  end;

  TTerrainProviderByGoogleEarth = class(TTerrainProviderByDLL)
  public
    constructor Create(const ACacheConfig: IGlobalCacheConfig);
  end;

  TTerrainProviderByGeoCacher = class(TTerrainProviderByDLL)
  public
    constructor Create(const ACacheConfig: IGlobalCacheConfig);
  end;
  
implementation

uses
  SysUtils,
  c_CoordConverter,
  c_TerrainProvider,
  i_TileInfoBasic,
  i_CoordConverterFactory,
  i_GoogleEarthTerrain,
  u_GeoFun,
  u_Notifier,
  u_ListenerByEvent,
  u_CoordConverterFactorySimple,
  u_TileStorageGETerrain;

const
  cMemCacheCapacity = 500;

{ TTerrainProviderByDLL }

constructor TTerrainProviderByDLL.Create(
  const APathConfig: IPathConfig;
  const AStorage: ITerrainStorage;
  const ACoordConverter: ICoordConverter
);
begin
  inherited Create;
  FPathConfig := APathConfig;
  FStorage := AStorage;
  FCoordConverter := ACoordConverter;

  FGoogleEarthLib := TGoogleEarthLibrary.Create;
  FMemCache := TTerrainProviderByGEMemCache.Create(cMemCacheCapacity);

  FAvailable := FStorage.Available and FGoogleEarthLib.Available;

  FStateChangeNotifierInternal := TNotifierBase.Create;
  FStateChangeNotifier := FStateChangeNotifierInternal;

  FCacheStateChangeListner := TNotifyNoMmgEventListener.Create(Self.OnCacheStateChange);
  FStorage.Notifier.Add(FCacheStateChangeListner);

  FCacheConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnCacheConfigChange);
  FPathConfig.ChangeNotifier.Add(FCacheConfigChangeListener);
end;

destructor TTerrainProviderByDLL.Destroy;
begin
  if FPathConfig <> nil then begin
    FPathConfig.ChangeNotifier.Remove(FCacheConfigChangeListener);
    FPathConfig := nil;
    FCacheConfigChangeListener := nil;
  end;
  if FStorage <> nil then begin
    FStorage.Notifier.Remove(FCacheStateChangeListner);
  end;
  FCoordConverter := nil;
  FStorage := nil;
  FStateChangeNotifier := nil;
  FStateChangeNotifierInternal := nil;
  FMemCache.Free;
  FGoogleEarthLib.Free;
  inherited Destroy;
end;

function TTerrainProviderByDLL.GetStateChangeNotifier: INotifier;
begin
  Result := FStateChangeNotifier;
end;

procedure TTerrainProviderByDLL.OnCacheConfigChange;
begin
  FStorage.SetPath(FPathConfig.Path);
  FAvailable := (FStorage.Available and FGoogleEarthLib.Available);
  FStateChangeNotifierInternal.Notify(nil);
end;

procedure TTerrainProviderByDLL.OnCacheStateChange;
begin
  FAvailable := (FStorage.Available and FGoogleEarthLib.Available);
  FStateChangeNotifierInternal.Notify(nil);
end;

function TTerrainProviderByDLL.GetAvailable: Boolean;
begin
  FAvailable := FStorage.Available and FGoogleEarthLib.Available;
  Result := FAvailable;
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

procedure TTerrainProviderByDLL.TryLoadTileToMemCache(
  const ATile: TPoint;
  const AZoom: Byte
);
var
  VTileInfo: ITileInfoBasic;
  VTileInfoWithData: ITileInfoWithData;
  VGoogleEarthTerrain: IGoogleEarthTerrain;
  VTneFound: Boolean;
begin
  VTileInfo := FStorage.GetTileInfo(ATile, AZoom);

  if VTileInfo <> nil then begin
    VTneFound := (not VTileInfo.IsExists or VTileInfo.IsExistsTNE);

    if (not VTneFound and Supports(VTileInfo, ITileInfoWithData, VTileInfoWithData)) then begin
      VGoogleEarthTerrain := FGoogleEarthLib.CreateObject(IID_IGoogleEarthTerrain) as IGoogleEarthTerrain;
      if Assigned(VGoogleEarthTerrain) then begin
        VGoogleEarthTerrain.Open(VTileInfoWithData.TileData.Buffer, VTileInfoWithData.TileData.Size);
        FMemCache.Add(ATile, AZoom, VGoogleEarthTerrain);
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
  Result := cUndefinedElevationValue;

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

      if (VTerrain <> nil) and (VTerrain.Exists) and Assigned(VTerrain.Parser) then begin

        VElevation := VTerrain.Parser.Elevation(VLonLat.X, VLonLat.Y);
        VFound := Round(VElevation) <> cUndefinedElevationValue;

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
  const ACacheConfig: IGlobalCacheConfig
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

  inherited Create(ACacheConfig.GECachePath, VStrorage, VConverter);
end;

{ TTerrainProviderByGeoCacher }

constructor TTerrainProviderByGeoCacher.Create(
  const ACacheConfig: IGlobalCacheConfig);
var
  VStrorage: ITerrainStorage;
  VConverter: ICoordConverter;
begin
  VStrorage :=
    TTileStorageGCTerrain.Create(ACacheConfig.GCCachePath.Path) as ITerrainStorage;

  VConverter :=
    (TCoordConverterFactorySimple.Create as ICoordConverterFactory).GetCoordConverterByCode(
      CGELonLatProjectionEPSG,
      CTileSplitQuadrate256x256
    );

  inherited Create(ACacheConfig.GCCachePath, VStrorage, VConverter);
end;

end.
