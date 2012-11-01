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
  i_CoordConverter,
  i_TerrainStorage,
  i_TerrainProvider,
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
    procedure TryLoadTileToMemCache(const ATile: TPoint; const AZoom: Byte);
    procedure CheckTileZoom(var AZoom: Byte);
    procedure OnCloseTerrainTile(const ATile: PTerrainTile);
  protected
    function GetPointElevation(const ALonLat: TDoublePoint; const AZoom: Byte): Single;
  public
    constructor Create(
      const AStorage: ITerrainStorage;
      const ACoordConverter: ICoordConverter
    );
    destructor Destroy; override;
  end;

  TTerrainProviderByGoogleEarth = class(TTerrainProviderByDLL)
    public
      constructor Create(const AStoragePath: string);
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
  u_CoordConverterFactorySimple,
  u_TileStorageGETerrain;

const
  cMemCacheCapacity = 500;

{ TTerrainProviderByDLL }

constructor TTerrainProviderByDLL.Create(
  const AStorage: ITerrainStorage;
  const ACoordConverter: ICoordConverter
);
begin
  inherited Create;
  FStorage := AStorage;
  FCoordConverter := ACoordConverter;
  FTerrainPerser := TGoogleEarthTerrainParser.Create;
  FMemCache := TTerrainProviderByGEMemCache.Create(cMemCacheCapacity, Self.OnCloseTerrainTile);
  FAvailable := FStorage.Available and FTerrainPerser.Available;
end;

destructor TTerrainProviderByDLL.Destroy;
begin
  FCoordConverter := nil;
  FStorage := nil;
  FMemCache.Free;
  inherited Destroy;
end;

procedure TTerrainProviderByDLL.CheckTileZoom(var AZoom: Byte);
begin
  // GE terrain's zooms mast be in values: [3,5,7,9,11,13,15,17,19,23,25]
  if AZoom <= 3 then begin
    AZoom := 5;
  end else if (AZoom mod 2) = 0 then begin
    AZoom := AZoom + 1;
  end;
end;

procedure TTerrainProviderByDLL.OnCloseTerrainTile(const ATile: PTerrainTile);
begin
  if (ATile <> nil) then begin
    FTerrainPerser.Close(ATile.ParserContext);
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

  // ToDo: Process in separate thread

  VContext := nil;
  VTileInfo := FStorage.GetTileInfo(ATile, AZoom);

  if VTileInfo <> nil then begin
    VTneFound := (not VTileInfo.IsExists or VTileInfo.IsExistsTNE);

    if (not VTneFound and Supports(VTileInfo, ITileInfoWithData, VTileInfoWithData)) then begin
      if FTerrainPerser.Open(VContext, VTileInfoWithData.TileData) then begin
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
  VTileZoom: Byte;
  VTerrain: PTerrainTile;
  VParentFound: Boolean;
  VElevation: Single;
begin
  Result := cUndefElevationValue;

  if FAvailable then begin
    VParentFound := False;
    VLonLat := ALonLat;
    VTileZoom := AZoom;

    FCoordConverter.CheckZoom(VTileZoom);
    FCoordConverter.CheckLonLatPos(VLonLat);

    Self.CheckTileZoom(VTileZoom);

    VTilePoint := PointFromDoublePoint(
      FCoordConverter.LonLat2TilePosFloat(VLonLat, VTileZoom),
      prToTopLeft
    );

    VTerrain := FMemCache.Get(VTilePoint, VTileZoom);

    if (VTerrain = nil) or (not VTerrain.Exists) then begin
      VTerrain := FMemCache.GetParent(VTilePoint, VTileZoom);
      VParentFound := (VTerrain <> nil);
    end;

    if VParentFound or (VTerrain = nil) then begin

      TryLoadTileToMemCache(VTilePoint, VTileZoom);

  //  ToDo: wait separate thread with time-out and read MemCache by event

      VTerrain := FMemCache.Get(VTilePoint, VTileZoom);
    end;

    if (VTerrain <> nil) and (VTerrain.Exists) then begin
      if
        FTerrainPerser.GetElevation(
          VTerrain.ParserContext,
          VLonLat,
          VTerrain.Zoom,
          VElevation
        )
      then begin
        Result := VElevation;
      end;
    end;
  end;  
end;

{ TTerrainProviderByGoogleEarth }

constructor TTerrainProviderByGoogleEarth.Create(const AStoragePath: string);
var
  VStrorage: ITerrainStorage;
  VConverter: ICoordConverter;
begin
  VStrorage := TTileStorageGETerrain.Create(AStoragePath) as ITerrainStorage;
  VConverter :=
    (TCoordConverterFactorySimple.Create as ICoordConverterFactory).GetCoordConverterByCode(
      CGELonLatProjectionEPSG,
      CTileSplitQuadrate256x256
    );
  inherited Create(VStrorage, VConverter);
end;

end.
