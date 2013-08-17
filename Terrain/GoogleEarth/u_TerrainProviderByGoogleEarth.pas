{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit u_TerrainProviderByGoogleEarth;

interface

uses
  Types,
  libge,
  t_GeoTypes,
  i_Listener,
  i_Notifier,
  i_PathConfig,
  i_CoordConverter,
  i_TerrainProvider,
  i_GoogleEarthTerrainTileStorage,
  u_BaseInterfacedObject;

type
  TTerrainProviderByGoogleEarth = class(TBaseInterfacedObject, ITerrainProvider)
  private
    FStorage: IGoogleEarthTerrainTileStorage;
    FCoordConverter: ICoordConverter;
    FCacheStateChangeListner: IListener;
    FCacheConfigChangeListener: IListener;
    FPathConfig: IPathConfig;
    FStateChangeNotifier: INotifier;
    FStateChangeNotifierInternal: INotifierInternal;
  protected
    function GetPointElevation(const ALonLat: TDoublePoint; const AZoom: Byte): Single;
    procedure OnCacheConfigChange;
    procedure OnCacheStateChange;
    function GetAvailable: Boolean;
    function GetStateChangeNotifier: INotifier;
  public
    constructor Create(
      const APathConfig: IPathConfig
    );
    destructor Destroy; override;
  end;
  
implementation

uses
  c_CoordConverter,
  c_TerrainProvider,
  i_CoordConverterFactory,  
  u_GeoFun,
  u_Notifier,
  u_ListenerByEvent,
  u_CoordConverterFactorySimple,
  u_GoogleEarthTerrainTileStorage;

procedure CheckGoogleEarthTerrainTileZoom(var AZoom: Byte);
begin
  // GE terrain's zooms must be in values: [3,5,7,9,11,13,15,17,19,23,25]
  if AZoom <= 3 then begin
    AZoom := 2;
  end else if (AZoom mod 2) > 0 then begin
    AZoom := AZoom - 1;
  end;
end;

{ TTerrainProviderByGoogleEarth }

constructor TTerrainProviderByGoogleEarth.Create(
  const APathConfig: IPathConfig
);
begin
  inherited Create;
  FPathConfig := APathConfig;

  FStorage := TGoogleEarthTerrainTileStorage.Create(FPathConfig.FullPath);

  FCoordConverter :=
    (TCoordConverterFactorySimple.Create as ICoordConverterFactory).GetCoordConverterByCode(
      CGELonLatProjectionEPSG,
      CTileSplitQuadrate256x256
    );

  FStateChangeNotifierInternal := TNotifierBase.Create;
  FStateChangeNotifier := FStateChangeNotifierInternal;

  FCacheStateChangeListner := TNotifyNoMmgEventListener.Create(Self.OnCacheStateChange);
  FStorage.Notifier.Add(FCacheStateChangeListner);

  FCacheConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnCacheConfigChange);
  FPathConfig.ChangeNotifier.Add(FCacheConfigChangeListener);
end;

destructor TTerrainProviderByGoogleEarth.Destroy;
begin
  if Assigned(FPathConfig) and Assigned(FCacheConfigChangeListener) then begin
    FPathConfig.ChangeNotifier.Remove(FCacheConfigChangeListener);
    FPathConfig := nil;
    FCacheConfigChangeListener := nil;
  end;
  if Assigned(FStorage) and Assigned(FCacheStateChangeListner) then begin
    FStorage.Notifier.Remove(FCacheStateChangeListner);
  end;
  FCoordConverter := nil;
  FStorage := nil;
  FStateChangeNotifier := nil;
  FStateChangeNotifierInternal := nil;
  inherited;
end;

function TTerrainProviderByGoogleEarth.GetStateChangeNotifier: INotifier;
begin
  Result := FStateChangeNotifier;
end;

procedure TTerrainProviderByGoogleEarth.OnCacheConfigChange;
begin
  FStorage.SetPath(FPathConfig.Path);
  FStateChangeNotifierInternal.Notify(nil);
end;

procedure TTerrainProviderByGoogleEarth.OnCacheStateChange;
begin
  FStateChangeNotifierInternal.Notify(nil);
end;

function TTerrainProviderByGoogleEarth.GetAvailable: Boolean;
begin
  Result := FStorage.Available;
end;

function TTerrainProviderByGoogleEarth.GetPointElevation(
 const ALonLat: TDoublePoint;
 const AZoom: Byte
): Single;
var
  VLonLat: TDoublePoint;
  VTilePoint: TPoint;
  VZoom: Byte;
  VElevation: Single;
  VTerrainProvider: IGoogleEarthTerrainTileProvider;
begin
  Result := cUndefinedElevationValue;

  if FStorage.Available then begin

    VLonLat := ALonLat;
    VZoom := AZoom;

    repeat
      CheckGoogleEarthTerrainTileZoom(VZoom);

      VTilePoint := PointFromDoublePoint(
        FCoordConverter.LonLat2TilePosFloat(VLonLat, VZoom),
        prToTopLeft
      );

      VTerrainProvider := FStorage.GetTileInfo(VTilePoint, VZoom);

      if Assigned(VTerrainProvider) then begin
        VElevation := VTerrainProvider.PointElevation(VLonLat.X, VLonLat.Y);
        if Round(VElevation) <> cUndefinedElevationValue then begin
          Result := VElevation;
          Exit;
        end;
      end;

      if VZoom > 2 then begin
        VZoom := VZoom - 1; // try get elevation info from prev. zoom
      end else begin
        Exit;
      end;
    until False;
  end;  
end;

end.
