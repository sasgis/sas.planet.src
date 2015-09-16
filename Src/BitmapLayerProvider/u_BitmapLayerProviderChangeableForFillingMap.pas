{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_BitmapLayerProviderChangeableForFillingMap;

interface

uses
  i_Listener,
  i_FillingMapPolygon,
  i_FillingMapLayerConfig,
  i_MapType,
  i_GeometryProjectedFactory,
  i_Bitmap32BufferFactory,
  i_BitmapLayerProvider,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForFillingMap = class(TBitmapLayerProviderChangeableBase)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FGeometryProjectedFactory: IGeometryProjectedFactory;
    FConfig: IFillingMapLayerConfig;
    FMapType: IMapTypeChangeable;
    FPolygon: IFillingMapPolygon;
    FVersionListener: IListener;
    FSourceMapLast: IMapType;
    procedure OnMapVersionChange;
    procedure OnConfigChange;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AGeometryProjectedFactory: IGeometryProjectedFactory;
      const AMapType: IMapTypeChangeable;
      const APolygon: IFillingMapPolygon;
      const AConfig: IFillingMapLayerConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_MapVersionRequest,
  i_FillingMapColorer,
  u_ListenerByEvent,
  u_FillingMapColorerSimple,
  u_BitmapLayerProviderFillingMap;

{ TBitmapLayerProviderChangeableForFillingMap }

constructor TBitmapLayerProviderChangeableForFillingMap.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AGeometryProjectedFactory: IGeometryProjectedFactory;
  const AMapType: IMapTypeChangeable;
  const APolygon: IFillingMapPolygon;
  const AConfig: IFillingMapLayerConfig
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AMapType));
  Assert(Assigned(APolygon));
  Assert(Assigned(AConfig));

  inherited Create;

  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FGeometryProjectedFactory := AGeometryProjectedFactory;
  FMapType := AMapType;
  FPolygon := APolygon;
  FConfig := AConfig;

  FVersionListener := TNotifyNoMmgEventListener.Create(Self.OnMapVersionChange);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FMapType.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FPolygon.ChangeNotifier
  );
end;

destructor TBitmapLayerProviderChangeableForFillingMap.Destroy;
begin
  if Assigned(FSourceMapLast) and Assigned(FVersionListener) then begin
    FSourceMapLast.VersionRequestConfig.ChangeNotifier.Remove(FVersionListener);
    FSourceMapLast := nil;
    FVersionListener := nil;
  end;
  inherited;
end;

function TBitmapLayerProviderChangeableForFillingMap.CreateStatic: IInterface;
var
  VConfig: IFillingMapLayerConfigStatic;
  VResult: IBitmapTileUniProvider;
  VMap: IMapType;
  VColorer: IFillingMapColorer;
  VVersionRequest: IMapVersionRequest;
begin
  VResult := nil;
  VConfig := FConfig.GetStatic;
  if VConfig.Visible then begin
    VMap := FMapType.GetStatic;
    if FSourceMapLast <> VMap then begin
      if Assigned(FSourceMapLast) then begin
        FSourceMapLast.VersionRequestConfig.ChangeNotifier.Remove(FVersionListener);
      end;
      FSourceMapLast := VMap;
      if Assigned(FSourceMapLast) then begin
        FSourceMapLast.VersionRequestConfig.ChangeNotifier.Add(FVersionListener);
      end;
    end;
    VVersionRequest := VMap.VersionRequestConfig.GetStatic;
    VColorer :=
      TFillingMapColorerSimple.Create(
        VConfig.NoTileColor,
        VConfig.ShowTNE,
        VConfig.TNEColor,
        VConfig.FillMode,
        VConfig.FilterMode,
        VConfig.FillFirstDay,
        VConfig.FillLastDay
      );
    VResult :=
      TBitmapLayerProviderFillingMap.Create(
        FBitmap32StaticFactory,
        FGeometryProjectedFactory,
        VMap.TileStorage,
        VVersionRequest,
        VConfig.UseRelativeZoom,
        VConfig.Zoom,
        FPolygon.Polygon,
        VColorer
      );
  end;
  Result := VResult;
end;

procedure TBitmapLayerProviderChangeableForFillingMap.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForFillingMap.OnMapVersionChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
