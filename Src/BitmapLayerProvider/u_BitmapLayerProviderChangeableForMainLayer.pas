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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_BitmapLayerProviderChangeableForMainLayer;

interface

uses
  i_Listener,
  i_TileError,
  i_BitmapPostProcessing,
  i_BitmapLayerProvider,
  i_UseTilePrevZoomConfig,
  i_Bitmap32BufferFactory,
  i_MapType,
  i_MapTypeListStatic,
  i_MapTypeListChangeable,
  i_ListenerNotifierLinksList,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForMainLayer = class(TBitmapLayerProviderChangeableBase)
  private
    FErrorLogger: ITileErrorLogger;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FPostProcessing: IBitmapPostProcessingChangeable;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    FMainMap: IMapTypeChangeable;
    FLayesList: IMapTypeListChangeable;

    FVersionListener: IListener;
    FMainMapLast: IMapType;
    FLayesListLast: IMapTypeListStatic;

    procedure OnMainMapChange;
    procedure OnLayerListChange;
    procedure OnConfigChange;
    procedure OnMapVersionChange;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const AMainMap: IMapTypeChangeable;
      const ALayesList: IMapTypeListChangeable;
      const APostProcessing: IBitmapPostProcessingChangeable;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AErrorLogger: ITileErrorLogger
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_BitmapLayerProviderForViewMaps;

{ TBitmapLayerProviderChangeableForMainLayer }

constructor TBitmapLayerProviderChangeableForMainLayer.Create(
  const AMainMap: IMapTypeChangeable;
  const ALayesList: IMapTypeListChangeable;
  const APostProcessing: IBitmapPostProcessingChangeable;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AErrorLogger: ITileErrorLogger
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FMainMap := AMainMap;
  FLayesList := ALayesList;
  FErrorLogger := AErrorLogger;
  FPostProcessing := APostProcessing;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;

  FVersionListener := TNotifyNoMmgEventListener.Create(Self.OnMapVersionChange);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMainMapChange),
    FMainMap.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerListChange),
    FLayesList.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FUseTilePrevZoomConfig.ChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FPostProcessing.ChangeNotifier
  );

end;

function TBitmapLayerProviderChangeableForMainLayer.CreateStatic: IInterface;
var
  i: Integer;
  VMap: IMapType;
  VMainMap: IMapType;
  VPostProcessingConfig: IBitmapPostProcessing;
  VLayersList: IMapTypeListStatic;
  VUsePrevConfig: IUseTilePrevZoomTileConfigStatic;
  VResult: IBitmapTileUniProvider;
begin
  VMainMap := FMainMap.GetStatic;
  if FMainMapLast <> VMainMap then begin
    if Assigned(FMainMapLast) then begin
      FMainMapLast.VersionRequestConfig.ChangeNotifier.Remove(FVersionListener);
    end;
    FMainMapLast := VMainMap;
    if Assigned(FMainMapLast) then begin
      FMainMapLast.VersionRequestConfig.ChangeNotifier.Add(FVersionListener);
    end;
  end;
  VLayersList := FLayesList.List;
  if (not Assigned(FLayesListLast) and Assigned(VLayersList)) or
    (Assigned(FLayesListLast) and FLayesListLast.IsEqual(VLayersList)) then begin
    if Assigned(FLayesListLast) then begin
      for i := 0 to FLayesListLast.Count - 1 do begin
        VMap := FLayesListLast.Items[i];
        VMap.VersionRequestConfig.ChangeNotifier.Remove(FVersionListener);
      end;
    end;
    FLayesListLast := VLayersList;
    if Assigned(FLayesListLast) then begin
      for i := 0 to FLayesListLast.Count - 1 do begin
        VMap := FLayesListLast.Items[i];
        VMap.VersionRequestConfig.ChangeNotifier.Add(FVersionListener);
      end;
    end;
  end;
  VUsePrevConfig := FUseTilePrevZoomConfig.GetStatic;
  VPostProcessingConfig := FPostProcessing.GetStatic;

  VResult :=
    TBitmapLayerProviderForViewMaps.Create(
      FBitmap32StaticFactory,
      VMainMap,
      VLayersList,
      VUsePrevConfig.UsePrevZoomAtMap,
      VUsePrevConfig.UsePrevZoomAtLayer,
      True,
      VPostProcessingConfig,
      FErrorLogger
    );
  Result := VResult;
end;

destructor TBitmapLayerProviderChangeableForMainLayer.Destroy;
var
  i: Integer;
  VMap: IMapType;
begin
  if Assigned(FMainMapLast) and Assigned(FVersionListener) then begin
    FMainMapLast.VersionRequestConfig.ChangeNotifier.Remove(FVersionListener);
    FMainMapLast := nil;
  end;
  if Assigned(FLayesListLast) and Assigned(FVersionListener) then begin
    for i := 0 to FLayesListLast.Count - 1 do begin
      VMap := FLayesListLast.Items[i];
      VMap.VersionRequestConfig.ChangeNotifier.Remove(FVersionListener);
    end;
    FLayesListLast := nil;
  end;
  inherited;
end;

procedure TBitmapLayerProviderChangeableForMainLayer.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForMainLayer.OnLayerListChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForMainLayer.OnMainMapChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForMainLayer.OnMapVersionChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
