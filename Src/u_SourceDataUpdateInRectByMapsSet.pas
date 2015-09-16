{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_SourceDataUpdateInRectByMapsSet;

interface

uses
  SysUtils,
  i_ObjectWithListener,
  i_Listener,
  i_TileRect,
  i_MapTypeSet,
  i_MapTypeSetChangeable,
  u_BaseInterfacedObject;

type
  TSourceDataUpdateInRectByMapsSet = class(TBaseInterfacedObject, IObjectWithListener)
  private
    FMapTypeSet: IMapTypeSetChangeable;

    FMapTypeSetListener: IListener;
    FMapListener: IListener;
    FCS: IReadWriteSync;

    FMapsListened: IMapTypeSet;

    FListener: IListener;
    FListenTileRect: ITileRect;
    procedure OnTileUpdate(const AMsg: IInterface);
    procedure OnMapSetChange;

    procedure _RemoveListeners(
      const AMapsListened: IMapTypeSet
    );
    procedure _SetListeners(
      const AMapsListened: IMapTypeSet;
      const ATileRect: ITileRect
    );
  private
    procedure SetListener(
      const AListener: IListener;
      const ATileRect: ITileRect
    );
    procedure RemoveListener;

  public
    constructor Create(
      const AMapTypeSet: IMapTypeSetChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  Math,
  t_GeoTypes,
  i_LonLatRect,
  i_ProjectionInfo,
  i_NotifierTilePyramidUpdate,
  i_MapType,
  u_ListenerByEvent,
  u_TileUpdateListenerToLonLat,
  u_GeoFunc,
  u_Synchronizer;

{ TSourceDataUpdateInRectByMapsSet }

constructor TSourceDataUpdateInRectByMapsSet.Create(
  const AMapTypeSet: IMapTypeSetChangeable
);
begin
  inherited Create;
  FMapTypeSet := AMapTypeSet;
  FMapTypeSetListener := TNotifyNoMmgEventListener.Create(Self.OnMapSetChange);
  FCS := GSync.SyncVariable.Make(Self.ClassName);
  FMapListener := TTileUpdateListenerToLonLat.Create(Self.OnTileUpdate);
  FMapTypeSet.ChangeNotifier.Add(FMapTypeSetListener);
  OnMapSetChange;
end;

destructor TSourceDataUpdateInRectByMapsSet.Destroy;
begin
  if Assigned(FMapTypeSet) and Assigned(FMapTypeSetListener) then begin
    FMapTypeSet.ChangeNotifier.Remove(FMapTypeSetListener);
    FMapTypeSet := nil;
    FMapTypeSetListener := nil;
  end;
  if Assigned(FMapsListened) and Assigned(FMapListener) then begin
    _RemoveListeners(FMapsListened);
  end;
  inherited;
end;

procedure TSourceDataUpdateInRectByMapsSet.OnMapSetChange;
var
  VMapSet: IMapTypeSet;
begin
  VMapSet := FMapTypeSet.GetStatic;
  FCS.BeginWrite;
  try
    if Assigned(FMapsListened) and not FMapsListened.IsEqual(VMapSet) then begin
      if Assigned(FListener) and Assigned(FListenTileRect) then begin
        _RemoveListeners(FMapsListened);
      end;
    end;
    if Assigned(VMapSet) and not VMapSet.IsEqual(FMapsListened) then begin
      if Assigned(FListener) and Assigned(FListenTileRect) then begin
        _SetListeners(VMapSet, FListenTileRect);
      end;
    end;
    FMapsListened := VMapSet;
  finally
    FCS.EndWrite;
  end;
end;

procedure TSourceDataUpdateInRectByMapsSet.OnTileUpdate(const AMsg: IInterface);
var
  VListener: IListener;
  VLonLatRect: ILonLatRect;
begin
  FCS.BeginRead;
  try
    VListener := FListener;
  finally
    FCS.EndRead;
  end;
  if VListener <> nil then begin
    if Supports(AMsg, ILonLatRect, VLonLatRect) then begin
      VListener.Notification(VLonLatRect);
    end else begin
      VListener.Notification(nil);
    end;
  end;
end;

procedure TSourceDataUpdateInRectByMapsSet.RemoveListener;
begin
  FCS.BeginWrite;
  try
    if Assigned(FListener) and Assigned(FListenTileRect) and Assigned(FMapsListened) then begin
      _RemoveListeners(FMapsListened);
    end;
    FListener := nil;
    FListenTileRect := nil;
  finally
    FCS.EndWrite;
  end;
end;

procedure TSourceDataUpdateInRectByMapsSet._RemoveListeners(
  const AMapsListened: IMapTypeSet
);
var
  i: Integer;
  VMap: IMapType;
  VNotifier: INotifierTilePyramidUpdate;
begin
  Assert(Assigned(FMapListener));
  if Assigned(AMapsListened) and Assigned(FMapListener) then begin
    for i := 0 to AMapsListened.Count - 1 do begin
      VMap := AMapsListened.Items[i];
      if Assigned(VMap) then begin
        VNotifier := VMap.TileStorage.TileNotifier;
        if VNotifier <> nil then begin
          VNotifier.Remove(FMapListener);
        end;
      end;
    end;
  end;
end;

procedure TSourceDataUpdateInRectByMapsSet.SetListener(
  const AListener: IListener;
  const ATileRect: ITileRect
);
begin
  FCS.BeginWrite;
  try
    if not Assigned(AListener) or not Assigned(ATileRect) then begin
      if Assigned(FListener) and Assigned(FListenTileRect) and Assigned(FMapsListened) then begin
        _RemoveListeners(FMapsListened);
      end;
      FListener := nil;
      FListenTileRect := nil;
    end else begin
      if not ATileRect.IsEqual(FListenTileRect) then begin
        if Assigned(FListener) and Assigned(FListenTileRect) and Assigned(FMapsListened) then begin
          _RemoveListeners(FMapsListened);
        end;
        if Assigned(FMapsListened) then begin
          _SetListeners(FMapsListened, ATileRect);
        end;
        FListenTileRect := ATileRect;
      end;
      FListener := AListener;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TSourceDataUpdateInRectByMapsSet._SetListeners(
  const AMapsListened: IMapTypeSet;
  const ATileRect: ITileRect
);
var
  i: Integer;
  VMap: IMapType;
  VProjection: IProjectionInfo;
  VTileRect: TRect;
  VLonLatRect: TDoubleRect;
  VMapProjection: IProjectionInfo;
  VMapLonLatRect: TDoubleRect;
  VNotifier: INotifierTilePyramidUpdate;
begin
  VProjection := ATileRect.ProjectionInfo;
  VLonLatRect := VProjection.TileRect2LonLatRect(ATileRect.Rect);
  for i := 0 to AMapsListened.Count - 1 do begin
    VMap := AMapsListened.Items[i];
    if VMap <> nil then begin
      VNotifier := VMap.TileStorage.TileNotifier;
      if VNotifier <> nil then begin
        VMapProjection := VMap.ProjectionSet.GetSuitableProjection(VProjection);
        VMapLonLatRect := VLonLatRect;
        VMapProjection.ProjectionType.ValidateLonLatRect(VMapLonLatRect);
        VTileRect :=
          RectFromDoubleRect(
            VMapProjection.LonLatRect2TileRectFloat(VMapLonLatRect),
            rrOutside
          );
        VNotifier.AddListenerByRect(FMapListener, VMapProjection.Zoom, VTileRect);
      end;
    end;
  end;
end;

end.
