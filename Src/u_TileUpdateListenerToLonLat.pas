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

unit u_TileUpdateListenerToLonLat;

interface

uses
  t_Listener,
  i_Listener,
  i_SimpleFlag,
  i_CoordConverter,
  u_ListenerByEvent,
  u_BaseInterfacedObject;

type
  TTileUpdateListenerToLonLat = class(TBaseInterfacedObject, IListener, IListenerDisconnectable)
  private
    FDisconnectFlag: ISimpleFlag;
    FEvent: TNotifyListenerEvent;
    FCoordConverter: ICoordConverter;
  private
    procedure Notification(const AMsg: IInterface);
  private
    procedure Disconnect;
  public
    constructor Create(
      const ACoordConverter: ICoordConverter;
      AEvent: TNotifyListenerEvent
    );
  end;

implementation

uses
  Types,
  SysUtils,
  i_TileKey,
  i_TileRect,
  i_LonLatRect,
  u_LonLatRect,
  u_SimpleFlagWithInterlock;

{ TTileUpdateListenerToLonLat }

constructor TTileUpdateListenerToLonLat.Create(
  const ACoordConverter: ICoordConverter;
  AEvent: TNotifyListenerEvent
);
begin
  Assert(ACoordConverter <> nil);
  inherited Create;
  FEvent := AEvent;
  FDisconnectFlag := TSimpleFlagWithInterlock.Create;
  Assert(Assigned(FEvent));
  FCoordConverter := ACoordConverter;
end;

procedure TTileUpdateListenerToLonLat.Disconnect;
begin
  FDisconnectFlag.SetFlag;
end;

procedure TTileUpdateListenerToLonLat.Notification(const AMsg: IInterface);
var
  VTileKey: ITileKey;
  VTileRect: ITileRect;
  VLonLatRect: ILonLatRect;
  VTile: TPoint;
  VZoom: Byte;
  VRect: TRect;
begin
  if not FDisconnectFlag.CheckFlag then begin
    if Supports(AMsg, ITileKey, VTileKey) then begin
      VTile := VTileKey.Tile;
      VZoom := VTileKey.Zoom;
      FCoordConverter.ValidateTilePosStrict(VTile, VZoom, True);
      VLonLatRect := TLonLatRect.Create(FCoordConverter.TilePos2LonLatRect(VTile, VZoom));
      FEvent(VLonLatRect);
    end else if Supports(AMsg, ITileRect, VTileRect) then begin
      VZoom := VTileRect.Zoom;
      VRect := VTileRect.Rect;
      FCoordConverter.ValidateTileRect(VRect, VZoom);
      VLonLatRect := TLonLatRect.Create(FCoordConverter.TileRect2LonLatRect(VRect, VZoom));
      FEvent(VLonLatRect);
    end else begin
      FEvent(nil);
    end;
  end;
end;

end.
