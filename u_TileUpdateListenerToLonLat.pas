unit u_TileUpdateListenerToLonLat;

interface

uses
  i_Listener,
  i_SimpleFlag,
  i_CoordConverter,
  u_ListenerByEvent;

type
  TTileUpdateListenerToLonLat = class(TInterfacedObject, IListener, IListenerDisconnectable)
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
  i_LonLatRect,
  u_LonLatRect,
  u_SimpleFlagWithInterlock;

{ TTileUpdateListenerToLonLat }

constructor TTileUpdateListenerToLonLat.Create(
  const ACoordConverter: ICoordConverter;
  AEvent: TNotifyListenerEvent
);
begin
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
  VLonLatRect: ILonLatRect;
  VTile: TPoint;
  VZoom: Byte;
begin
  if not FDisconnectFlag.CheckFlag then begin
    if Supports(AMsg, ITileKey, VTileKey) then begin
      VTile := VTileKey.Tile;
      VZoom := VTileKey.Zoom;
      FCoordConverter.CheckTilePosStrict(VTile, VZoom, True);
      VLonLatRect := TLonLatRect.Create(FCoordConverter.TilePos2LonLatRect(VTile, VZoom));
      FEvent(VLonLatRect);
    end else begin
      FEvent(nil);
    end;
  end;
end;

end.

