unit u_TileUpdateListenerToLonLat;

interface

uses
  i_CoordConverter,
  u_NotifyEventListener;

type
  TTileUpdateListenerToLonLat = class(TNotifyEventListener)
  private
    FCoordConverter: ICoordConverter;
  protected
    procedure Notification(const AMsg: IInterface); override;
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
  u_LonLatRect;

{ TTileUpdateListenerToLonLat }

constructor TTileUpdateListenerToLonLat.Create(
  const ACoordConverter: ICoordConverter;
  AEvent: TNotifyListenerEvent
);
begin
  inherited Create(AEvent);
  FCoordConverter := ACoordConverter;
end;

procedure TTileUpdateListenerToLonLat.Notification(const AMsg: IInterface);
var
  VTileKey: ITileKey;
  VLonLatRect: ILonLatRect;
  VTile: TPoint;
  VZoom: Byte;
begin
  if Supports(AMsg, ITileKey, VTileKey) then begin
    VTile := VTileKey.Tile;
    VZoom := VTileKey.Zoom;
    FCoordConverter.CheckTilePosStrict(VTile, VZoom, True);
    VLonLatRect := TLonLatRect.Create(FCoordConverter.TilePos2LonLatRect(VTile, VZoom));
    inherited Notification(VLonLatRect);
  end else begin
    inherited Notification(nil);
  end;
end;

end.
