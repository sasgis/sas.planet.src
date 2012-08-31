unit i_NotifierTileRectUpdate;

interface

uses
  Types,
  i_Listener,
  i_TileKey,
  i_TileRect,
  i_CoordConverter;

type
  INotifierTileRectUpdate = interface
    ['{67415555-955C-4BC7-BC8F-2F9BCDD0F065}']
    function GetGeoCoder: ICoordConverter;
    property GeoCoder: ICoordConverter read GetGeoCoder;

    procedure AddListenerByRect(
      const AListener: IListener;
      const AZoom: Byte;
      const ATileRect: TRect
    );
    procedure AddListenerByZoom(
      const AListener: IListener;
      const AZoom: Byte
    );
    procedure AddListener(
      const AListener: IListener
    );
    procedure Remove(const AListener: IListener);
  end;

  INotifierTileRectUpdateInternal = interface
    procedure TileUpdateNotify(const ATileKey: ITileKey);
    procedure TileRectUpdateNotify(const ATileRect: ITileRect);
  end;

implementation

end.
