unit i_NotifierTilePyramidUpdate;

interface

uses
  Types,
  i_Listener,
  i_TileKey,
  i_TileRect,
  i_CoordConverter;

type
  INotifierTilePyramidUpdate = interface
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

  INotifierTilePyramidUpdateInternal = interface
    procedure TileFullUpdateNotify;
    procedure TileUpdateNotify(const ATileKey: ITileKey); overload;
    procedure TileUpdateNotify(const ATile: TPoint; const AZoom: Byte); overload;
    procedure TileRectUpdateNotify(const ATileRect: ITileRect); overload;
    procedure TileRectUpdateNotify(const ATileRect: TRect; const AZoom: Byte); overload;
  end;

implementation

end.
