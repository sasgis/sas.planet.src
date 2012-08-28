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
    ['{63FC7494-8ECF-42BE-A516-3908337F77CE}']
    function GetGeoCoder: ICoordConverter;
    property GeoCoder: ICoordConverter read GetGeoCoder;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    procedure Add(
      const AListener: IListener;
      const ATileRect: TRect
    );
    procedure Remove(const AListener: IListener);
  end;

  INotifierTileRectUpdateInternal = interface
    procedure TileUpdateNotify(const ATileKey: ITileKey);
    procedure TileRectUpdateNotify(const ATileRect: ITileRect);
  end;

implementation

end.
