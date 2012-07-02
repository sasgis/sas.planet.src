unit i_TileRectUpdateNotifier;

interface

uses
  Types,
  i_Notify,
  i_TileKey,
  i_CoordConverter;

type
  ITileRectUpdateNotifier = interface
    ['{63FC7494-8ECF-42BE-A516-3908337F77CE}']
    function GetGeoCoder: ICoordConverter; stdcall;
    property GeoCoder: ICoordConverter read GetGeoCoder;

    function GetZoom: Byte; stdcall;
    property Zoom: Byte read GetZoom;

    procedure Add(
      const AListener: IListener;
      const ATileRect: TRect
    ); stdcall;
    procedure Remove(const AListener: IListener); stdcall;
  end;

  ITileRectUpdateNotifierInternal = interface
    ['{86C0C887-1DD8-43B8-9B5A-0504B4BFA809}']
    procedure TileUpdateNotify(const ATileKey: ITileKey); stdcall;
  end;

implementation

end.

