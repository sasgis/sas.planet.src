unit i_TileRectUpdateNotifier;

interface

uses
  Types,
  i_JclNotify,
  i_CoordConverter;

type
  ITileRectUpdateNotifier = interface
    ['{63FC7494-8ECF-42BE-A516-3908337F77CE}']
    function GetGeoCoder: ICoordConverter; stdcall;
    property GeoCoder: ICoordConverter read GetGeoCoder;

    function GetZoom: Byte; stdcall;
    property Zoom: Byte read GetZoom;

    procedure Add(AListener: IJclListener; ATileRect: TRect); stdcall;
    procedure Remove(AListener: IJclListener); stdcall;
    procedure TileUpdateNotify(ATile: TPoint); stdcall;
  end;

implementation

end.
