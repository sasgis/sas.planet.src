unit i_ProjectionInfo;

interface

uses
  i_CoordConverter;

type
  IProjectionInfo = interface
    ['{1BAC7D2B-21F1-4DA7-AE3B-F9D91548E440}']
    function GetIsSameProjectionInfo(const AProjection: IProjectionInfo): Boolean;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetGeoConverter: ICoordConverter;
    property GeoConverter: ICoordConverter read GetGeoConverter;
  end;

implementation

end.
