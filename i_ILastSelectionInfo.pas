unit i_ILastSelectionInfo;

interface

uses
  t_GeoTypes,
  i_IConfigDataElement;

type
  ILastSelectionInfo = interface(IConfigDataElement)
    ['{8F34A418-0320-4B19-A569-B44FA73146F4}']
    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetPolygon: TArrayOfDoublePoint;
    property Polygon: TArrayOfDoublePoint read GetPolygon;

    procedure SetPolygon(ALonLatPolygon: TArrayOfDoublePoint; AZoom: Byte);
  end;

implementation

end.
