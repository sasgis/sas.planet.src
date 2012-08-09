unit i_TileProvider;

interface

uses
  Types,
  i_CoordConverter,
  i_VectorDataItemSimple,
  i_Bitmap32Static;

type
  IBitmapTileProvider = interface
    ['{88ACB3F9-FDEE-4451-89A0-EA24133E2DB5}']
    function GetGeoConverter: ICoordConverter;
    property GeoConverter: ICoordConverter read GetGeoConverter;

    function GetTile(
      const ATile: TPoint;
      const AZoom: Byte
    ): IBitmap32Static;
  end;

  IVectorTileProvider = interface
    ['{00ADB9F4-D421-4F71-A9B6-3F8A6E8FFCB9}']
    function GetGeoConverter: ICoordConverter;
    property GeoConverter: ICoordConverter read GetGeoConverter;

    function GetTile(
      const ATile: TPoint;
      const AZoom: Byte
    ): IVectorDataItemList;
  end;

implementation

end.
