unit i_RegionProcessParamsFrame;

interface

uses
  i_VectorItemLonLat,
  u_MapType;

type
  IRegionProcessParamsFrameBase = interface
    ['{F5346D9B-766C-4B3B-AC4B-9AC71FF62F05}']
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
  end;

  IRegionProcessParamsFrameOneMapAndZoom = interface(IRegionProcessParamsFrameBase)
    ['{0C15FA38-5F6E-4BF6-A4ED-31B74FF8169F}']
    function GetMapType: TMapType;
    property MapType: TMapType read GetMapType;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;
  end;

  IRegionProcessParamsFrameTargetPath = interface(IRegionProcessParamsFrameBase)
    ['{9FDF9521-9A18-455D-8AAC-E9FF9FC08791}']
    function GetPath: string;
    property Path: string read GetPath;
  end;

implementation

end.
