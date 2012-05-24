unit i_RegionProcessParamsFrame;

interface

uses
  Types,
  i_VectorItemLonLat,
  i_BitmapLayerProvider,
  i_MapCalibration,
  i_ProjectionInfo,
  u_MapType;

type
  IRegionProcessParamsFrameBase = interface
    ['{F5346D9B-766C-4B3B-AC4B-9AC71FF62F05}']
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
  end;

  IRegionProcessParamsFrameOneMap = interface(IRegionProcessParamsFrameBase)
    ['{0C15FA38-5F6E-4BF6-A4ED-31B74FF8169F}']
    function GetMapType: TMapType;
    property MapType: TMapType read GetMapType;
  end;

  IRegionProcessParamsFrameOneZoom = interface(IRegionProcessParamsFrameBase)
    ['{51668A2D-05E8-470F-9642-05E83B6D2956}']
    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;
  end;

  IRegionProcessParamsFrameZoomArray = interface(IRegionProcessParamsFrameBase)
    ['{0C15FA38-5F6E-4BF6-A4ED-31B74FF8169F}']
    function GetZoomArray: TByteDynArray;
    property ZoomArray: TByteDynArray read GetZoomArray;
  end;

  IRegionProcessParamsFrameTargetProjection = interface(IRegionProcessParamsFrameBase)
    ['{0C15FA38-5F6E-4BF6-A4ED-31B74FF8169F}']
    function GetProjection: IProjectionInfo;
    property Projection: IProjectionInfo read GetProjection;
  end;

  IRegionProcessParamsFrameMapCalibrationList = interface(IRegionProcessParamsFrameBase)
    ['{0C15FA38-5F6E-4BF6-A4ED-31B74FF8169F}']
    function GetMapCalibrationList: IMapCalibrationList;
    property MapCalibrationList: IMapCalibrationList read GetMapCalibrationList;
  end;

  IRegionProcessParamsFrameImageProvider = interface(IRegionProcessParamsFrameBase)
    ['{9FDF9521-9A18-455D-8AAC-E9FF9FC08791}']
    function GetProvider: IBitmapLayerProvider;
    property Provider: IBitmapLayerProvider read GetProvider;
  end;

  IRegionProcessParamsFrameTargetPath = interface(IRegionProcessParamsFrameBase)
    ['{9FDF9521-9A18-455D-8AAC-E9FF9FC08791}']
    function GetPath: string;
    property Path: string read GetPath;
  end;

implementation

end.
