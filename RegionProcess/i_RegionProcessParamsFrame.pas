unit i_RegionProcessParamsFrame;

interface

uses
  Types,
  i_VectorItemLonLat,
  i_BitmapLayerProvider,
  i_MapCalibration,
  i_ProjectionInfo,
  i_PredicateByTileInfo,
  u_MapType;

type
  IRegionProcessParamsFrameBase = interface
    ['{F5346D9B-766C-4B3B-AC4B-9AC71FF62F05}']
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatMultiPolygon
    );
    function Validate: Boolean;
  end;

  IRegionProcessParamsFrameOneMap = interface(IRegionProcessParamsFrameBase)
    ['{240B7587-DDC0-4471-BDF4-AD2EE0040526}']
    function GetMapType: TMapType;
    property MapType: TMapType read GetMapType;
  end;

  IRegionProcessParamsFrameOneZoom = interface(IRegionProcessParamsFrameBase)
    ['{A1A9D2C3-4C9F-4205-B19C-5A768E938808}']
    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;
  end;

  IRegionProcessParamsFrameZoomArray = interface(IRegionProcessParamsFrameBase)
    ['{9DB542F9-7F4E-4DFF-8957-E0E81B8A9096}']
    function GetZoomArray: TByteDynArray;
    property ZoomArray: TByteDynArray read GetZoomArray;
  end;

  IRegionProcessParamsFrameTargetProjection = interface(IRegionProcessParamsFrameBase)
    ['{F0FACC2E-C686-4282-99A1-E5E2F1F5CE2D}']
    function GetProjection: IProjectionInfo;
    property Projection: IProjectionInfo read GetProjection;
  end;

  IRegionProcessParamsFrameMapCalibrationList = interface(IRegionProcessParamsFrameBase)
    ['{41A9899D-D431-4D12-8DC4-1F65B36A8CAB}']
    function GetMapCalibrationList: IMapCalibrationList;
    property MapCalibrationList: IMapCalibrationList read GetMapCalibrationList;
  end;

  IRegionProcessParamsFrameImageProvider = interface(IRegionProcessParamsFrameBase)
    ['{98A4BE9B-AF50-45F5-8E26-0DBF0F094C0B}']
    function GetProvider: IBitmapLayerProvider;
    property Provider: IBitmapLayerProvider read GetProvider;
  end;

  IRegionProcessParamsFrameProcessPredicate = interface(IRegionProcessParamsFrameBase)
    ['{DF8D4BBB-BA83-412A-BA70-3A1E454AD3C3}']
    function GetPredicate: IPredicateByTileInfo;
    property Predicate: IPredicateByTileInfo read GetPredicate;
  end;

  IRegionProcessParamsFrameTargetPath = interface(IRegionProcessParamsFrameBase)
    ['{A0510824-7E26-430F-9C04-AE71EBAD65FF}']
    function GetPath: string;
    property Path: string read GetPath;
  end;

implementation

end.
