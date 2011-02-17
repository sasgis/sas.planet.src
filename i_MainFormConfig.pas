unit i_MainFormConfig;

interface

uses
  i_IConfigDataElement,
  i_IActiveMapsConfig,
  i_IViewPortState,
  i_INavigationToPoint,
  i_IStatBarConfig,
  i_IMapLayerGPSMarkerConfig,
  i_IMapLayerGPSTrackConfig,
  i_IMapLayerNavToPointMarkerConfig,
  i_IMainFormBehaviourByGPSConfig,
  i_IMainGeoCoderConfig,
  i_IUsedMarksConfig,
  i_IKmlLayerConfig,
  i_IMiniMapLayerConfig,
  i_ICenterScaleConfig,
  i_IScaleLineConfig,
  i_ILastSelectionLayerConfig,
  i_ICalcLineLayerConfig,
  i_ISelectionRectLayerConfig,
  i_ISelectionPolygonLayerConfig,
  i_IMarkPolygonLayerConfig,
  i_IMarkPolyLineLayerConfig,
  i_IFillingMapLayerConfig,
  i_IDownloadUIConfig,
  i_MapLayerGridsConfig;

type
  IMainFormMainConfig = interface(IConfigDataElement)
    ['{5388E4B8-801A-445F-BF07-DE520A5AFA06}']
    // Фиксировать центр изменения масштаба под курсором мыши
    function GetZoomingAtMousePos: Boolean;
    procedure SetZoomingAtMousePos(AValue: Boolean);
    property ZoomingAtMousePos: Boolean read GetZoomingAtMousePos write SetZoomingAtMousePos;

    function GetShowMapName: Boolean;
    procedure SetShowMapName(AValue: Boolean);
    property ShowMapName: Boolean read GetShowMapName write SetShowMapName;

    //Инвертировать направление при зуме колесом мышки
    function GetMouseScrollInvert: Boolean;
    procedure SetMouseScrollInvert(AValue: Boolean);
    property MouseScrollInvert: Boolean read GetMouseScrollInvert write SetMouseScrollInvert;

    //Анимированный зум
    function GetAnimateZoom: Boolean;
    procedure SetAnimateZoom(AValue: Boolean);
    property AnimateZoom: Boolean read GetAnimateZoom write SetAnimateZoom;

    // Показывать хинты при нахождении мыши над меткой
    function GetShowHintOnMarks: Boolean;
    procedure SetShowHintOnMarks(AValue: Boolean);
    property ShowHintOnMarks: Boolean read GetShowHintOnMarks write SetShowHintOnMarks;
  end;

  IMainWindowToolbarsLock = interface(IConfigDataElement)
    ['{CA2386E9-10BE-4A7C-AE42-3E771BD390BA}']
    function GetIsLock: Boolean;
    procedure SetLock(AValue: Boolean);
  end;

  IMainFormLayersConfig = interface(IConfigDataElement)
    ['{02A323E8-25E4-43E5-BE24-AABDF9B331EC}']
    function GetMapLayerGridsConfig: IMapLayerGridsConfig;
    property MapLayerGridsConfig: IMapLayerGridsConfig read GetMapLayerGridsConfig;

    function GetStatBar: IStatBarConfig;
    property StatBar: IStatBarConfig read GetStatBar;

    function GetGPSMarker: IMapLayerGPSMarkerConfig;
    property GPSMarker: IMapLayerGPSMarkerConfig read GetGPSMarker;

    function GetGPSTrackConfig: IMapLayerGPSTrackConfig;
    property GPSTrackConfig: IMapLayerGPSTrackConfig read GetGPSTrackConfig;

    function GetNavToPointMarkerConfig: IMapLayerNavToPointMarkerConfig;
    property NavToPointMarkerConfig: IMapLayerNavToPointMarkerConfig read GetNavToPointMarkerConfig;

    function GetMarksShowConfig: IUsedMarksConfig;
    property MarksShowConfig: IUsedMarksConfig read GetMarksShowConfig;

    function GetKmlLayerConfig: IKmlLayerConfig;
    property KmlLayerConfig: IKmlLayerConfig read GetKmlLayerConfig;

    function GetMiniMapLayerConfig: IMiniMapLayerConfig;
    property MiniMapLayerConfig: IMiniMapLayerConfig read GetMiniMapLayerConfig;

    function GetCenterScaleConfig: ICenterScaleConfig;
    property CenterScaleConfig: ICenterScaleConfig read GetCenterScaleConfig;

    function GetScaleLineConfig: IScaleLineConfig;
    property ScaleLineConfig: IScaleLineConfig read GetScaleLineConfig;

    function GetLastSelectionLayerConfig: ILastSelectionLayerConfig;
    property LastSelectionLayerConfig: ILastSelectionLayerConfig read GetLastSelectionLayerConfig;

    function GetCalcLineLayerConfig: ICalcLineLayerConfig;
    property CalcLineLayerConfig: ICalcLineLayerConfig read GetCalcLineLayerConfig;

    function GetSelectionRectLayerConfig: ISelectionRectLayerConfig;
    property SelectionRectLayerConfig: ISelectionRectLayerConfig read GetSelectionRectLayerConfig;

    function GetSelectionPolygonLayerConfig: ISelectionPolygonLayerConfig;
    property SelectionPolygonLayerConfig: ISelectionPolygonLayerConfig read GetSelectionPolygonLayerConfig;

    function GetMarkPolygonLayerConfig: IMarkPolygonLayerConfig;
    property MarkPolygonLayerConfig: IMarkPolygonLayerConfig read GetMarkPolygonLayerConfig;

    function GetMarkPolyLineLayerConfig: IMarkPolyLineLayerConfig;
    property MarkPolyLineLayerConfig: IMarkPolyLineLayerConfig read GetMarkPolyLineLayerConfig;

    function GetFillingMapLayerConfig: IFillingMapLayerConfig;
    property FillingMapLayerConfig: IFillingMapLayerConfig read GetFillingMapLayerConfig;
  end;

  IMainFormConfig = interface(IConfigDataElement)
    ['{87184149-7B22-4184-A0BF-703C0C89B3AB}']
    function GetMainConfig: IMainFormMainConfig;
    property MainConfig: IMainFormMainConfig read GetMainConfig;

    function GetLayersConfig: IMainFormLayersConfig;
    property LayersConfig: IMainFormLayersConfig read GetLayersConfig;

    function GetToolbarsLock: IMainWindowToolbarsLock;
    property ToolbarsLock: IMainWindowToolbarsLock read GetToolbarsLock;

    function GetNavToPoint: INavigationToPoint;
    property NavToPoint: INavigationToPoint read GetNavToPoint;

    function GetGPSBehaviour: IMainFormBehaviourByGPSConfig;
    property GPSBehaviour: IMainFormBehaviourByGPSConfig read GetGPSBehaviour;

    function GetMainGeoCoderConfig: IMainGeoCoderConfig;
    property MainGeoCoderConfig: IMainGeoCoderConfig read GetMainGeoCoderConfig;

    function GetMainMapsConfig: IMainMapsConfig;
    property MainMapsConfig: IMainMapsConfig read GetMainMapsConfig;

    function GetViewPortState: IViewPortState;
    property ViewPortState: IViewPortState read GetViewPortState;

    function GetDownloadUIConfig: IDownloadUIConfig;
    property DownloadUIConfig: IDownloadUIConfig read GetDownloadUIConfig;
  end;

implementation

end.
