{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit i_MainFormConfig;

interface

uses
  GR32,
  i_ConfigDataElement,
  i_ActiveMapsConfig,
  i_ViewPortState,
  i_NavigationToPoint,
  i_StatBarConfig,
  i_MapLayerGPSMarkerConfig,
  i_MapLayerGPSTrackConfig,
  i_MapLayerNavToPointMarkerConfig,
  i_MainFormBehaviourByGPSConfig,
  i_MainGeoCoderConfig,
  i_KeyMovingConfig,
  i_MarksLayerConfig,
  i_KmlLayerConfig,
  i_MapMovingConfig,
  i_MapZoomingConfig,
  i_MiniMapLayerConfig,
  i_CenterScaleConfig,
  i_ScaleLineConfig,
  i_LastSelectionLayerConfig,
  i_CalcLineLayerConfig,
  i_SelectionRectLayerConfig,
  i_SelectionPolygonLayerConfig,
  i_SelectionPolylineLayerConfig,
  i_MarkPolygonLayerConfig,
  i_MarkPolyLineLayerConfig,
  i_FillingMapLayerConfig,
  i_DownloadUIConfig,
  i_GotoLayerConfig,
  I_LastSearchResultConfig,
  i_MapLayerGridsConfig;

type
  IMainFormMainConfig = interface(IConfigDataElement)
    ['{5388E4B8-801A-445F-BF07-DE520A5AFA06}']
    function GetShowMapName: Boolean;
    procedure SetShowMapName(AValue: Boolean);
    property ShowMapName: Boolean read GetShowMapName write SetShowMapName;

    //Инвертировать направление при зуме колесом мышки
    function GetMouseScrollInvert: Boolean;
    procedure SetMouseScrollInvert(AValue: Boolean);
    property MouseScrollInvert: Boolean read GetMouseScrollInvert write SetMouseScrollInvert;

    // Показывать хинты при нахождении мыши над меткой
    function GetShowHintOnMarks: Boolean;
    procedure SetShowHintOnMarks(AValue: Boolean);
    property ShowHintOnMarks: Boolean read GetShowHintOnMarks write SetShowHintOnMarks;

    function GetRuller: TCustomBitmap32;
    property Ruller: TCustomBitmap32 read GetRuller;

    function GetTumbler: TCustomBitmap32;
    property Tumbler: TCustomBitmap32 read GetTumbler;
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

    function GetMarksLayerConfig: IMarksLayerConfig;
    property MarksLayerConfig: IMarksLayerConfig read GetMarksLayerConfig;

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

    function GetSelectionPolylineLayerConfig: ISelectionPolylineLayerConfig;
    property SelectionPolylineLayerConfig: ISelectionPolylineLayerConfig read GetSelectionPolylineLayerConfig;

    function GetMarkPolygonLayerConfig: IMarkPolygonLayerConfig;
    property MarkPolygonLayerConfig: IMarkPolygonLayerConfig read GetMarkPolygonLayerConfig;

    function GetMarkPolyLineLayerConfig: IMarkPolyLineLayerConfig;
    property MarkPolyLineLayerConfig: IMarkPolyLineLayerConfig read GetMarkPolyLineLayerConfig;

    function GetFillingMapLayerConfig: IFillingMapLayerConfig;
    property FillingMapLayerConfig: IFillingMapLayerConfig read GetFillingMapLayerConfig;

    function GetGotoLayerConfig: IGotoLayerConfig;
    property GotoLayerConfig: IGotoLayerConfig read GetGotoLayerConfig;
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

    function GetKeyMovingConfig: IKeyMovingConfig;
    property KeyMovingConfig: IKeyMovingConfig read GetKeyMovingConfig;

    function GetMapZoomingConfig: IMapZoomingConfig;
    property MapZoomingConfig: IMapZoomingConfig read GetMapZoomingConfig;

    function GetMapMovingConfig: IMapMovingConfig;
    property MapMovingConfig: IMapMovingConfig read GetMapMovingConfig;

    function GetLastSearchResultConfig: ILastSearchResultConfig;
    property LastSearchResultConfig: ILastSearchResultConfig read GetLastSearchResultConfig;
 end;

implementation

end.
