unit i_MainFormConfig;

interface

uses
  i_IConfigDataElement,
  i_INavigationToPoint,
  i_MapLayerGridsConfig;

type
  IMainFormMainConfig = interface(IConfigDataElement)
    ['{5388E4B8-801A-445F-BF07-DE520A5AFA06}']
    // Фиксировать центр изменения масштаба под курсором мыши
    function GetZoomingAtMousePos: Boolean;
    procedure SetZoomingAtMousePos(AValue: Boolean);
    property ZoomingAtMousePos: Boolean read GetZoomingAtMousePos write SetZoomingAtMousePos;
  end;

  IMainWindowToolbarsLock = interface(IConfigDataElement)
    ['{CA2386E9-10BE-4A7C-AE42-3E771BD390BA}']
    function GetIsLock: Boolean;
    procedure SetLock(AValue: Boolean);
  end;

  IMainFormConfig = interface(IConfigDataElement)
    ['{87184149-7B22-4184-A0BF-703C0C89B3AB}']
    function GetMainConfig: IMainFormMainConfig;
    property MainConfig: IMainFormMainConfig read GetMainConfig;

    function GetToolbarsLock: IMainWindowToolbarsLock;
    property ToolbarsLock: IMainWindowToolbarsLock read GetToolbarsLock;

    function GetMapLayerGridsConfig: IMapLayerGridsConfig;
    property MapLayerGridsConfig: IMapLayerGridsConfig read GetMapLayerGridsConfig;

    function GetNavToPoint: INavigationToPoint;
    property NavToPoint: INavigationToPoint read GetNavToPoint;
  end;

implementation

end.
