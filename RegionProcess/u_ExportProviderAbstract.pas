unit u_ExportProviderAbstract;

interface

uses
  Controls,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  t_GeoTypes;

type
  TExportProviderAbstract = class
  private
    FParent: TWinControl;
    FLanguageManager: ILanguageManager;
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
  protected
    property Parent: TWinControl read FParent;
    property LanguageManager: ILanguageManager read FLanguageManager;
    property MainMapsConfig: IMainMapsConfig read FMainMapsConfig;
    property FullMapsSet: IMapTypeSet read FFullMapsSet;
    property GUIConfigList: IMapTypeGUIConfigList read FGUIConfigList;
  public
    constructor Create(
      AParent: TWinControl;
      ALanguageManager: ILanguageManager;
      AMainMapsConfig: IMainMapsConfig;
      AFullMapsSet: IMapTypeSet;
      AGUIConfigList: IMapTypeGUIConfigList
    );
    function GetCaption: string; virtual; abstract;
    procedure InitFrame(Azoom: byte; APolygon: ILonLatPolygon); virtual; abstract;
    procedure Show; virtual; abstract;
    procedure Hide; virtual; abstract;
    procedure RefreshTranslation; virtual; abstract;
    procedure StartProcess(APolygon: ILonLatPolygon); virtual; abstract;
  end;

implementation

{ TExportProviderAbstract }

constructor TExportProviderAbstract.Create(
  AParent: TWinControl;
  ALanguageManager: ILanguageManager;
  AMainMapsConfig: IMainMapsConfig;
  AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList
);
begin
  FParent := AParent;
  FLanguageManager := ALanguageManager;
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
end;

end.
