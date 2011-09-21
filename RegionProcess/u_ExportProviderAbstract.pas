unit u_ExportProviderAbstract;

interface

uses
  Classes,
  Controls,
  Forms,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  t_GeoTypes;

type
  TExportProviderAbstract = class
  protected
    FParent: TWinControl;
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
  public
    constructor Create(
      AParent: TWinControl;
      AMainMapsConfig: IMainMapsConfig;
      AFullMapsSet: IMapTypeSet;
      AGUIConfigList: IMapTypeGUIConfigList
    );
    function GetCaption: string; virtual; abstract;
    procedure InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint); virtual; abstract;
    procedure Show; virtual; abstract;
    procedure Hide; virtual; abstract;
    procedure RefreshTranslation; virtual; abstract;
    procedure StartProcess(APolygon: TArrayOfDoublePoint); virtual; abstract;
  end;

implementation

{ TExportProviderAbstract }

constructor TExportProviderAbstract.Create(
  AParent: TWinControl;
  AMainMapsConfig: IMainMapsConfig;
  AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList
);
begin
  FParent := AParent;
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
end;

end.
