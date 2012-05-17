unit u_ExportProviderAbstract;

interface

uses
  Controls,
  i_LanguageManager,
  i_VectorItemLonLat,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_CommonFormAndFrameParents;

type
  TExportProviderAbstract = class
  private
    FFrame: TFrame;
    FParent: TWinControl;
    FLanguageManager: ILanguageManager;
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
  protected
    procedure SetFrame(AValue: TFrame); virtual;
    property LanguageManager: ILanguageManager read FLanguageManager;
    property MainMapsConfig: IMainMapsConfig read FMainMapsConfig;
    property FullMapsSet: IMapTypeSet read FFullMapsSet;
    property GUIConfigList: IMapTypeGUIConfigList read FGUIConfigList;
  public
    constructor Create(
      AParent: TWinControl;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList
    );
    destructor Destroy; override;
    function GetCaption: string; virtual; abstract;
    procedure InitFrame(
      Azoom: byte;
      const APolygon: ILonLatPolygon
    ); virtual; abstract;
    procedure Show;
    procedure Hide;
    procedure RefreshTranslation;
    procedure StartProcess(const APolygon: ILonLatPolygon); virtual; abstract;
  end;

implementation

uses
  SysUtils;

{ TExportProviderAbstract }

constructor TExportProviderAbstract.Create(
  AParent: TWinControl;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList
);
begin
  inherited Create;
  FParent := AParent;
  FLanguageManager := ALanguageManager;
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
end;

destructor TExportProviderAbstract.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

procedure TExportProviderAbstract.Hide;
begin
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TExportProviderAbstract.RefreshTranslation;
begin
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TExportProviderAbstract.SetFrame(AValue: TFrame);
var
  VWasVisible: Boolean;
begin
  VWasVisible := False;
  if FFrame <> nil then begin
    VWasVisible := FFrame.Visible;
    FreeAndNil(FFrame);
  end;
  FFrame := AValue;
  if FFrame <> nil then begin
    FFrame.Visible := VWasVisible;
    FFrame.Parent := FParent;
  end;
end;

procedure TExportProviderAbstract.Show;
begin
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

end.
