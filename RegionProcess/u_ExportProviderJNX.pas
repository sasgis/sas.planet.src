unit u_ExportProviderJNX;

interface

uses
  Controls,
  i_VectorItemLonLat,
  i_CoordConverterFactory,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  u_ExportProviderAbstract,
  fr_ExportToJNX;

type
  TExportProviderJNX = class(TExportProviderAbstract)
  private
    FFrame: TfrExportToJNX;
    FCoordConverterFactory: ICoordConverterFactory;
  public
    constructor Create(
      AParent: TWinControl;
      ALanguageManager: ILanguageManager;
      AMainMapsConfig: IMainMapsConfig;
      AFullMapsSet: IMapTypeSet;
      AGUIConfigList: IMapTypeGUIConfigList;
      ACoordConverterFactory: ICoordConverterFactory
    );
    destructor Destroy; override;
    function GetCaption: string; override;
    procedure InitFrame(Azoom: byte; APolygon: ILonLatPolygon); override;
    procedure Show; override;
    procedure Hide; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(APolygon: ILonLatPolygon); override;
  end;

implementation

uses
  SysUtils,
  u_ThreadExportToJNX,
  u_ResStrings,
  u_MapType;

{ TExportProviderJNX }

constructor TExportProviderJNX.Create(
  AParent: TWinControl;
  ALanguageManager: ILanguageManager;
  AMainMapsConfig: IMainMapsConfig;
  AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList;
  ACoordConverterFactory: ICoordConverterFactory
);
begin
  inherited Create(AParent, ALanguageManager, AMainMapsConfig, AFullMapsSet, AGUIConfigList);
  FCoordConverterFactory := ACoordConverterFactory;
end;

destructor TExportProviderJNX.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TExportProviderJNX.GetCaption: string;
begin
  Result := SAS_STR_ExportJNXPackCaption;
end;

procedure TExportProviderJNX.InitFrame(Azoom: byte; APolygon: ILonLatPolygon);
begin
  if FFrame = nil then begin
    FFrame := TfrExportToJNX.CreateForFileType(
      nil,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'JNX |*.jnx',
      'jnx'
    );
    FFrame.Visible := False;
    FFrame.Parent := Self.Parent;
  end;
  FFrame.Init;
end;

procedure TExportProviderJNX.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TExportProviderJNX.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TExportProviderJNX.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TExportProviderJNX.StartProcess(APolygon: ILonLatPolygon);
var
  i:integer;
  path:string;
  Zoomarr:array [0..23] of boolean;
  VMapType: TMapType;
  VProductName : string;
  VMapName : string;
  VJNXVersion : integer;
  VZorder : integer;
  VProductID : integer;
begin
  inherited;
  for i:=0 to 23 do begin
    ZoomArr[i]:= FFrame.chklstZooms.Checked[i];
  end;
  VMapType:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  path:=FFrame.edtTargetFile.Text;
  VProductName := FFrame.EProductName.Text;
  VMapName := FFrame.EmapName.Text;
  if FFrame.v3.checked then begin
      VJNXVersion := 3;
      VZorder := 30;
  end else begin
      VJNXVersion := 4;
      VZorder := FFrame.EZorder.Value;
  end;
  VProductID := FFrame.EProductID.ItemIndex + 1;
  if VProductID = 1 then VProductID := 0;




  TThreadExportToJNX.Create(
    FCoordConverterFactory,
    path,
    APolygon.Item[0],
    Zoomarr,
    VMapType,
    VProductName,
    VMapName,
    VJNXVersion,
    VZorder,
    VProductID
  );
end;

end.
