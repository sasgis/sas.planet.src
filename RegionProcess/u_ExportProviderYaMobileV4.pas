unit u_ExportProviderYaMobileV4;

interface

uses
  Controls,
  Forms,
  t_GeoTypes,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  u_ExportProviderAbstract,
  fr_ExportYaMobileV4;

type
  TExportProviderYaMobileV4 = class(TExportProviderAbstract)
  private
    FFrame: TfrExportYaMobileV4;
    FCoordConverterFactory: ICoordConverterFactory;
  public
    constructor Create(
      AParent: TWinControl;
      AMainMapsConfig: IMainMapsConfig;
      AFullMapsSet: IMapTypeSet;
      AGUIConfigList: IMapTypeGUIConfigList;
      ACoordConverterFactory: ICoordConverterFactory
    );
    destructor Destroy; override;
    function GetCaption: string; override;
    procedure InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint); override;
    procedure Show; override;
    procedure Hide; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(APolygon: TArrayOfDoublePoint); override;
  end;


implementation

uses
  SysUtils,
  u_ThreadExportYaMobileV4,
  u_ResStrings,
  u_MapType;

{ TExportProviderYaMaps }

constructor TExportProviderYaMobileV4.Create(AParent: TWinControl;
  AMainMapsConfig: IMainMapsConfig; AFullMapsSet: IMapTypeSet;
  AGUIConfigList: IMapTypeGUIConfigList;
  ACoordConverterFactory: ICoordConverterFactory);
begin
  inherited Create(AParent, AMainMapsConfig, AFullMapsSet, AGUIConfigList);
  FCoordConverterFactory := ACoordConverterFactory;
end;

destructor TExportProviderYaMobileV4.Destroy;
begin
  FreeAndNil(FFrame);
  inherited;
end;

function TExportProviderYaMobileV4.GetCaption: string;
begin
  Result := SAS_STR_ExportYaMobileV4Caption;
end;

procedure TExportProviderYaMobileV4.InitFrame(Azoom: byte; APolygon: TArrayOfDoublePoint);
begin
  if FFrame = nil then begin
    FFrame := TfrExportYaMobileV4.Create(
      nil,
      FMainMapsConfig,
      FFullMapsSet,
      FGUIConfigList
    );
    FFrame.Visible := False;
    FFrame.Parent := FParent;
  end;
  FFrame.Init;
end;

procedure TExportProviderYaMobileV4.RefreshTranslation;
begin
  inherited;
  if FFrame <> nil then begin
    FFrame.RefreshTranslation;
  end;
end;

procedure TExportProviderYaMobileV4.Hide;
begin
  inherited;
  if FFrame <> nil then begin
    if FFrame.Visible then begin
      FFrame.Hide;
    end;
  end;
end;

procedure TExportProviderYaMobileV4.Show;
begin
  inherited;
  if FFrame <> nil then begin
    if not FFrame.Visible then begin
      FFrame.Show;
    end;
  end;
end;

procedure TExportProviderYaMobileV4.StartProcess(APolygon: TArrayOfDoublePoint);
var
  i:integer;
  path:string;
  Zoomarr:array [0..23] of boolean;
  typemaparr:array of TMapType;
  comprSat,comprMap:byte;
begin
  inherited;
  for i:=0 to 23 do ZoomArr[i]:= FFrame.chklstZooms.Checked[i];
  setlength(typemaparr,3);
  typemaparr[0]:=TMapType(FFrame.cbbSat.Items.Objects[FFrame.cbbSat.ItemIndex]);
  typemaparr[1]:=TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
  typemaparr[2]:=TMapType(FFrame.cbbHybr.Items.Objects[FFrame.cbbHybr.ItemIndex]);
  comprSat:=FFrame.seSatCompress.Value;
  comprMap:=FFrame.seMapCompress.Value;
  path:=IncludeTrailingPathDelimiter(FFrame.edtTargetPath.Text);
  TThreadExportYaMobileV4.Create(
    FCoordConverterFactory,
    path,
    APolygon,
    ZoomArr,
    typemaparr,
    true,
    comprSat,
    comprMap
  );
end;

end.
