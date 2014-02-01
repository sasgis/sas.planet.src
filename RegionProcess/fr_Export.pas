unit fr_Export;

interface

uses
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  u_CommonFormAndFrameParents,
  i_LanguageManager,
  i_CoordConverterFactory,
  i_GeometryLonLat,
  i_VectorGeometryProjectedFactory,
  i_BitmapTileSaveLoadFactory,
  i_ArchiveReadWriteFactory,
  i_LocalCoordConverterFactorySimpe,
  i_Bitmap32StaticFactory,
  i_MapTypeSet,
  i_MapTypeListBuilder,
  i_ActiveMapsConfig,
  i_TileFileNameGeneratorsList,
  i_MapTypeGUIConfigList,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract;

type
  TfrExport = class(TFrame)
    Label9: TLabel;
    CBFormat: TComboBox;
    pnlExport: TPanel;
    pnlTop: TPanel;
    procedure CBFormatChange(Sender: TObject);
  private
    FZoom: byte;
    FPolygon: IGeometryLonLatMultiPolygon;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const ATileNameGenerator: ITileFileNameGeneratorsList
    ); reintroduce;
    destructor Destroy; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
    procedure Show(
      AParent: TWinControl;
      AZoom: byte;
      const APolygon: IGeometryLonLatMultiPolygon
    );
    function Validate: Boolean;
  end;

implementation

uses
  gnugettext,
  u_ExportProviderRMapsSQLite,
  u_ExportProviderYaMobileV3,
  u_ExportProviderYaMobileV4,
  u_ExportProviderGEKml,
  u_ExportProviderIPhone,
  u_ExportProviderAUX,
  u_ExportProviderZip,
  u_ExportProviderTar,
  u_ExportProviderJNX,
  u_ExportProviderOgf2,
  u_ExportProviderCE;

{$R *.dfm}

{ TfrExport }

constructor TfrExport.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const ATileNameGenerator: ITileFileNameGeneratorsList
);
var
  VExportProvider: TExportProviderAbstract;
begin
  TP_Ignore(Self, 'CBFormat.Items');
  inherited Create(ALanguageManager);
  VExportProvider :=
    TExportProviderIPhone.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      ACoordConverterFactory,
      ALocalConverterFactory,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      True
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderIPhone.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      ACoordConverterFactory,
      ALocalConverterFactory,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      False
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderGEKml.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorGeometryProjectedFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderYaMobileV3.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      ALocalConverterFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderYaMobileV4.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      ALocalConverterFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderAUX.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorGeometryProjectedFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderZip.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      AArchiveReadWriteFactory,
      ATileNameGenerator
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderTar.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      AArchiveReadWriteFactory,
      ATileNameGenerator
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderJNX.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AMapTypeListBuilderFactory,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      ABitmapTileSaveLoadFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderOgf2.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      ALocalConverterFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderCE.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderRMapsSQLite.Create(
      AProgressFactory,
      ALanguageManager,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorGeometryProjectedFactory,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      ACoordConverterFactory,
      ALocalConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  CBFormat.ItemIndex := 0;
end;

destructor TfrExport.Destroy;
var
  i: Integer;
begin
  if Assigned(CBFormat) then begin
    for i := 0 to CBFormat.Items.Count - 1 do begin
      CBFormat.Items.Objects[i].Free;
      CBFormat.Items.Objects[i] := nil;
    end;
  end;
  inherited;
end;

procedure TfrExport.CBFormatChange(Sender: TObject);
var
  VExportProvider: TExportProviderAbstract;
  i: Integer;
begin
  for i := 0 to CBFormat.Items.Count - 1 do begin
    VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[i]);
    if VExportProvider <> nil then begin
      if i = CBFormat.ItemIndex then begin
        VExportProvider.Show(pnlExport, FZoom, FPolygon);
      end else begin
        VExportProvider.Hide;
      end;
    end;
  end;
end;

procedure TfrExport.RefreshTranslation;
var
  i: Integer;
  VProvider: TExportProviderAbstract;
  VIndex: Integer;
begin
  inherited;
  VIndex := CBFormat.ItemIndex;
  for i := 0 to CBFormat.Items.Count - 1 do begin
    VProvider := TExportProviderAbstract(CBFormat.Items.Objects[i]);
    CBFormat.Items[i] := VProvider.GetCaption;
  end;
  CBFormat.ItemIndex := VIndex;
end;

procedure TfrExport.Show(AParent: TWinControl; AZoom: byte;
  const APolygon: IGeometryLonLatMultiPolygon);
var
  i:integer;
  VExportProvider: TExportProviderAbstract;
begin
  Parent := AParent;
  FZoom := AZoom;
  FPolygon := APolygon;
  for i := 0 to CBFormat.Items.Count - 1 do begin
    VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[i]);
    if VExportProvider <> nil then begin
      VExportProvider.Show(pnlExport, AZoom, APolygon);
    end;
  end;
  CBFormatChange(nil);
end;

procedure TfrExport.StartProcess(const APolygon: IGeometryLonLatMultiPolygon);
var
  VExportProvider: TExportProviderAbstract;
begin
  VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[CBFormat.ItemIndex]);
  if VExportProvider <> nil then begin
    VExportProvider.StartProcess(APolygon);
  end;
end;

function TfrExport.Validate: Boolean;
var
  VExportProvider: TExportProviderAbstract;
begin
  Result := False;
  VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[CBFormat.ItemIndex]);
  if VExportProvider <> nil then begin
    Result := VExportProvider.Validate;
  end;
end;

end.
