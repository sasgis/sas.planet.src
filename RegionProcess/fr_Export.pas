unit fr_Export;

interface

uses
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  i_NotifierTime,
  i_NotifierOperation,
  u_CommonFormAndFrameParents,
  i_LanguageManager,
  i_CoordConverterFactory,
  i_VectorItemLonLat,
  i_VectorItemsFactory,
  i_BitmapTileSaveLoadFactory,
  i_ArchiveReadWriteFactory,
  i_LocalCoordConverterFactorySimpe,
  i_Bitmap32StaticFactory,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_TileFileNameGeneratorsList,
  i_MapTypeGUIConfigList,
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
    FPolygon: ILonLatPolygon;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const ACoordConverterFactory: ICoordConverterFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const ATileNameGenerator: ITileFileNameGeneratorsList
    ); reintroduce;
    destructor Destroy; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(const APolygon: ILonLatPolygon);
    procedure Show(
      AParent: TWinControl;
      AZoom: byte;
      const APolygon: ILonLatPolygon
    );
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
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const ACoordConverterFactory: ICoordConverterFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
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
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      ACoordConverterFactory,
      ALocalConverterFactory,
      AProjectionFactory,
      AVectorItemsFactory,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      True
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderIPhone.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      ACoordConverterFactory,
      ALocalConverterFactory,
      AProjectionFactory,
      AVectorItemsFactory,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      False
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderGEKml.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItemsFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderYaMobileV3.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItemsFactory,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      ALocalConverterFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderYaMobileV4.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItemsFactory,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      ALocalConverterFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderAUX.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItemsFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderZip.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItemsFactory,
      AArchiveReadWriteFactory,
      ATileNameGenerator
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderTar.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItemsFactory,
      AArchiveReadWriteFactory,
      ATileNameGenerator
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderJNX.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItemsFactory,
      ABitmapTileSaveLoadFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

    VExportProvider :=
    TExportProviderOgf2.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItemsFactory,
      ABitmapFactory,
      ABitmapTileSaveLoadFactory,
      ALocalConverterFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderCE.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItemsFactory,
      ACoordConverterFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  VExportProvider :=
    TExportProviderRMapsSQLite.Create(
      ALanguageManager,
      AAppClosingNotifier,
      ATimerNoifier,
      AMainMapsConfig,
      AFullMapsSet,
      AGUIConfigList,
      AProjectionFactory,
      AVectorItemsFactory
    );
  CBFormat.Items.AddObject(VExportProvider.GetCaption, VExportProvider);

  CBFormat.ItemIndex := 0;
end;

destructor TfrExport.Destroy;
var
  i: Integer;
begin
  for i := 0 to CBFormat.Items.Count - 1 do begin
    CBFormat.Items.Objects[i].Free;
    CBFormat.Items.Objects[i] := nil;
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
  const APolygon: ILonLatPolygon);
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

procedure TfrExport.StartProcess(const APolygon: ILonLatPolygon);
var
  VExportProvider: TExportProviderAbstract;
begin
  VExportProvider := TExportProviderAbstract(CBFormat.Items.Objects[CBFormat.ItemIndex]);
  if VExportProvider <> nil then begin
    VExportProvider.StartProcess(APolygon);
  end;
end;

end.
