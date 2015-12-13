{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit fr_Combine;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  i_LanguageManager,
  i_ProjectionSetList,
  i_ProjectionSetChangeable,
  i_GeometryLonLat,
  i_UseTilePrevZoomConfig,
  i_BitmapTileSaveLoadFactory,
  i_ArchiveReadWriteFactory,
  i_BitmapPostProcessing,
  i_Bitmap32BufferFactory,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarkSystem,
  i_MapType,
  i_FillingMapLayerConfig,
  i_FillingMapPolygon,
  i_MapLayerGridsConfig,
  i_CoordToStringConverter,
  i_MapCalibration,
  i_MapTypeListChangeable,
  i_GeometryProjectedFactory,
  i_GeometryProjectedProvider,
  i_VectorItemSubsetBuilder,
  i_GlobalViewMainConfig,
  i_RegionProcessProgressInfoInternalFactory,
  i_RegionProcessProvider,
  i_InterfaceListStatic,
  fr_MapSelect,
  u_CommonFormAndFrameParents;

type
  TfrCombine = class(TFrame)
    pnlTop: TPanel;
    pnlOutputFormat: TPanel;
    lblOutputFormat: TLabel;
    cbbOutputFormat: TComboBox;
    pnlExport: TPanel;
    procedure cbbOutputFormatChange(Sender: TObject);
  private
    FZoom: byte;
    FPolygon: IGeometryLonLatPolygon;
    FProviders: IInterfaceListStatic;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsSet: IMapTypeListChangeable;
      const AViewConfig: IGlobalViewMainConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
      const AProjectionSet: IProjectionSetChangeable;
      const AProjectionSetList: IProjectionSetList;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectedGeometryProvider: IGeometryProjectedProvider;
      const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMarksDrawConfig: IMarksDrawConfig;
      const AMarksDB: IMarkSystem;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const AFillingMapConfig: IFillingMapLayerConfig;
      const AFillingMapType: IMapTypeChangeable;
      const AFillingMapPolygon: IFillingMapPolygon;
      const AGridsConfig: IMapLayerGridsConfig;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AMapCalibrationList: IMapCalibrationList
    ); reintroduce;
    destructor Destroy; override;
    procedure RefreshTranslation; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon);
    procedure Show(
      AParent: TWinControl;
      AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate(const APolygon: IGeometryLonLatPolygon): Boolean;
  end;

implementation

uses
  gnugettext,
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_ProviderMapCombineBMP,
  u_ProviderMapCombineJPG,
  u_ProviderMapCombinePNG,
  u_ProviderMapCombineKMZ,
  u_ProviderMapCombineECW,
  u_ProviderMapCombineRAW,
  u_ProviderMapCombineJP2;

{$R *.dfm}

{ TfrCombine }

constructor TfrCombine.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsSet: IMapTypeListChangeable;
  const AViewConfig: IGlobalViewMainConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
  const AProjectionSet: IProjectionSetChangeable;
  const AProjectionSetList: IProjectionSetList;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectedGeometryProvider: IGeometryProjectedProvider;
  const AVectorSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMarksDrawConfig: IMarksDrawConfig;
  const AMarksDB: IMarkSystem;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const AFillingMapConfig: IFillingMapLayerConfig;
  const AFillingMapType: IMapTypeChangeable;
  const AFillingMapPolygon: IFillingMapPolygon;
  const AGridsConfig: IMapLayerGridsConfig;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AMapCalibrationList: IMapCalibrationList
);
var
  VExportProvider: IRegionProcessProvider;
  VList: IInterfaceListSimple;
begin
  TP_Ignore(Self, 'cbbOutputFormat.Items');
  inherited Create(ALanguageManager);
  VList := TInterfaceListSimple.Create;

  VExportProvider :=
    TProviderMapCombineJPG.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);
  cbbOutputFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TProviderMapCombinePNG.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);
  cbbOutputFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TProviderMapCombineBMP.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);
  cbbOutputFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TProviderMapCombineECW.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);
  cbbOutputFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TProviderMapCombineJP2.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList,
      False // Lossless
    );
  VList.Add(VExportProvider);
  cbbOutputFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TProviderMapCombineJP2.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList,
      True // Lossless
    );
  VList.Add(VExportProvider);
  cbbOutputFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TProviderMapCombineKMZ.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      ABitmapTileSaveLoadFactory,
      AArchiveReadWriteFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);
  cbbOutputFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TProviderMapCombineRAW.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsSet,
      AViewConfig,
      AUseTilePrevZoomConfig,
      AProjectionSet,
      AProjectionSetList,
      AVectorGeometryProjectedFactory,
      AProjectedGeometryProvider,
      AVectorSubsetBuilderFactory,
      AMarksShowConfig,
      AMarksDrawConfig,
      AMarksDB,
      ABitmapFactory,
      ABitmapPostProcessing,
      AFillingMapConfig,
      AFillingMapType,
      AFillingMapPolygon,
      AGridsConfig,
      ACoordToStringConverter,
      AMapCalibrationList
    );
  VList.Add(VExportProvider);
  cbbOutputFormat.Items.Add(VExportProvider.GetCaption);

  cbbOutputFormat.ItemIndex := 0;
  FProviders := VList.MakeStaticAndClear;
  Assert(cbbOutputFormat.Items.Count = FProviders.Count);
end;

destructor TfrCombine.Destroy;
begin
  FProviders := nil;
  inherited;
end;

procedure TfrCombine.cbbOutputFormatChange(Sender: TObject);
var
  VExportProvider: IRegionProcessProvider;
  i: Integer;
begin
  for i := 0 to FProviders.Count - 1 do begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[i]);
    if VExportProvider <> nil then begin
      if i = cbbOutputFormat.ItemIndex then begin
        VExportProvider.Show(pnlExport, FZoom, FPolygon);
      end else begin
        VExportProvider.Hide;
      end;
    end;
  end;
end;

procedure TfrCombine.RefreshTranslation;
var
  i: Integer;
  VProvider: IRegionProcessProvider;
  VIndex: Integer;
begin
  inherited;
  VIndex := cbbOutputFormat.ItemIndex;
  for i := 0 to FProviders.Count - 1 do begin
    VProvider := IRegionProcessProvider(FProviders.Items[i]);
    cbbOutputFormat.Items[i] := VProvider.GetCaption;
  end;
  cbbOutputFormat.ItemIndex := VIndex;
end;

procedure TfrCombine.Show(
  AParent: TWinControl;
  AZoom: byte;
  const APolygon: IGeometryLonLatPolygon
);
var
  i: integer;
  VExportProvider: IRegionProcessProvider;
begin
  Parent := AParent;
  FZoom := AZoom;
  FPolygon := APolygon;
  for i := 0 to FProviders.Count - 1 do begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[i]);
    if VExportProvider <> nil then begin
      VExportProvider.Show(pnlExport, AZoom, APolygon);
    end;
  end;
  cbbOutputFormatChange(nil);
end;

procedure TfrCombine.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VExportProvider: IRegionProcessProvider;
begin
  VExportProvider := IRegionProcessProvider(FProviders.Items[cbbOutputFormat.ItemIndex]);
  if VExportProvider <> nil then begin
    VExportProvider.StartProcess(APolygon);
  end;
end;

function TfrCombine.Validate(const APolygon: IGeometryLonLatPolygon): Boolean;
var
  VExportProvider: IRegionProcessProvider;
begin
  Result := False;
  VExportProvider := IRegionProcessProvider(FProviders.Items[cbbOutputFormat.ItemIndex]);
  if VExportProvider <> nil then begin
    Result := VExportProvider.Validate(APolygon);
  end;
end;

end.
