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

unit fr_Export;

interface

uses
  Classes,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  i_LanguageManager,
  i_ProjectionSetFactory,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_BitmapTileSaveLoadFactory,
  i_BitmapPostProcessing,
  i_ImageResamplerFactory,
  i_ImageResamplerConfig,
  i_ArchiveReadWriteFactory,
  i_Bitmap32BufferFactory,
  i_TileStorageTypeList,
  i_TileFileNameGeneratorsList,
  i_RegionProcessProgressInfoInternalFactory,
  i_RegionProcessProvider,
  i_InterfaceListStatic,
  i_MapTypeListChangeable,
  fr_MapSelect,
  u_CommonFormAndFrameParents;

type
  TfrExport = class(TFrame)
    Label9: TLabel;
    CBFormat: TComboBox;
    pnlExport: TPanel;
    pnlTop: TPanel;
    procedure CBFormatChange(Sender: TObject);
  private
    FZoom: byte;
    FPolygon: IGeometryLonLatPolygon;
    FProviders: IInterfaceListStatic;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsList: IMapTypeListChangeable;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AImageResamplerFactoryList: IImageResamplerFactoryList;
      const ATileReprojectResamplerConfig: IImageResamplerConfig;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ATileNameGenerator: ITileFileNameGeneratorsList
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
  u_ExportProviderRMP,
  u_ExportProviderMBTiles,
  u_ExportProviderRMapsSQLite,
  u_ExportProviderOruxMapsSQLite,
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
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsList: IMapTypeListChangeable;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapPostProcessing: IBitmapPostProcessingChangeable;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AImageResamplerFactoryList: IImageResamplerFactoryList;
  const ATileReprojectResamplerConfig: IImageResamplerConfig;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ATileNameGenerator: ITileFileNameGeneratorsList
);
var
  VExportProvider: IRegionProcessProvider;
  VList: IInterfaceListSimple;
begin
  TP_Ignore(Self, 'CBFormat.Items');
  inherited Create(ALanguageManager);
  VList := TInterfaceListSimple.Create;

  VExportProvider :=
    TExportProviderIPhone.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AProjectionSetFactory,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      True
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderIPhone.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AProjectionSetFactory,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      False
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderGEKml.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderYaMobileV3.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderYaMobileV4.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderAUX.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderZip.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory,
      AArchiveReadWriteFactory,
      ATileStorageTypeList,
      ATileNameGenerator
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderTar.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory,
      AArchiveReadWriteFactory,
      ATileStorageTypeList,
      ATileNameGenerator
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderJNX.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory,
      ABitmapTileSaveLoadFactory,
      ABitmapPostProcessing
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderOgf2.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderCE.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AVectorGeometryProjectedFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderRMapsSQLite.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsList,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderOruxMapsSQLite.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsList,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderMBTiles.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsList,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  VExportProvider :=
    TExportProviderRMP.Create(
      AProgressFactory,
      ALanguageManager,
      AMapSelectFrameBuilder,
      AActiveMapsList,
      AVectorGeometryProjectedFactory,
      ABitmap32StaticFactory,
      ABitmapTileSaveLoadFactory,
      AImageResamplerFactoryList,
      ATileReprojectResamplerConfig,
      AProjectionSetFactory
    );
  VList.Add(VExportProvider);
  CBFormat.Items.Add(VExportProvider.GetCaption);

  CBFormat.ItemIndex := 0;
  CBFormat.DropDownCount := VList.Count;

  FProviders := VList.MakeStaticAndClear;
  Assert(CBFormat.Items.Count = FProviders.Count);
end;

destructor TfrExport.Destroy;
begin
  FProviders := nil;
  inherited;
end;

procedure TfrExport.CBFormatChange(Sender: TObject);
var
  VExportProvider: IRegionProcessProvider;
  i: Integer;
begin
  for i := 0 to FProviders.Count - 1 do begin
    VExportProvider := IRegionProcessProvider(FProviders.Items[i]);
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
  VProvider: IRegionProcessProvider;
  VIndex: Integer;
begin
  inherited;
  VIndex := CBFormat.ItemIndex;
  for i := 0 to FProviders.Count - 1 do begin
    VProvider := IRegionProcessProvider(FProviders.Items[i]);
    CBFormat.Items[i] := VProvider.GetCaption;
  end;
  CBFormat.ItemIndex := VIndex;
end;

procedure TfrExport.Show(
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
  CBFormatChange(nil);
end;

procedure TfrExport.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VExportProvider: IRegionProcessProvider;
begin
  VExportProvider := IRegionProcessProvider(FProviders.Items[CBFormat.ItemIndex]);
  if VExportProvider <> nil then begin
    VExportProvider.StartProcess(APolygon);
  end;
end;

function TfrExport.Validate(const APolygon: IGeometryLonLatPolygon): Boolean;
var
  VExportProvider: IRegionProcessProvider;
begin
  Result := False;
  VExportProvider := IRegionProcessProvider(FProviders.Items[CBFormat.ItemIndex]);
  if VExportProvider <> nil then begin
    Result := VExportProvider.Validate(APolygon);
  end;
end;

end.
