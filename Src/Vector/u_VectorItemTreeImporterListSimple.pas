{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_VectorItemTreeImporterListSimple;

interface

uses
  i_Notifier,
  i_ArchiveReadWriteFactory,
  i_VectorItemTreeImporter,
  i_VectorItemTreeImporterList,
  i_GeometryLonLatFactory,
  i_VectorDataFactory,
  i_VectorDataLoader,
  i_VectorItemSubsetBuilder,
  i_CoordToStringConverter,
  i_InternalPerformanceCounter,
  i_MarkPicture,
  i_HashFunction,
  i_AppearanceOfMarkFactory,
  i_MarkFactory,
  i_MarkCategoryFactory,
  i_MarkSystemImplFactory,
  i_PathConfig,
  i_ContentTypeManager,
  i_ProjConverter,
  u_BaseInterfacedObject;

type
  TVectorItemTreeImporterListSimple = class(TBaseInterfacedObject, IVectorItemTreeImporterListChangeable)
  private
    FNotifierFake: INotifier;
    FList: IVectorItemTreeImporterListStatic;
  private
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
    function GetStatic: IVectorItemTreeImporterListStatic;
  public
    constructor Create(
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AMarkPictureList: IMarkPictureList;
      const AHashFunction: IHashFunction;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkFactory: IMarkFactory;
      const ACategoryFactory: IMarkCategoryFactory;
      const AMarkSystemImplFactoryListStatic: IMarkSystemImplFactoryListStatic;
      const AMediaDataPath: IPathConfig;
      const AContentTypeManager: IContentTypeManager;
      const AProjConverterFactory: IProjConverterFactory;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  c_MarkSystem,
  i_InterfaceListSimple,
  u_Notifier,
  u_InterfaceListSimple,
  u_VectorItemTreeImporterList,
  u_VectorItemTreeImporterByVectorLoader,
  u_VectorItemTreeImporterJpegWithExif,
  u_VectorItemTreeMarksDb,
  u_VectorItemTreeImporterXML,
  u_VectorItemTreeImporterKMZ,
  u_VectorDataLoaderWithCounter,
  u_GeoJsonParser,
  u_PLTSimpleParser,
  u_SlsParser,
  u_HlgParser,
  u_MpSimpleParser,
  u_CsvParser;


{ TVectorItemTreeImporterListSimple }

constructor TVectorItemTreeImporterListSimple.Create(
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AMarkPictureList: IMarkPictureList;
  const AHashFunction: IHashFunction;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const ACategoryFactory: IMarkCategoryFactory;
  const AMarkSystemImplFactoryListStatic: IMarkSystemImplFactoryListStatic;
  const AMediaDataPath: IPathConfig;
  const AContentTypeManager: IContentTypeManager;
  const AProjConverterFactory: IProjConverterFactory;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VList: IInterfaceListSimple;
  VImporter: IVectorItemTreeImporter;
  VItem: IVectorItemTreeImporterListItem;
  VLoader: IVectorDataLoader;
begin
  inherited Create;

  FNotifierFake := TNotifierFaked.Create;
  VList := TInterfaceListSimple.Create;

  // KML
  VImporter :=
    TVectorItemTreeImporterXML.Create(
      False,
      AMarkPictureList,
      AAppearanceOfMarkFactory,
      AVectorDataItemMainInfoFactory,
      AVectorGeometryLonLatFactory,
      AVectorDataFactory,
      AVectorItemSubsetBuilderFactory
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'kml',
      'Google KML files'
    );
  VList.Add(VItem);

  // KMZ
  VImporter :=
    TVectorItemTreeImporterKMZ.Create(
      AMarkPictureList,
      AAppearanceOfMarkFactory,
      AArchiveReadWriteFactory.Zip.ReaderFactory,
      AVectorDataItemMainInfoFactory,
      AVectorGeometryLonLatFactory,
      AVectorDataFactory,
      AVectorItemSubsetBuilderFactory
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'kmz',
      'Google KMZ files'
    );
  VList.Add(VItem);

  // GPX
  VImporter :=
    TVectorItemTreeImporterXML.Create(
      False,
      AMarkPictureList,
      AAppearanceOfMarkFactory,
      AVectorDataItemMainInfoFactory,
      AVectorGeometryLonLatFactory,
      AVectorDataFactory,
      AVectorItemSubsetBuilderFactory
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'gpx',
      'GPS Exchange files'
    );
  VList.Add(VItem);

  // GeoJSON
  VLoader :=
    TGeoJsonParser.Create(
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AVectorGeometryLonLatFactory,
      AProjConverterFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('GeoJSON')
    );
  VImporter :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'json',
      'GeoJSON files'
    );
  VList.Add(VItem);

  // OziExplorer PLT (track file)
  VLoader :=
    TPLTSimpleParser.Create(
      AVectorGeometryLonLatFactory,
      AVectorDataFactory,
      AVectorItemSubsetBuilderFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('PLT')
    );
  VImporter :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'plt',
      'OziExplorer Track File'
    );
  VList.Add(VItem);

  // CSV
  VLoader :=
    TCsvParser.Create(
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AVectorGeometryLonLatFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('CSV')
    );
  VImporter :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'csv',
      'Universal CSV files'
    );
  VList.Add(VItem);

  // Polish MP format
  VLoader :=
    TMpSimpleParser.Create(
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AVectorGeometryLonLatFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('MP')
    );
  VImporter :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'mp',
      'Single Polygon from Polish MP file'
    );
  VList.Add(VItem);

  // JPEG with EXIF
  VImporter :=
    TVectorItemTreeImporterJpegWithExif.Create(
      AHashFunction,
      AVectorGeometryLonLatFactory,
      AVectorDataItemMainInfoFactory,
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AAppearanceOfMarkFactory,
      AMediaDataPath,
      ACoordToStringConverter,
      AContentTypeManager
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'jpg',
      'JPEG Image with GPS Exif info'
    );
  VList.Add(VItem);

  // HLG (selection polygon)
  VLoader :=
    THlgParser.Create(
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AVectorGeometryLonLatFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('HLG')
    );
  VImporter :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'hlg',
      'SAS.Planet Selection Polygon'
    );
  VList.Add(VItem);

  // SLS (download session)
  VLoader :=
    TSlsParser.Create(
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AVectorGeometryLonLatFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('SLS')
    );
  VImporter :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'sls',
      'SAS.Planet Download Session Polygon'
    );
  VList.Add(VItem);

  // SML marks db
  VImporter :=
    TVectorItemTreeMarksDb.Create(
      cSMLMarksDbGUID,
      AMarkFactory,
      ACategoryFactory,
      AAppearanceOfMarkFactory,
      AMarkSystemImplFactoryListStatic
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'sml',
      'SAS.Planet Marks Database in XML format'
    );
  VList.Add(VItem);

  // SQLite3 marks db
  VImporter :=
    TVectorItemTreeMarksDb.Create(
      cORMSQLiteMarksDbGUID,
      AMarkFactory,
      ACategoryFactory,
      AAppearanceOfMarkFactory,
      AMarkSystemImplFactoryListStatic
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'db3',
      'SAS.Planet Marks Database in SQLite3 format'
    );
  VList.Add(VItem);

  FList := TVectorItemTreeImporterListStatic.Create(VList.MakeStaticAndClear);
end;

function TVectorItemTreeImporterListSimple.GetAfterChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TVectorItemTreeImporterListSimple.GetBeforeChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TVectorItemTreeImporterListSimple.GetChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TVectorItemTreeImporterListSimple.GetStatic: IVectorItemTreeImporterListStatic;
begin
  Result := FList;
end;

end.
