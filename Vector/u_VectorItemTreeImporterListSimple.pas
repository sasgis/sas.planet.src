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
  i_ValueToStringConverter,
  i_InternalPerformanceCounter,
  i_MarkPicture,
  i_HashFunction,
  i_AppearanceOfMarkFactory,
  i_MarkFactory,
  i_HtmlToHintTextConverter,
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
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AMarkPictureList: IMarkPictureList;
      const AHashFunction: IHashFunction;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkFactory: IMarkFactory;
      const AHintConverter: IHtmlToHintTextConverter;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  i_InterfaceListSimple,
  u_Notifier,
  u_InterfaceListSimple,
  u_VectorItemTreeImporterList,
  u_VectorItemTreeImporterByVectorLoader,
  u_VectorItemTreeImporterJpegWithExif,
  u_VectorItemTreeImporterSmlMarks,
  u_VectorDataLoaderWithCounter,
  u_XmlInfoSimpleParser,
  u_KmzInfoSimpleParser,
  u_PLTSimpleParser,
  u_SlsParser,
  u_HlgParser,
  u_MpSimpleParser,
  u_CsvParser;


{ TVectorItemTreeImporterListSimple }

constructor TVectorItemTreeImporterListSimple.Create(
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AMarkPictureList: IMarkPictureList;
  const AHashFunction: IHashFunction;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const AHintConverter: IHtmlToHintTextConverter;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VList: IInterfaceListSimple;
  VImporter: IVectorItemTreeImporter;
  VItem: IVectorItemTreeImporterListItem;
  VKmlLoader: IVectorDataLoader;
  VLoader: IVectorDataLoader;
begin
  inherited Create;
  FNotifierFake := TNotifierFaked.Create;
  VList := TInterfaceListSimple.Create;
  VKmlLoader :=
    TXmlInfoSimpleParser.Create(
      AVectorGeometryLonLatFactory,
      AVectorDataFactory,
      AVectorItemSubsetBuilderFactory,
      True
    );

  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VKmlLoader,
      APerfCounterList.CreateAndAddNewCounter('Kml')
    );
  VImporter :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'kml',
      'Google KML files'
    );
  VList.Add(VItem);

  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VKmlLoader,
      APerfCounterList.CreateAndAddNewCounter('KmlFromKmz')
    );
  VLoader :=
    TKmzInfoSimpleParser.Create(
      VLoader,
      AArchiveReadWriteFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('Kmz')
    );
  VImporter :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'kmz',
      'Google KMZ files'
    );
  VList.Add(VItem);

  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VKmlLoader,
      APerfCounterList.CreateAndAddNewCounter('Gpx')
    );
  VImporter :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'gpx',
      'GPS Exchange files'
    );
  VList.Add(VItem);

  VLoader :=
    TPLTSimpleParser.Create(
      AVectorGeometryLonLatFactory,
      AVectorDataFactory,
      AVectorItemSubsetBuilderFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('Plt')
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
      'OziExplorer Track Point File Version 2.1'
    );
  VList.Add(VItem);

  VLoader :=
    TCsvParser.Create(
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AVectorGeometryLonLatFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('Csv')
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

  VLoader :=
    TMpSimpleParser.Create(
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AVectorGeometryLonLatFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('Mp')
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
      'Single Polygone from MP file'
    );
  VList.Add(VItem);

  VImporter :=
    TVectorItemTreeImporterJpegWithExif.Create(
      AVectorGeometryLonLatFactory,
      AVectorDataItemMainInfoFactory,
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AValueToStringConverter
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'jpg',
      'JPEG Image with GPS Exif info'
    );
  VList.Add(VItem);

  VLoader :=
    THlgParser.Create(
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AVectorGeometryLonLatFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('Hlg')
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
      'SAS.Planet Selection Polygone'
    );
  VList.Add(VItem);

  VLoader :=
    TSlsParser.Create(
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AVectorGeometryLonLatFactory
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VLoader,
      APerfCounterList.CreateAndAddNewCounter('Sls')
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
      'SAS.Planet Download Session Polygone'
    );
  VList.Add(VItem);

  VImporter :=
    TVectorItemTreeImporterSmlMarks.Create(
      AMarkPictureList,
      AHashFunction,
      AAppearanceOfMarkFactory,
      AVectorGeometryLonLatFactory,
      AVectorItemSubsetBuilderFactory,
      AMarkFactory,
      APerfCounterList.CreateAndAddNewCounter('ImportSMLLoader'),
      APerfCounterList.CreateAndAddNewCounter('ImportSMLSaver'),
      AHintConverter
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'sml',
      'SAS.Planet Marks Database in XML format'
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
