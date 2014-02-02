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
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
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
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
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
      'GPS Exchange files (*.gpx)'
    );
  VList.Add(VItem);

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
      'Google KML files (*.kml)'
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
      'Google KMZ files (*.kmz)'
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
      'OziExplorer Track Point File Version 2.1 (*.plt)'
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
      'Universal CSV files (*.csv)'
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
      'Selection polygone (*.hlg)'
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
      'Single polygone from mp file (*.mp)'
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
      'Download session polygone(*.sls)'
    );
  VList.Add(VItem);

  VImporter :=
    TVectorItemTreeImporterJpegWithExif.Create(
      AVectorGeometryLonLatFactory,
      AVectorDataItemMainInfoFactory,
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AValueToStringConverterConfig
    );
  VItem :=
    TVectorItemTreeImporterListItem.Create(
      VImporter,
      'jpg',
      'JPEG Image whtg GPS Exif (*.jpg)'
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
