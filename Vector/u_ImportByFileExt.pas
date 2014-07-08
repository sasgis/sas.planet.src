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

unit u_ImportByFileExt;

interface

uses
  i_VectorItemTreeImporter,
  i_GeometryLonLatFactory,
  i_VectorDataFactory,
  i_VectorDataLoader,
  i_VectorItemSubsetBuilder,
  i_ArchiveReadWriteFactory,
  i_ValueToStringConverter,
  i_InternalPerformanceCounter,
  i_VectorItemTree,
  i_MarkPicture,
  i_HashFunction,
  i_AppearanceOfMarkFactory,
  i_MarkFactory,
  i_HtmlToHintTextConverter,
  u_BaseInterfacedObject;

type
  TImportByFileExt = class(TBaseInterfacedObject, IVectorItemTreeImporter)
  private
    FImportGPX: IVectorItemTreeImporter;
    FImportPLT: IVectorItemTreeImporter;
    FImportCSV: IVectorItemTreeImporter;
    FImportKML: IVectorItemTreeImporter;
    FImportKMZ: IVectorItemTreeImporter;
    FImportHLG: IVectorItemTreeImporter;
    FImportMP: IVectorItemTreeImporter;
    FImportSLS: IVectorItemTreeImporter;
    FImportJPG: IVectorItemTreeImporter;
    FImportSML: IVectorItemTreeImporter;
  private
    function ProcessImport(
      const AFileName: string
    ): IVectorItemTree;
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

 {!!! Not used yet. Proper importer in u_VectorItemTreeImporterListSimple.pas !!! }

implementation

uses
  SysUtils,
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

{ TImportByFileExt }

constructor TImportByFileExt.Create(
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
  VKmlLoader: IVectorDataLoader;
  VLoader: IVectorDataLoader;
begin
  inherited Create;
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
  FImportGPX :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
  VLoader :=
    TVectorDataLoaderWithCounter.Create(
      VKmlLoader,
      APerfCounterList.CreateAndAddNewCounter('Kml')
    );
  FImportKML :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
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
  FImportKMZ :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
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
  FImportPLT :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
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
  FImportCSV :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
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
  FImportHLG :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
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
  FImportMP :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
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
  FImportSLS :=
    TVectorItemTreeImporterByVectorLoader.Create(
      AVectorDataItemMainInfoFactory,
      VLoader
    );
  FImportJPG :=
    TVectorItemTreeImporterJpegWithExif.Create(
      AVectorGeometryLonLatFactory,
      AVectorDataItemMainInfoFactory,
      AVectorItemSubsetBuilderFactory,
      AVectorDataFactory,
      AValueToStringConverter
    );
  FImportSML :=
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
end;

function TImportByFileExt.ProcessImport(
  const AFileName: string
): IVectorItemTree;
var
  VExtLwr: String;
begin
  Result := nil;
  VExtLwr := LowerCase(ExtractFileExt(AFileName));
  if ('.gpx' = VExtLwr) then begin
    Result := FImportGPX.ProcessImport(AFileName);
  end else if ('.kml' = VExtLwr) then begin
    Result := FImportKML.ProcessImport(AFileName);
  end else if ('.kmz' = VExtLwr) then begin
    Result := FImportKMZ.ProcessImport(AFileName);
  end else if ('.plt' = VExtLwr) then begin
    Result := FImportPLT.ProcessImport(AFileName);
  end else if ('.csv' = VExtLwr) then begin
    Result := FImportCSV.ProcessImport(AFileName);
  end else if ('.hlg' = VExtLwr) then begin
    Result := FImportHLG.ProcessImport(AFileName);
  end else if ('.mp' = VExtLwr) then begin
    Result := FImportMP.ProcessImport(AFileName);
  end else if ('.sls' = VExtLwr) then begin
    Result := FImportSLS.ProcessImport(AFileName);
  end else if ('.jpg' = VExtLwr) then begin
    Result := FImportJPG.ProcessImport(AFileName);
  end else if ('.sml' = VExtLwr) then begin
    Result := FImportSML.ProcessImport(AFileName);
  end;
end;

end.
