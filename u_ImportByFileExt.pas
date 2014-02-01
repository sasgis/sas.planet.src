{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ImportByFileExt;

interface

uses
  i_VectorItemTreeImporter,
  i_GeometryLonLatFactory,
  i_VectorDataFactory,
  i_VectorDataLoader,
  i_VectorItemSubsetBuilder,
  i_ValueToStringConverter,
  i_VectorItemTree,
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
  private
    function ProcessImport(
      const AFileName: string
    ): IVectorItemTree;
  public
    constructor Create(
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AGpxLoader: IVectorDataLoader;
      const APltLoader: IVectorDataLoader;
      const AKmlLoader: IVectorDataLoader;
      const AKmzLoader: IVectorDataLoader
    );
  end;

implementation

uses
  SysUtils,
  u_ImportByVectorLoader,
  u_ImportJpegWithExif,
  u_SlsParser,
  u_HlgParser,
  u_MpSimpleParser,
  u_CsvParser;

{ TImportByFileExt }

constructor TImportByFileExt.Create(
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AGpxLoader: IVectorDataLoader;
  const APltLoader: IVectorDataLoader;
  const AKmlLoader: IVectorDataLoader;
  const AKmzLoader: IVectorDataLoader
);
begin
  inherited Create;
  FImportGPX := TImportByVectorLoader.Create(AVectorDataItemMainInfoFactory, AGpxLoader);
  FImportKML := TImportByVectorLoader.Create(AVectorDataItemMainInfoFactory, AKmlLoader);
  FImportKMZ := TImportByVectorLoader.Create(AVectorDataItemMainInfoFactory, AKmzLoader);
  FImportPLT := TImportByVectorLoader.Create(AVectorDataItemMainInfoFactory, APltLoader);
  FImportCSV := TImportByVectorLoader.Create(AVectorDataItemMainInfoFactory, TCsvParser.Create(AVectorItemSubsetBuilderFactory, AVectorDataFactory, AVectorGeometryLonLatFactory));
  FImportHLG := TImportByVectorLoader.Create(AVectorDataItemMainInfoFactory, THlgParser.Create(AVectorItemSubsetBuilderFactory, AVectorDataFactory, AVectorGeometryLonLatFactory));
  FImportMP := TImportByVectorLoader.Create(AVectorDataItemMainInfoFactory, TMpSimpleParser.Create(AVectorItemSubsetBuilderFactory, AVectorDataFactory, AVectorGeometryLonLatFactory));
  FImportSLS := TImportByVectorLoader.Create(AVectorDataItemMainInfoFactory, TSlsParser.Create(AVectorItemSubsetBuilderFactory, AVectorDataFactory, AVectorGeometryLonLatFactory));
  FImportJPG := TImportJpegWithExif.Create(AVectorGeometryLonLatFactory, AVectorDataItemMainInfoFactory, AVectorItemSubsetBuilderFactory, AVectorDataFactory, AValueToStringConverterConfig);
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
  end;
end;

end.
