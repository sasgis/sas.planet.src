{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit u_VectorItemTreeImporterKMZ;

interface

uses
  Classes,
  SysUtils,
  i_BinaryData,
  i_VectorDataFactory,
  i_VectorItemSubsetBuilder,
  i_VectorItemTree,
  i_MarkPicture,
  i_GeometryLonLatFactory,
  i_VectorItemTreeImporter,
  i_NotifierOperation,
  i_ArchiveReadWriteFactory,
  i_AppearanceOfMarkFactory,
  u_VectorItemTreeImporterXML,
  u_BaseInterfacedObject;

type
  TVectorItemTreeImporterKMZ = class(TBaseInterfacedObject, IVectorItemTreeImporter)
  private
    FMarkPictureList: IMarkPictureList;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorDataFactory: IVectorDataFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FImporter: IVectorItemTreeImporterXMLInternal;
  private
    function ProcessImport(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AConfig: IInterface
    ): IVectorItemTree;
  public
    constructor Create(
      const AMarkPictureList: IMarkPictureList;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory
    );
  end;

implementation

uses
  i_ImportConfig,
  i_VectorDataLoader,
  i_ArchiveReadWrite,
  u_StreamReadOnlyByBinaryData;

{ TVectorItemTreeImporterKMZ }

constructor TVectorItemTreeImporterKMZ.Create(
  const AMarkPictureList: IMarkPictureList;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory
);
begin
  inherited Create;
  FMarkPictureList := AMarkPictureList;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorDataFactory := AVectorDataFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;

  FImporter :=
    TVectorItemTreeImporterXML.Create(
      False,
      FMarkPictureList,
      FAppearanceOfMarkFactory,
      FVectorDataItemMainInfoFactory,
      FVectorGeometryLonLatFactory,
      FVectorDataFactory,
      FVectorItemSubsetBuilderFactory
    );
end;

function TVectorItemTreeImporterKMZ.ProcessImport(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AConfig: IInterface
): IVectorItemTree;
var
  VConfig: IImportConfig;
  VContext: TVectorLoadContext;
  VZip: IArchiveReader;
  VItemsCount: Integer;
  VData: IBinaryData;
  VIndex: Integer;
  I: Integer;
  VFileName: string;
  VStream: TStreamReadOnlyByBinaryData;
begin
  Result := nil;
  VZip := FArchiveReadWriteFactory.Zip.ReaderFactory.BuildByFileName(AFileName);
  VItemsCount := VZip.GetItemsCount;
  if VItemsCount > 0 then begin
    VData := VZip.GetItemByName('doc.kml');
    if VData = nil then begin
      VIndex := 0;
      for I := 0 to VItemsCount - 1 do begin
        if LowerCase(ExtractFileExt(VZip.GetItemNameByIndex(I))) = '.kml' then begin
          VIndex := I;
          Break;
        end;
      end;
      VData := VZip.GetItemByIndex(VIndex, VFileName);
    end;

    if VData = nil then begin
      Exit;
    end;

    VStream := TStreamReadOnlyByBinaryData.Create(VData);
    try
      VContext.Init;
      VContext.MainInfoFactory := FVectorDataItemMainInfoFactory;
      if Supports(AConfig, IImportConfig, VConfig) then begin
        VContext.PointParams := VConfig.PointParams;
        VContext.LineParams := VConfig.LineParams;
        VContext.PolygonParams := VConfig.PolyParams;
      end;
      Result := FImporter.LoadFromStream(VContext, VStream);
    finally
      VStream.Free;
    end;
  end;
end;

end.
