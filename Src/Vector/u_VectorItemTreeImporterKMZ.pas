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
  private
    function ProcessImport(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      var AConfig: IInterface
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
  i_ArchiveReadWrite,
  u_VectorItemTreeImporterXML,
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
end;

function TVectorItemTreeImporterKMZ.ProcessImport(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  var AConfig: IInterface
): IVectorItemTree;
var
  VZip: IArchiveReader;
  VItemsCount: Integer;
  VData: IBinaryData;
  VIndex: Integer;
  I: Integer;
  VFileName: string;
  VImporter: IVectorItemTreeImporterXMLInternal;
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

    VImporter :=
      TVectorItemTreeImporterXML.Create(
        FMarkPictureList,
        FAppearanceOfMarkFactory,
        FVectorDataItemMainInfoFactory,
        FVectorGeometryLonLatFactory,
        FVectorDataFactory,
        FVectorItemSubsetBuilderFactory
      );
    VStream := TStreamReadOnlyByBinaryData.Create(VData);
    try
      Result := VImporter.LoadFromStream(VStream, nil, AConfig);
    finally
      VStream.Free;
    end;
  end;
end;

end.
