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

unit u_KmzInfoSimpleParser;

interface

uses
  Classes,
  i_BinaryData,
  i_VectorDataFactory,
  i_VectorDataItemSimple,
  i_VectorItemsFactory,
  i_ArchiveReadWriteFactory,
  i_InternalPerformanceCounter,
  i_VectorDataLoader,
  u_BaseInterfacedObject;

type
  TKmzInfoSimpleParser = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FKmlParser: IVectorDataLoader;
    FLoadKmzStreamCounter: IInternalPerformanceCounter;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
  private
    function LoadFromStream(
      AStream: TStream;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorDataItemList;
    function Load(
      const AData: IBinaryData;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorDataItemList;
  public
    constructor Create(
      const AFactory: IVectorItemsFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  SysUtils,
  i_ArchiveReadWrite,
  u_KmlInfoSimpleParser,
  u_StreamReadOnlyByBinaryData;

{ TKmzInfoSimpleParser }

constructor TKmzInfoSimpleParser.Create(
  const AFactory: IVectorItemsFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VPerfCounterList: IInternalPerformanceCounterList;
begin
  inherited Create;
  VPerfCounterList := APerfCounterList.CreateAndAddNewSubList('KmzLoader');
  FKmlParser := TKmlInfoSimpleParser.Create(AFactory, VPerfCounterList);
  FLoadKmzStreamCounter := VPerfCounterList.CreateAndAddNewCounter('LoadKmzStream');
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

function TKmzInfoSimpleParser.Load(
  const AData: IBinaryData;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
): IVectorDataItemList;
var
  VStream: TStreamReadOnlyByBinaryData;
begin
  Result := nil;
  VStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    Result := LoadFromStream(VStream, AIdData, AFactory);
  finally
    VStream.Free;
  end;
end;

function TKmzInfoSimpleParser.LoadFromStream(
  AStream: TStream;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
): IVectorDataItemList;
var
  I: Integer;
  VZip: IArchiveReader;
  VItemsCount: Integer;
  VMemStream: TMemoryStream;
  VStreamKml: TStream;
  VIndex: Integer;
  VCounterContext: TInternalPerformanceCounterContext;
  VData: IBinaryData;
  VFileName: string;
begin
  Result := nil;
  VCounterContext := FLoadKmzStreamCounter.StartOperation;
  try
    VMemStream := TMemoryStream.Create;
    try
      VMemStream.LoadFromStream(AStream);
      VMemStream.Position := 0;
      VZip := FArchiveReadWriteFactory.CreateZipReaderByStream(VMemStream);
      VItemsCount := VZip.GetItemsCount;
      if VItemsCount > 0 then begin
        VData := VZip.GetItemByName('doc.kml');
        if VData = nil then begin
          VIndex := 0;
          for I := 0 to VItemsCount - 1 do begin
            if ExtractFileExt(VZip.GetItemNameByIndex(I)) = '.kml' then begin
              VIndex := I;
              Break;
            end;
          end;
          VData := VZip.GetItemByIndex(VIndex, VFileName);
        end;
        Result := FKmlParser.Load(VData, AIdData, AFactory);
      end;
    finally
      FreeAndNil(VMemStream);
    end;
  finally
    FLoadKmzStreamCounter.FinishOperation(VCounterContext);
  end;
end;

end.
