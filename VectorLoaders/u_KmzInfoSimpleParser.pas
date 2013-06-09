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
  i_VectorItemSubset,
  i_ArchiveReadWriteFactory,
  i_InternalPerformanceCounter,
  i_VectorDataLoader,
  u_BaseInterfacedObject;

type
  TKmzInfoSimpleParser = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FKmlParser: IVectorDataLoader;
    FLoadKmzStreamCounter: IInternalPerformanceCounter;
    FLoadKmzCounter: IInternalPerformanceCounter;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    function LoadFromStreamInternal(
      AStream: TStream;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorItemSubset;
  private
    function Load(
      const AData: IBinaryData;
      const AIdData: Pointer;
      const AFactory: IVectorDataFactory
    ): IVectorItemSubset;
  public
    constructor Create(
      const AKmlParser: IVectorDataLoader;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  SysUtils,
  i_ArchiveReadWrite,
  u_StreamReadOnlyByBinaryData;

{ TKmzInfoSimpleParser }

constructor TKmzInfoSimpleParser.Create(
  const AKmlParser: IVectorDataLoader;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VPerfCounterList: IInternalPerformanceCounterList;
begin
  inherited Create;
  FKmlParser := AKmlParser;
  VPerfCounterList := APerfCounterList.CreateAndAddNewSubList('KmzLoader');
  FLoadKmzCounter := VPerfCounterList.CreateAndAddNewCounter('LoadKmz');
  FLoadKmzStreamCounter := VPerfCounterList.CreateAndAddNewCounter('LoadZip');
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

function TKmzInfoSimpleParser.Load(
  const AData: IBinaryData;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
): IVectorItemSubset;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VStream: TStreamReadOnlyByBinaryData;
begin
  Result := nil;
  VCounterContext := FLoadKmzStreamCounter.StartOperation;
  try
    VStream := TStreamReadOnlyByBinaryData.Create(AData);
    try
      Result := LoadFromStreamInternal(VStream, AIdData, AFactory);
    finally
      VStream.Free;
    end;
  finally
    FLoadKmzStreamCounter.FinishOperation(VCounterContext);
  end;
end;

function TKmzInfoSimpleParser.LoadFromStreamInternal(
  AStream: TStream;
  const AIdData: Pointer;
  const AFactory: IVectorDataFactory
): IVectorItemSubset;
var
  VZip: IArchiveReader;
  VItemsCount: Integer;
  VData: IBinaryData;
  VIndex: Integer;
  I: Integer;
  VFileName: string;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VZip := FArchiveReadWriteFactory.CreateZipReaderByStream(AStream);
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

    VCounterContext := FLoadKmzCounter.StartOperation;
    try
      Result := FKmlParser.Load(VData, AIdData, AFactory);
    finally
      FLoadKmzCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

end.
