{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_TileStorageArchive;

interface

uses
  Types,
  SysUtils,
  i_BinaryData,
  i_TileStorage,
  i_StorageState,
  i_TileInfoBasic,
  i_ProjectionSet,
  i_MapVersionInfo,
  i_MapVersionRequest,
  i_MapVersionListStatic,
  i_ContentTypeInfo,
  i_ArchiveReadWrite,
  i_ContentTypeManager,
  i_TileFileNameParser,
  i_TileFileNameGenerator,
  i_TileStorageAbilities,
  i_NotifierOperation,
  i_NotifierTilePyramidUpdate,
  u_BaseInterfacedObject;

type
  TTileStorageArchive = class(TBaseInterfacedObject, ITileStorage)
  protected
    FStorageTypeAbilities: ITileStorageTypeAbilities;
    FSync: IReadWriteSync;
    FReader: IArchiveReaderBase;
    FWriter: IArchiveWriter;
    FWriterSeq: IArchiveWriterSequential;
    FArchiveFileName: string;
    FContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;
    FTileNotifier: INotifierTilePyramidUpdate;
    FState: IStorageStateChangeble;
    FProjectionSet: IProjectionSet;
    FTileNameParser: ITileFileNameParser;
    FTileNameGenerator: ITileFileNameGenerator;
  private
    { ITileStorage }
    function GetStorageTypeAbilities: ITileStorageTypeAbilities;
    function GetTileNotifier: INotifierTilePyramidUpdate;
    function GetState: IStorageStateChangeble;
    function GetProjectionSet: IProjectionSet;
    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo
    ): string;
    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic;
    function GetTileInfoEx(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic;
    function DeleteTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo
    ): Boolean;
    function SaveTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersion: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData;
      const AIsOverwrite: Boolean
    ): Boolean;
    function GetListOfTileVersions(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest
    ): IMapVersionListStatic;
    function GetTileRectInfo(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ARect: TRect;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest
    ): ITileRectInfo;
    function ScanTiles(
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    ): IEnumTileInfo;
  public
    constructor Create(
      const AStorageTypeAbilities: ITileStorageTypeAbilities;
      const AArchiveFileName: string;
      const AArchiveReader: IArchiveReaderBase;
      const AArchiveWriter: IArchiveWriterBase;
      const AContentType: IContentTypeInfoBasic;
      const AContentTypeManager: IContentTypeManager;
      const AProjectionSet: IProjectionSet;
      const ATileNameParser: ITileFileNameParser;
      const ATileNameGenerator: ITileFileNameGenerator
    );
  end;

implementation

uses
  u_Synchronizer,
  u_BinaryData,
  u_StrFunc;

type
  TEnumTileInfoArchive = class(TBaseInterfacedObject, IEnumTileInfo)
  private
    FIndex: Int64;
    FCount: Int64;
    FReader: IArchiveReader;
    FReaderSeq: IArchiveReaderSequential;
    FIgnoreTNE: Boolean;
    FContentTypeManager: IContentTypeManager;
    FTileFileNameParser: ITileFileNameParser;
  private
    { IEnumTileInfo }
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const AIgnoreTNE: Boolean;
      const AArchiveReader: IArchiveReaderBase;
      const AContentTypeManager: IContentTypeManager;
      const ATileFileNameParser: ITileFileNameParser
    );
  end;

{ TTileStorageArchive }

constructor TTileStorageArchive.Create(
  const AStorageTypeAbilities: ITileStorageTypeAbilities;
  const AArchiveFileName: string;
  const AArchiveReader: IArchiveReaderBase;
  const AArchiveWriter: IArchiveWriterBase;
  const AContentType: IContentTypeInfoBasic;
  const AContentTypeManager: IContentTypeManager;
  const AProjectionSet: IProjectionSet;
  const ATileNameParser: ITileFileNameParser;
  const ATileNameGenerator: ITileFileNameGenerator
);
begin
  Assert(AArchiveFileName <> '');
  inherited Create;
  FStorageTypeAbilities := AStorageTypeAbilities;
  FArchiveFileName := AArchiveFileName;
  if FStorageTypeAbilities.BaseStorageAbilities.AllowAdd then begin
    if Supports(AArchiveWriter, IArchiveWriter, FWriter) then begin
      FWriterSeq := nil;
    end else if Supports(AArchiveWriter, IArchiveWriterSequential, FWriterSeq) then begin
      FWriter := nil;
    end else begin
      raise Exception.Create('ArchiveWriter: Unexpected interface type!');
    end;
  end;
  FReader := AArchiveReader;
  FContentType := AContentType;
  FContentTypeManager := AContentTypeManager;
  FTileNotifier := nil;
  FState := nil;
  FProjectionSet := AProjectionSet;
  FTileNameParser := ATileNameParser;
  FTileNameGenerator := ATileNameGenerator;
  FSync := GSync.SyncBig.Make(Self.ClassName);
end;

function TTileStorageArchive.GetTileNotifier: INotifierTilePyramidUpdate;
begin
  Result := FTileNotifier;
end;

function TTileStorageArchive.GetState: IStorageStateChangeble;
begin
  Result := FState;
end;

function TTileStorageArchive.GetStorageTypeAbilities: ITileStorageTypeAbilities;
begin
  Result := FStorageTypeAbilities;
end;

function TTileStorageArchive.GetProjectionSet: IProjectionSet;
begin
  Result := FProjectionSet;
end;

function TTileStorageArchive.GetTileFileName(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo
): string;
begin
  Result := FArchiveFileName + PathDelim +
    FTileNameGenerator.GetTileFileName(AXY, AZoom) + FContentType.GetDefaultExt;
end;

function TTileStorageArchive.GetTileInfo(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
begin
  Result := nil;
end;

function TTileStorageArchive.GetTileInfoEx(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
begin
  Result := nil;
end;

function TTileStorageArchive.DeleteTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo
): Boolean;
begin
  Result := False;
end;

function TTileStorageArchive.SaveTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersion: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AContentType: IContentTypeInfoBasic;
  const AData: IBinaryData;
  const AIsOverwrite: Boolean
): Boolean;
var
  VTileName: string;
  VData: IBinaryData;
begin
  Result := False;
  if (FWriter = nil) and (FWriterSeq = nil) then begin
    Assert(False);
    Exit;
  end;
  FSync.BeginWrite;
  try
    if Assigned(AContentType) and Assigned(AData) then begin
      VData := AData;
      VTileName := FTileNameGenerator.GetTileFileName(AXY, AZoom) + FContentType.GetDefaultExt;
      VTileName := FTileNameGenerator.AddExt(VTileName, AContentType.GetDefaultExt);
    end else begin
      VData := TBinaryData.Create(0, nil);
      VTileName := FTileNameGenerator.GetTileFileName(AXY, AZoom) + '.tne';
    end;
    if FWriterSeq <> nil then begin
      FWriterSeq.Add(VData, VTileName, ALoadDate);
    end else begin
      Assert(FWriter <> nil);
      FWriter.AddFile(VData, VTileName, ALoadDate);
    end;
    Result := True;
  finally
    FSync.EndWrite;
  end;
end;

function TTileStorageArchive.ScanTiles(
  const AIgnoreTNE: Boolean;
  const AIgnoreMultiVersionTiles: Boolean
): IEnumTileInfo;
begin
  if FStorageTypeAbilities.BaseStorageAbilities.AllowScan then begin
    Result :=
      TEnumTileInfoArchive.Create(
        AIgnoreTNE,
        FReader,
        FContentTypeManager,
        FTileNameParser
      );
  end else begin
    raise Exception.Create('Storage do not support ScanTiles method!');
  end;
end;

function TTileStorageArchive.GetListOfTileVersions(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest
): IMapVersionListStatic;
begin
  Result := nil;
end;

function TTileStorageArchive.GetTileRectInfo(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ARect: TRect;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest
): ITileRectInfo;
begin
  Result := nil;
end;

{ TEnumTileInfoArchive }

constructor TEnumTileInfoArchive.Create(
  const AIgnoreTNE: Boolean;
  const AArchiveReader: IArchiveReaderBase;
  const AContentTypeManager: IContentTypeManager;
  const ATileFileNameParser: ITileFileNameParser
);
begin
  Assert(AArchiveReader <> nil);
  inherited Create;
  if Supports(AArchiveReader, IArchiveReader, FReader) then begin
    FReaderSeq := nil;
    FIndex := 0;
    FCount := FReader.GetItemsCount;
  end else if Supports(AArchiveReader, IArchiveReaderSequential, FReaderSeq) then begin
    FReader := nil;
  end else begin
    raise Exception.Create('ArchiveReader: Unexpected interface type!');
  end;
  FIgnoreTNE := AIgnoreTNE;
  FContentTypeManager := AContentTypeManager;
  FTileFileNameParser := ATileFileNameParser;
end;

function TEnumTileInfoArchive.Next(var ATileInfo: TTileInfo): Boolean;
var
  VZoom: Byte;
  VPoint: TPoint;
  VExt: string;
  VName: string;
  VExtA: AnsiString;
  VNameA: AnsiString;
begin
  Result := False;

  while not Result do begin

    if FReaderSeq <> nil then begin
      if not FReaderSeq.Next(ATileInfo.FData, VName, ATileInfo.FLoadDate) then begin
        Exit;
      end;
    end else begin
      if FIndex < FCount then begin
        ATileInfo.FData := FReader.GetItemByIndex(FIndex, VName);
        ATileInfo.FLoadDate := Now; // ToDo ?
        Inc(FIndex);
      end else begin
        Exit;
      end;
    end;

    VNameA := StringToAnsiSafe(VName);
    if FTileFileNameParser.GetTilePoint(VNameA, VPoint, VZoom) then begin
      ATileInfo.FTile := VPoint;
      ATileInfo.FZoom := VZoom;
      ATileInfo.FVersionInfo := nil;
      VExt := LowerCase(ExtractFileExt(VName));
      if VExt = '.tne' then begin
        if not FIgnoreTNE then begin
          ATileInfo.FInfoType := titTneExists;
          ATileInfo.FContentType := nil;
          ATileInfo.FData := nil;
          Result := True;
        end;
      end else begin
        if ATileInfo.FData <> nil then begin
          VExtA := StringToAnsiSafe(VExt);
          ATileInfo.FInfoType := titExists;
          ATileInfo.FContentType := FContentTypeManager.GetInfoByExt(VExtA);
          Result := True;
        end else begin
          Assert(False, 'Tile data is empty!');
        end;
      end;
    end else begin
      Assert(False, 'Tile name parse error: ' + VName);
    end;
  end;
end;

end.
