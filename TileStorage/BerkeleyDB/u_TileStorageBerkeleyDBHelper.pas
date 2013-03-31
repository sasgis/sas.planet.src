{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit u_TileStorageBerkeleyDBHelper;

interface

uses
  Types,
  Classes,
  i_Listener,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_BinaryData,
  i_BerkeleyDBKeyValue,
  i_BerkeleyDBFactory,
  i_GlobalBerkeleyDBHelper,
  i_BerkeleyDB,
  i_BerkeleyDBEnv,
  i_BerkeleyDBPool;

type
  TPointArray = array of TPoint;

  TTileStorageBerkeleyDBHelper = class(TObject)
  private
    type
      TTileOperation = (toRead = 0, toWrite = 1, toDelete = 2, toExists = 3);
  private
    FPool: IBerkeleyDBPool;
    FEnvironment: IBerkeleyDBEnvironment;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FSyncCallListener: IListener;
    function GetTileKey(
        const AOperation: TTileOperation;
        const ATileXY: TPoint;
        const AVersionInfo: IMapVersionInfo;
        const ADatabase: IBerkeleyDB;
        const ATileDate: TDateTime = 0;
        const ATileCRC: Cardinal = 0
    ): IBinaryData;
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AStorageRootPath: string;
      const AStorageEPSG: Integer
    );
    destructor Destroy; override;
    function CreateDirIfNotExists(APath: string): Boolean;

    function SaveTile(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const ATileDate: TDateTime;
      const AVersionInfo: IMapVersionInfo;
      const ATileContetType: IContentTypeInfoBasic;
      const AData: IBinaryData
    ): Boolean;

    function DeleteTile(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean;

    function LoadTile(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileBinaryData: IBinaryData;
      out ATileVersion: WideString;
      out ATileContentType: WideString;
      out ATileDate: TDateTime
    ): Boolean;

    function TileExists(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean;

    function IsTNEFound(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileDate: TDateTime
    ): Boolean;

    procedure Sync;

    function GetTileExistsArray(
      const ADatabaseFileName: string;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileExistsArray: TPointArray
    ): Boolean;
  end;

implementation

uses
  Windows,
  SysUtils,
  CRC32,
  u_ListenerByEvent,
  u_BerkeleyDBKey,
  u_BerkeleyDBValue,
  u_BerkeleyDBPool,
  u_BerkeleyDBFactory,
  u_BinaryDataByBerkeleyDBValue;

const
  cBerkeleyDBPoolSize = 32;
  cBerkeleyDBUnusedPoolObjectsTTL = 60000; // 60 sec

{ TTileStorageBerkeleyDBHelper }

constructor TTileStorageBerkeleyDBHelper.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AStorageRootPath: string;
  const AStorageEPSG: Integer
);
var
  VMetaKey: IBinaryData;
  VMetaValue: IBinaryData;
  VDatabaseFactory: IBerkeleyDBFactory;
begin
  inherited Create;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;

  FEnvironment := FGlobalBerkeleyDBHelper.AllocateEnvironment(AStorageRootPath);

  VMetaKey := TBerkeleyDBMetaKey.Create;
  VMetaValue := TBerkeleyDBMetaValue.Create(AStorageEPSG);

  FSyncCallListener := TNotifyNoMmgEventListener.Create(Self.Sync);

  VDatabaseFactory := TBerkeleyDBFactory.Create(
    FGlobalBerkeleyDBHelper,
    FEnvironment,
    FSyncCallListener,
    VMetaKey,
    VMetaValue
  );

  FPool := TBerkeleyDBPool.Create(
    FGlobalBerkeleyDBHelper,
    VDatabaseFactory,
    cBerkeleyDBPoolSize,
    cBerkeleyDBUnusedPoolObjectsTTL
  );
end;

destructor TTileStorageBerkeleyDBHelper.Destroy;
begin
  FSyncCallListener := nil;
  FGlobalBerkeleyDBHelper.FreeEnvironment(FEnvironment);
  FPool := nil;
  FEnvironment := nil;
  FGlobalBerkeleyDBHelper := nil;
  inherited Destroy;
end;

function TTileStorageBerkeleyDBHelper.CreateDirIfNotExists(APath: string): Boolean;
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim, APath);
  APath := copy(APath, 1, I);
  Result := DirectoryExists(APath);
  if not Result then begin
    Result := ForceDirectories(APath);
  end;
end;

function TTileStorageBerkeleyDBHelper.GetTileKey(
  const AOperation: TTileStorageBerkeleyDBHelper.TTileOperation;
  const ATileXY: TPoint;
  const AVersionInfo: IMapVersionInfo;
  const ADatabase: IBerkeleyDB;
  const ATileDate: TDateTime = 0;
  const ATileCRC: Cardinal = 0
): IBinaryData;
var
  I: Integer;
  VKey: IBinaryData;
  VValue: IBinaryData;
  VMaxID: Word;
  VVersionID: Word;
  VTileInfoIndex: Integer;
  VVersionMeta: IBerkeleyDBVersionedMetaValue;
  VMetaElement: IBerkeleyDBVersionedMetaValueElement;
begin
  Result := nil;
  if Assigned(AVersionInfo) and (AVersionInfo.StoreString <> '') then begin

    VMaxID := 0;
    VVersionID := 0;
    VTileInfoIndex := -1;

    VKey := TBerkeleyDBVersionedMetaKey.Create(ATileXY);
    VValue := ADatabase.Read(VKey);
    if Assigned(VValue) then begin
      VVersionMeta := TBerkeleyDBVersionedMetaValue.Create(VValue);
      for I := 0 to VVersionMeta.ItemsCount - 1 do begin
        if WideSameStr(VVersionMeta.Item[I].TileVersionInfo, AVersionInfo.StoreString) then begin
          VVersionID := VVersionMeta.Item[I].VersionID;
          VTileInfoIndex := I;
          Break;
        end else if (AOperation = toWrite) and (VVersionMeta.Item[I].TileCRC = ATileCRC) then begin
          //ToDo
        end;
        if VVersionMeta.Item[I].VersionID > VMaxID then begin
          VMaxID := VVersionMeta.Item[I].VersionID;
        end;
      end;
    end else begin
      if AOperation = toWrite then begin
        VVersionMeta := TBerkeleyDBVersionedMetaValue.Create;
      end;
    end;

    if AOperation in [toRead, toDelete, toExists] then begin
      if (VVersionID <> 0) and (VTileInfoIndex <> -1) then begin
        Result := TBerkeleyDBVersionedKey.Create(ATileXY, VVersionID);
        if AOperation = toDelete then begin
          VVersionMeta.Del(VTileInfoIndex);
          if VVersionMeta.ItemsCount > 0 then begin
            VValue := VVersionMeta as IBinaryData;
            ADatabase.Write(VKey, VValue);
          end;
        end;
      end;
    end else begin // toWrite
      if VVersionID = 0 then begin
        VVersionID := VMaxID + 1;
      end;

      VMetaElement :=
        TBerkeleyDBVersionedMetaValueElement.Create(
          VVersionID,
          ATileDate,
          ATileCRC,
          AVersionInfo
        );

      if VTileInfoIndex <> -1 then begin
        VVersionMeta.Replace(VTileInfoIndex, VMetaElement);
      end else begin
        VVersionMeta.Add(VMetaElement);
      end;

      VValue := VVersionMeta as IBinaryData;
      if ADatabase.Write(VKey, VValue) then begin
        Result := TBerkeleyDBVersionedKey.Create(ATileXY, VVersionID);
      end;
    end;
  end else begin
    Result := TBerkeleyDBKey.Create(ATileXY);
  end;
end;

function TTileStorageBerkeleyDBHelper.SaveTile(
  const ADatabaseFileName: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const ATileDate: TDateTime;
  const AVersionInfo: IMapVersionInfo;
  const ATileContetType: IContentTypeInfoBasic;
  const AData: IBinaryData
): Boolean;
var
  VKey: IBinaryData;
  VValue: IBinaryData;
  VDatabase: IBerkeleyDB;
  VTile: Pointer;
  VSize: Integer;
  VTileCRC: Cardinal;
begin
  VDatabase := FPool.Acquire(ADatabaseFileName);
  try
    if Assigned(AData) then begin
      VTile := AData.Buffer;
      VSize := AData.Size;
      VTileCRC := CRC32Buf(VTile, VSize);
    end else begin
      VTile := nil;
      VSize := 0;
      VTileCRC := 0;
    end; 
    VKey := GetTileKey(toWrite, ATileXY, AVersionInfo, VDatabase, ATileDate, VTileCRC);
    if Assigned(VKey) then begin
      VValue := TBerkeleyDBValue.Create(VTile, VSize, ATileDate, AVersionInfo, ATileContetType);
      Result := VDatabase.Write(VKey, VValue);
    end else begin
      Result := False;
    end;
  finally
    FPool.Release(VDatabase);
  end;
end;

function TTileStorageBerkeleyDBHelper.DeleteTile(
  const ADatabaseFileName: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VKey: IBinaryData;
  VDatabase: IBerkeleyDB;
begin
  VDatabase := FPool.Acquire(ADatabaseFileName);
  try
    VKey := GetTileKey(toDelete, ATileXY, AVersionInfo, VDatabase);
    if Assigned(VKey) then begin
      Result := VDatabase.Del(VKey);
    end else begin
      Result := False;
    end;
  finally
    FPool.Release(VDatabase);
  end;
end;

function TTileStorageBerkeleyDBHelper.LoadTile(
  const ADatabaseFileName: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  out ATileBinaryData: IBinaryData;
  out ATileVersion: WideString;
  out ATileContentType: WideString;
  out ATileDate: TDateTime
): Boolean;
var
  VKey: IBinaryData;
  VBinValue: IBinaryData;
  VValue: IBerkeleyDBValue;
  VDatabase: IBerkeleyDB;
begin
  Result := False;
  VDatabase := FPool.Acquire(ADatabaseFileName);
  try
    VKey := GetTileKey(toRead, ATileXY, AVersionInfo, VDatabase);
    if Assigned(VKey) then begin
      VBinValue := VDatabase.Read(VKey);
      if Assigned(VBinValue) then begin
        VValue := TBerkeleyDBValue.Create(VBinValue);
        if (VValue.TileSize > 0) and (VValue.TileBody <> nil) then begin
          ATileBinaryData := TBinaryDataByBerkeleyDBValue.Create(VValue);
          ATileVersion := VValue.TileVersionInfo;
          ATileContentType := VValue.TileContentType;
          ATileDate := VValue.TileDate;
          Result := True;
        end;
      end;
    end;
  finally
    FPool.Release(VDatabase);
  end;
end;

function TTileStorageBerkeleyDBHelper.TileExists(
  const ADatabaseFileName: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VKey: IBinaryData;
  VDatabase: IBerkeleyDB;
begin
  VDatabase := FPool.Acquire(ADatabaseFileName);
  try
    VKey := GetTileKey(toExists, ATileXY, AVersionInfo, VDatabase);
    if Assigned(VKey) then begin
      Result := VDatabase.Exists(VKey);
    end else begin
      Result := False;
    end;
  finally
    FPool.Release(VDatabase);
  end;
end;

function TTileStorageBerkeleyDBHelper.IsTNEFound(
  const ADatabaseFileName: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  out ATileDate: TDateTime
): Boolean;
var
  VKey: IBinaryData;
  VBinValue: IBinaryData;
  VValue: IBerkeleyDBValue;
  VDatabase: IBerkeleyDB;
begin
  Result := False;
  VDatabase := FPool.Acquire(ADatabaseFileName);
  try
    VKey := GetTileKey(toRead, ATileXY, AVersionInfo, VDatabase);
    if Assigned(VKey) then begin
      VBinValue := VDatabase.Read(VKey);
      if Assigned(VBinValue) then begin
        VValue := TBerkeleyDBValue.Create(VBinValue);
        ATileDate := VValue.TileDate;
        Result := True;
      end;
    end;
  finally
    FPool.Release(VDatabase);
  end;
end;

procedure TTileStorageBerkeleyDBHelper.Sync;
begin
  if Assigned(FPool) then begin
    FPool.Sync;
  end;
  if Assigned(FEnvironment) then begin
    FEnvironment.TransactionCheckPoint;
    FEnvironment.RemoveUnUsedLogs;
  end;
end;

function TTileStorageBerkeleyDBHelper.GetTileExistsArray(
  const ADatabaseFileName: string;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  out ATileExistsArray: TPointArray
): Boolean;
var
  VKey: IBerkeleyDBKey;
  VVersionedKey: IBerkeleyDBVersionedKey;
  VBinKey: IBinaryData;
  VValidKeyCount: Integer;
  VDatabase: IBerkeleyDB;
  VList: IInterfaceList;
  I: Integer;
begin
  Result := False;
  VDatabase := FPool.Acquire(ADatabaseFileName);
  try
    VList := VDatabase.ExistsList;
    if Assigned(VList) then begin
      SetLength(ATileExistsArray, VList.Count);
      VValidKeyCount := 0;
      VKey := TBerkeleyDBKey.Create(Point(0, 0));
      VVersionedKey := TBerkeleyDBVersionedKey.Create(Point(0, 0), 0);
      for I := 0 to VList.Count - 1 do begin
        VBinKey := VList.Items[I] as IBinaryData;
        if VKey.Assign(VBinKey.Buffer, VBinKey.Size, False) then begin
          if not IsMetaKey(VKey) then begin
            ATileExistsArray[VValidKeyCount] := VKey.Point;
            Inc(VValidKeyCount);
          end;
        end else if VVersionedKey.Assign(VBinKey.Buffer, VBinKey.Size, False) then begin
          if not IsMetaKey(VVersionedKey) then begin
            ATileExistsArray[VValidKeyCount] := VVersionedKey.Point;
            Inc(VValidKeyCount);
          end;
        end;
      end;
      SetLength(ATileExistsArray, VValidKeyCount);
      Result := VValidKeyCount > 0;
    end;
  finally
    FPool.Release(VDatabase);
  end;
end;

end.
