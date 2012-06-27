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

unit u_TileStorageBerkeleyDBHelper;

interface

uses
  Types,
  Classes,
  SyncObjs,
  i_MapVersionInfo,
  i_BinaryData,
  u_BerkeleyDB,
  u_BerkeleyDBEnv,
  u_BerkeleyDBPool,
  u_TileStorageBerkeleyDBRecParser;

type
  TPointArray = array of TPoint;

  TTileStorageBerkeleyDBHelper = class(TObject)
  private
    FEnv: TBerkeleyDBEnv;
    FPool: TBerkeleyDBPool;
    FSingleMode: Boolean;
    FStorageRootPath: string;
    FStorageEPSG: Integer;
    FEvent: TEvent;

    function OnBDBObjCreate(const AFileName: string): TBerkeleyDB;
    procedure OnBDBFileCreate(Sender: TObject);
    procedure OnBDBFileFirstOpen(Sender: TObject);

    procedure CreateEnvironment(const APath: string);
  public
    constructor Create(
      const AStorageRootPath: string;
      AStorageEPSG: Integer
    ); 
    destructor Destroy; override;

    procedure ChangeRootPath(const AStorageNewRootPath: string);

    function CreateDirIfNotExists(APath: string): Boolean;

    class procedure OnBDBClose(Sender: TObject);

    function SaveTile(
      const ADataBase: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const ATileDate: TDateTime;
      const AVersionInfo: IMapVersionInfo;
      const ATileContetType: PWideChar;
      const AData: IBinaryData
    ): Boolean;

    function DeleteTile(
      const ADataBase: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean;

    function LoadTile(
      const ADataBase: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileBinaryData: IBinaryData;
      out ATileVersion: WideString;
      out ATileContentType: WideString;
      out ATileDate: TDateTime
    ): Boolean;

    function TileExists(
      const ADataBase: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean;

    function IsTNEFound(
      const ADataBase: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileDate: TDateTime
    ): Boolean;

    procedure Sync(Sender: TObject);

    function GetTileExistsArray(
      const ADataBase: string;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileExistsArray: TPointArray
    ): Boolean;
  end;

implementation

uses
  Windows,
  SysUtils,
  libdb51,
  u_BinaryData;

const
  CPageSize = 1024; // 1k
  CCacheSize = BDB_DEF_CACHE_SIZE; //256k

{ TTileStorageBerkeleyDBHelper }

constructor TTileStorageBerkeleyDBHelper.Create(
  const AStorageRootPath: string;
  AStorageEPSG: Integer
);
begin
  inherited Create;
  FStorageRootPath := AStorageRootPath;
  FStorageEPSG := AStorageEPSG;
  FEvent := TEvent.Create;
  FEvent.SetEvent;

  CreateEnvironment(AStorageRootPath);
end;

destructor TTileStorageBerkeleyDBHelper.Destroy;
begin
  GlobalFreeEnvironment(FEnv);
  FEvent.Free;
  inherited Destroy;
end;

procedure TTileStorageBerkeleyDBHelper.CreateEnvironment(const APath: string);
begin
  // create in-memory environment (for single process access only)
  FSingleMode := False;

  FEnv := GlobalAllocateEnvironment(APath, FSingleMode);
  if Assigned(FEnv) then begin
    FPool := FEnv.Pool;
    FPool.OnObjCreate := Self.OnBDBObjCreate;
  end else begin
    BDBRaiseException(
      'Error [BerkeleyDB]: Can''t allocate environment: ' + APath
    );
  end;
end;

procedure TTileStorageBerkeleyDBHelper.ChangeRootPath(
  const AStorageNewRootPath: string
);
begin
  if FStorageRootPath <> AStorageNewRootPath then begin
    FEvent.ResetEvent;
    try
      if Assigned(FEnv) then begin
        GlobalFreeEnvironment(FEnv);
      end;
      FStorageRootPath := AStorageNewRootPath;
      CreateEnvironment(AStorageNewRootPath);
    finally
      FEvent.SetEvent;
    end;
  end;
end;

function TTileStorageBerkeleyDBHelper.OnBDBObjCreate(
  const AFileName: string
): TBerkeleyDB;
var
  VBDB: TBerkeleyDB;
  VMeta: PBDBStorageMetaInfo;
begin
  New(VMeta);
  try
    if FSingleMode then begin
      TBerkeleyDBEnv.LsnReset(AFileName);
    end;
    VBDB := TBerkeleyDB.Create;
    VBDB.OnCreate := Self.OnBDBFileCreate;
    VBDB.OnOpen := Self.OnBDBFileFirstOpen;
    VBDB.OnCheckPoint := FEnv.CheckPoint;
    VBDB.OnClose := TTileStorageBerkeleyDBHelper.OnBDBClose;

    FillChar(VMeta^, SizeOf(TBDBStorageMetaInfo), 0);
    VBDB.AppData := VMeta;

    if VBDB.Open(FEnv.EnvPtr, AFileName, CPageSize, CCacheSize) then begin
      Result := VBDB;
    end else begin
      Result := nil;
      BDBRaiseException(
        'Error [BerkeleyDB]: Can''t open file: ' + AFileName
      );
    end;
  except
    Dispose(VMeta);
    FreeAndNil(VBDB);
    raise;
  end;
end;

procedure TTileStorageBerkeleyDBHelper.OnBDBFileCreate(Sender: TObject);
var
  VKey: TBDBKey;
  VMeta: TBDBStorageMetaInfo;
  VMemStream: TMemoryStream;
  VBDB: TBerkeleyDB;
begin
  if Sender is TBerkeleyDB then begin
    VBDB := Sender as TBerkeleyDB;
    if Assigned(VBDB) then begin
      VMemStream := TMemoryStream.Create;
      try
        VKey := PointToKey(Point(CBDBMetaKeyX, CBDBMetaKeyY));
        VMeta.StorageEPSG := FStorageEPSG;
        if PBDBMetaInfoToMemStream(@VMeta, VMemStream) then begin
          VMemStream.Position := 0;
          VBDB.Write(@VKey, SizeOf(TBDBKey), VMemStream.Memory, VMemStream.Size);
        end;
      finally
        VMemStream.Free;
      end;
    end;
  end;
end;

procedure TTileStorageBerkeleyDBHelper.OnBDBFileFirstOpen(Sender: TObject);
var
  VKey: TBDBKey;
  VBDB: TBerkeleyDB;
  VRawMetaData: Pointer;
  VRawMetaDataSize: Cardinal;
  VMeta: PBDBStorageMetaInfo;
begin
  if Sender is TBerkeleyDB then begin
    VBDB := Sender as TBerkeleyDB;
    if Assigned(VBDB) then begin
      VKey := PointToKey(Point(CBDBMetaKeyX, CBDBMetaKeyY));
      if VBDB.Read(@VKey, SizeOf(TBDBKey), VRawMetaData, VRawMetaDataSize) then
      try
        VMeta := VBDB.AppData;
        if VMeta <> nil then begin
          PRawMetaToPBDBMetaInfo(VRawMetaData, VRawMetaDataSize, VMeta);
        end;
      finally
        if VRawMetaData <> nil then begin
          FreeMem(VRawMetaData);
        end;
      end;
    end;
  end;
end;

class procedure TTileStorageBerkeleyDBHelper.OnBDBClose(Sender: TObject);
var
  VBDB: TBerkeleyDB;
  VMeta: PBDBStorageMetaInfo;
begin
  if Sender is TBerkeleyDB then begin
    VBDB := Sender as TBerkeleyDB;
    if Assigned(VBDB) then begin
      VMeta := VBDB.AppData;
      if VMeta <> nil then begin
        Dispose(VMeta);
        VBDB.AppData := nil;
      end;
    end;
  end;
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

function TTileStorageBerkeleyDBHelper.SaveTile(
  const ADataBase: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const ATileDate: TDateTime;
  const AVersionInfo: IMapVersionInfo;
  const ATileContetType: PWideChar;
  const AData: IBinaryData
): Boolean;
var
  VKey: TBDBKey;
  VData: TBDBData;
  VBDB: TBerkeleyDB;
  VMemStream: TMemoryStream;
  VVersionString: WideString;
begin
  Result := False;
  FEvent.WaitFor(INFINITE);
  VBDB := FPool.Acquire(ADataBase);
  try
    if Assigned(VBDB) then begin

      VKey := PointToKey(ATileXY);

      VMemStream := TMemoryStream.Create;
      try
        if AVersionInfo <> nil then begin
          VVersionString := AVersionInfo.StoreString;
        end else begin
          VVersionString := '';
        end;
        VData.TileDate := ATileDate;
        VData.TileVer  := PWideChar(VVersionString);
        VData.TileMIME := ATileContetType;

        if AData <> nil then begin
          VData.TileSize := AData.Size;
          VData.TileBody := AData.Buffer;
          PBDBDataToMemStream(@VData, VMemStream);
        end else begin
          VData.TileSize := 0;
          VData.TileBody := nil;
          PBDBDataToMemStream(@VData, VMemStream);
        end;

        VMemStream.Position := 0;

        Result := VBDB.Write(
          @VKey,
          SizeOf(TBDBKey),
          VMemStream.Memory,
          VMemStream.Size
        );

      finally
        VMemStream.Free;
      end;
    end;
  finally
    FPool.Release(VBDB);
  end;
end;

function TTileStorageBerkeleyDBHelper.DeleteTile(
  const ADataBase: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VKey: TBDBKey;
  VBDB: TBerkeleyDB;
begin
  Result := False;
  FEvent.WaitFor(INFINITE);
  VBDB := FPool.Acquire(ADataBase);
  try
    if Assigned(VBDB) then begin
      VKey := PointToKey(ATileXY);
      // TODO: Del tile with ATileVersion
      Result := VBDB.Del(@VKey, SizeOf(TBDBKey));
    end;
  finally
    FPool.Release(VBDB);
  end;
end;

function TTileStorageBerkeleyDBHelper.LoadTile(
  const ADataBase: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  out ATileBinaryData: IBinaryData;
  out ATileVersion: WideString;
  out ATileContentType: WideString;
  out ATileDate: TDateTime
): Boolean;
var
  VKey: TBDBKey;
  VBDB: TBerkeleyDB;
  VRawData: Pointer;
  VRawDataSize: Cardinal;
  VBDBData: TBDBData;
begin
  Result := False;
  FEvent.WaitFor(INFINITE);
  VBDB := FPool.Acquire(ADataBase);
  try
    if Assigned(VBDB) then begin
      VKey := PointToKey(ATileXY);
      // TODO: Load tile with ATileVersion
      if VBDB.Read(@VKey, SizeOf(TBDBKey), VRawData, VRawDataSize) then begin
        if (VRawData <> nil) and (VRawDataSize > 0) then
        try
          if PRawDataToPBDBData(VRawData, VRawDataSize, @VBDBData) then begin
            if (VBDBData.TileSize > 0) and (VBDBData.TileBody <> nil) then begin
              ATileBinaryData :=
                TBinaryData.Create(
                  VBDBData.TileSize,
                  VBDBData.TileBody,
                  False
                );
              ATileVersion := VBDBData.TileVer;
              ATileContentType := VBDBData.TileMIME;
              ATileDate := VBDBData.TileDate;
              Result := True;
            end;
          end;
        finally
          FreeMemory(VRawData);
        end;
      end;
    end;
  finally
    FPool.Release(VBDB);
  end;
end;

function TTileStorageBerkeleyDBHelper.TileExists(
  const ADataBase: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VKey: TBDBKey;
  VBDB: TBerkeleyDB;
begin
  Result := False;
  FEvent.WaitFor(INFINITE);
  VBDB := FPool.Acquire(ADataBase);
  try
    if Assigned(VBDB) then begin
      VKey := PointToKey(ATileXY);
      // TODO: Exists tile with ATileVersion
      Result := VBDB.Exists(@VKey, SizeOf(TBDBKey));
    end;
  finally
    FPool.Release(VBDB);
  end;
end;

function TTileStorageBerkeleyDBHelper.IsTNEFound(
  const ADataBase: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  out ATileDate: TDateTime
): Boolean;
var
  VKey: TBDBKey;
  VBDB: TBerkeleyDB;
  VRawData: Pointer;
  VRawDataSize: Cardinal;
  VBDBData: TBDBData;
begin
  Result := False;
  FEvent.WaitFor(INFINITE);
  VBDB := FPool.Acquire(ADataBase);
  try
    if Assigned(VBDB) then begin
      VKey := PointToKey(ATileXY);
      // TODO: Load tile with ATileVersion
      if VBDB.Read(@VKey, SizeOf(TBDBKey), VRawData, VRawDataSize) then begin
        if (VRawData <> nil) and (VRawDataSize > 0) then
        try
          if PRawDataToPBDBData(VRawData, VRawDataSize, @VBDBData) then begin
            ATileDate := VBDBData.TileDate;
            Result := True;
          end;
        finally
          FreeMemory(VRawData);
        end;
      end;
    end;
  finally
    FPool.Release(VBDB);
  end;
end;

procedure TTileStorageBerkeleyDBHelper.Sync(Sender: TObject);
begin
  FEvent.WaitFor(INFINITE);
  if Assigned(FEnv) then begin
    if Assigned(FPool) then begin
      FPool.Sync();
    end;
    FEnv.CheckPoint(nil);
  end;
end;

function TTileStorageBerkeleyDBHelper.GetTileExistsArray(
  const ADataBase: string;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  out ATileExistsArray: TPointArray
): Boolean;
var
  VKey: TBDBKey;
  VRawKey: Pointer;
  VValidKeyCount: Integer;
  VBDB: TBerkeleyDB;
  VList: TList;
  I: Integer;
begin
  Result := False;
  FEvent.WaitFor(INFINITE);
  VBDB := FPool.Acquire(ADataBase);
  try
    if Assigned(VBDB) then begin
      VList := TList.Create;
      try
        if VBDB.GetKeyExistsList(SizeOf(TBDBKey), VList) then begin
          SetLength(ATileExistsArray, VList.Count);
          VValidKeyCount := 0;
          for I := 0 to VList.Count - 1 do begin
            VRawKey := VList.Items[I];
            try
              VKey := TBDBKey(VRawKey^);
              if (VKey.TileX <> CBDBMetaKeyX) and (VKey.TileY <> CBDBMetaKeyY) then begin
                ATileExistsArray[VValidKeyCount] := KeyToPoint(VKey);
                Inc(VValidKeyCount);
              end;
            finally
              FreeMemory(VRawKey);
            end;
          end;
          SetLength(ATileExistsArray, VValidKeyCount);
          Result := VValidKeyCount > 0;
        end;
      finally
        VList.Free;
      end;
    end;
  finally
    FPool.Release(VBDB);
  end; 
end;

end.
