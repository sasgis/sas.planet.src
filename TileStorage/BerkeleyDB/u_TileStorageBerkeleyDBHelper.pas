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
  i_ContentTypeInfo,
  i_BinaryData,
  i_GlobalBerkeleyDBHelper,
  u_BerkeleyDB,
  u_BerkeleyDBEnv,
  u_BerkeleyDBPool;

type
  TPointArray = array of TPoint;

  TTileStorageBerkeleyDBHelper = class(TObject)
  private
    FEnv: TBerkeleyDBEnv;
    FPool: TBerkeleyDBPool;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FStorageRootPath: string;
    FStorageEPSG: Integer;
    FEvent: TEvent;

    function OnBDBObjCreate(const AFileName: string): TBerkeleyDB;
    procedure OnBDBFileCreate(Sender: TObject);
    procedure CreateEnvironment(const APath: string);
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AStorageRootPath: string;
      const AStorageEPSG: Integer
    );
    destructor Destroy; override;

    procedure ChangeRootPath(const AStorageNewRootPath: string);

    function CreateDirIfNotExists(APath: string): Boolean;

    function SaveTile(
      const ADataBase: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const ATileDate: TDateTime;
      const AVersionInfo: IMapVersionInfo;
      const ATileContetType: IContentTypeInfoBasic;
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
  i_BerkeleyDBKeyValue,
  u_BerkeleyDBKey,
  u_BerkeleyDBValue;

const
  CPageSize = 1024; // 1k
  CCacheSize = BDB_DEF_CACHE_SIZE; //256k

{ TTileStorageBerkeleyDBHelper }

constructor TTileStorageBerkeleyDBHelper.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AStorageRootPath: string;
  const AStorageEPSG: Integer
);
begin
  inherited Create;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
  FStorageRootPath := AStorageRootPath;
  FStorageEPSG := AStorageEPSG;
  FEvent := TEvent.Create;
  FEvent.SetEvent;

  CreateEnvironment(AStorageRootPath);
end;

destructor TTileStorageBerkeleyDBHelper.Destroy;
begin
  FGlobalBerkeleyDBHelper.FreeEnvironment(@FEnv);
  FEnv := nil;
  FEvent.Free;
  inherited Destroy;
end;

procedure TTileStorageBerkeleyDBHelper.CreateEnvironment(const APath: string);
begin
  FEnv := TBerkeleyDBEnv(FGlobalBerkeleyDBHelper.AllocateEnvironment(APath)^);
  if Assigned(FEnv) then begin
    FPool := FEnv.Pool;
    FPool.OnObjCreate := Self.OnBDBObjCreate;
  end else begin
    FGlobalBerkeleyDBHelper.RaiseException(
      'Error [BerkeleyDB]: Can''t allocate environment: ' + AnsiString(APath)
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
        FGlobalBerkeleyDBHelper.FreeEnvironment(@FEnv);
        FEnv := nil;
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
  VDatabase: TBerkeleyDB;
begin
  try
    VDatabase := TBerkeleyDB.Create(FGlobalBerkeleyDBHelper);
    VDatabase.OnCreate := Self.OnBDBFileCreate;
    VDatabase.OnCheckPoint := FEnv.CheckPoint;

    if VDatabase.Open(FEnv.EnvPtr, AFileName, CPageSize, CCacheSize) then begin
      Result := VDatabase;
    end else begin
      Result := nil;
      FGlobalBerkeleyDBHelper.RaiseException(
        'Error [BerkeleyDB]: Can''t open file: ' + AnsiString(AFileName)
      );
    end;
  except
    FreeAndNil(VDatabase);
    raise;
  end;
end;

procedure TTileStorageBerkeleyDBHelper.OnBDBFileCreate(Sender: TObject);
var
  VKey: IBerkeleyDBKey;
  VMetaValue: IBerkeleyDBMetaValue;
  VDatabase: TBerkeleyDB;
begin
  if Sender is TBerkeleyDB then begin
    VDatabase := Sender as TBerkeleyDB;
    if Assigned(VDatabase) then begin
      VKey := TBerkeleyDBKey.Create(Point(cBerkeleyDBMetaKeyX, cBerkeleyDBMetaKeyY));
      VMetaValue := TBerkeleyDBMetaValue.Create(FStorageEPSG);
      VDatabase.Write(VKey.Data, VKey.Size, VMetaValue.Data, VMetaValue.Size);
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
  const ATileContetType: IContentTypeInfoBasic;
  const AData: IBinaryData
): Boolean;
var
  VKey: IBerkeleyDBKey;
  VValue: IBerkeleyDBValue;
  VDatabase: TBerkeleyDB;
  VTile: Pointer;
  VSize: Integer;
begin
  Result := False;
  FEvent.WaitFor(INFINITE);
  VDatabase := FPool.Acquire(ADataBase);
  try
    if Assigned(VDatabase) then begin
      if Assigned(AData) then begin
        VTile := AData.Buffer;
        VSize := AData.Size;
      end else begin
        VTile := nil;
        VSize := 0;
      end;
      VKey := TBerkeleyDBKey.Create(ATileXY);
      VValue := TBerkeleyDBValue.Create(VTile, VSize, ATileDate, AVersionInfo, ATileContetType);
      Result := VDatabase.Write(VKey.Data, VKey.Size, VValue.Data, VValue.Size);
    end;
  finally
    FPool.Release(VDatabase);
  end;
end;

function TTileStorageBerkeleyDBHelper.DeleteTile(
  const ADataBase: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VKey: IBerkeleyDBKey;
  VBDB: TBerkeleyDB;
begin
  Result := False;
  FEvent.WaitFor(INFINITE);
  VBDB := FPool.Acquire(ADataBase);
  try
    if Assigned(VBDB) then begin
      VKey := TBerkeleyDBKey.Create(ATileXY);
      // TODO: Del tile with ATileVersion
      Result := VBDB.Del(VKey.Data, VKey.Size);
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
  VKey: IBerkeleyDBKey;
  VValue: IBerkeleyDBValue;
  VDatabase: TBerkeleyDB;
  VRawData: Pointer;
  VRawDataSize: Cardinal;
begin
  Result := False;
  FEvent.WaitFor(INFINITE);
  VDatabase := FPool.Acquire(ADataBase);
  try
    if Assigned(VDatabase) then begin
      VKey := TBerkeleyDBKey.Create(ATileXY);
      // TODO: Load tile with ATileVersion
      if VDatabase.Read(VKey.Data, VKey.Size, VRawData, VRawDataSize) then begin
        if (VRawData <> nil) and (VRawDataSize > 0) then begin
          VValue := TBerkeleyDBValue.Create(VRawData, VRawDataSize, True);
          if (VValue.TileSize > 0) and (VValue.TileBody <> nil) then begin
            ATileBinaryData := VValue as IBinaryData;
            ATileVersion := VValue.TileVersionInfo;
            ATileContentType := VValue.TileContentType;
            ATileDate := VValue.TileDate;
            Result := True;
          end;
        end;
      end;
    end;
  finally
    FPool.Release(VDatabase);
  end;
end;

function TTileStorageBerkeleyDBHelper.TileExists(
  const ADataBase: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VKey: IBerkeleyDBKey;
  VBDB: TBerkeleyDB;
begin
  Result := False;
  FEvent.WaitFor(INFINITE);
  VBDB := FPool.Acquire(ADataBase);
  try
    if Assigned(VBDB) then begin
      VKey := TBerkeleyDBKey.Create(ATileXY);
      // TODO: Exists tile with ATileVersion
      Result := VBDB.Exists(VKey.Data, VKey.Size);
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
  VKey: IBerkeleyDBKey;
  VValue: IBerkeleyDBValue;
  VDatabase: TBerkeleyDB;
  VRawData: Pointer;
  VRawDataSize: Cardinal;
begin
  Result := False;
  FEvent.WaitFor(INFINITE);
  VDatabase := FPool.Acquire(ADataBase);
  try
    if Assigned(VDatabase) then begin
      VKey := TBerkeleyDBKey.Create(ATileXY);
      // TODO: Load tile with ATileVersion
      if VDatabase.Read(VKey.Data, VKey.Size, VRawData, VRawDataSize) then begin
        if (VRawData <> nil) and (VRawDataSize > 0) then begin
          VValue := TBerkeleyDBValue.Create(VRawData, VRawDataSize, True);
          ATileDate := VValue.TileDate;
          Result := True;
        end;
      end;
    end;
  finally
    FPool.Release(VDatabase);
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
  VKey: IBerkeleyDBKey;
  VKeySize: Integer;
  VPoint: TPoint;
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
        VKey := TBerkeleyDBKey.Create(Point(0, 0));
        VKeySize := VKey.Size;
        if VBDB.GetKeyExistsList(VKeySize, VList) then begin
          SetLength(ATileExistsArray, VList.Count);
          VValidKeyCount := 0;
          for I := 0 to VList.Count - 1 do begin
            VKey.Assign(VList.Items[I], VKeySize, True);
            VPoint := VKey.Point;
            if
              (Cardinal(VPoint.X) <> cBerkeleyDBMetaKeyX) and
              (Cardinal(VPoint.Y) <> cBerkeleyDBMetaKeyY)
            then begin
              ATileExistsArray[VValidKeyCount] := VPoint;
              Inc(VValidKeyCount);
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
