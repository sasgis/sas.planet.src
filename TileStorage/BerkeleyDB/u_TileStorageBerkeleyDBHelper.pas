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
    FPool: IBerkeleyDBPool;
    FEnvironment: IBerkeleyDBEnvironment;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
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

  VMetaKey := TBerkeleyDBKey.Create(Point(cBerkeleyDBMetaKeyX, cBerkeleyDBMetaKeyY));
  VMetaValue := TBerkeleyDBMetaValue.Create(AStorageEPSG);

  VDatabaseFactory := TBerkeleyDBFactory.Create(
    FGlobalBerkeleyDBHelper,
    FEnvironment,
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
begin
  VDatabase := FPool.Acquire(ADatabaseFileName);
  try
    if Assigned(AData) then begin
      VTile := AData.Buffer;
      VSize := AData.Size;
    end else begin
      VTile := nil;
      VSize := 0;
    end;
    VKey := TBerkeleyDBKey.Create(ATileXY);
    VValue := TBerkeleyDBValue.Create(VTile, VSize, ATileDate, AVersionInfo, ATileContetType);
    Result := VDatabase.Write(VKey, VValue);
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
    VKey := TBerkeleyDBKey.Create(ATileXY);
    Result := VDatabase.Del(VKey);
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
    VKey := TBerkeleyDBKey.Create(ATileXY);
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
    VKey := TBerkeleyDBKey.Create(ATileXY);
    Result := VDatabase.Exists(VKey);
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
    VKey := TBerkeleyDBKey.Create(ATileXY);
    VBinValue := VDatabase.Read(VKey);
    if Assigned(VBinValue) then begin
      VValue := TBerkeleyDBValue.Create(VBinValue);
      ATileDate := VValue.TileDate;
      Result := True;
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
  VBinKey: IBinaryData;
  VPoint: TPoint;
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
      for I := 0 to VList.Count - 1 do begin
        VBinKey := VList.Items[I] as IBinaryData;
        VKey.Assign(VBinKey.Buffer, VBinKey.Size, False);
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
    FPool.Release(VDatabase);
  end;
end;

end.
