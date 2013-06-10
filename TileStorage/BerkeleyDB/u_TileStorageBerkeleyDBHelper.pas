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
  SysUtils,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_BinaryData,
  i_MapVersionConfig,
  i_BerkeleyDBKeyValue,
  i_BerkeleyDBFactory,
  i_GlobalBerkeleyDBHelper,
  i_TileStorageBerkeleyDBHelper,
  i_BerkeleyDB,
  i_BerkeleyDBEnv,
  i_BerkeleyDBPool,
  u_BaseInterfacedObject;

type
  TTileStorageBerkeleyDBHelper = class(TBaseInterfacedObject, ITileStorageBerkeleyDBHelper)
  private
    type
      TTileOperation = (toRead = 0, toWrite = 1, toDelete = 2, toExists = 3);
  private
    FPool: IBerkeleyDBPool;
    FEnvironment: IBerkeleyDBEnvironment;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FLock: IReadWriteSync;
    FIsReadOnly: Boolean;
    FIsVersioned: Boolean;
    FMapVersionFactory: IMapVersionFactory;
    function GetTileKey(
      const AOperation: TTileOperation;
      const ATileXY: TPoint;
      const AVersionInfo: IMapVersionInfo;
      const ADatabase: IBerkeleyDB;
      const AContentType: IContentTypeInfoBasic = nil;
      const ATileSize: Integer = 0;
      const ATileDate: TDateTime = 0;
      const ATileCRC: Cardinal = 0;
      const AAllowReplace: Boolean = False
    ): IBinaryData;
  private
    { ITileStorageBerkeleyDBHelper }
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

    function LoadTileInfo(
      const ADatabaseFileName: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ASingleTileInfo: Boolean;
      out ATileVersionListStatic: IMapVersionListStatic;
      out ATileVersion: WideString;
      out ATileContentType: WideString;
      out ATileSize: Integer;
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

    function GetTileExistsArray(
      const ADatabaseFileName: string;
      const ATileZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileExistsArray: TPointArray
    ): Boolean;

    procedure Sync(out AHotDatabaseCount: Integer);
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AMapVersionFactory: IMapVersionFactory;
      const AStorageRootPath: string;
      const AIsReadOnly: Boolean;
      const AIsVersioned: Boolean;
      const AStorageEPSG: Integer
    );
    destructor Destroy; override;
  end;

function CreateDirIfNotExists(APath: string): Boolean;

implementation

uses
  Windows,
  libdb51,
  CRC32,
  u_Synchronizer,
  u_BerkeleyDBKey,
  u_BerkeleyDBValue,
  u_BerkeleyDBPool,
  u_BerkeleyDBFactory,
  u_MapVersionListStatic,
  u_BinaryDataByBerkeleyDBValue;

const
  cBerkeleyDBPoolSize = 32;
  cBerkeleyDBUnusedPoolObjectsTTL = 60000; // 60 sec

function CreateDirIfNotExists(APath: string): Boolean;
begin
  APath := Copy(APath, 1, LastDelimiter(PathDelim, APath));
  Result := DirectoryExists(APath);
  if not Result then begin
    Result := ForceDirectories(APath);
  end;
end;

{ TTileStorageBerkeleyDBHelper }

constructor TTileStorageBerkeleyDBHelper.Create(
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AMapVersionFactory: IMapVersionFactory;
  const AStorageRootPath: string;
  const AIsReadOnly: Boolean;
  const AIsVersioned: Boolean;
  const AStorageEPSG: Integer
);
var
  VDatabaseFactory: IBerkeleyDBFactory;
begin
  inherited Create;

  FMapVersionFactory := AMapVersionFactory;
  FIsReadOnly := AIsReadOnly;
  FIsVersioned := AIsVersioned;

  if FIsVersioned then begin
    FLock := MakeSyncRW_Big(Self, False);
  end else begin
    FLock := MakeSyncFake(Self);
  end;

  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;

  FEnvironment := FGlobalBerkeleyDBHelper.AllocateEnvironment(AStorageRootPath);

  VDatabaseFactory := TBerkeleyDBFactory.Create(
    FGlobalBerkeleyDBHelper,
    FEnvironment,
    FIsReadOnly,
    TBerkeleyDBMetaKey.Create as IBinaryData,
    TBerkeleyDBMetaValue.Create(AStorageEPSG) as IBinaryData
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
  if Assigned(FGlobalBerkeleyDBHelper) then begin
    FGlobalBerkeleyDBHelper.FreeEnvironment(FEnvironment);
  end;
  FPool := nil;
  FEnvironment := nil;
  FGlobalBerkeleyDBHelper := nil;
  inherited;
end;

function TTileStorageBerkeleyDBHelper.GetTileKey(
  const AOperation: TTileOperation;
  const ATileXY: TPoint;
  const AVersionInfo: IMapVersionInfo;
  const ADatabase: IBerkeleyDB;
  const AContentType: IContentTypeInfoBasic = nil;
  const ATileSize: Integer = 0;
  const ATileDate: TDateTime = 0;
  const ATileCRC: Cardinal = 0;
  const AAllowReplace: Boolean = False
): IBinaryData;
const
  cOnDeadLockRetryCount = 3;
var
  I, J: Integer;
  VKey: IBinaryData;
  VValue: IBinaryData;
  VMaxID: Word;
  VVersionID: Word;
  VYoungestTileDate: TDateTime;
  VYoungestTileVersionID: Word;
  VTileInfoIndex: Integer;
  VIsDeadLock: Boolean;
  VTransaction: PBerkeleyTxn;
  VTransactionFlag: Cardinal;
  VVersionMeta: IBerkeleyDBVersionedMetaValue;
  VMetaElement: IBerkeleyDBVersionedMetaValueElement;
begin
  if FIsVersioned then begin
    if not Assigned(AVersionInfo) or (Assigned(AVersionInfo) and (AVersionInfo.StoreString = '')) then begin
      Result := TBerkeleyDBKey.Create(ATileXY);
    end else begin
      J := 0;
      VIsDeadLock := False;
      repeat
        Inc(J);

        Result := nil;

        VMaxID := 0;
        VVersionID := 0;
        VTileInfoIndex := -1;
        VYoungestTileDate := 0;
        VYoungestTileVersionID := 0;

        VTransaction := nil;
        VTransactionFlag := 0;
        if AOperation in [toWrite, toDelete] then begin
          // заводим транзакцию, для защиты от одновременного редактирования
          // метаинформации о тайле из разных процессов
          FEnvironment.TransactionBegin(VTransaction);
          if VTransaction <> nil then begin
            // флаг для специальной оптимизации локера в БД - уменьшает
            // вероятность возникновения дедлоков
            VTransactionFlag := DB_RMW; // read-modify-write cycle
          end;
        end;

        try
          VKey := TBerkeleyDBVersionedMetaKey.Create(ATileXY);
          VValue := ADatabase.Read(VKey, VTransaction, VIsDeadLock, VTransactionFlag);
          if VIsDeadLock then begin
            FEnvironment.TransactionAbort(VTransaction);
            Continue;
          end;

          if Assigned(VValue) then begin
            try
              VVersionMeta := TBerkeleyDBVersionedMetaValue.Create(VValue);
            except
              on E: EBerkeleyDBBadValue do begin
                FGlobalBerkeleyDBHelper.LogException(E.Message);
                FEnvironment.TransactionAbort(VTransaction);
                ADatabase.Del(VKey);
                Exit;
              end else begin
                raise;
              end;
            end;
            // есть метаинформация о версионных тайлах
            for I := 0 to VVersionMeta.ItemsCount - 1 do begin
              VMetaElement := VVersionMeta.Item[I];
              if WideSameStr(VMetaElement.TileVersionInfo, AVersionInfo.StoreString) then begin
                // нашли тайл с такой же версией
                VVersionID := VMetaElement.VersionID;
                if not AAllowReplace and (AOperation = toWrite) then begin
                  FEnvironment.TransactionAbort(VTransaction);
                  Exit;
                end else begin
                  VTileInfoIndex := I; // индекс тайла, для перезаписи его метаинформации
                  Break;
                end;
              end else if (AOperation = toWrite) and (VMetaElement.TileCRC = ATileCRC) then begin
                // версия у тайлов не совпадает, но они сами по себе идентичны
                FEnvironment.TransactionAbort(VTransaction);
                Exit;
              end;
              if VMetaElement.VersionID > VMaxID then begin
                VMaxID := VMetaElement.VersionID;
              end;
              if VMetaElement.TileDate > VYoungestTileDate then begin
                VYoungestTileDate := VMetaElement.TileDate;
                VYoungestTileVersionID := VMetaElement.VersionID;
              end;
            end;
          end else begin
            // метаинформации, а, соответственно, и версионных тайлов ещё нету
            if AOperation = toWrite then begin
              VVersionMeta := TBerkeleyDBVersionedMetaValue.Create;
            end else begin
              if AVersionInfo.ShowPrevVersion and (AOperation in [toRead, toExists]) then begin
                // но возможно, найдётся неверсионный тайл
                Result := TBerkeleyDBKey.Create(ATileXY);
              end;
              FEnvironment.TransactionAbort(VTransaction);
              Exit;
            end;
          end;

          if AOperation in [toRead, toDelete, toExists] then begin
            if (VVersionID <> 0) and (VTileInfoIndex <> -1) then begin
              // нашли нужный тайл
              if (AOperation = toDelete) and Assigned(VVersionMeta) then begin
                // подчистим метаинформацию
                VVersionMeta.Del(VTileInfoIndex);
                if VVersionMeta.ItemsCount > 0 then begin
                  if ADatabase.Write(VKey, (VVersionMeta as IBinaryData), VTransaction, VIsDeadLock) then begin
                    FEnvironment.TransactionCommit(VTransaction);
                  end else begin
                    FEnvironment.TransactionAbort(VTransaction);
                    if VIsDeadLock then begin
                      Continue;
                    end else begin
                      Assert(False);
                      Exit;
                    end;
                  end;
                end else begin
                  if ADatabase.Del(VKey, VTransaction, VIsDeadLock) then begin
                    FEnvironment.TransactionCommit(VTransaction);
                  end else begin
                    FEnvironment.TransactionAbort(VTransaction);
                    if VIsDeadLock then begin
                      Continue;
                    end else begin
                      Assert(False);
                      Exit;
                    end;
                  end;
                end;
              end;
              Result := TBerkeleyDBVersionedKey.Create(ATileXY, VVersionID);
            end else if AOperation = toDelete then begin
              FEnvironment.TransactionAbort(VTransaction);
            end else if AVersionInfo.ShowPrevVersion and (AOperation in [toRead, toExists]) and (VYoungestTileVersionID <> 0) then begin
              // не нашли тайл нужной версии - отдаём хоть какой (только для чтения)
              Result := TBerkeleyDBVersionedKey.Create(ATileXY, VYoungestTileVersionID);
            end;
          end else begin // toWrite

            if VVersionID = 0 then begin
              VVersionID := VMaxID + 1;
            end;

            VMetaElement :=
              TBerkeleyDBVersionedMetaValueElement.Create(
                VVersionID,
                0,
                ATileSize,
                ATileDate,
                ATileCRC,
                AVersionInfo,
                AContentType
              );

            if VTileInfoIndex <> -1 then begin
              VVersionMeta.Replace(VTileInfoIndex, VMetaElement);
            end else begin
              VVersionMeta.Add(VMetaElement);
            end;

            if ADatabase.Write(VKey, (VVersionMeta as IBinaryData), VTransaction, VIsDeadLock) then begin
              FEnvironment.TransactionCommit(VTransaction);
              Result := TBerkeleyDBVersionedKey.Create(ATileXY, VVersionID);
            end else begin
              FEnvironment.TransactionAbort(VTransaction);
              if VIsDeadLock then begin
                Continue;
              end else begin
                Assert(False);
                Exit;
              end;
            end;
          end;
        except
          FEnvironment.TransactionAbort(VTransaction);
          raise;
        end;
        Break; // повтор разрешаем только при дедлоках, через Continue
      until Assigned(Result) or (J > cOnDeadLockRetryCount);

      if VIsDeadLock and (Result = nil) then begin
        CheckBDB(DB_LOCK_DEADLOCK); // raise exception about deadlock
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
    FLock.BeginWrite;
    try
      VKey := GetTileKey(toWrite, ATileXY, AVersionInfo, VDatabase, ATileContetType, VSize, ATileDate, VTileCRC);
      if Assigned(VKey) then begin
        VValue := TBerkeleyDBValue.Create(VTile, VSize, ATileDate, AVersionInfo, ATileContetType);
        Result := VDatabase.Write(VKey, VValue);
      end else begin
        Result := False;
      end;
    finally
      FLock.EndWrite;
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
    FLock.BeginWrite;
    try
      VKey := GetTileKey(toDelete, ATileXY, AVersionInfo, VDatabase);
      if Assigned(VKey) then begin
        Result := VDatabase.Del(VKey);
      end else begin
        Result := False;
      end;
    finally
      FLock.EndWrite;
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
    FLock.BeginRead;
    try
      VKey := GetTileKey(toRead, ATileXY, AVersionInfo, VDatabase);
      if Assigned(VKey) then begin
        VBinValue := VDatabase.Read(VKey);
        if Assigned(VBinValue) then begin
          try
            VValue := TBerkeleyDBValue.Create(VBinValue);
          except
            on E: EBerkeleyDBBadValue do begin
              FGlobalBerkeleyDBHelper.LogException(E.Message);
              VKey := GetTileKey(toDelete, ATileXY, AVersionInfo, VDatabase);
              if Assigned(VKey) then begin
                VDatabase.Del(VKey);
              end;
              Exit;
            end else begin
              raise;
            end;
          end;
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
      FLock.EndRead;
    end;
  finally
    FPool.Release(VDatabase);
  end;
end;

function TTileStorageBerkeleyDBHelper.LoadTileInfo(
  const ADatabaseFileName: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ASingleTileInfo: Boolean;
  out ATileVersionListStatic: IMapVersionListStatic;
  out ATileVersion: WideString;
  out ATileContentType: WideString;
  out ATileSize: Integer;
  out ATileDate: TDateTime
): Boolean;
var
  I: Integer;
  VKey: IBinaryData;
  VBinValue: IBinaryData;
  VDatabase: IBerkeleyDB;
  VValue: IBerkeleyDBValue;
  VVersionMeta: IBerkeleyDBVersionedMetaValue;
  VMetaElement: IBerkeleyDBVersionedMetaValueElement;
  VList: IInterfaceList;
  VTileInfoIndex: Integer;
  VYoungestTileIndex: Integer;
  VYoungestTileDate: TDateTime;
  VMapVersionInfo: IMapVersionInfo;
begin
  Result := False;
  ATileVersionListStatic := nil;

  VDatabase := FPool.Acquire(ADatabaseFileName);
  try
    if FIsVersioned then begin
      VKey := TBerkeleyDBVersionedMetaKey.Create(ATileXY);
      FLock.BeginRead;
      try
        VBinValue := VDatabase.Read(VKey);
      finally
        FLock.EndRead;
      end;
      if Assigned(VBinValue) then begin
        VTileInfoIndex := -1;
        VYoungestTileIndex := -1;
        VYoungestTileDate := 0;

        if not ASingleTileInfo then begin
          VList := TInterfaceList.Create;
        end else begin
          VList := nil;
        end;

        try
          VVersionMeta := TBerkeleyDBVersionedMetaValue.Create(VBinValue);
        except
          on E: EBerkeleyDBBadValue do begin
            FGlobalBerkeleyDBHelper.LogException(E.Message);
            VKey := GetTileKey(toDelete, ATileXY, AVersionInfo, VDatabase);
            if Assigned(VKey) then begin
              VDatabase.Del(VKey);
            end;
            Exit;
          end else begin
            raise;
          end;
        end;

        for I := 0 to VVersionMeta.ItemsCount - 1 do begin
          VMetaElement := VVersionMeta.Item[I];
          if Assigned(VList) and (VMetaElement.TileVersionInfo <> '') then begin
            VMapVersionInfo :=
              FMapVersionFactory.CreateByStoreString(
                VMetaElement.TileVersionInfo,
                AVersionInfo.ShowPrevVersion
              );
            VList.Add(VMapVersionInfo);
          end;
          if WideSameStr(VMetaElement.TileVersionInfo, AVersionInfo.StoreString) then begin
            VTileInfoIndex := I;
            if ASingleTileInfo then begin
              Break;
            end;
          end;
          if VMetaElement.TileDate > VYoungestTileDate then begin
            VYoungestTileDate := VMetaElement.TileDate;
            VYoungestTileIndex := I;
          end;
        end;

        if ASingleTileInfo then begin
          if (VTileInfoIndex = -1) and (VYoungestTileIndex <> -1) then begin
            if AVersionInfo.ShowPrevVersion then begin
              VTileInfoIndex := VYoungestTileIndex;
            end;
          end;

          if VTileInfoIndex <> -1 then begin
            VMetaElement := VVersionMeta.Item[VTileInfoIndex];

            ATileVersion := VMetaElement.TileVersionInfo;
            ATileContentType := VMetaElement.TileContentType;
            ATileDate := VMetaElement.TileDate;
            ATileSize := VMetaElement.TileSize;

            Result := True;
          end;
        end else begin
          if Assigned(VList) and (VList.Count > 0) then begin
            ATileVersionListStatic := TMapVersionListStatic.Create(VList);
            Result := True;
          end;
        end;
      end;
    end;

    if ASingleTileInfo and (not FIsVersioned or (FIsVersioned and not Result and AVersionInfo.ShowPrevVersion)) then begin
      VKey := TBerkeleyDBKey.Create(ATileXY);
      FLock.BeginRead;
      try
        VBinValue := VDatabase.Read(VKey);
      finally
        FLock.EndRead;
      end;
      if Assigned(VBinValue) then begin

        try
          VValue := TBerkeleyDBValue.Create(VBinValue);
        except
          on E: EBerkeleyDBBadValue do begin
            FGlobalBerkeleyDBHelper.LogException(E.Message);
            VKey := GetTileKey(toDelete, ATileXY, AVersionInfo, VDatabase);
            if Assigned(VKey) then begin
              VDatabase.Del(VKey);
            end;
            Exit;
          end else begin
            raise;
          end;
        end;

        ATileVersion := VValue.TileVersionInfo;
        ATileContentType := VValue.TileContentType;
        ATileDate := VValue.TileDate;
        ATileSize := VValue.TileSize;
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
    FLock.BeginRead;
    try
      VKey := GetTileKey(toExists, ATileXY, AVersionInfo, VDatabase);
      if Assigned(VKey) then begin
        Result := VDatabase.Exists(VKey);
      end else begin
        Result := False;
      end;
    finally
      FLock.EndRead;
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
    FLock.BeginRead;
    try
      VKey := GetTileKey(toRead, ATileXY, AVersionInfo, VDatabase);
      if Assigned(VKey) then begin
        VBinValue := VDatabase.Read(VKey);
        if Assigned(VBinValue) then begin

          try
            VValue := TBerkeleyDBValue.Create(VBinValue);
          except
            on E: EBerkeleyDBBadValue do begin
              FGlobalBerkeleyDBHelper.LogException(E.Message);
              VKey := GetTileKey(toDelete, ATileXY, AVersionInfo, VDatabase);
              if Assigned(VKey) then begin
                VDatabase.Del(VKey);
              end;
              Exit;
            end else begin
              raise;
            end;
          end;

          ATileDate := VValue.TileDate;
          Result := True;
        end;
      end;
    finally
      FLock.EndRead;
    end;
  finally
    FPool.Release(VDatabase);
  end;
end;

procedure TTileStorageBerkeleyDBHelper.Sync(out AHotDatabaseCount: Integer);
begin
  Assert(Assigned(FPool));
  Assert(Assigned(FEnvironment));

  FEnvironment.Sync;
  FPool.Sync;
  AHotDatabaseCount := FPool.Count;
end;

function TTileStorageBerkeleyDBHelper.GetTileExistsArray(
  const ADatabaseFileName: string;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  out ATileExistsArray: TPointArray
): Boolean;

  procedure _KeyArrayToPointArray(
    const AKey: IBerkeleyDBKey;
    const AKeySize: Integer;
    const AKeyArray: TList;
    const ABitMask: TBits
  );
  var
    I, J: Integer;
    VBitIndex: Integer;
    VTilePoint: TPoint;
  begin
    J := Length(ATileExistsArray);
    SetLength(ATileExistsArray, Length(ATileExistsArray) + AKeyArray.Count);
    for I := 0 to AKeyArray.Count - 1 do begin
      if AKey.Assign(AKeyArray.Items[I], AKeySize, False) then begin
        if not IsMetaKey(AKey) then begin
          VTilePoint := AKey.Point;
          VBitIndex := (VTilePoint.X mod 256) * 256 + (VTilePoint.Y mod 256);
          if not ABitMask.Bits[VBitIndex] then begin
            ABitMask.Bits[VBitIndex] := True;
            ATileExistsArray[J] := VTilePoint;
            Inc(J);
          end;
        end;
      end;
    end;
    SetLength(ATileExistsArray, J);
  end;

var
  I: Integer;
  VMask: TBits;
  VKeySize: Integer;
  VVersionedKeySize: Integer;
  VKey: IBerkeleyDBKey;
  VVersionedKey: IBerkeleyDBVersionedKey;
  VDatabase: IBerkeleyDB;
  VList: TExistsKeyArray;
begin
  Result := False;
  SetLength(ATileExistsArray, 0);

  VDatabase := FPool.Acquire(ADatabaseFileName);
  try
    FLock.BeginRead;
    try
       if not VDatabase.CreateExistsKeyArray(VList) then begin
         Exit;
       end;
    finally
      FLock.EndRead;
    end;
    try
      VKey := TBerkeleyDBKey.Create(Point(0, 0));
      VKeySize := VKey.Size;

      VVersionedKey := TBerkeleyDBVersionedKey.Create(Point(0, 0), 0);
      VVersionedKeySize := VVersionedKey.Size;

      VMask := TBits.Create;
      try
        VMask.Size := 256 * 256; // max tile points in sdb file
        for I := 0 to Length(VList) - 1 do begin
          if VList[I].KeySize = VKeySize then begin
            _KeyArrayToPointArray(VKey, VKeySize, VList[I].KeyData, VMask);
          end else if VList[I].KeySize = VVersionedKeySize then begin
            _KeyArrayToPointArray(VVersionedKey, VVersionedKeySize, VList[I].KeyData, VMask);
          end else begin
            // unknown keys
          end;
        end;
      finally
        VMask.Free;
      end;
      Result := Length(ATileExistsArray) > 0;
    finally
      VDatabase.ReleaseExistsKeyArray(VList);
    end;
  finally
    FPool.Release(VDatabase);
  end;
end;

end.
