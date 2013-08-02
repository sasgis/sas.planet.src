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
  t_BerkeleyDB,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_BinaryData,
  i_MapVersionConfig,
  i_BerkeleyDBKeyValue,
  i_GlobalBerkeleyDBHelper,
  i_TileStorageBerkeleyDBHelper,
  i_TileStorageBerkeleyDBConfigStatic,
  i_BerkeleyDB,
  i_BerkeleyDBEnv,
  u_BaseInterfacedObject;

type
  TTileStorageBerkeleyDBHelper = class(TBaseInterfacedObject, ITileStorageBerkeleyDBHelper)
  private
    type
      TVersionIDInfo = record
        IsSameVersionFound: Boolean;
        IsSameCRCFound: Boolean;
        YoungestTileVersionID: Word;
        TileIndexInMetaValue: Integer;
      end;
      PVersionIDInfo = ^TVersionIDInfo;
  private
    FEnvironment: IBerkeleyDBEnvironment;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FIsReadOnly: Boolean;
    FIsVersioned: Boolean;
    FMapVersionFactory: IMapVersionFactory;
    FOnDeadLockRetryCount: Integer;
    FSyncLock: IReadWriteSync;
  private
    function CheckVersionInfo(const AVersionInfo: IMapVersionInfo): IMapVersionInfo;

    function ReadVersionedMetaValue(
      const AVersionedMetaKey: IBinaryData;
      const ATransaction: PBerkeleyTxn;
      const ADatabase: IBerkeleyDB;
      var AIsDeadLock: Boolean
    ): IBerkeleyDBVersionedMetaValue;

    function GetTileVersionID(
      const ATileCRC: Cardinal;
      const AVersionInfo: IMapVersionInfo;
      const AVersionedMetaValue: IBerkeleyDBVersionedMetaValue;
      const AVersionIDInfo: PVersionIDInfo
    ): Word;

    function GetReadOnlyKey(
      const ATileXY: TPoint;
      const AVersionInfo: IMapVersionInfo;
      const ADatabase: IBerkeleyDB
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
      const AData: IBinaryData;
      const AKeepExisting: Boolean
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

    function GetRefCount: Integer;
  public
    constructor Create(
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AMapVersionFactory: IMapVersionFactory;
      const AStorageConfig: ITileStorageBerkeleyDBConfigStatic;
      const AStorageRootPath: string;
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
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_Synchronizer,
  u_BerkeleyDBKey,
  u_BerkeleyDBValue,
  u_MapVersionInfo,
  u_MapVersionListStatic,
  u_BinaryDataByBerkeleyDBValue;

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
  const AStorageConfig: ITileStorageBerkeleyDBConfigStatic;
  const AStorageRootPath: string;
  const AIsVersioned: Boolean;
  const AStorageEPSG: Integer
);
begin
  Assert(AGlobalBerkeleyDBHelper <> nil);
  Assert(AMapVersionFactory <> nil);
  Assert(AStorageConfig <> nil);

  inherited Create;

  FMapVersionFactory := AMapVersionFactory;
  FIsReadOnly := AStorageConfig.IsReadOnly;
  FIsVersioned := AIsVersioned;
  FOnDeadLockRetryCount := AStorageConfig.OnDeadLockRetryCount;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;

  FSyncLock := MakeSyncRW_Std(Self, False);

  FEnvironment := FGlobalBerkeleyDBHelper.AllocateEnvironment(
    AStorageConfig,
    AStorageEPSG,
    AStorageRootPath
  );
  Assert(FEnvironment <> nil);
end;

destructor TTileStorageBerkeleyDBHelper.Destroy;
begin
  if Assigned(FGlobalBerkeleyDBHelper) then begin
    FGlobalBerkeleyDBHelper.FreeEnvironment(FEnvironment);
  end;
  FEnvironment := nil;
  FGlobalBerkeleyDBHelper := nil;
  FSyncLock := nil;
  inherited;
end;

function TTileStorageBerkeleyDBHelper.CheckVersionInfo(
  const AVersionInfo: IMapVersionInfo
): IMapVersionInfo;
begin
  Result := AVersionInfo;
  if not Assigned(Result) then begin
    Result := TMapVersionInfo.Create('', True);
  end;
end;

function TTileStorageBerkeleyDBHelper.ReadVersionedMetaValue(
  const AVersionedMetaKey: IBinaryData;
  const ATransaction: PBerkeleyTxn;
  const ADatabase: IBerkeleyDB;
  var AIsDeadLock: Boolean
): IBerkeleyDBVersionedMetaValue;
var
  VFlag: Cardinal;
  VValue: IBinaryData;
begin
  Result := nil;

  if ATransaction <> nil then begin
    VFlag := DB_RMW; // read-modify-write cycle
  end else begin
    VFlag := 0;
  end;

  VValue := ADatabase.Read(AVersionedMetaKey, ATransaction, AIsDeadLock, VFlag);

  if not AIsDeadLock and Assigned(VValue) then begin
    try
      Result := TBerkeleyDBVersionedMetaValue.Create(VValue);
    except
      on E: EBerkeleyDBBadValue do begin
        FGlobalBerkeleyDBHelper.LogException(E.ClassName + ': ' + E.Message);
        ADatabase.Del(AVersionedMetaKey, ATransaction, AIsDeadLock);
        Result := nil;
      end;
    else
      raise;
    end;
  end;
end;

function TTileStorageBerkeleyDBHelper.GetTileVersionID(
  const ATileCRC: Cardinal;
  const AVersionInfo: IMapVersionInfo;
  const AVersionedMetaValue: IBerkeleyDBVersionedMetaValue;
  const AVersionIDInfo: PVersionIDInfo
): Word;
var
  I: Integer;
  VMaxID: Word;
  VYoungestTileDate: TDateTime;
  VMetaElement: IBerkeleyDBVersionedMetaValueElement;
begin
  Result := 0;

  AVersionIDInfo.IsSameVersionFound := False;
  AVersionIDInfo.IsSameCRCFound := False;
  AVersionIDInfo.YoungestTileVersionID := $FFFF;
  AVersionIDInfo.TileIndexInMetaValue := -1;

  VMaxID := 0;
  VYoungestTileDate := 0;

  for I := 0 to AVersionedMetaValue.ItemsCount - 1 do begin
    VMetaElement := AVersionedMetaValue.Item[I];
    if WideSameText(VMetaElement.TileVersionInfo, AVersionInfo.StoreString) then begin
      AVersionIDInfo.IsSameVersionFound := True;
      AVersionIDInfo.TileIndexInMetaValue := I;
      Result := VMetaElement.VersionID;
      Break;
    end else if (ATileCRC <> 0) and (VMetaElement.TileCRC = ATileCRC) then begin
      AVersionIDInfo.IsSameCRCFound := True;
      AVersionIDInfo.TileIndexInMetaValue := I;
      Result := VMetaElement.VersionID;
      Break;
    end;
    if VMetaElement.VersionID > VMaxID then begin
      VMaxID := VMetaElement.VersionID;
    end;
    if VMetaElement.TileDate > VYoungestTileDate then begin
      VYoungestTileDate := VMetaElement.TileDate;
      AVersionIDInfo.YoungestTileVersionID := VMetaElement.VersionID;
    end;
  end;

  if (Result = 0) and (AVersionInfo.StoreString <> '') then begin
    Result := VMaxID + 1;
  end;
end;

function TTileStorageBerkeleyDBHelper.GetReadOnlyKey(
  const ATileXY: TPoint;
  const AVersionInfo: IMapVersionInfo;
  const ADatabase: IBerkeleyDB
): IBinaryData;
var
  VKey: IBinaryData;
  VIsDeadLock: Boolean;
  VVersionID: Word;
  VVersionIDInfo: TVersionIDInfo;
  VVersionInfo: IMapVersionInfo;
  VVersionedMeta: IBerkeleyDBVersionedMetaValue;
begin
  Result := nil;
  if FIsVersioned then begin
    VVersionInfo := CheckVersionInfo(AVersionInfo);

    VKey := TBerkeleyDBVersionedMetaKey.Create(ATileXY);
    VVersionedMeta := ReadVersionedMetaValue(VKey, nil, ADatabase, VIsDeadLock);

    if Assigned(VVersionedMeta) then begin
      VVersionID := GetTileVersionID(0, VVersionInfo, VVersionedMeta, @VVersionIDInfo);

      if VVersionIDInfo.IsSameVersionFound then begin
        // show versioned tile
        Result := TBerkeleyDBVersionedKey.Create(ATileXY, VVersionID);
      end else if VVersionInfo.ShowPrevVersion and (VVersionIDInfo.YoungestTileVersionID <> $FFFF) then begin
        // show yougest versioned tile
        Result := TBerkeleyDBVersionedKey.Create(ATileXY, VVersionIDInfo.YoungestTileVersionID);
      end;
    end;
  end else begin
    // show not versioned tile
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
  const AData: IBinaryData;
  const AKeepExisting: Boolean
): Boolean;
var
  I: Integer;
  VKey: IBinaryData;
  VValue: IBinaryData;
  VDatabase: IBerkeleyDB;
  VTile: Pointer;
  VSize: Integer;
  VTileCRC: Cardinal;
  VIsDeadLock: Boolean;
  VTransaction: PBerkeleyTxn;
  VVersionID: Word;
  VVersionIDInfo: TVersionIDInfo;
  VVersionInfo: IMapVersionInfo;
  VVersionedMeta: IBerkeleyDBVersionedMetaValue;
  VMetaElement: IBerkeleyDBVersionedMetaValueElement;
begin
  Result := False;
  
  FSyncLock.BeginRead;
  try
    VDatabase := FEnvironment.Acquire(ADatabaseFileName);
    try
      VDatabase.LockWrite;
      try
        if Assigned(AData) then begin
          VTile := AData.Buffer;
          VSize := AData.Size;
          VTileCRC := CRC32Buf(VTile, VSize);
        end else begin // will save TNE info
          VTile := nil;
          VSize := 0;
          VTileCRC := 0;
        end;

        VVersionInfo := CheckVersionInfo(AVersionInfo);
        VValue := TBerkeleyDBValue.Create(VTile, VSize, ATileDate, VVersionInfo, ATileContetType);

        if FIsVersioned then begin
          I := 0;
          VIsDeadLock := False;
          repeat
            FEnvironment.TransactionBegin(VTransaction);
            try
              VKey := TBerkeleyDBVersionedMetaKey.Create(ATileXY);
              VVersionedMeta := ReadVersionedMetaValue(VKey, VTransaction, VDatabase, VIsDeadLock);

              if VIsDeadLock then begin
                FEnvironment.TransactionAbort(VTransaction);
                Sleep(50);
                Continue;
              end;

              if not Assigned(VVersionedMeta) then begin
                VVersionedMeta := TBerkeleyDBVersionedMetaValue.Create;
              end;

              VVersionID := GetTileVersionID(VTileCRC, VVersionInfo, VVersionedMeta, @VVersionIDInfo);

              if VVersionIDInfo.IsSameCRCFound then begin
                FEnvironment.TransactionAbort(VTransaction);
                Exit;
              end;

              if AKeepExisting and VVersionIDInfo.IsSameVersionFound then begin
                FEnvironment.TransactionAbort(VTransaction);
                Exit;
              end;

              VMetaElement :=
                TBerkeleyDBVersionedMetaValueElement.Create(
                  VVersionID,
                  0, // TileZOrder
                  VSize,
                  ATileDate,
                  VTileCRC,
                  VVersionInfo,
                  ATileContetType
                );

              if VVersionIDInfo.TileIndexInMetaValue <> -1 then begin
                VVersionedMeta.Replace(VVersionIDInfo.TileIndexInMetaValue, VMetaElement);
              end else begin
                VVersionedMeta.Add(VMetaElement);
              end;

              if VDatabase.Write(VKey, (VVersionedMeta as IBinaryData), VTransaction, VIsDeadLock) then begin
                VKey := TBerkeleyDBVersionedKey.Create(ATileXY, VVersionID);
                if VDatabase.Write(VKey, VValue, VTransaction, VIsDeadLock) then begin
                  FEnvironment.TransactionCommit(VTransaction);
                  Result := True;
                  Exit;
                end else if VIsDeadLock then begin
                  FEnvironment.TransactionAbort(VTransaction);
                  Sleep(50);
                  Continue;
                end else begin
                  FEnvironment.TransactionAbort(VTransaction);
                  Assert(False);
                  Exit;
                end;
              end else if VIsDeadLock then begin
                FEnvironment.TransactionAbort(VTransaction);
                Sleep(50);
                Continue;
              end else begin
                FEnvironment.TransactionAbort(VTransaction);
                Assert(False);
                Exit;
              end;
            except
              FEnvironment.TransactionAbort(VTransaction);
              raise;
            end;
            Inc(I);
          until I > FOnDeadLockRetryCount;
          if VIsDeadLock and (not Result) then begin
            CheckBDB(DB_LOCK_DEADLOCK); // raise exception about deadlock
          end;
        end else begin
          VKey := TBerkeleyDBKey.Create(ATileXY);
          // check if need to keep existing tile
          if AKeepExisting and VDatabase.Exists(VKey) then begin
            Exit;
          end;
          // save tile
          Result := VDatabase.Write(VKey, VValue);
        end;
      finally
        VDatabase.UnLockWrite;
      end;
    finally
      FEnvironment.Release(VDatabase);
    end;
  finally
    FSyncLock.EndRead;
  end;
end;

function TTileStorageBerkeleyDBHelper.DeleteTile(
  const ADatabaseFileName: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  I: Integer;
  VKey, VMetaKey: IBinaryData;
  VDatabase: IBerkeleyDB;
  VIsDeadLock: Boolean;
  VTransaction: PBerkeleyTxn;
  VVersionID: Word;
  VVersionIDInfo: TVersionIDInfo;
  VVersionInfo: IMapVersionInfo;
  VVersionedMeta: IBerkeleyDBVersionedMetaValue;
begin
  Assert(FileExists(ADatabaseFileName));

  Result := False;

  FSyncLock.BeginRead;
  try
    VDatabase := FEnvironment.Acquire(ADatabaseFileName);
    try
      VDatabase.LockWrite;
      try
        if FIsVersioned then begin
          I := 0;
          VIsDeadLock := False;
          repeat
            FEnvironment.TransactionBegin(VTransaction);
            try
              VMetaKey := TBerkeleyDBVersionedMetaKey.Create(ATileXY);
              VVersionedMeta := ReadVersionedMetaValue(VMetaKey, VTransaction, VDatabase, VIsDeadLock);

              if VIsDeadLock then begin
                FEnvironment.TransactionAbort(VTransaction);
                Sleep(50);
                Continue;
              end;

              if not Assigned(VVersionedMeta) then begin
                FEnvironment.TransactionAbort(VTransaction);
                Exit;
              end;

              VVersionInfo := CheckVersionInfo(AVersionInfo);
              VVersionID := GetTileVersionID(0, VVersionInfo, VVersionedMeta, @VVersionIDInfo);

              if VVersionIDInfo.IsSameVersionFound then begin
                VKey := TBerkeleyDBVersionedKey.Create(ATileXY, VVersionID);
                VVersionedMeta.Del(VVersionIDInfo.TileIndexInMetaValue);
                if VVersionedMeta.ItemsCount > 0 then begin
                  if VDatabase.Write(VMetaKey, (VVersionedMeta as IBinaryData), VTransaction, VIsDeadLock) then begin
                    if VDatabase.Del(VKey, VTransaction, VIsDeadLock) then begin
                      FEnvironment.TransactionCommit(VTransaction);
                      Result := True;
                      Exit;
                    end else if VIsDeadLock then begin
                      FEnvironment.TransactionAbort(VTransaction);
                      Sleep(50);
                      Continue;
                    end else begin
                      FEnvironment.TransactionAbort(VTransaction);
                      Assert(False);
                      Exit;
                    end;
                  end else if VIsDeadLock then begin
                    FEnvironment.TransactionAbort(VTransaction);
                    Sleep(50);
                    Continue;
                  end else begin
                    FEnvironment.TransactionAbort(VTransaction);
                    Assert(False);
                    Exit;
                  end;
                end else begin
                  if VDatabase.Del(VMetaKey, VTransaction, VIsDeadLock) then begin
                    if VDatabase.Del(VKey, VTransaction, VIsDeadLock) then begin
                      FEnvironment.TransactionCommit(VTransaction);
                      Result := True;
                      Exit;
                    end else if VIsDeadLock then begin
                      FEnvironment.TransactionAbort(VTransaction);
                      Sleep(50);
                      Continue;
                    end else begin
                      FEnvironment.TransactionAbort(VTransaction);
                      Assert(False);
                      Exit;
                    end;
                  end else if VIsDeadLock then begin
                    FEnvironment.TransactionAbort(VTransaction);
                    Sleep(50);
                    Continue;
                  end else begin
                    FEnvironment.TransactionAbort(VTransaction);
                    Assert(False);
                    Exit;
                  end;
                end;
              end else begin
                FEnvironment.TransactionAbort(VTransaction);
                Exit;
              end;
            except
              FEnvironment.TransactionAbort(VTransaction);
              raise;
            end;
            Inc(I);
          until I > FOnDeadLockRetryCount;
          if VIsDeadLock and (not Result) then begin
            CheckBDB(DB_LOCK_DEADLOCK); // raise exception about deadlock
          end;
        end else begin
          VKey := TBerkeleyDBKey.Create(ATileXY);
          Result := VDatabase.Del(VKey);
        end;
      finally
        VDatabase.UnLockWrite;
      end;
    finally
      FEnvironment.Release(VDatabase);
    end;
  finally
    FSyncLock.EndRead;
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
  VFoundBadValue: Boolean;
begin
  Assert(FileExists(ADatabaseFileName));

  Result := False;
  VFoundBadValue := False;

  FSyncLock.BeginRead;
  try
    VDatabase := FEnvironment.Acquire(ADatabaseFileName);
    try
      VDatabase.LockRead;
      try
        VKey := GetReadOnlyKey(ATileXY, AVersionInfo, VDatabase);
        if Assigned(VKey) then begin
          VBinValue := VDatabase.Read(VKey);
          if Assigned(VBinValue) then begin
            try
              VValue := TBerkeleyDBValue.Create(VBinValue);
              if Assigned(VValue) and (VValue.TileSize > 0) and (VValue.TileBody <> nil) then begin
                ATileBinaryData := TBinaryDataByBerkeleyDBValue.Create(VValue);
                ATileVersion := VValue.TileVersionInfo;
                ATileContentType := VValue.TileContentType;
                ATileDate := VValue.TileDate;
                Result := True;
              end;
            except
              on E: EBerkeleyDBBadValue do begin
                VFoundBadValue := True;
                FGlobalBerkeleyDBHelper.LogException(E.ClassName + ': ' + E.Message);
              end;
            else
              raise;
            end;
          end;
        end;
      finally
        VDatabase.UnLockRead;
      end;
    finally
      FEnvironment.Release(VDatabase);
    end;
  finally
    FSyncLock.EndRead;
  end;

  if VFoundBadValue and not FIsReadOnly then begin
    Self.DeleteTile(ADatabaseFileName, ATileXY, ATileZoom, AVersionInfo);
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
  VList: IInterfaceListSimple;
  VTileInfoIndex: Integer;
  VYoungestTileIndex: Integer;
  VYoungestTileDate: TDateTime;
  VVersionInfo: IMapVersionInfo;
  VMapVersionInfo: IMapVersionInfo;
  VFoundBadValue: Boolean;
begin
  Assert(FileExists(ADatabaseFileName));

  Result := False;
  VFoundBadValue := False;
  ATileVersionListStatic := nil;
  VVersionInfo := CheckVersionInfo(AVersionInfo);

  FSyncLock.BeginRead;
  try
    VDatabase := FEnvironment.Acquire(ADatabaseFileName);
    try
      VDatabase.LockRead;
      try
        if FIsVersioned then begin
          VKey := TBerkeleyDBVersionedMetaKey.Create(ATileXY);
          VBinValue := VDatabase.Read(VKey);
          if Assigned(VBinValue) then begin
            VTileInfoIndex := -1;
            VYoungestTileIndex := -1;
            VYoungestTileDate := 0;

            if not ASingleTileInfo then begin
              VList := TInterfaceListSimple.Create;
            end else begin
              VList := nil;
            end;

            try
              VVersionMeta := TBerkeleyDBVersionedMetaValue.Create(VBinValue);
            except
              on E: EBerkeleyDBBadValue do begin
                VFoundBadValue := True;
                FGlobalBerkeleyDBHelper.LogException(E.ClassName + ': ' + E.Message);
              end;
            else
              raise;
            end;

            if not VFoundBadValue then begin
              for I := 0 to VVersionMeta.ItemsCount - 1 do begin
                VMetaElement := VVersionMeta.Item[I];
                if Assigned(VList) and (VMetaElement.TileVersionInfo <> '') then begin
                  VMapVersionInfo :=
                    FMapVersionFactory.CreateByStoreString(
                      VMetaElement.TileVersionInfo,
                      VVersionInfo.ShowPrevVersion
                    );
                  VList.Add(VMapVersionInfo);
                end;
                if WideSameText(VMetaElement.TileVersionInfo, VVersionInfo.StoreString) then begin
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
                  if VVersionInfo.ShowPrevVersion then begin
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
                  ATileVersionListStatic := TMapVersionListStatic.Create(VList.MakeStaticAndClear);
                  Result := True;
                end;
              end;
            end;
          end;
        end else begin
          VKey := TBerkeleyDBKey.Create(ATileXY);
          VBinValue := VDatabase.Read(VKey);
          if Assigned(VBinValue) then begin

            try
              VValue := TBerkeleyDBValue.Create(VBinValue);
            except
              on E: EBerkeleyDBBadValue do begin
                VFoundBadValue := True;
                FGlobalBerkeleyDBHelper.LogException(E.ClassName + ': ' + E.Message);
              end;
            else
              raise;
            end;

            if not VFoundBadValue then begin
              ATileVersion := VValue.TileVersionInfo;
              ATileContentType := VValue.TileContentType;
              ATileDate := VValue.TileDate;
              ATileSize := VValue.TileSize;
              Result := True;
            end;
          end;
        end;
      finally
        VDatabase.UnLockRead;
      end;
    finally
      FEnvironment.Release(VDatabase);
    end;
  finally
    FSyncLock.EndRead;
  end;

  if VFoundBadValue and not FIsReadOnly then begin
    Self.DeleteTile(ADatabaseFileName, ATileXY, ATileZoom, AVersionInfo);
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
  Assert(FileExists(ADatabaseFileName));

  Result := False;

  FSyncLock.BeginRead;
  try
    VDatabase := FEnvironment.Acquire(ADatabaseFileName);
    try
      VDatabase.LockRead;
      try
        VKey := GetReadOnlyKey(ATileXY, AVersionInfo, VDatabase);
        if Assigned(VKey) then begin
          Result := VDatabase.Exists(VKey);
        end;
      finally
        VDatabase.UnLockRead;
      end;
    finally
      FEnvironment.Release(VDatabase);
    end;
  finally
    FSyncLock.EndRead;
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
  VFoundBadValue: Boolean;
begin
  Assert(FileExists(ADatabaseFileName));

  Result := False;
  VFoundBadValue := False;

  FSyncLock.BeginRead;
  try
    VDatabase := FEnvironment.Acquire(ADatabaseFileName);
    try
      VDatabase.LockRead;
      try
        VKey := GetReadOnlyKey(ATileXY, AVersionInfo, VDatabase);
        if Assigned(VKey) then begin
          VBinValue := VDatabase.Read(VKey);
          if Assigned(VBinValue) then begin
            try
              VValue := TBerkeleyDBValue.Create(VBinValue);
              ATileDate := VValue.TileDate;
              Result := True;
            except
              on E: EBerkeleyDBBadValue do begin
                VFoundBadValue := True;
                FGlobalBerkeleyDBHelper.LogException(E.ClassName + ': ' + E.Message);
              end;
            else
              raise;
            end;
          end;
        end;
      finally
        VDatabase.UnLockRead;
      end;
    finally
      FEnvironment.Release(VDatabase);
    end;
  finally
    FSyncLock.EndRead;
  end;

  if VFoundBadValue and not FIsReadOnly then begin
    Self.DeleteTile(ADatabaseFileName, ATileXY, ATileZoom, AVersionInfo);
  end;
end;

procedure TTileStorageBerkeleyDBHelper.Sync(out AHotDatabaseCount: Integer);
begin
  Assert(FEnvironment <> nil);

  FSyncLock.BeginWrite;
  try
    FEnvironment.Sync(AHotDatabaseCount);
  finally
    FSyncLock.EndWrite;
  end;
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
  Assert(FileExists(ADatabaseFileName));

  Result := False;
  SetLength(ATileExistsArray, 0);

  FSyncLock.BeginRead;
  try
    VDatabase := FEnvironment.Acquire(ADatabaseFileName);
    try
      VDatabase.LockRead;
      try
        if VDatabase.CreateExistsKeyArray(VList) then begin
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
        end;
      finally
        VDatabase.UnLockRead;
      end;
    finally
      FEnvironment.Release(VDatabase);
    end;
  finally
    FSyncLock.EndRead;
  end;
end;

function TTileStorageBerkeleyDBHelper.GetRefCount: Integer;
begin
  Result := Self.RefCount;
end;

end.

