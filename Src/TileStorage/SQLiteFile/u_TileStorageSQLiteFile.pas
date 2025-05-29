{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_TileStorageSQLiteFile;

interface

uses
  Types,
  SysUtils,
  t_TileStorageSQLiteFile,
  i_BinaryData,
  i_MapVersionInfo,
  i_MapVersionFactory,
  i_MapVersionRequest,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_ProjectionSet,
  i_NotifierOperation,
  i_NotifierTilePyramidUpdate,
  i_TileStorageAbilities,
  i_TileStorage,
  i_TileInfoBasicMemCache,
  i_TileStorageSQLiteFileConnectionBuilder,
  u_TileStorageSQLiteFileConnectionPool,
  u_TileStorageAbstract;

type
  TTileStorageSQLiteFile = class(TTileStorageAbstract)
  private
    FFileName: string;
    FMainContentType: IContentTypeInfoBasic;
    FTileNotExistsTileInfo: ITileInfoBasic;
    FTileInfoMemCache: ITileInfoBasicMemCache;
    FConnectionPool: TTileStorageSQLiteFileConnectionPool;
    FConnectionBuilder: ITileStorageSQLiteFileConnectionBuilder;
    FLock: IReadWriteSync;
  protected
    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): string; override;

    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; override;

    function GetTileInfoEx(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionRequest;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; override;

    function GetTileRectInfo(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ARect: TRect;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionRequest
    ): ITileRectInfo; override;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    function SaveTile(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData;
      const AIsOverwrite: Boolean
    ): Boolean; override;

    function ScanTiles(
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    ): IEnumTileInfo; override;
  public
    constructor Create(
      const AStorageTypeAbilities: ITileStorageTypeAbilities;
      const AStorageForceAbilities: ITileStorageAbilities;
      const ATileInfoMemCache: ITileInfoBasicMemCache;
      const AProjectionSet: IProjectionSet;
      const ATileNotifier: INotifierTilePyramidUpdateInternal;
      const AMapVersionFactory: IMapVersionFactory;
      const AMainContentType: IContentTypeInfoBasic;
      const AFileName: string;
      const AFormatId: TTileStorageSQLiteFileFormatId
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_EnumTileInfoBySQLiteFile,
  u_Synchronizer,
  u_TileStorageSQLiteFileConnection,
  u_TileStorageSQLiteFileConnectionBuilder,
  u_TileInfoBasic;

{ TTileStorageSQLiteFile }

constructor TTileStorageSQLiteFile.Create(
  const AStorageTypeAbilities: ITileStorageTypeAbilities;
  const AStorageForceAbilities: ITileStorageAbilities;
  const ATileInfoMemCache: ITileInfoBasicMemCache;
  const AProjectionSet: IProjectionSet;
  const ATileNotifier: INotifierTilePyramidUpdateInternal;
  const AMapVersionFactory: IMapVersionFactory;
  const AMainContentType: IContentTypeInfoBasic;
  const AFileName: string;
  const AFormatId: TTileStorageSQLiteFileFormatId
);
begin
  inherited Create(
    AStorageTypeAbilities,
    AStorageForceAbilities,
    AMapVersionFactory,
    AProjectionSet,
    ATileNotifier,
    ''
  );

  FMainContentType := AMainContentType;

  FTileInfoMemCache := ATileInfoMemCache;
  if Assigned(FTileInfoMemCache) then begin
    FTileInfoMemCache.OnTileInfoUpdate := Self.NotifyTileUpdate;
  end;

  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);

  FFileName :=
    TTileStorageSQLiteFileConnectionBuilder.PrepareFileName(
      AFileName,
      AFormatId
    );

  FConnectionBuilder :=
    TTileStorageSQLiteFileConnectionBuilder.Create(
      FFileName,
      StorageStateInternal,
      FMainContentType,
      AProjectionSet,
      AFormatId
    );

  FConnectionPool := TTileStorageSQLiteFileConnectionPool.Create(FConnectionBuilder);

  FLock := GSync.SyncStd.Make(Self.ClassName);
end;

destructor TTileStorageSQLiteFile.Destroy;
begin
  if Assigned(FTileInfoMemCache) then begin
    FTileInfoMemCache.OnTileInfoUpdate := nil;
  end;
  FreeAndNil(FConnectionPool);
  inherited Destroy;
end;

function TTileStorageSQLiteFile.GetTileFileName(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): string;
begin
  Result := FFileName;
end;

function TTileStorageSQLiteFile.GetTileInfo(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  VConnection: TTileStorageSQLiteFileConnection;
begin
  Result := nil;

  if not StorageStateInternal.ReadAccess then begin
    Exit;
  end;

  if Assigned(FTileInfoMemCache) then begin
    Result := FTileInfoMemCache.Get(AXY, AZoom, nil, AMode, True);
    if Result <> nil then begin
      Exit;
    end;
  end;

  VConnection := FConnectionPool.Acquire;
  try
    FLock.BeginRead;
    try
      Result := VConnection.FetchOne(AXY, AZoom, AMode);
    finally
      FLock.EndRead;
    end;
    if Result = nil then begin
      Result := FTileNotExistsTileInfo;
    end;
  finally
    FConnectionPool.Release(VConnection);
  end;

  if Assigned(FTileInfoMemCache) then begin
    FTileInfoMemCache.Add(AXY, AZoom, nil, Result);
  end;
end;

function TTileStorageSQLiteFile.GetTileInfoEx(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionRequest;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
begin
  Result := Self.GetTileInfo(AXY, AZoom, nil, AMode);
end;

function TTileStorageSQLiteFile.GetTileRectInfo(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ARect: TRect;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionRequest
): ITileRectInfo;
var
  VRect: TRect;
  VCount: TPoint;
  VConnection: TTileStorageSQLiteFileConnection;
begin
  Result := nil;

  if not StorageStateInternal.ReadAccess then begin
    Exit;
  end;

  VRect := ARect;
  ProjectionSet.Zooms[AZoom].ValidateTileRect(VRect);

  VCount.X := VRect.Right - VRect.Left;
  VCount.Y := VRect.Bottom - VRect.Top;

  if (VCount.X > 0) and (VCount.Y > 0) and (VCount.X <= 2048) and (VCount.Y <= 2048) then begin
    VConnection := FConnectionPool.Acquire;
    try
      FLock.BeginRead;
      try
        Result := VConnection.FetchRectInfo(ARect, AZoom, AOperationID, ACancelNotifier);
      finally
        FLock.EndRead;
      end;
    finally
      FConnectionPool.Release(VConnection);
    end;
  end;
end;

function TTileStorageSQLiteFile.SaveTile(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AContentType: IContentTypeInfoBasic;
  const AData: IBinaryData;
  const AIsOverwrite: Boolean
): Boolean;
var
  VConnection: TTileStorageSQLiteFileConnection;
begin
  Result := False;

  if AIsOverwrite then begin
    if not StorageStateInternal.ReplaceAccess then begin
      Exit;
    end;
  end else begin
    if not StorageStateInternal.AddAccess then begin
      Exit;
    end;
  end;

  if Assigned(AContentType) then begin
    if not FMainContentType.CheckOtherForSaveCompatible(AContentType) then begin
      raise Exception.Create('Bad content type for this tile storage');
    end;
  end;

  if Assigned(AContentType) and Assigned(AData) then begin
    VConnection := FConnectionPool.Acquire;
    try
      FLock.BeginWrite;
      try
        Result := VConnection.Insert(AXY, AZoom, ALoadDate, AData, AIsOverwrite);
      finally
        FLock.EndWrite;
      end;
    finally
      FConnectionPool.Release(VConnection);
    end;

    if Result then begin
      if Assigned(FTileInfoMemCache) then begin
        FTileInfoMemCache.Add(
          AXY, AZoom, nil, TTileInfoBasicExistsWithTile.Create(ALoadDate, AData, nil, FMainContentType)
        );
      end;
      NotifyTileUpdate(AXY, AZoom, nil);
    end;
  end else begin
    // TNE is not supported, all we can do here is delete existing tile (if any)
    Result := DeleteTile(AXY, AZoom, AVersionInfo);
  end;
end;

function TTileStorageSQLiteFile.DeleteTile(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VConnection: TTileStorageSQLiteFileConnection;
begin
  Result := False;

  if not StorageStateInternal.DeleteAccess then begin
    Exit;
  end;

  VConnection := FConnectionPool.Acquire;
  try
    FLock.BeginWrite;
    try
      Result := VConnection.Delete(AXY, AZoom);
    finally
      FLock.EndWrite;
    end;
  finally
    FConnectionPool.Release(VConnection);
  end;

  if Result then begin
    if Assigned(FTileInfoMemCache) then begin
      FTileInfoMemCache.Add(AXY, AZoom, nil, FTileNotExistsTileInfo);
    end;
    NotifyTileUpdate(AXY, AZoom, nil);
  end;
end;

function TTileStorageSQLiteFile.ScanTiles(
  const AIgnoreTNE, AIgnoreMultiVersionTiles: Boolean
): IEnumTileInfo;
begin
  if StorageStateInternal.ScanAccess then begin
    Result := TEnumTileInfoBySQLiteFile.Create(FConnectionBuilder, FLock);
  end else begin
    Result := nil;
  end;
end;

end.
