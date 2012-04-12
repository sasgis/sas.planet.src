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

unit u_TileStorageBerkeleyDB;

interface

uses
  Types,
  Classes,
  SysUtils,
  i_BinaryData,
  i_SimpleTileStorageConfig,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_ContentTypeManager,
  i_TTLCheckNotifier,
  i_TTLCheckListener,
  i_InternalPerformanceCounter,
  u_TileStorageBerkeleyDBHelper,
  u_GlobalCahceConfig,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

{$IFDEF DEBUG}
  {$DEFINE WITH_PERF_COUTER}
{$ENDIF}

type
  TTileStorageBerkeleyDB = class(TTileStorageAbstract)
  private
    FHelper: TTileStorageBerkeleyDBHelper;
    FCacheConfig: TMapTypeCacheConfigBerkeleyDB;
    FMainContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;
    FTileNotExistsTileInfo: ITileInfoBasic;
    FGCList: ITTLCheckNotifier;
    FTTLListener: ITTLCheckListener;
    {$IFDEF WITH_PERF_COUTER}
    FPerfCounterList: IInternalPerformanceCounterList;
    FGetTileInfoCounter: IInternalPerformanceCounter;
    FLoadTileCounter: IInternalPerformanceCounter;
    FDeleteTileCounter: IInternalPerformanceCounter;
    FDeleteTNECounter: IInternalPerformanceCounter;
    FSaveTileCounter: IInternalPerformanceCounter;
    FSaveTNECounter: IInternalPerformanceCounter;
    {$ENDIF}
    procedure OnMapSettingsEdit(Sender: TObject);
  public
    constructor Create(
      AGCList: ITTLCheckNotifier;
      AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      AContentTypeManager: IContentTypeManager;
      APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;

    function GetMainContentType: IContentTypeInfoBasic; override;
    function GetAllowDifferentContentTypes: Boolean; override;

    function GetCacheConfig: TMapTypeCacheConfigAbstract; override;

    function GetTileFileName(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): string; override;

    function GetTileInfo(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): ITileInfoBasic; override;
    function GetTileRectInfo(
      const ARect: TRect;
      const Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): ITileRectInfo; override;

    function LoadTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo;
      out ATileInfo: ITileInfoBasic
    ): IBinaryData; override;

    function DeleteTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    function DeleteTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    procedure SaveTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo;
      AData: IBinaryData
    ); override;

    procedure SaveTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ); override;
  end;

implementation

uses
  t_CommonTypes,
  i_TileIterator,
  u_BinaryDataByMemStream,
  u_MapVersionFactorySimpleString,
  u_TTLCheckListener,
  u_TileRectInfo,
  u_TileFileNameBDB,
  u_TileIteratorByRect,
  u_TileStorageBerkeleyDBRecParser,
  u_TileStorageTypeAbilities,
  u_TileInfoBasic;

{ TTileStorageBerkeleyDB }

constructor TTileStorageBerkeleyDB.Create(
  AGCList: ITTLCheckNotifier;
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  AContentTypeManager: IContentTypeManager;
  APerfCounterList: IInternalPerformanceCounterList
);
const
  CBDBSync = 300000; // 5 min
  CBDBSyncCheckInterval = 60000; // 60 sec
begin
  inherited Create(
    TTileStorageTypeAbilitiesBerkeleyDB.Create,
    TMapVersionFactorySimpleString.Create,
    AConfig
  );

  {$IFDEF WITH_PERF_COUTER}
  FPerfCounterList := APerfCounterList.CreateAndAddNewSubList('BerkeleyDB');
  FGetTileInfoCounter := FPerfCounterList.CreateAndAddNewCounter('GetTileInfo');
  FLoadTileCounter := FPerfCounterList.CreateAndAddNewCounter('LoadTile');
  FDeleteTileCounter := FPerfCounterList.CreateAndAddNewCounter('DeleteTile');
  FDeleteTNECounter := FPerfCounterList.CreateAndAddNewCounter('DeleteTNE');
  FSaveTileCounter := FPerfCounterList.CreateAndAddNewCounter('SaveTile');
  FSaveTNECounter := FPerfCounterList.CreateAndAddNewCounter('SaveTNE');
  {$ENDIF}

  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);
  FCacheConfig := TMapTypeCacheConfigBerkeleyDB.Create(
    AConfig,
    TTileFileNameBDB.Create,
    AGlobalCacheConfig,
    Self.OnMapSettingsEdit
  );
  FContentTypeManager := AContentTypeManager;
  FMainContentType := FContentTypeManager.GetInfoByExt(Config.TileFileExt);
  FHelper := TTileStorageBerkeleyDBHelper.Create(
    FCacheConfig.BasePath,
    AConfig.CoordConverter.Datum.EPSG
  );
  FTTLListener := TTTLCheckListener.Create(
    FHelper.Sync,
    CBDBSync,
    CBDBSyncCheckInterval
  );
  FGCList := AGCList;
  FGCList.Add(FTTLListener);
end;

destructor TTileStorageBerkeleyDB.Destroy;
begin
  if Assigned(FGCList) then begin
    FGCList.Remove(FTTLListener);
    FGCList := nil;
  end;
  FTTLListener := nil;
  FreeAndNil(FHelper);
  FMainContentType := nil;
  FContentTypeManager := nil;
  FreeAndNil(FCacheConfig);
  FTileNotExistsTileInfo := nil;
  inherited;
end;

procedure TTileStorageBerkeleyDB.OnMapSettingsEdit(Sender: TObject);
var
  VCacheConfig: TMapTypeCacheConfigBerkeleyDB;
begin
  if Assigned(FHelper) then begin
    if Sender is TMapTypeCacheConfigBerkeleyDB then begin
      VCacheConfig := Sender as TMapTypeCacheConfigBerkeleyDB;
      if Assigned(VCacheConfig) then begin
        FHelper.ChangeRootPath(VCacheConfig.BasePath);
      end;
    end;
  end;
end;

function TTileStorageBerkeleyDB.GetAllowDifferentContentTypes: Boolean;
begin
  Result := True;
end;

function TTileStorageBerkeleyDB.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageBerkeleyDB.GetMainContentType: IContentTypeInfoBasic;
begin
  Result := FMainContentType;
end;

function TTileStorageBerkeleyDB.GetTileFileName(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): string;
begin
  Result := FCacheConfig.GetTileFileName(AXY, Azoom) + PathDelim +
    'x' + IntToStr(AXY.X) + PathDelim + 'y' + IntToStr(AXY.Y) +
    FMainContentType.GetDefaultExt;
end;

function TTileStorageBerkeleyDB.GetTileInfo(
  AXY: TPoint;
  AZoom: byte;
  AVersionInfo: IMapVersionInfo
): ITileInfoBasic;
var
  VPath: string;
  VResult: Boolean;
  VData: TBDBData;
  VStream: TMemoryStream;
  VTileData: IBinaryData;
{$IFDEF WITH_PERF_COUTER}
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  {$IFDEF WITH_PERF_COUTER}
  VCounterContext := FGetTileInfoCounter.StartOperation;
  try
  {$ENDIF}
    Result := FTileNotExistsTileInfo;
    if StorageStateStatic.ReadAccess <> asDisabled then begin

      VPath := FCacheConfig.GetTileFileName(AXY, AZoom);

      VResult := False;

      if FileExists(VPath) then begin
        VStream := TMemoryStream.Create;
        try
          VResult := FHelper.LoadTile(
            VPath,
            AXY,
            AZoom,
            AVersionInfo,
            VStream,
            VData
          );
          if VResult then begin
            VTileData := TBinaryDataByMemStream.CreateWithOwn(VStream);
            VStream := nil;
            Result := TTileInfoBasicExistsWithTile.Create(
              VData.TileDate,
              VTileData,
              VTileData.Size,
              MapVersionFactory.CreateByStoreString(WideString(VData.TileVer)),
              FContentTypeManager.GetInfo(WideString(VData.TileMIME))
            );
          end;
        finally
          VStream.Free;
        end;
      end;

      if not VResult then begin
        VPath := ChangeFileExt(VPath, '.tne');
        if FileExists(VPath) then begin
          VResult := FHelper.IsTNEFound(
            VPath,
            AXY,
            AZoom,
            AVersionInfo,
            VData
          );
          if VResult then begin
            Result := TTileInfoBasicTNE.Create(VData.TileDate, AVersionInfo);
          end;
        end;
      end;

      if not VResult then begin
        Result := TTileInfoBasicNotExists.Create(0, AVersionInfo);
      end;
    end;
  {$IFDEF WITH_PERF_COUTER}
  finally
    FGetTileInfoCounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

function TTileStorageBerkeleyDB.GetTileRectInfo(
  const ARect: TRect;
  const Azoom: byte;
  AVersionInfo: IMapVersionInfo
): ITileRectInfo;
var
  VRect: TRect;
  VZoom: Byte;
  VCount: TPoint;
  VItems: PTileInfoInternalArray;
  VIndex: Integer;
  VFileName: string;
  VTile: TPoint;
  VIterator: ITileIterator;
  VFolderName: string;
  VPrevFolderName: string;
  VPrevFolderExist: Boolean;
  VFolderExists: Boolean;
begin
  Result := nil;
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    VRect := ARect;
    VZoom := Azoom;
    Config.CoordConverter.CheckTileRect(VRect, VZoom);
    VCount.X := VRect.Right - VRect.Left;
    VCount.Y := VRect.Bottom - VRect.Top;
    if (VCount.X > 0) and (VCount.Y > 0)   then begin
      VItems := GetMemory(VCount.X * VCount.Y * SizeOf(TTileInfoInternal));
      try
        VPrevFolderName := '';
        VPrevFolderExist := False;
          VIterator := TTileIteratorByRect.Create(VRect);
          while VIterator.Next(VTile) do begin
            VIndex := (VTile.Y - VRect.Top) * VCount.X + (VTile.X - VRect.Left);
            VFileName := FCacheConfig.GetTileFileName(VTile, VZoom);
            VFolderName := ExtractFilePath(VFileName);

            if VFolderName = VPrevFolderName then begin
              VFolderExists := VPrevFolderExist;
            end else begin
              VFolderExists := DirectoryExists(VFolderName);
              VPrevFolderName := VFolderName;
              VPrevFolderExist := VFolderExists;
            end;
            if VFolderExists then begin
              //TODO: доделать.
            end else begin
              // neither tile nor tne
              VItems[VIndex].FLoadDate := 0;
              VItems[VIndex].FVersionInfo := nil;
              VItems[VIndex].FContentType := nil;
              VItems[VIndex].FData := nil;
              VItems[VIndex].FSize := 0;
              VItems[VIndex].FInfoType := titNotExists;
            end;
          end;
        Result :=
          TTileRectInfo.CreateWithOwn(
            VRect,
            VZoom,
            VItems
          );
        VItems := nil;
      finally
        if VItems <> nil then begin
          FreeMemory(VItems);
        end;
      end;
    end;
  end;
end;

function TTileStorageBerkeleyDB.LoadTile(
  AXY: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo;
  out ATileInfo: ITileInfoBasic
): IBinaryData;
{$IFDEF WITH_PERF_COUTER}
var
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  {$IFDEF WITH_PERF_COUTER}
  VCounterContext := FLoadTileCounter.StartOperation;
  try
  {$ENDIF}
    Result := nil;
    ATileInfo := FTileNotExistsTileInfo;
    if StorageStateStatic.ReadAccess <> asDisabled then begin
      ATileInfo := GetTileInfo(AXY, AZoom, AVersionInfo);
      if ATileInfo.IsExists then begin
        Result := ATileInfo.TileData;
      end;
    end;
  {$IFDEF WITH_PERF_COUTER}
  finally
    FLoadTileCounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

procedure TTileStorageBerkeleyDB.SaveTile(
  AXY: TPoint;
  AZoom: byte;
  AVersionInfo: IMapVersionInfo;
  AData: IBinaryData
);
var
  VPath: string;
  VResult: Boolean;
{$IFDEF WITH_PERF_COUTER}
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  {$IFDEF WITH_PERF_COUTER}
  VCounterContext := FSaveTileCounter.StartOperation;
  try
  {$ENDIF}
    if StorageStateStatic.WriteAccess <> asDisabled then begin
      VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
      if FHelper.CreateDirIfNotExists(VPath) then begin
        VResult := FHelper.SaveTile(
          VPath,
          AXY,
          AZoom,
          Now,
          AVersionInfo,
          PWideChar(FMainContentType.GetContentType),
          AData
        );
        if VResult then begin
          NotifyTileUpdate(AXY, Azoom, AVersionInfo);
        end;
      end;
    end;
  {$IFDEF WITH_PERF_COUTER}
  finally
    FSaveTileCounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

procedure TTileStorageBerkeleyDB.SaveTNE(
  AXY: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo
);
var
  VPath: String;
  VResult: Boolean;
{$IFDEF WITH_PERF_COUTER}
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  {$IFDEF WITH_PERF_COUTER}
  VCounterContext := FSaveTNECounter.StartOperation;
  try
  {$ENDIF}
    if StorageStateStatic.WriteAccess <> asDisabled then begin
      VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
      VPath := ChangeFileExt(VPath, '.tne');
      if FHelper.CreateDirIfNotExists(VPath) then begin
        VResult := FHelper.SaveTile(
          VPath,
          AXY,
          AZoom,
          Now,
          AVersionInfo,
          PWideChar(FMainContentType.GetContentType),
          nil
        );
        if VResult then begin
          NotifyTileUpdate(AXY, Azoom, AVersionInfo);
        end;
      end;
    end;
  {$IFDEF WITH_PERF_COUTER}
  finally
    FSaveTNECounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

function TTileStorageBerkeleyDB.DeleteTile(
  AXY: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
var
  VPath: string;
{$IFDEF WITH_PERF_COUTER}
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  {$IFDEF WITH_PERF_COUTER}
  VCounterContext := FDeleteTileCounter.StartOperation;
  try
  {$ENDIF}
    Result := False;
    if StorageStateStatic.DeleteAccess <> asDisabled then begin
      try
        VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
        if FileExists(VPath) then begin
          Result := FHelper.DeleteTile(
            VPath,
            AXY,
            AZoom,
            AVersionInfo
          );
        end;
        if not Result then begin
          Result := DeleteTNE(AXY, AZoom, AVersionInfo);
        end;
      except
        Result := False;
      end;
      if Result then begin
        NotifyTileUpdate(AXY, Azoom, AVersionInfo);
      end;
    end;
  {$IFDEF WITH_PERF_COUTER}
  finally
    FDeleteTileCounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

function TTileStorageBerkeleyDB.DeleteTNE(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
var
  VPath: string;
{$IFDEF WITH_PERF_COUTER}
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  {$IFDEF WITH_PERF_COUTER}
  VCounterContext := FDeleteTNECounter.StartOperation;
  try
  {$ENDIF}
    Result := False;
    if StorageStateStatic.DeleteAccess <> asDisabled then begin
      try
        VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
        VPath := ChangeFileExt(VPath, '.tne');
        if FileExists(VPath) then begin
          Result := FHelper.DeleteTile(
            VPath,
            AXY,
            AZoom,
            AVersionInfo
          );
        end;
      except
        Result := False;
      end;
    end;
  {$IFDEF WITH_PERF_COUTER}
  finally
    FDeleteTNECounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

end.
