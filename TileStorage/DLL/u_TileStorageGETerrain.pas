{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_TileStorageGETerrain deprecated;

interface

uses
  Windows,
  SysUtils,
  Classes,
  t_CommonTypes,
  t_DLLCache,
  i_Notifier,
  i_TerrainStorage,
  i_TileInfoBasic,
  u_BaseInterfacedObject;

type
  TTileStorageDLLTerrain = class(TBaseInterfacedObject, ITerrainStorage)
  private
    FReadAccess: TAccesState;
    FStoragePath: string;
    FTileNotExistsTileInfo: ITileInfoBasic;
    FDLLSync: IReadWriteSync;
    FDLLHandle: THandle;
    FDLLCacheHandle: TDLLCacheHandle;
    FDLLCache_QueryTerrainTile: Pointer;
    FNotifier: INotifier;
    FNotifierInternal: INotifierInternal;
  protected
    function InternalLib_CleanupProc: Boolean; virtual;
    function InternalLib_Initialize: Boolean; virtual;
    function InternalLib_CheckInitialized: Boolean; virtual;
    function InternalLib_Unload: Boolean; virtual;
    function InternalLib_NotifyStateChanged(const AEnabled: Boolean): Boolean;
    function InternalLib_SetPath(const APath: PAnsiChar): Boolean;
    function InternalLib_QueryTerrainTile(const ATileInfo: PQueryTileInfo): Boolean;
  public
    constructor Create(
      const AStoragePath: string
    );
    destructor Destroy; override;

    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: Byte
    ): ITileInfoBasic;

    function SetPath(const APath: string): Boolean;
    function GetAvailable: Boolean;
    function GetNotifier: INotifier;
  end;

  TTileStorageGCTerrain = class(TTileStorageDLLTerrain)
  protected
    function InternalLib_Initialize: Boolean; override;
  end;

implementation

uses
  u_BinaryDataByMemStream,
  u_Notifier,
  u_Synchronizer,
  u_TileInfoBasic;

function HostStateChangedProc(
  const AContext: Pointer;
  const AEnabled: Boolean
): Boolean; stdcall;
begin
  Result := FALSE;
  if (nil <> AContext) then begin
    try
      if TTileStorageDLLTerrain(AContext).InternalLib_NotifyStateChanged(AEnabled) then begin
        Inc(Result);
      end;
    except
    end;
  end;
end;

function DLLCache_QueryTerrainTile_Callback(
  const AContext: Pointer;
  const ATileInfo: PQueryTileInfo;
  const ATileBuffer: Pointer
): Boolean; stdcall;
begin
  Result := FALSE;
  if (nil <> ATileInfo) then begin
    try
      if (nil <> ATileBuffer) and (ATileInfo^.TileSize > 0) and (nil <> ATileInfo^.TileStream) then begin
        with TMemoryStream(ATileInfo^.TileStream) do begin
          WriteBuffer(ATileBuffer^, ATileInfo^.TileSize);
          Position := 0;
        end;
        Result := TRUE;
      end;
    except
    end;
  end;
end;

{ TTerrainTileStorageDLL }

constructor TTileStorageDLLTerrain.Create(
  const AStoragePath: string
);
begin
  inherited Create;
  FStoragePath := IncludeTrailingPathDelimiter(AStoragePath);
  FReadAccess := asUnknown;
  FDLLHandle := 0;
  FDLLCacheHandle := nil;
  FDLLSync := GSync.SyncBig.Make(Self.ClassName);
  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);
  FNotifierInternal := TNotifierBase.Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifier'));
  FNotifier := FNotifierInternal;
  InternalLib_CleanupProc;
  InternalLib_SetPath(PAnsiChar(FStoragePath));
end;

destructor TTileStorageDLLTerrain.Destroy;
begin
  if Assigned(FDLLSync) then begin
    FDLLSync.BeginWrite;
    try
      FReadAccess := asDisabled;
      InternalLib_Unload;
    finally
      FDLLSync.EndWrite;
    end;
  end;
  FTileNotExistsTileInfo := nil;
  FDLLSync := nil;
  FNotifier := nil;
  FNotifierInternal := nil;
  inherited;
end;

function TTileStorageDLLTerrain.GetAvailable: Boolean;
begin
  Result := (FReadAccess = asEnabled);
end;

function TTileStorageDLLTerrain.GetNotifier: INotifier;
begin
  Result := FNotifier;
end;

function TTileStorageDLLTerrain.InternalLib_Initialize: Boolean;
var
  P: Pointer;
begin
  Result := FALSE;
  if (0 <> FDLLHandle) then begin
    P := GetProcAddress(FDLLHandle, 'DLLCache_Init');
    if (nil <> P) then begin
      Result := TDLLCache_Init(P)(@FDLLCacheHandle, 0, Self);
    end;
    if Result then begin
      P := GetProcAddress(FDLLHandle, 'DLLCache_SetInformation');
      if (nil <> P) then begin
        TDLLCache_SetInformation(P)(@FDLLCacheHandle, DLLCACHE_SIC_STATE_CHANGED, 0, @HostStateChangedProc);
      end;
      FDLLCache_QueryTerrainTile := GetProcAddress(FDLLHandle, 'DLLCache_QueryTerrainTile');
    end;
  end;
end;

function TTileStorageDLLTerrain.InternalLib_NotifyStateChanged(const AEnabled: Boolean): Boolean;
begin
  Result := FALSE;
  if AEnabled then begin
    FReadAccess := asEnabled;
  end else begin
    FReadAccess := asDisabled;
  end;
  FNotifierInternal.Notify(nil);
end;

function TTileStorageDLLTerrain.InternalLib_QueryTerrainTile(const ATileInfo: PQueryTileInfo): Boolean;
begin
  Result := FALSE;
  if (nil <> FDLLCache_QueryTerrainTile) then begin
    try
      Result := TDLLCache_QueryTerrainTile(FDLLCache_QueryTerrainTile)(@FDLLCacheHandle, ATileInfo, DLLCache_QueryTerrainTile_Callback);
    except
    end;
  end;
end;

function TTileStorageDLLTerrain.InternalLib_SetPath(const APath: PAnsiChar): Boolean;
var
  P: Pointer;
begin
  Result := FALSE;
  try
    if not DirectoryExists(APath) then begin
      Exit;
    end;
    if (0 = FDLLHandle) then begin
      InternalLib_Initialize;
    end;
    if InternalLib_CheckInitialized then begin
      P := GetProcAddress(FDLLHandle, 'DLLCache_SetPath');
      if (nil <> P) then begin
        Result := TDLLCache_SetPath(P)(@FDLLCacheHandle, APath);
      end;
    end;
  finally
    InternalLib_NotifyStateChanged(Result);
  end;
end;

function TTileStorageDLLTerrain.InternalLib_Unload: Boolean;
var
  P: Pointer;
begin
  Result := FALSE;
  if (0 <> FDLLHandle) then begin
    P := GetProcAddress(FDLLHandle, 'DLLCache_Uninit');
    if (nil <> P) then begin
      TDLLCache_Uninit(P)(@FDLLCacheHandle);
    end;
    Inc(Result);
    FreeLibrary(FDLLHandle);
    FDLLHandle := 0;
    InternalLib_CleanupProc;
    InternalLib_NotifyStateChanged(FALSE);
  end;
end;

function TTileStorageDLLTerrain.InternalLib_CheckInitialized: Boolean;
begin
  Result := (0 <> FDLLHandle) and
    (nil <> FDLLCacheHandle) and
    (nil <> FDLLCache_QueryTerrainTile);
end;

function TTileStorageDLLTerrain.InternalLib_CleanupProc: Boolean;
begin
  Result := FALSE;
  FDLLCache_QueryTerrainTile := nil;
end;

function TTileStorageDLLTerrain.SetPath(const APath: string): Boolean;
begin
  Result := InternalLib_SetPath(PAnsiChar(IncludeTrailingPathDelimiter(APath)));
end;

function TTileStorageDLLTerrain.GetTileInfo(
  const AXY: TPoint;
  const AZoom: Byte
): ITileInfoBasic;
var
  VQTInfo: TQueryTileInfo;
begin
  Result := nil;
  FDLLSync.BeginRead;
  try
    if FReadAccess <> asDisabled then begin
      FillChar(VQTInfo, SizeOf(VQTInfo), #0);
      VQTInfo.Common.Size := SizeOf(VQTInfo);
      VQTInfo.Common.Zoom := AZoom;
      VQTInfo.Common.XY := AXY;
      VQTInfo.Common.FlagsInp := DLLCACHE_QTI_LOAD_TILE;
      VQTInfo.TileStream := TMemoryStream.Create;
      try
        if InternalLib_QueryTerrainTile(@VQTInfo) then begin
          if (VQTInfo.TileSize > 0) then begin
            Result := TTileInfoBasicExistsWithTile.Create(
              VQTInfo.DateOut,
              TBinaryDataByMemStream.CreateWithOwn(TMemoryStream(VQTInfo.TileStream)),
              nil,
              nil
            );
            VQTInfo.TileStream := nil;
          end else begin
            Result := FTileNotExistsTileInfo;
          end;
        end else begin
          Result := FTileNotExistsTileInfo;
        end;
      finally
        TMemoryStream(VQTInfo.TileStream).Free;
      end;
    end;
  finally
    FDLLSync.EndRead;
  end;
end;

{ TTileStorageGCTerrain }

function TTileStorageGCTerrain.InternalLib_Initialize: Boolean;
begin
  if (0 = FDLLHandle) then begin
    FDLLHandle := LoadLibrary('TileStorage_GC.dll');
  end;
  Result := inherited InternalLib_Initialize;
end;

end.
