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

unit u_TileStorageAbstract;

interface

uses
  Types,
  SysUtils,
  i_Notifier,
  i_Listener,
  i_BinaryData,
  i_CoordConverter,
  i_SimpleTileStorageConfig,
  i_ContentTypeInfo,
  i_MapVersionInfo,
  i_MapVersionConfig,
  i_StorageTypeAbilities,
  i_StorageState,
  i_StorageStateInternal,
  i_TileInfoBasic,
  i_NotifierTileRectUpdate,
  u_MapTypeCacheConfig;

type
  TGetTileInfoMode = (gtimWithData = 1, gtimWithoutData = -1, gtimAsIs = 0);

  TOnTileStorageScan = function(
      Sender: TObject;
      const ATileNameInCache: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const ATileInfo: ITileInfoBasic;
      const ATileBinaryData: IBinaryData
    ): Boolean of object;

  TTileStorageAbstract = class
  private
    FConfig: ISimpleTileStorageConfig;
    FMapVersionFactory: IMapVersionFactory;
    FMinValidZoom: Byte;
    FMaxValidZoom: Byte;
    FNotifierByZoom: array of INotifierTileRectUpdate;
    FConfigListener: IListener;
    FStorageState: IStorageStateChangeble;
    FStorageStateListener: IListener;
    FStorageStateStatic: IStorageStateStatic;
    FStorageStateStaticCS: IReadWriteSync;
    FStorageStateInternal: IStorageStateInternal;
    FNotifierByZoomInternal: array of INotifierTileRectUpdateInternal;
    function GetNotifierByZoom(AZoom: Byte): INotifierTileRectUpdate;
    function GetNotifierByZoomInternal(
      AZoom: Byte): INotifierTileRectUpdateInternal;
    procedure OnStateChange;
    procedure OnConfigChange;
    function GetStorageStateStatic: IStorageStateStatic;
    property NotifierByZoomInternal[AZoom: Byte]: INotifierTileRectUpdateInternal read GetNotifierByZoomInternal;
  protected
    procedure NotifyTileUpdate(
      const ATile: TPoint;
      const AZoom: Byte;
      const AVersion: IMapVersionInfo
    );
    property StorageStateStatic: IStorageStateStatic read GetStorageStateStatic;
    property StorageStateInternal: IStorageStateInternal read FStorageStateInternal;
    property Config: ISimpleTileStorageConfig read FConfig;
  public
    constructor Create(
      const AStorageTypeAbilities: IStorageTypeAbilities;
      const AMapVersionFactory: IMapVersionFactory;
      const AConfig: ISimpleTileStorageConfig
    );
    destructor Destroy; override;
    function GetMainContentType: IContentTypeInfoBasic; virtual; abstract;
    function GetAllowDifferentContentTypes: Boolean; virtual; abstract;

    function GetCacheConfig: TMapTypeCacheConfigAbstract; virtual; abstract;

    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): string; virtual; abstract;
    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; virtual; abstract;
    function GetTileRectInfo(
      const ARect: TRect;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileRectInfo; virtual; abstract;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; virtual; abstract;
    function DeleteTNE(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; virtual; abstract;
    procedure SaveTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AData: IBinaryData
    ); virtual; abstract;
    procedure SaveTNE(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ); virtual; abstract;

    function GetListOfTileVersions(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): IMapVersionListStatic; virtual;

    function ScanTiles(
      const AIgnoreTNE: Boolean
    ): IEnumTileInfo; virtual;

    property State: IStorageStateChangeble read FStorageState;
    property MapVersionFactory: IMapVersionFactory read FMapVersionFactory;
    property NotifierByZoom[AZoom: Byte]: INotifierTileRectUpdate read GetNotifierByZoom;
  end;

implementation

uses
  u_Synchronizer,
  t_CommonTypes,
  u_ListenerByEvent,
  i_TileKey,
  u_TileKey,
  u_NotifierTileRectUpdate,
  u_StorageStateInternal;

{ TTileStorageAbstract }

constructor TTileStorageAbstract.Create(
  const AStorageTypeAbilities: IStorageTypeAbilities;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ISimpleTileStorageConfig
);
var
  VCount: Integer;
  i: Integer;
  VNotifier: TNotifierTileRectUpdate;
  VState: TStorageStateInternal;
begin
  inherited Create;
  FConfig := AConfig;
  FMapVersionFactory := AMapVersionFactory;
  FStorageStateStaticCS := MakeSyncRW_Var(Self);

  VState := TStorageStateInternal.Create(AStorageTypeAbilities);
  FStorageStateInternal := VState;
  FStorageState := VState;

  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigListener);
  OnConfigChange;

  FStorageStateListener := TNotifyNoMmgEventListener.Create(Self.OnStateChange);
  FStorageState.ChangeNotifier.Add(FStorageStateListener);
  OnStateChange;

  FMinValidZoom := FConfig.CoordConverter.MinZoom;
  FMaxValidZoom := FConfig.CoordConverter.MaxZoom;
  Assert(FMinValidZoom <= FMaxValidZoom);
  VCount := FMaxValidZoom - FMinValidZoom + 1;
  SetLength(FNotifierByZoom, VCount);
  SetLength(FNotifierByZoomInternal, VCount);
  for i := 0 to VCount - 1 do begin
    VNotifier := TNotifierTileRectUpdate.Create(FMinValidZoom + i, FConfig.CoordConverter);
    FNotifierByZoom[i] := VNotifier;
    FNotifierByZoomInternal[i] := VNotifier;
  end;
end;

destructor TTileStorageAbstract.Destroy;
var
  i: Integer;
begin
  FStorageState.ChangeNotifier.Remove(FStorageStateListener);
  FStorageStateListener := nil;

  FConfig.ChangeNotifier.Remove(FConfigListener);
  FConfigListener := nil;

  for i := 0 to Length(FNotifierByZoom) - 1 do begin
    FNotifierByZoom[i] := nil;
  end;
  for i := 0 to Length(FNotifierByZoomInternal) - 1 do begin
    FNotifierByZoomInternal[i] := nil;
  end;
  FStorageStateStaticCS := nil;
  inherited;
end;

function TTileStorageAbstract.GetListOfTileVersions(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): IMapVersionListStatic;
begin
  Result := nil;
end;

function TTileStorageAbstract.GetNotifierByZoom(
  AZoom: Byte
): INotifierTileRectUpdate;
begin
  Result := nil;
  if (AZoom >= FMinValidZoom) and (AZoom <= FMaxValidZoom) then begin
    Result := FNotifierByZoom[AZoom - FMinValidZoom];
  end;
end;

function TTileStorageAbstract.GetNotifierByZoomInternal(
  AZoom: Byte): INotifierTileRectUpdateInternal;
begin
  Result := FNotifierByZoomInternal[AZoom - FMinValidZoom];
end;

function TTileStorageAbstract.GetStorageStateStatic: IStorageStateStatic;
begin
  FStorageStateStaticCS.BeginRead;
  try
    Result := FStorageStateStatic;
  finally
    FStorageStateStaticCS.EndRead;
  end;
end;

procedure TTileStorageAbstract.NotifyTileUpdate(
  const ATile: TPoint;
  const AZoom: Byte;
  const AVersion: IMapVersionInfo
);
var
  VKey: ITileKey;
  VNotifier: INotifierTileRectUpdateInternal;
begin
  VNotifier := NotifierByZoomInternal[AZoom];
  if VNotifier <> nil then begin
    VKey := TTileKey.Create(ATile, AZoom, AVersion);
    VNotifier.TileUpdateNotify(VKey);
  end;
end;

procedure TTileStorageAbstract.OnConfigChange;
var
  VConfig: ISimpleTileStorageConfigStatic;
begin
  VConfig := FConfig.GetStatic;
  FStorageStateInternal.LockWrite;
  try
    if VConfig.IsReadOnly then begin
      FStorageStateInternal.WriteAccess := asDisabled;
    end else begin
      FStorageStateInternal.WriteAccess := asUnknown;
      if VConfig.AllowAdd then begin
        FStorageStateInternal.AddAccess := asUnknown;
      end else begin
        FStorageStateInternal.AddAccess := asDisabled;
      end;
      if VConfig.AllowDelete then begin
        FStorageStateInternal.DeleteAccess := asUnknown;
      end else begin
        FStorageStateInternal.DeleteAccess := asDisabled;
      end;
      if VConfig.AllowReplace then begin
        FStorageStateInternal.ReplaceAccess := asUnknown;
      end else begin
        FStorageStateInternal.ReplaceAccess := asDisabled;
      end;
    end;
  finally
    FStorageStateInternal.UnlockWrite;
  end;
end;

procedure TTileStorageAbstract.OnStateChange;
begin
  FStorageStateStaticCS.BeginWrite;
  try
    FStorageStateStatic := FStorageState.GetStatic;
  finally
    FStorageStateStaticCS.EndWrite;
  end;
end;

function TTileStorageAbstract.ScanTiles(
  const AIgnoreTNE: Boolean): IEnumTileInfo;
begin
  Result := nil;
end;

end.
