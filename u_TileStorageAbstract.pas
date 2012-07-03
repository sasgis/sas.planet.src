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
  Classes,
  SysUtils,
  GR32,
  i_Notifier,
  i_Listener,
  i_BinaryData,
  i_FillingMapColorer,
  i_NotifierOperation,
  i_CoordConverter,
  i_SimpleTileStorageConfig,
  i_ContentTypeInfo,
  i_MapVersionInfo,
  i_MapVersionConfig,
  i_StorageTypeAbilities,
  i_StorageState,
  i_StorageStateInternal,
  i_TileInfoBasic,
  i_TileRectUpdateNotifier,
  t_RangeFillingMap,
  u_MapTypeCacheConfig;

type
  TRangeFillingMapEvent = function(
      Sender: TObject;
      const ASourceTilesRect: PRect;
      const AVersionInfo: IMapVersionInfo;
      const ARangeFillingMapInfo: PRangeFillingMapInfo
    ): Boolean of object;

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
    FOnRangeFillingMap: TRangeFillingMapEvent;
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
    property OnRangeFillingMap: TRangeFillingMapEvent read FOnRangeFillingMap write FOnRangeFillingMap;
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
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): string; virtual; abstract;
    function GetTileInfo(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileInfoBasic; virtual; abstract;
    function LoadTile(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileInfo: ITileInfoBasic
    ): IBinaryData; virtual; abstract;
    function DeleteTile(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; virtual; abstract;
    function DeleteTNE(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; virtual; abstract;
    procedure SaveTile(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AData: IBinaryData
    ); virtual; abstract;
    procedure SaveTNE(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ); virtual; abstract;

    function GetListOfTileVersions(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): IMapVersionListStatic; virtual;

    function GetTileRectInfo(
      const ARect: TRect;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileRectInfo; virtual; abstract;

    function LoadFillingMap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      btm: TCustomBitmap32;
      const AXY: TPoint;
      Azoom: byte;
      ASourceZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AColorer: IFillingMapColorer
    ): boolean; virtual;

    function GetRangeFillingMapItemSize: SmallInt; virtual;

    procedure Scan(
      const AOnTileStorageScan: TOnTileStorageScan;
      const AIgnoreTNE: Boolean;
      const ARemoveTileAfterProcess: Boolean
    ); virtual;

    property State: IStorageStateChangeble read FStorageState;
    property MapVersionFactory: IMapVersionFactory read FMapVersionFactory;
    property NotifierByZoom[AZoom: Byte]: INotifierTileRectUpdate read GetNotifierByZoom;
  end;

implementation

uses
  u_Synchronizer,
  t_CommonTypes,
  t_GeoTypes,
  i_TileIterator,
  u_NotifyEventListener,
  i_TileKey,
  u_TileKey,
  u_TileRectUpdateNotifier,
  u_StorageStateInternal,
  u_TileIteratorByRect;

{ TTileStorageAbstract }

constructor TTileStorageAbstract.Create(
  const AStorageTypeAbilities: IStorageTypeAbilities;
  const AMapVersionFactory: IMapVersionFactory;
  const AConfig: ISimpleTileStorageConfig
);
var
  VCount: Integer;
  i: Integer;
  VNotifier: TTileRectUpdateNotifier;
  VState: TStorageStateInternal;
begin
  inherited Create;
  FOnRangeFillingMap := nil;
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
    VNotifier := TTileRectUpdateNotifier.Create(FMinValidZoom + i, FConfig.CoordConverter);
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
  const Azoom: byte;
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

function TTileStorageAbstract.GetRangeFillingMapItemSize: SmallInt;
begin
  // use SizeOf(TRangeFillingItem1)
  // or SizeOf(TRangeFillingItem4)
  // or SizeOf(TRangeFillingItem8)
  // depending of tiledates in storage
  Result := 0; // no range filling map abilities
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

function TTileStorageAbstract.LoadFillingMap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  btm: TCustomBitmap32;
  const AXY: TPoint;
  Azoom, ASourceZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AColorer: IFillingMapColorer
): boolean;
var
  VPixelsRect: TRect;
  VRelativeRect: TDoubleRect;
  VSourceTilesRect: TRect;
  VCurrTile: TPoint;
  VTileSize: TPoint;
  VSolidDrow: Boolean;
  VIterator: ITileIterator;
  VTileInfo: ITileInfoBasic;
  VTileColor: TColor32;
  VGeoConvert: ICoordConverter;
  // range
  VRangeFillingMapInfo: TRangeFillingMapInfo;
  i, j: Cardinal;
  VTileMapItemPtr: Pointer;
  VTile: TPoint;

  procedure _PaintCurrTile;
  var
    VSourceTilePixels: TRect;
  begin
    VRelativeRect := VGeoConvert.TilePos2RelativeRect(VCurrTile, ASourceZoom);
    VSourceTilePixels := VGeoConvert.RelativeRect2PixelRect(VRelativeRect, Azoom);
    if VSourceTilePixels.Left < VPixelsRect.Left then begin
      VSourceTilePixels.Left := VPixelsRect.Left;
    end;
    if VSourceTilePixels.Top < VPixelsRect.Top then begin
      VSourceTilePixels.Top := VPixelsRect.Top;
    end;
    if VSourceTilePixels.Right > VPixelsRect.Right then begin
      VSourceTilePixels.Right := VPixelsRect.Right;
    end;
    if VSourceTilePixels.Bottom > VPixelsRect.Bottom then begin
      VSourceTilePixels.Bottom := VPixelsRect.Bottom;
    end;

    VSourceTilePixels.Left := VSourceTilePixels.Left - VPixelsRect.Left;
    VSourceTilePixels.Top := VSourceTilePixels.Top - VPixelsRect.Top;
    VSourceTilePixels.Right := VSourceTilePixels.Right - VPixelsRect.Left;
    VSourceTilePixels.Bottom := VSourceTilePixels.Bottom - VPixelsRect.Top;

    if not VSolidDrow then begin
      Dec(VSourceTilePixels.Right);
      Dec(VSourceTilePixels.Bottom);
    end;

    if ((VSourceTilePixels.Right - VSourceTilePixels.Left) = 1) and
      ((VSourceTilePixels.Bottom - VSourceTilePixels.Top) = 1) then begin
      btm.Pixel[VSourceTilePixels.Left, VSourceTilePixels.Top] := VTileColor;
    end else begin
      btm.FillRectS(VSourceTilePixels.Left, VSourceTilePixels.Top, VSourceTilePixels.Right, VSourceTilePixels.Bottom, VTileColor);
    end;
  end;

begin
  Result := FALSE;
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    try
      VGeoConvert := FConfig.CoordConverter;
      VTile := AXY;
      VGeoConvert.CheckTilePosStrict(VTile, Azoom, True);
      VGeoConvert.CheckZoom(ASourceZoom);

      VPixelsRect := VGeoConvert.TilePos2PixelRect(VTile, Azoom);

      VTileSize := Point(VPixelsRect.Right - VPixelsRect.Left, VPixelsRect.Bottom - VPixelsRect.Top);

      btm.Width := VTileSize.X;
      btm.Height := VTileSize.Y;
      btm.Clear(0);

      VRelativeRect := VGeoConvert.TilePos2RelativeRect(VTile, Azoom);
      VSourceTilesRect := VGeoConvert.RelativeRect2TileRect(VRelativeRect, ASourceZoom);
      VSolidDrow := (VTileSize.X <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left)) or (VTileSize.Y <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left));

      if Assigned(FOnRangeFillingMap) and (ASourceZoom > Azoom) then begin
        // make buffer: 1 tile has 2^(ASourceZoom-Azoom) tiles for each side
        with VRangeFillingMapInfo do begin
          SourceZoom := ASourceZoom;
          Zoom := AZoom;
          TileMapSize := 1 shl (ASourceZoom - Azoom);

          // set buffer format (and size) depending on particular storage capabilities
          ItemSize := GetRangeFillingMapItemSize;

          // allocate
          try
            // EOutOfMemory allowed!
            TileMapAddr := AllocMem(LongInt(TileMapSize) * LongInt(TileMapSize) * ItemSize);
          except
            try
              // no memory - use minimal buffer (flags without date)
              ItemSize := SizeOf(TRangeFillingItem1);
              TileMapAddr := AllocMem(LongInt(TileMapSize) * LongInt(TileMapSize) * ItemSize);
            except
              // epic fail
              ItemSize := 0;
            end;
          end;
        end;

        if (VRangeFillingMapInfo.TileMapAddr <> nil) and (VRangeFillingMapInfo.ItemSize > 0) then begin
          try
            // call storage
            Result := FOnRangeFillingMap(Self, @VSourceTilesRect, AVersionInfo, @VRangeFillingMapInfo);

            // check buffer
            if Result then begin
              // loop through arrays
              VTileMapItemPtr := VRangeFillingMapInfo.TileMapAddr;
              for i := 0 to VRangeFillingMapInfo.TileMapSize - 1 do begin
                // check cancelled
                if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
                  Result := FALSE;
                  break;
                end;

                for j := 0 to VRangeFillingMapInfo.TileMapSize - 1 do begin
                  // check cancelled
                  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
                    Result := FALSE;
                    break;
                  end;

                  // get color
                  VTileColor := AColorer.GetRangeColor(VTileMapItemPtr, VRangeFillingMapInfo.ItemSize);

                  // apply color
                  VCurrTile := VSourceTilesRect.TopLeft;
                  Inc(VCurrTile.X, i);
                  Inc(VCurrTile.Y, j);
                  _PaintCurrTile;

                  // next item in buffer
                  VTileMapItemPtr := Pointer(LongInt(VTileMapItemPtr) + VRangeFillingMapInfo.ItemSize);
                end;
              end;
            end;
          finally
            FreeMem(VRangeFillingMapInfo.TileMapAddr);
          end;
        end;
      end;

      if (not Result) then begin
        Result := TRUE;
        VIterator := TTileIteratorByRect.Create(VSourceTilesRect);
        while VIterator.Next(VCurrTile) do begin
          // check cancelled
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            Result := FALSE;
            break;
          end;

          VTileInfo := GetTileInfo(VCurrTile, ASourceZoom, AVersionInfo);
          VTileColor := AColorer.GetColor(VTileInfo);

          if VTileColor <> 0 then begin
            // check cancelled
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              Result := FALSE;
              break;
            end;

            _PaintCurrTile;
          end;
        end;
      end;
    except
      Result := false;
    end;
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

procedure TTileStorageAbstract.Scan(
  const AOnTileStorageScan: TOnTileStorageScan;
  const AIgnoreTNE: Boolean;
  const ARemoveTileAfterProcess: Boolean
);
begin
  // You mast override Scan method in custom tile storage, if you need it
  raise Exception.Create('Operation not supported on this tile storage type!');
end;

end.
