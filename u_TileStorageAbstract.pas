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
  SyncObjs,
  GR32,
  i_JclNotify,
  i_BinaryData,
  i_FillingMapColorer,
  i_OperationNotifier,
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
  u_MapTypeCacheConfig;

type
  TTileStorageAbstract = class
  private
    FConfig: ISimpleTileStorageConfig;
    FMapVersionFactory: IMapVersionFactory;
    FMinValidZoom: Byte;
    FMaxValidZoom: Byte;
    FNotifierByZoom: array of ITileRectUpdateNotifier;
    FConfigListener: IJclListener;
    FStorageState: IStorageStateChangeble;
    FStorageStateListener: IJclListener;
    FStorageStateStatic: IStorageStateStatic;
    FStorageStateStaticCS: TCriticalSection;
    FStorageStateInternal: IStorageStateInternal;
    FNotifierByZoomInternal: array of ITileRectUpdateNotifierInternal;
    function GetNotifierByZoom(AZoom: Byte): ITileRectUpdateNotifier;
    function GetNotifierByZoomInternal(
      AZoom: Byte): ITileRectUpdateNotifierInternal;
    procedure OnStateChange;
    procedure OnConfigChange;
    function GetStorageStateStatic: IStorageStateStatic;
    property NotifierByZoomInternal[AZoom: Byte]: ITileRectUpdateNotifierInternal read GetNotifierByZoomInternal;
  protected
    procedure NotifyTileUpdate(ATile: TPoint; AZoom: Byte; AVersion: IMapVersionInfo);
    property StorageStateStatic: IStorageStateStatic read GetStorageStateStatic;
    property StorageStateInternal: IStorageStateInternal read FStorageStateInternal;
    property Config: ISimpleTileStorageConfig read FConfig;
  public
    constructor Create(
      AStorageTypeAbilities: IStorageTypeAbilities;
      AMapVersionFactory: IMapVersionFactory;
      AConfig: ISimpleTileStorageConfig
    );
    destructor Destroy; override;
    function GetMainContentType: IContentTypeInfoBasic; virtual; abstract;
    function GetAllowDifferentContentTypes: Boolean; virtual; abstract;

    function GetCacheConfig: TMapTypeCacheConfigAbstract; virtual; abstract;

    function GetTileFileName(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): string; virtual; abstract;
    function GetTileInfo(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): ITileInfoBasic; virtual; abstract;

    function LoadTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo;
      AStream: TStream;
      out ATileInfo: ITileInfoBasic
    ): Boolean; virtual; abstract;
    function DeleteTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): Boolean; virtual; abstract;
    function DeleteTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): Boolean; virtual; abstract;
    procedure SaveTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo;
      AData: IBinaryData
    ); virtual; abstract;
    procedure SaveTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ); virtual; abstract;

    function GetListOfTileVersions(
      const AXY: TPoint;
      const Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): IMapVersionListStatic; virtual;

    function LoadFillingMap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      btm: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      ASourceZoom: byte;
      AVersionInfo: IMapVersionInfo;
      AColorer: IFillingMapColorer
    ): boolean; virtual;
    property State: IStorageStateChangeble read FStorageState;
    property MapVersionFactory: IMapVersionFactory read FMapVersionFactory;
    property NotifierByZoom[AZoom: Byte]: ITileRectUpdateNotifier read GetNotifierByZoom;
  end;

implementation

uses
  SysUtils,
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
  AStorageTypeAbilities: IStorageTypeAbilities;
  AMapVersionFactory: IMapVersionFactory;
  AConfig: ISimpleTileStorageConfig
);
var
  VCount: Integer;
  i: Integer;
  VNotifier: TTileRectUpdateNotifier;
  VState: TStorageStateInternal;
begin
  FConfig := AConfig;
  FMapVersionFactory := AMapVersionFactory;
  FStorageStateStaticCS := TCriticalSection.Create;

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
  FreeAndNil(FStorageStateStaticCS);
  inherited;
end;

function TTileStorageAbstract.GetListOfTileVersions(
  const AXY: TPoint;
  const Azoom: byte;
  AVersionInfo: IMapVersionInfo
): IMapVersionListStatic;
begin
  Result := nil;
end;

function TTileStorageAbstract.GetNotifierByZoom(
  AZoom: Byte
): ITileRectUpdateNotifier;
begin
  Result := nil;
  if (AZoom >= FMinValidZoom) and (AZoom <= FMaxValidZoom) then begin
    Result := FNotifierByZoom[AZoom - FMinValidZoom];
  end;
end;

function TTileStorageAbstract.GetNotifierByZoomInternal(
  AZoom: Byte): ITileRectUpdateNotifierInternal;
begin
  Result := FNotifierByZoomInternal[AZoom - FMinValidZoom];
end;

function TTileStorageAbstract.GetStorageStateStatic: IStorageStateStatic;
begin
  FStorageStateStaticCS.Acquire;
  try
    Result := FStorageStateStatic;
  finally
    FStorageStateStaticCS.Release;
  end;
end;

function TTileStorageAbstract.LoadFillingMap(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  btm: TCustomBitmap32;
  AXY: TPoint;
  Azoom, ASourceZoom: byte;
  AVersionInfo: IMapVersionInfo;
  AColorer: IFillingMapColorer
): boolean;
var
  VPixelsRect: TRect;
  VRelativeRect: TDoubleRect;
  VSourceTilesRect: TRect;
  VCurrTile: TPoint;
  VTileSize: TPoint;
  VSourceTilePixels: TRect;
  VSolidDrow: Boolean;
  VIterator: ITileIterator;
  VTileInfo: ITileInfoBasic;
  VTileColor: TColor32;
  VGeoConvert: ICoordConverter;
begin
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    Result := true;
    try
      VGeoConvert := FConfig.CoordConverter;

      VGeoConvert.CheckTilePosStrict(AXY, Azoom, True);
      VGeoConvert.CheckZoom(ASourceZoom);

      VPixelsRect := VGeoConvert.TilePos2PixelRect(AXY, Azoom);

      VTileSize := Point(VPixelsRect.Right - VPixelsRect.Left, VPixelsRect.Bottom - VPixelsRect.Top);

      btm.Width := VTileSize.X;
      btm.Height := VTileSize.Y;
      btm.Clear(0);

      VRelativeRect := VGeoConvert.TilePos2RelativeRect(AXY, Azoom);
      VSourceTilesRect := VGeoConvert.RelativeRect2TileRect(VRelativeRect, ASourceZoom);
      VSolidDrow := (VTileSize.X <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left))
        or (VTileSize.Y <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left));
      VIterator := TTileIteratorByRect.Create(VSourceTilesRect);
      while VIterator.Next(VCurrTile) do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then break;
        VTileInfo := GetTileInfo(VCurrTile, ASourceZoom, AVersionInfo);
        VTileColor := AColorer.GetColor(VTileInfo);
        if VTileColor <> 0 then begin
          if ACancelNotifier.IsOperationCanceled(AOperationID) then break;
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

          if ((VSourceTilePixels.Right-VSourceTilePixels.Left)=1)and
             ((VSourceTilePixels.Bottom-VSourceTilePixels.Top)=1)then begin
            btm.Pixel[VSourceTilePixels.Left,VSourceTilePixels.Top]:=VTileColor;
          end else begin
            btm.FillRect(VSourceTilePixels.Left,VSourceTilePixels.Top,VSourceTilePixels.Right,VSourceTilePixels.Bottom, VTileColor);
          end;
        end;
      end;
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Result := false;
      end;
    except
      Result := false;
    end;
  end else begin
    Result := False;
  end;
end;

procedure TTileStorageAbstract.NotifyTileUpdate(
  ATile: TPoint;
  AZoom: Byte;
  AVersion: IMapVersionInfo
);
var
  VKey: ITileKey;
  VNotifier: ITileRectUpdateNotifierInternal;
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
  FStorageStateStaticCS.Acquire;
  try
    FStorageStateStatic := FStorageState.GetStatic;
  finally
    FStorageStateStaticCS.Release;
  end;
end;

end.
