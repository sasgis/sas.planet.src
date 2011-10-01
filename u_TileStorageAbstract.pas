{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  GR32,
  i_OperationNotifier,
  i_CoordConverter,
  i_SimpleTileStorageConfig,
  i_ContentTypeInfo,
  i_MapVersionInfo,
  i_TileInfoBasic,
  u_MapTypeCacheConfig;

type
  TTileStorageAbstract = class
  private
    FConfig: ISimpleTileStorageConfig;
  protected
    property Config: ISimpleTileStorageConfig read FConfig;
  public
    constructor Create(
      AConfig: ISimpleTileStorageConfig
    );
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
      AStream: TStream
    ); virtual; abstract;
    procedure SaveTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ); virtual; abstract;

    function LoadFillingMap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      btm: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      ASourceZoom: byte;
      AVersionInfo: IMapVersionInfo;
      ANoTileColor: TColor32;
      AShowTNE: Boolean;
      ATNEColor: TColor32
    ): boolean; virtual;
  end;

implementation

uses
  t_GeoTypes,
  i_TileIterator,
  u_TileIteratorByRect;

{ TTileStorageAbstract }

constructor TTileStorageAbstract.Create(AConfig: ISimpleTileStorageConfig);
begin
  FConfig := AConfig;
end;

function TTileStorageAbstract.LoadFillingMap(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  btm: TCustomBitmap32;
  AXY: TPoint;
  Azoom, ASourceZoom: byte;
  AVersionInfo: IMapVersionInfo;
  ANoTileColor: TColor32;
  AShowTNE: Boolean;
  ATNEColor: TColor32
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
   { if (VTileSize.X >= (VSourceTilesRect.Right - VSourceTilesRect.Left + 1)) and
      (VTileSize.Y >= (VSourceTilesRect.Right - VSourceTilesRect.Left + 1)) then  }
    begin
      VSolidDrow := (VTileSize.X <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left))
        or (VTileSize.Y <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left));
      VIterator := TTileIteratorByRect.Create(VSourceTilesRect);
      while VIterator.Next(VCurrTile) do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then break;
        VTileInfo := GetTileInfo(VCurrTile, ASourceZoom, AVersionInfo);
        if not VTileInfo.GetIsExists then begin
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
          if AShowTNE then begin
            if VTileInfo.GetIsExistsTNE then begin
              VTileColor := ATNEColor;
            end else begin
              VTileColor := ANoTileColor;
            end;
          end else begin
            VTileColor := ANoTileColor;
          end;

          if ((VSourceTilePixels.Right-VSourceTilePixels.Left)=1)and
             ((VSourceTilePixels.Bottom-VSourceTilePixels.Top)=1)then begin
            btm.Pixel[VSourceTilePixels.Left,VSourceTilePixels.Top]:=VTileColor;
          end else begin
            btm.FillRect(VSourceTilePixels.Left,VSourceTilePixels.Top,VSourceTilePixels.Right,VSourceTilePixels.Bottom, VTileColor);
          end;
        end;
      end;
    end;
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Result := false;
    end;
  except
    Result := false;
  end;
end;

end.
