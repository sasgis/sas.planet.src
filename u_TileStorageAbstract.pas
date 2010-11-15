unit u_TileStorageAbstract;

interface

uses
  Types,
  Classes,
  GR32,
  i_ICoordConverter,
  i_ContentTypeInfo,
  i_ITileInfoBasic,
  i_IConfigDataProvider,
  u_MapTypeCacheConfig;

type
  TTileStorageAbstract = class
  public
    function GetMainContentType: IContentTypeInfoBasic; virtual; abstract;
    function GetAllowDifferentContentTypes: Boolean; virtual; abstract;

    function GetIsStoreFileCache: Boolean; virtual; abstract;
    function GetUseDel: boolean; virtual; abstract;
    function GetUseSave: boolean; virtual; abstract;
    function GetIsStoreReadOnly: boolean; virtual; abstract;
    function GetTileFileExt: string; virtual; abstract;
    function GetCoordConverter: ICoordConverter; virtual; abstract;
    function GetCacheConfig: TMapTypeCacheConfigAbstract; virtual; abstract;

    function GetTileFileName(AXY: TPoint; Azoom: byte; AVersion: Variant): string; virtual; abstract;
    function GetTileInfo(AXY: TPoint; Azoom: byte; AVersion: Variant): ITileInfoBasic; virtual; abstract;

    function LoadTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream; out ATileInfo: ITileInfoBasic): Boolean; virtual; abstract;
    function DeleteTile(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean; virtual; abstract;
    function DeleteTNE(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean; virtual; abstract;
    procedure SaveTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream); virtual; abstract;
    procedure SaveTNE(AXY: TPoint; Azoom: byte; AVersion: Variant); virtual; abstract;

    function LoadFillingMap(btm: TCustomBitmap32; AXY: TPoint; Azoom: byte; ASourceZoom: byte; AVersion: Variant; IsStop: PBoolean): boolean; virtual;

    property CacheConfig: TMapTypeCacheConfigAbstract read GetCacheConfig;
    property TileFileExt: string read GetTileFileExt;
//    property GeoConvert: ICoordConverter read FCoordConverter;
  end;

implementation

uses
  Graphics,
  t_GeoTypes,
  i_ITileIterator,
  u_TileIteratorByRect,
  u_GlobalState;

{ TTileStorageAbstract }

function TTileStorageAbstract.LoadFillingMap(btm: TCustomBitmap32; AXY: TPoint;
  Azoom, ASourceZoom: byte; AVersion: Variant; IsStop: PBoolean): boolean;
var
  VPixelsRect: TRect;
  VRelativeRect: TDoubleRect;
  VSourceTilesRect: TRect;
  VCurrTile: TPoint;
  VTileSize: TPoint;
  VSourceTilePixels: TRect;
  VClMZ: TColor32;
  VClTne: TColor32;
  VSolidDrow: Boolean;
  VIterator: ITileIterator;
  VTileInfo: ITileInfoBasic;
  VTileColor: TColor32;
  VGeoConvert: ICoordConverter;
begin
  Result := true;
  try
    VGeoConvert := GetCoordConverter;

    VGeoConvert.CheckTilePosStrict(AXY, Azoom, True);
    VGeoConvert.CheckZoom(ASourceZoom);

    VPixelsRect := VGeoConvert.TilePos2PixelRect(AXY, Azoom);

    VTileSize := Point(VPixelsRect.Right - VPixelsRect.Left, VPixelsRect.Bottom - VPixelsRect.Top);

    btm.Width := VTileSize.X;
    btm.Height := VTileSize.Y;
    btm.Clear(clBlack);

    VRelativeRect := VGeoConvert.TilePos2RelativeRect(AXY, Azoom);
    VSourceTilesRect := VGeoConvert.RelativeRect2TileRect(VRelativeRect, ASourceZoom);
   { if (VTileSize.X >= (VSourceTilesRect.Right - VSourceTilesRect.Left + 1)) and
      (VTileSize.Y >= (VSourceTilesRect.Right - VSourceTilesRect.Left + 1)) then  }
    begin
      VSolidDrow := (VTileSize.X <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left))
        or (VTileSize.Y <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left));
      VClMZ := SetAlpha(Color32(GState.MapZapColor), GState.MapZapAlpha);
      VClTne := SetAlpha(Color32(GState.MapZapTneColor), GState.MapZapAlpha);
      VIterator := TTileIteratorByRect.Create(VSourceTilesRect);
      while VIterator.Next(VCurrTile) do begin
        if IsStop^ then break;
        VTileInfo := GetTileInfo(VCurrTile, ASourceZoom, AVersion);
        if not VTileInfo.GetIsExists then begin
          if IsStop^ then break;
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
          if GState.MapZapShowTNE then begin
            if VTileInfo.GetIsExistsTNE then begin
              VTileColor := VClTne;
            end else begin
              VTileColor := VClMZ;
            end;
          end else begin
            VTileColor := VClMZ;
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
    if IsStop^ then begin
      Result := false;
    end;
  except
    Result := false;
  end;
end;

end.
