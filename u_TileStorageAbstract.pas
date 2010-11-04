unit u_TileStorageAbstract;

interface

uses
  Types,
  Classes,
  GR32,
  i_ICoordConverter,
  u_MapTypeCacheConfig;

type
  TTileStorageAbstract = class
  private
    FCoordConverter: ICoordConverter;
  public
    constructor Create(ACoordConverter: ICoordConverter);
    destructor Destroy; override;
    function GetIsStoreFileCache: Boolean; virtual; abstract;
    function GetUseDel: boolean; virtual; abstract;
    function GetUseSave: boolean; virtual; abstract;
    function GetIsStoreReadOnly: boolean; virtual; abstract;

    function ExistsTile(AXY: TPoint; Azoom: byte): Boolean; virtual; abstract;
    function ExistsTNE(AXY: TPoint; Azoom: byte): Boolean; virtual; abstract;

    function DeleteTile(AXY: TPoint; Azoom: byte): Boolean; virtual; abstract;
    function DeleteTNE(AXY: TPoint; Azoom: byte): Boolean; virtual; abstract;

    function GetTileFileName(AXY: TPoint; Azoom: byte): string; virtual; abstract;
    function GetTileFileExt: string; virtual; abstract;
    function GetCacheConfig: TMapTypeCacheConfigAbstract; virtual; abstract;

    function LoadTile(AXY: TPoint; Azoom: byte; AStream: TStream): Boolean; virtual; abstract;
    function TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime; virtual; abstract;
    function TileSize(AXY: TPoint; Azoom: byte): integer; virtual; abstract;

    procedure SaveTile(AXY: TPoint; Azoom: byte; AStream: TStream); virtual; abstract;
    procedure SaveTNE(AXY: TPoint; Azoom: byte); virtual; abstract;

    function LoadFillingMap(btm: TCustomBitmap32; AXY: TPoint; Azoom: byte; ASourceZoom: byte; IsStop: PBoolean): boolean; virtual;

    property CacheConfig: TMapTypeCacheConfigAbstract read GetCacheConfig;
    property TileFileExt: string read GetTileFileExt;
    property GeoConvert: ICoordConverter read FCoordConverter;
  end;

implementation

uses
  Graphics,
  t_GeoTypes,
  i_ITileIterator,
  u_TileIteratorByRect,
  u_GlobalState;

{ TTileStorageAbstract }

constructor TTileStorageAbstract.Create(ACoordConverter: ICoordConverter);
begin
  FCoordConverter := ACoordConverter;
end;

destructor TTileStorageAbstract.Destroy;
begin
  FCoordConverter := nil;
  inherited;
end;

function TTileStorageAbstract.LoadFillingMap(btm: TCustomBitmap32; AXY: TPoint;
  Azoom, ASourceZoom: byte; IsStop: PBoolean): boolean;
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
begin
  Result := true;
  try
    GeoConvert.CheckTilePosStrict(AXY, Azoom, True);
    GeoConvert.CheckZoom(ASourceZoom);

    VPixelsRect := GeoConvert.TilePos2PixelRect(AXY, Azoom);

    VTileSize := Point(VPixelsRect.Right - VPixelsRect.Left, VPixelsRect.Bottom - VPixelsRect.Top);

    btm.Width := VTileSize.X;
    btm.Height := VTileSize.Y;
    btm.Clear(clBlack);

    VRelativeRect := GeoConvert.TilePos2RelativeRect(AXY, Azoom);
    VSourceTilesRect := GeoConvert.RelativeRect2TileRect(VRelativeRect, ASourceZoom);
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
        if not ExistsTile(VCurrTile, ASourceZoom) then begin
          if IsStop^ then break;
          VRelativeRect := GeoConvert.TilePos2RelativeRect(VCurrTile, ASourceZoom);
          VSourceTilePixels := GeoConvert.RelativeRect2PixelRect(VRelativeRect, Azoom);
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
            if GState.MapZapShowTNE and ExistsTNE(VCurrTile, ASourceZoom) then begin
              btm.Pixel[VSourceTilePixels.Left,VSourceTilePixels.Top]:=VClTne;
            end else begin
              btm.Pixel[VSourceTilePixels.Left,VSourceTilePixels.Top]:=VClMZ;
            end;
          end else begin
            if GState.MapZapShowTNE and ExistsTNE(VCurrTile, ASourceZoom) then begin
              btm.FillRect(VSourceTilePixels.Left,VSourceTilePixels.Top,VSourceTilePixels.Right,VSourceTilePixels.Bottom, VClTne);
            end else begin
              btm.FillRect(VSourceTilePixels.Left,VSourceTilePixels.Top,VSourceTilePixels.Right,VSourceTilePixels.Bottom, VClMZ);
            end;
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
