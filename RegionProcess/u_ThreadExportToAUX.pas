unit u_ThreadExportToAUX;

interface

uses
  Windows,
  SysUtils,
  UMapType,
  t_GeoTypes,
  u_ThreadRegionProcessAbstract;

type
  TThreadExportToAUX = class(TThreadRegionProcessAbstract)
  private
    FMapType: TMapType;
    FFileName: string;
    FZoom: Byte;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress; virtual;
  public
    constructor Create(
      APolygon: TDoublePointArray;
      AZoom: Byte;
      AMapType: TMapType;
      AFileName: string
    );
  end;
implementation

uses
  Classes,
  i_ICoordConverter,
  UResStrings,
  u_TileIteratorAbstract,
  u_TileIteratorStuped;

{ TThreadExportToAUX }

constructor TThreadExportToAUX.Create(APolygon: TDoublePointArray;
  AZoom: Byte; AMapType: TMapType; AFileName: string);
begin
  inherited Create(APolygon);
  FZoom := AZoom;
  FMapType := AMapType;
  FFileName := AFileName;
end;

procedure TThreadExportToAUX.ProcessRegion;
var
  VTileIterator: TTileIteratorAbstract;
  VGeoConvert: ICoordConverter;
  VTile: TPoint;
  VFileStream: TFileStream;
  VPixelRect: TRect;
  VRectOfTilePixels: TRect;
  VFileName: string;
  VOutString: string;
  VOutPos: TPoint;
begin
  inherited;
  VGeoConvert := FMapType.GeoConvert;
  VTileIterator := TTileIteratorStuped.Create(FZoom, FPolygLL, VGeoConvert);
  try
    FTilesToProcess := VTileIterator.TilesTotal;
    ProgressFormUpdateCaption(
      SAS_STR_ExportTiles,
      SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
    );
    FTilesProcessed := 0;
    ProgressFormUpdateOnProgress;
    VPixelRect := VGeoConvert.TileRect2PixelRect(VTileIterator.TilesRect, FZoom);
    VFileStream := TFileStream.Create(FFileName, fmCreate);
    try
      while VTileIterator.Next(VTile) do begin
        if IsCancel then begin
          exit;
        end;
        if FMapType.TileExists(VTile, FZoom) then begin
          VRectOfTilePixels := VGeoConvert.TilePos2PixelRect(VTile, FZoom);
          VOutPos.X := VRectOfTilePixels.Left - VPixelRect.Left;
          VOutPos.Y := VPixelRect.Bottom - VRectOfTilePixels.Bottom;
          VFileName := FMapType.GetTileFileName(VTile, FZoom);
          VOutString := '"' + VFileName + '" ' + IntToStr(VOutPos.X) + ' ' + IntToStr(VOutPos.Y) + #13#10;
          VFileStream.WriteBuffer(VOutString[1], Length(VOutString));
        end;
        inc(FTilesProcessed);
        if FTilesProcessed mod 100 = 0 then begin
          ProgressFormUpdateOnProgress;
        end;
      end;
    finally
      VFileStream.Free;
    end;
  finally
    FreeAndNil(VTileIterator);
  end;
  ProgressFormUpdateOnProgress;
end;

procedure TThreadExportToAUX.ProgressFormUpdateOnProgress;
begin
  ProgressFormUpdateProgressAndLine1(
    round((FTilesProcessed / FTilesToProcess) * 100),
    SAS_STR_Processed + ' ' + inttostr(FTilesProcessed)
  );
end;

end.
