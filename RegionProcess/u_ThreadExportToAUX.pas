unit u_ThreadExportToAUX;

interface

uses
  Windows,
  SysUtils,
  i_TileStorage,
  i_MapVersionInfo,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_VectorItemProjected,
  u_ThreadRegionProcessAbstract;

type
  TThreadExportToAUX = class(TThreadRegionProcessAbstract)
  private
    FTileStorage: ITileStorage;
    FVersion: IMapVersionInfo;
    FPolyProjected: IGeometryProjectedMultiPolygon;
    FFileName: string;
    FZoom: Byte;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress(AProcessed, AToProcess: Int64);
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolygon: IGeometryLonLatMultiPolygon;
      const AProjectedPolygon: IGeometryProjectedMultiPolygon;
      AZoom: Byte;
      const ATileStorage: ITileStorage;
      const AVersion: IMapVersionInfo;
      const AFileName: string
    );
  end;

implementation

uses
  Classes,
  i_CoordConverter,
  i_TileInfoBasic,
  u_ResStrings,
  i_TileIterator,
  u_TileIteratorByPolygon;

{ TThreadExportToAUX }

constructor TThreadExportToAUX.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: IGeometryLonLatMultiPolygon;
  const AProjectedPolygon: IGeometryProjectedMultiPolygon;
  AZoom: Byte;
  const ATileStorage: ITileStorage;
  const AVersion: IMapVersionInfo;
  const AFileName: string
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    Self.ClassName
  );
  FPolyProjected := AProjectedPolygon;
  FZoom := AZoom;
  FTileStorage := ATileStorage;
  FVersion := AVersion;
  FFileName := AFileName;
end;

procedure TThreadExportToAUX.ProcessRegion;
var
  VTileIterator: ITileIterator;
  VGeoConvert: ICoordConverter;
  VTile: TPoint;
  VFileStream: TFileStream;
  VPixelRect: TRect;
  VRectOfTilePixels: TRect;
  VFileName: string;
  VOutString: string;
  VOutPos: TPoint;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VTileInfo: ITileInfoBasic;
begin
  inherited;
  VGeoConvert := FTileStorage.CoordConverter;
  VTileIterator := TTileIteratorByPolygon.Create(FPolyProjected);
  try
    VTilesToProcess := VTileIterator.TilesTotal;
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
    VPixelRect := VGeoConvert.TileRect2PixelRect(VTileIterator.TilesRect, FZoom);
    VFileStream := TFileStream.Create(FFileName, fmCreate);
    try
      while VTileIterator.Next(VTile) do begin
        if CancelNotifier.IsOperationCanceled(OperationID) then begin
          exit;
        end;
        VTileInfo := FTileStorage.GetTileInfo(VTile, FZoom, FVersion, gtimAsIs);
        if VTileInfo.GetIsExists then begin
          VRectOfTilePixels := VGeoConvert.TilePos2PixelRect(VTile, FZoom);
          VOutPos.X := VRectOfTilePixels.Left - VPixelRect.Left;
          VOutPos.Y := VPixelRect.Bottom - VRectOfTilePixels.Bottom;
          VFileName := FTileStorage.GetTileFileName(VTile, FZoom, FVersion);
          VOutString := '"' + VFileName + '" ' + IntToStr(VOutPos.X) + ' ' + IntToStr(VOutPos.Y) + #13#10;
          VFileStream.WriteBuffer(VOutString[1], Length(VOutString));
        end;
        inc(VTilesProcessed);
        if VTilesProcessed mod 100 = 0 then begin
          ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
        end;
      end;
    finally
      VFileStream.Free;
    end;
  finally
    VTileIterator := nil;
  end;
end;

procedure TThreadExportToAUX.ProgressFormUpdateOnProgress(AProcessed, AToProcess: Int64);
begin
  ProgressInfo.SetProcessedRatio(AProcessed / AToProcess);
  ProgressInfo.SetSecondLine(SAS_STR_Processed + ' ' + inttostr(AProcessed));
end;

end.
