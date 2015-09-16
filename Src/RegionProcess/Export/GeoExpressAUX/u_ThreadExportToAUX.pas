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

unit u_ThreadExportToAUX;

interface

uses
  Windows,
  SysUtils,
  i_TileStorage,
  i_ProjectionInfo,
  i_MapVersionInfo,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_GeometryProjected,
  u_ThreadRegionProcessAbstract;

type
  TThreadExportToAUX = class(TThreadRegionProcessAbstract)
  private
    FTileStorage: ITileStorage;
    FVersion: IMapVersionInfo;
    FProjection: IProjectionInfo;
    FPolyProjected: IGeometryProjectedPolygon;
    FFileName: string;
    FZoom: Byte;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress(AProcessed, AToProcess: Int64);
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolygon: IGeometryLonLatPolygon;
      const AProjectedPolygon: IGeometryProjectedPolygon;
      const AProjection: IProjectionInfo;
      const ATileStorage: ITileStorage;
      const AVersion: IMapVersionInfo;
      const AFileName: string
    );
  end;

implementation

uses
  Classes,
  ALString,
  i_TileInfoBasic,
  u_ResStrings,
  i_TileIterator,
  u_TileIteratorByPolygon;

{ TThreadExportToAUX }

constructor TThreadExportToAUX.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolygon: IGeometryLonLatPolygon;
  const AProjectedPolygon: IGeometryProjectedPolygon;
  const AProjection: IProjectionInfo;
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
  FProjection := AProjection;
  FZoom := AProjection.Zoom;
  FTileStorage := ATileStorage;
  FVersion := AVersion;
  FFileName := AFileName;
end;

procedure TThreadExportToAUX.ProcessRegion;
var
  VTileIterator: ITileIterator;
  VTile: TPoint;
  VFileStream: TFileStream;
  VPixelRect: TRect;
  VRectOfTilePixels: TRect;
  VFileName: string;
  VOutString: AnsiString;
  VOutPos: TPoint;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VTileInfo: ITileInfoBasic;
begin
  inherited;
  VTileIterator := TTileIteratorByPolygon.Create(FProjection, FPolyProjected);
  try
    VTilesToProcess := VTileIterator.TilesTotal;
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
    VPixelRect := FProjection.TileRect2PixelRect(VTileIterator.TilesRect.Rect);
    VFileStream := TFileStream.Create(FFileName, fmCreate);
    try
      while VTileIterator.Next(VTile) do begin
        if CancelNotifier.IsOperationCanceled(OperationID) then begin
          exit;
        end;
        VTileInfo := FTileStorage.GetTileInfo(VTile, FZoom, FVersion, gtimAsIs);
        if VTileInfo.GetIsExists then begin
          VRectOfTilePixels := FProjection.TilePos2PixelRect(VTile);
          VOutPos.X := VRectOfTilePixels.Left - VPixelRect.Left;
          VOutPos.Y := VPixelRect.Bottom - VRectOfTilePixels.Bottom;
          VFileName := FTileStorage.GetTileFileName(VTile, FZoom, FVersion);
          VOutString := '"' + AnsiToUtf8(VFileName) + '" ' + ALIntToStr(VOutPos.X) + ' ' + ALIntToStr(VOutPos.Y) + #13#10;
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
