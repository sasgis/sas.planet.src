{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ExportTaskToJnx;

interface

uses
  t_GeoTypes,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_TileIteratorFactory,
  i_BitmapPostProcessing,
  u_ExportToJnxTask,
  u_RegionProcessTaskAbstract;

type
  TExportTaskToJnx = class(TRegionProcessTaskAbstract)
  private
    FTasks: TExportTaskJnxArray;
    FTargetFile: string;
    FProductName: string; // копирайт
    FMapName: string;  // имя карты
    FJNXVersion: Byte;  // 3..4
    FZorder: Integer;   // для 4 версии
    FProductID: Integer; // 0,2,3,4,5,6,7,8,9
    FBitmapPostProcessing: IBitmapPostProcessing;

    FTileProcessErrorFmt: string;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ATileIteratorFactory: ITileIteratorFactory;
      const ATargetFile: string;
      const APolygon: IGeometryLonLatPolygon;
      const ATasks: TExportTaskJnxArray;
      const AProductName: string;
      const AMapName: string;
      const ABitmapPostProcessing: IBitmapPostProcessing;
      const AJNXVersion: Integer;
      const AZorder: Integer;
      const AProductID: Integer
    );
  end;

implementation

uses
  Types,
  Classes,
  SysUtils,
  JNXlib,
  i_TileStorage,
  i_TileInfoBasic,
  i_ContentTypeInfo,
  i_ProjectionSet,
  i_Projection,
  i_Bitmap32Static,
  i_TileIterator,
  i_BinaryData,
  i_MapVersionRequest,
  i_BitmapTileSaveLoad,
  u_AnsiStr,
  u_Dialogs,
  u_ContentTypeFunc,
  u_ResStrings;

constructor TExportTaskToJnx.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ATileIteratorFactory: ITileIteratorFactory;
  const ATargetFile: string;
  const APolygon: IGeometryLonLatPolygon;
  const ATasks: TExportTaskJnxArray;
  const AProductName: string;
  const AMapName: string;
  const ABitmapPostProcessing: IBitmapPostProcessing;
  const AJNXVersion: Integer;
  const AZorder: Integer;
  const AProductID: Integer
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    ATileIteratorFactory
  );
  FTargetFile := ATargetFile;
  FTasks := ATasks;
  FProductName := AProductName;
  FMapName := AMapName;
  FJNXVersion := AJNXVersion;
  FZorder := AZorder;
  FProductID := AProductID;
  FBitmapPostProcessing := ABitmapPostProcessing;

  FTileProcessErrorFmt := '[JNX] ' + SAS_ERR_TileProcessError;
end;

procedure TExportTaskToJnx.ProcessRegion;
const
  cZoomToScale: array [0..26] of Integer = (
    2446184, 1834628, 1223072, 611526, 458642, 305758, 152877, 114657, 76437,
    38218, 28664, 19109, 9554, 7166, 4777, 2388, 1791, 1194,
    597, 448, 298, 149, 112, 75, 37, 28, 19
  );
var
  I: Integer;
  VBitmapTile: IBitmap32Static;
  VZoom: Byte;
  VTile: TPoint;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VSaver: IBitmapTileSaver;
  VProjectionSet: IProjectionSet;
  VStringStream: TStringStreamA;
  VWriter: TMultiVolumeJNXWriter;
  VTileBounds: TJNXRect;
  VTopLeft: TDoublePoint;
  VBottomRight: TDoublePoint;
  VProjection: IProjection;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VData: IBinaryData;
  VTileStorage: ITileStorage;
  VVersion: IMapVersionRequest;
  VTileInfo: ITileInfoWithData;
  VContentTypeInfoBitmap: IContentTypeInfoBitmap;
  VRecompress: Boolean;
  VTileProcessErrorMsg: string;
begin
  inherited;
  VTilesToProcess := 0;
  SetLength(VTileIterators, Length(FTasks));
  for I := 0 to Length(FTasks) - 1 do begin
    VZoom := FTasks[I].FZoom;
    if not FTasks[I].FBlankLevel then begin
      VProjectionSet := FTasks[I].FTileStorage.ProjectionSet;
      VProjection := VProjectionSet.Zooms[VZoom];
      VTileIterators[I] := Self.MakeTileIterator(VProjection);
      VTilesToProcess := VTilesToProcess + VTileIterators[I].TilesTotal;
    end;
  end;

  VWriter := TMultiVolumeJNXWriter.Create(FTargetFile);
  try
    VWriter.Levels := Length(FTasks);
    VWriter.ProductName := FProductName;
    VWriter.MapName := FMapName;
    VWriter.Version := FJNXVersion;
    VWriter.ZOrder := FZorder;
    VWriter.ProductID := FProductID;

    for I := 0 to Length(FTasks) - 1 do begin
      VWriter.LevelScale[I] := cZoomToScale[FTasks[I].FScale];
      if FTasks[I].FBlankLevel then begin
        VWriter.TileCount[I] := 1;
      end else begin
        VWriter.TileCount[I] := VTileIterators[I].TilesTotal;
      end;
      VWriter.LevelDescription[I] := FTasks[I].FLevelDesc;
      VWriter.LevelName[I] := FTasks[I].FLevelName;
      VWriter.LevelCopyright[I] := FTasks[I].FLevelCopyright;
      VWriter.LevelZoom[I] := FTasks[I].FZoom;
    end;

    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(SAS_STR_AllSaves + ' ' + IntToStr(VTilesToProcess) + ' ' + SAS_STR_Files);
    VStringStream := TStringStreamA.Create('');
    try
      VTilesProcessed := 0;
      ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
      for I := 0 to Length(FTasks) - 1 do begin
        if FTasks[I].FBlankLevel then begin
          VWriter.WriteEmptyTile(I);
        end else begin
          VSaver := FTasks[I].FSaver;
          VRecompress := FTasks[I].FRecompress or (FBitmapPostProcessing <> nil);
          VTileStorage := FTasks[I].FTileStorage;
          VVersion := FTasks[I].FMapVersion;
          VZoom := FTasks[I].FZoom;
          VProjectionSet := VTileStorage.ProjectionSet;
          VTileIterator := VTileIterators[I];
          while VTileIterator.Next(VTile) do begin
            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              Exit;
            end;

            if Supports(VTileStorage.GetTileInfoEx(VTile, VZoom, VVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
              VData := nil;
              if VRecompress or not IsJpegContentType(VTileInfo.ContentType) then begin
                if Supports(VTileInfo.ContentType, IContentTypeInfoBitmap, VContentTypeInfoBitmap) then begin
                  try
                    VBitmapTile := VContentTypeInfoBitmap.GetLoader.Load(VTileInfo.TileData);
                    if FBitmapPostProcessing <> nil then begin
                      VBitmapTile := FBitmapPostProcessing.Process(VBitmapTile);
                    end;
                    if Assigned(VBitmapTile) then begin
                      VData := VSaver.Save(VBitmapTile);
                    end;
                  except
                    on E: Exception do begin
                      VTileProcessErrorMsg := Format(
                        FTileProcessErrorFmt,
                        [VTile.X, VTile.Y, VZoom + 1, E.ClassName, E.Message]
                      );
                      if ShowErrorMessageSync(VTileProcessErrorMsg, MB_YESNO) <> ID_YES then begin
                        Exit;
                      end;
                      VData := nil;
                    end;
                  end;
                end;
              end else begin
                VData := VTileInfo.TileData;
              end;

              if Assigned(VData) then begin
                VTopLeft := VProjectionSet.Zooms[VZoom].TilePos2LonLat(Point(VTile.X, VTile.Y + 1));
                VBottomRight := VProjectionSet.Zooms[VZoom].TilePos2LonLat(Point(VTile.X + 1, VTile.Y));

                VTileBounds := JNXRect(
                  WGS84CoordToJNX(VBottomRight.Y),
                  WGS84CoordToJNX(VBottomRight.X),
                  WGS84CoordToJNX(VTopLeft.Y),
                  WGS84CoordToJNX(VTopLeft.X)
                );

                VStringStream.Size := 0;
                VStringStream.WriteBuffer(VData.Buffer^, VData.Size);

                VWriter.WriteTile(
                  I,
                  256,
                  256,
                  VTileBounds,
                  VStringStream.DataString
                );
              end;
            end;

            Inc(VTilesProcessed);
            if VTilesProcessed mod 100 = 0 then begin
              ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
            end;
          end;
        end;
      end;
    finally
      VStringStream.Free;
    end;
  finally
    VWriter.Free;
  end;
end;

end.
