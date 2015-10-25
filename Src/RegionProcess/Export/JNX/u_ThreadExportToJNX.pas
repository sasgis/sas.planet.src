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

unit u_ThreadExportToJNX;

interface

uses
  t_GeoTypes,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_BitmapPostProcessing,
  u_ExportToJnxTask,
  u_ThreadRegionProcessAbstract;

type
  TThreadExportToJnx = class(TThreadRegionProcessAbstract)
  private
    FTasks: TExportTaskJnxArray;
    FTargetFile: string;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FProductName: string; // копирайт
    FMapName: string;  // имя карты
    FJNXVersion: byte;  // 3..4
    FZorder: integer;   // для 4 версии
    FProductID: integer; // 0,2,3,4,5,6,7,8,9
    FBitmapPostProcessing: IBitmapPostProcessing;

  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ATargetFile: string;
      const APolygon: IGeometryLonLatPolygon;
      const ATasks: TExportTaskJnxArray;
      const AProductName: string;
      const AMapName: string;
      const ABitmapPostProcessing: IBitmapPostProcessing;
      AJNXVersion: integer;
      AZorder: integer;
      AProductID: integer
    );
  end;

implementation

uses
  Types,
  SysUtils,
  ALString,
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
  i_GeometryProjected,
  u_ResStrings,
  u_TileIteratorByPolygon;

constructor TThreadExportToJnx.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ATargetFile: string;
  const APolygon: IGeometryLonLatPolygon;
  const ATasks: TExportTaskJnxArray;
  const AProductName: string;
  const AMapName: string;
  const ABitmapPostProcessing: IBitmapPostProcessing;
  AJNXVersion: integer;
  AZorder: integer;
  AProductID: integer
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    Self.ClassName
  );
  FTargetFile := ATargetFile;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FTasks := ATasks;
  FProductName := AProductName;
  FMapName := AMapName;
  FJNXVersion := AJNXVersion;
  FZorder := AZorder;
  FProductID := AProductID;
  FBitmapPostProcessing := ABitmapPostProcessing;
end;

procedure TThreadExportToJnx.ProcessRegion;
const
  ZoomToScale: array [0..26] of integer = (
    2446184, 1834628, 1223072, 611526, 458642, 305758, 152877, 114657, 76437,
    38218, 28664, 19109, 9554, 7166, 4777, 2388, 1791, 1194,
    597, 448, 298, 149, 112, 75, 37, 28, 19
  );

var
  i: integer;
  VBitmapTile: IBitmap32Static;
  VZoom: Byte;
  VTile: TPoint;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VSaver: IBitmapTileSaver;
  VProjectionSet: IProjectionSet;
  VStringStream: TALStringStream;
  VWriter: TMultiVolumeJNXWriter;
  VTileBounds: TJNXRect;
  VTopLeft: TDoublePoint;
  VBottomRight: TDoublePoint;
  VProjection: IProjection;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VData: IBinaryData;
  VTileStorage: ITileStorage;
  VVersion: IMapVersionRequest;
  VTileInfo: ITileInfoWithData;
  VContentTypeInfoBitmap: IContentTypeInfoBitmap;
  VRecompress: Boolean;
begin
  inherited;
  VTilesToProcess := 0;
  SetLength(VTileIterators, Length(FTasks));
  for i := 0 to Length(FTasks) - 1 do begin
    VZoom := FTasks[i].FZoom;
    VProjectionSet := FTasks[i].FTileStorage.ProjectionSet;
    VProjection := VProjectionSet.Zooms[VZoom];
    VProjectedPolygon :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        VProjection,
        PolygLL
      );
    VTileIterators[i] := TTileIteratorByPolygon.Create(VProjection, VProjectedPolygon);
    VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal;
  end;

  VWriter := TMultiVolumeJNXWriter.Create(FTargetFile);
  try
    VWriter.Levels := Length(FTasks);
    VWriter.ProductName := FProductName;
    VWriter.MapName := FMapName;
    VWriter.Version := FJNXVersion;
    VWriter.ZOrder := FZorder;
    VWriter.ProductID := FProductID;

    for i := 0 to Length(FTasks) - 1 do begin
      VWriter.LevelScale[i] := ZoomToScale[FTasks[i].FScale];
      VWriter.TileCount[i] := VTileIterators[i].TilesTotal;
      VWriter.LevelDescription[i] := FTasks[i].FLevelDesc;
      VWriter.LevelName[i] := FTasks[i].FLevelName;
      VWriter.LevelCopyright[i] := FTasks[i].FLevelCopyright;
      VWriter.LevelZoom[i] := FTasks[i].FZoom;
    end;

    try
      ProgressInfo.SetCaption(SAS_STR_ExportTiles);
      ProgressInfo.SetFirstLine(SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files);
      VStringStream := TALStringStream.Create('');
      try
        VTilesProcessed := 0;
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
        for i := 0 to Length(FTasks) - 1 do begin
          VSaver := FTasks[i].FSaver;
          VRecompress := FTasks[i].FRecompress or (FBitmapPostProcessing <> nil);
          VTileStorage := FTasks[i].FTileStorage;
          VVersion := FTasks[i].FMapVersion;
          VZoom := FTasks[i].FZoom;
          VProjectionSet := VTileStorage.ProjectionSet;
          VTileIterator := VTileIterators[i];
          while VTileIterator.Next(VTile) do begin
            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              exit;
            end;

            if Supports(VTileStorage.GetTileInfoEx(VTile, VZoom, VVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
              VData := Nil;
              if VRecompress or not ALSameText(VTileInfo.ContentType.GetContentType, 'image/jpg') then begin
                if Supports(VTileInfo.ContentType, IContentTypeInfoBitmap, VContentTypeInfoBitmap) then begin
                  VBitmapTile := VContentTypeInfoBitmap.GetLoader.Load(VTileInfo.TileData);
                  if FBitmapPostProcessing <> nil then begin
                    VBitmapTile := FBitmapPostProcessing.Process(VBitmapTile);
                  end;
                  if Assigned(VBitmapTile) then begin
                    VData := VSaver.Save(VBitmapTile);
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
                  i,
                  256,
                  256,
                  VTileBounds,
                  VStringStream.DataString
                );
              end;
            end;
            inc(VTilesProcessed);
            if VTilesProcessed mod 100 = 0 then begin
              ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
            end;
          end;
        end;
      finally
        VStringStream.Free;
      end;
    finally
      for i := 0 to Length(VTileIterators) - 1 do begin
        VTileIterators[i] := nil;
      end;
      VTileIterators := nil;
    end;
  finally
    VWriter.Free;
  end;
end;

end.
