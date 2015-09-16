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

unit u_ThreadExportToOgf2;

interface

uses
  Types,
  SysUtils,
  Classes,
  i_NotifierOperation,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  i_BinaryData,
  i_Bitmap32BufferFactory,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_ProjectionInfo,
  i_ProjectionSet,
  i_CoordConverterFactory,
  i_GeometryProjectedFactory,
  u_ResStrings,
  u_ThreadRegionProcessAbstract;

type
  TThreadExportToOgf2 = class(TThreadRegionProcessAbstract)
  private
    FZoom: Byte;
    FOgf2TileWidth: Integer;
    FOgf2TileHeight: Integer;
    FTileSaver: IBitmapTileSaver;
    FTargetFile: string;
    FImageProvider: IBitmapTileUniProvider;
    FProjectionSetFactory: IProjectionSetFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    function GetMapPreview(
      const ABitmapSaver: IBitmapTileSaver;
      out AMapPreviewWidth: Integer;
      out AMapPreviewHeight: Integer
    ): IBinaryData;
    function GetEmptyTile(
      const ABitmapSaver: IBitmapTileSaver
    ): IBinaryData;
    procedure SaveOziCalibrationMap(
      const AProjection: IProjection;
      const APixelRect: TRect
    );
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ACoordConverterFactory: IProjectionSetFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ATargetFile: string;
      const APolygon: IGeometryLonLatPolygon;
      const AImageProvider: IBitmapTileUniProvider;
      AZoom: Byte;
      const ATileSize: TPoint;
      const ATileSaver: IBitmapTileSaver
    );
  end;

implementation

uses
  Math,
  GR32,
  Ogf2Writer,
  t_GeoTypes,
  c_CoordConverter,
  i_MapCalibration,
  i_Bitmap32Static,
  i_GeometryProjected,
  u_TileIteratorByRect,
  u_MapCalibrationOzi,
  u_BitmapFunc,
  u_GeometryFunc,
  u_GeoFunc;

const
  cBackGroundColor = $CCCCCCCC;

{ TThreadExportToOgf2 }

constructor TThreadExportToOgf2.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ACoordConverterFactory: IProjectionSetFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ATargetFile: string;
  const APolygon: IGeometryLonLatPolygon;
  const AImageProvider: IBitmapTileUniProvider;
  AZoom: Byte;
  const ATileSize: TPoint;
  const ATileSaver: IBitmapTileSaver
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    Self.ClassName
  );
  FImageProvider := AImageProvider;
  FZoom := AZoom;
  FTargetFile := ATargetFile;
  FProjectionSetFactory := ACoordConverterFactory;
  FBitmapFactory := ABitmapFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FTileSaver := ATileSaver;
  FOgf2TileWidth := ATileSize.X;
  FOgf2TileHeight := ATileSize.Y;
end;

procedure TThreadExportToOgf2.SaveOziCalibrationMap(
  const AProjection: IProjection;
  const APixelRect: TRect
);
var
  VOziCalibrationMap: IMapCalibration;
begin
  VOziCalibrationMap := TMapCalibrationOzi.Create;
  VOziCalibrationMap.SaveCalibrationInfo(
    FTargetFile,
    APixelRect.TopLeft,
    APixelRect.BottomRight,
    AProjection
  );
end;


function TThreadExportToOgf2.GetMapPreview(
  const ABitmapSaver: IBitmapTileSaver;
  out AMapPreviewWidth: Integer;
  out AMapPreviewHeight: Integer
): IBinaryData;
var
  VBuffer: IBitmap32Buffer;
  VBitmapStatic: IBitmap32Static;
begin
  AMapPreviewWidth := 256;
  AMapPreviewHeight := 256;
  VBuffer :=
    FBitmapFactory.BufferFactory.BuildEmptyClear(
      Point(AMapPreviewWidth, AMapPreviewHeight),
      cBackGroundColor
    );
  VBitmapStatic := FBitmapFactory.BuildWithOwnBuffer(VBuffer);
  //TODO: generate some preview and make it sizeble
  Result := ABitmapSaver.Save(VBitmapStatic);
end;

function TThreadExportToOgf2.GetEmptyTile(
  const ABitmapSaver: IBitmapTileSaver
): IBinaryData;
var
  VBuffer: IBitmap32Buffer;
  VBitmapStatic: IBitmap32Static;
begin
  VBuffer :=
    FBitmapFactory.BufferFactory.BuildEmptyClear(
      Point(FOgf2TileWidth, FOgf2TileHeight),
      cBackGroundColor
    );
  VBitmapStatic := FBitmapFactory.BuildWithOwnBuffer(VBuffer);
  Result := ABitmapSaver.Save(VBitmapStatic);
end;

procedure TThreadExportToOgf2.ProcessRegion;
var
  VOfg2FileStream: TFileStream;
  VPreviewImageWidth: Integer;
  VPreviewImageHeight: Integer;
  VPreviewImageData: IBinaryData;
  VEmptyTile: IBinaryData;
  VBitmap: TCustomBitmap32;
  VBitmapTile: IBitmap32Static;
  VZoom: Byte;
  VTile: TPoint;
  VTileIterator: TTileIteratorByRectRecord;
  VSaver: IBitmapTileSaver;
  VProjectionSet: IProjectionSet;
  VWriter: TOgf2Writer;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VProjection: IProjection;
  VProjected: IGeometryProjectedPolygon;
  VLine: IGeometryProjectedSinglePolygon;
  VBounds: TDoubleRect;
  VPixelRect: TRect;
  VTileRect: TRect;
  I, J: Integer;
  VStaticBitmapCrop: IBitmap32Static;
  VDataToSave: IBinaryData;
begin
  inherited;
  VTilesProcessed := 0;
  VTilesToProcess := 0;

  VZoom := FZoom;
  VSaver := FTileSaver;
  VProjectionSet :=
    FProjectionSetFactory.GetProjectionSetByCode(
      CGoogleProjectionEPSG, // Merkator, WSG84, EPSG = 3785
      CTileSplitQuadrate256x256
    );
  VProjection := VProjectionSet.Zooms[VZoom];
  VProjected :=
    FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
      VProjection,
      Self.PolygLL
    );

  VLine := GetProjectedSinglePolygonByProjectedPolygon(VProjected);
  VBounds := VLine.Bounds;
  VPixelRect := RectFromDoubleRect(VBounds, rrOutside);
  VTileRect := VProjection.PixelRect2TileRect(VPixelRect);

  SaveOziCalibrationMap(
    VProjection,
    VProjection.TileRect2PixelRect(VTileRect)
  );

  VTileIterator.Init(VTileRect);
    VTilesToProcess := VTilesToProcess + VTileIterator.TilesTotal;

    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' +
      IntToStr(VTilesToProcess) + ' ' +
      SAS_STR_Files
    );

    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);

    VPreviewImageData :=
      GetMapPreview(
        VSaver,
        VPreviewImageWidth,
        VPreviewImageHeight
      );

    VEmptyTile := GetEmptyTile(VSaver);

    if (VPreviewImageData <> nil) and (VEmptyTile <> nil) then begin

      VOfg2FileStream := TFileStream.Create(FTargetFile, fmCreate);
      try
        VWriter := TOgf2Writer.Create(
          VOfg2FileStream,
          (VTileRect.Right - VTileRect.Left) * 256,
          (VTileRect.Bottom - VTileRect.Top) * 256,
          FOgf2TileWidth,
          FOgf2TileHeight,
          VPreviewImageWidth,
          VPreviewImageHeight,
          VPreviewImageData.Buffer,
          VPreviewImageData.Size,
          VEmptyTile.Buffer,
          VEmptyTile.Size
        );
        try
          VBitmap := TCustomBitmap32.Create;
          try
            VBitmap.Width := FOgf2TileWidth;
            VBitmap.Height := FOgf2TileHeight;

            while VTileIterator.Next(VTile) do begin
              if CancelNotifier.IsOperationCanceled(OperationID) then begin
                Exit;
              end;

              VBitmapTile :=
                FImageProvider.GetTile(
                  OperationID,
                  CancelNotifier,
                  VProjection,
                  VTile
                );

              for I := 0 to (256 div FOgf2TileWidth) - 1 do begin
                for J := 0 to (256 div FOgf2TileHeight) - 1 do begin
                  if VBitmapTile <> nil then begin
                    VBitmap.Clear(cBackGroundColor);

                    BlockTransfer(
                      VBitmap,
                      0,
                      0,
                      VBitmapTile,
                      Bounds(FOgf2TileWidth * I, FOgf2TileHeight * J, FOgf2TileWidth, FOgf2TileHeight),
                      dmOpaque
                    );

                    VStaticBitmapCrop :=
                      FBitmapFactory.Build(
                        Point(FOgf2TileWidth, FOgf2TileHeight),
                        VBitmap.Bits
                      );
                    VDataToSave := VSaver.Save(VStaticBitmapCrop);

                    VWriter.Add(
                      (VTile.X * 2) + I,
                      (VTile.Y * 2) + J,
                      VDataToSave.Buffer,
                      VDataToSave.Size
                    );
                  end else begin
                    VWriter.AddEmpty(
                      (VTile.X * 2) + I,
                      (VTile.Y * 2) + J
                    );
                  end;
                end;
              end;

              Inc(VTilesProcessed);

              if VTilesProcessed mod 100 = 0 then begin
                ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
              end;
            end;

            VWriter.SaveAllocationTable; // finalize export

          finally
            VBitmap.Free;
          end;
        finally
          VWriter.Free;
        end;
      finally
        VOfg2FileStream.Free;
      end;
    end;
end;

end.
