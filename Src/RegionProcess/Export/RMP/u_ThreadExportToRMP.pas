{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit u_ThreadExportToRMP;

interface

uses
  Types,
  Windows,
  SysUtils,
  Classes,
  i_BinaryData,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_ProjectionSet,
  i_GeometryProjected,
  i_GeometryProjectedFactory,
  i_GeometryLonLat,
  i_TileInfoBasic,
  i_TileStorage,
  i_TileIterator,
  i_MapVersionRequest,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  i_Bitmap32BufferFactory,
  i_ImageResamplerFactory,
  u_ThreadExportAbstract,
  u_RMPWriter;

type
  TThreadExportToRMP = class(TThreadExportAbstract)
  private
    type
      TProcessSingleGeometry = procedure(
        const ARMPWriter: TRMPFileWriter;
        const AZoom: Byte;
        const APolygon: IGeometryProjectedSinglePolygon;
        const ATilesToProcess: Int64;
        var ATilesProcessed: Int64
      ) of object;
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FProjectionSet: IProjectionSet;
    FExportPath: string;
    FExportFileName: string;
    FTileStorage: ITileStorage;
    FMapVersion: IMapVersionRequest;
    FBitmapTileSaver: IBitmapTileSaver;
    FBitmapUniProvider: IBitmapUniProvider;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FImageResamplerFactory: IImageResamplerFactory;
    FDirectTilesCopy: Boolean;
    FAlignSelection: Boolean;
    FImgName, FProduct, FProvider, FComments: AnsiString;
    procedure CalcCounts(
      const AProjectedPolygons: array of IGeometryProjectedPolygon;
      out ARmpLayersCount: Integer;
      out ATilesToProcessCount: Int64
    );
    procedure ProcessSingleGeometry(
      const ARMPWriter: TRMPFileWriter;
      const AZoom: Byte;
      const APolygon: IGeometryProjectedSinglePolygon;
      const ATilesToProcess: Int64;
      var ATilesProcessed: Int64
    );
    procedure ProcessSingleGeometryUni(
      const ARMPWriter: TRMPFileWriter;
      const AZoom: Byte;
      const APolygon: IGeometryProjectedSinglePolygon;
      const ATilesToProcess: Int64;
      var ATilesProcessed: Int64
    );
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AExportPath: string;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AProjectionSet: IProjectionSet;
      const APolygon: IGeometryLonLatPolygon;
      const AZoomArr: TByteDynArray;
      const ATileStorage: ITileStorage;
      const AMapVersion: IMapVersionRequest;
      const ABitmapTileSaver: IBitmapTileSaver;
      const ABitmapProvider: IBitmapUniProvider;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AImageResamplerFactory: IImageResamplerFactory;
      const ADirectTilesCopy: Boolean;
      const AAlignSelection: Boolean;
      const AProduct, AProvider: AnsiString
    );
  end;

implementation

uses
  Math,
  GR32,
  librmp,
  c_CoordConverter,
  t_GeoTypes,
  i_Projection,
  i_Bitmap32Static,
  i_TileRect,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap,
  u_GeoFunc,
  u_GeometryFunc,
  u_ResStrings;

{ TThreadExportToRMP }

constructor TThreadExportToRMP.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AExportPath: string;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AProjectionSet: IProjectionSet;
  const APolygon: IGeometryLonLatPolygon;
  const AZoomArr: TByteDynArray;
  const ATileStorage: ITileStorage;
  const AMapVersion: IMapVersionRequest;
  const ABitmapTileSaver: IBitmapTileSaver;
  const ABitmapProvider: IBitmapUniProvider;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AImageResamplerFactory: IImageResamplerFactory;
  const ADirectTilesCopy: Boolean;
  const AAlignSelection: Boolean;
  const AProduct, AProvider: AnsiString
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    Self.ClassName
  );
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FProjectionSet := AProjectionSet;
  FExportPath := ExtractFilePath(AExportPath);
  FExportFileName := AExportPath;
  FTileStorage := ATileStorage;
  FMapVersion := AMapVersion;
  FBitmapTileSaver := ABitmapTileSaver;
  FBitmapUniProvider := ABitmapProvider;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FImageResamplerFactory := AImageResamplerFactory;
  FDirectTilesCopy := ADirectTilesCopy;
  FAlignSelection := AAlignSelection;

  FImgName := '1';
  FProduct := AProduct;
  FProvider := AProvider;
  FComments := 'Created with SAS.Planet';
end;

procedure TThreadExportToRMP.ProcessRegion;
var
  I, J: Integer;
  VZoom: Byte;
  VRmpLayersCount: Integer;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VProjectedPolygons: array of IGeometryProjectedPolygon;
  VProjection: IProjection;
  VSingleLine: IGeometryProjectedSinglePolygon;
  VMultiProjected: IGeometryProjectedMultiPolygon;
  VRMPWriter: TRMPFileWriter;
  VProcessSingleGeometry: TProcessSingleGeometry;
  VIsToLatLon: Boolean;
begin
  inherited;

  VIsToLatLon :=
    FProjectionSet.Zooms[0].ProjectionType.ProjectionEPSG = CGELonLatProjectionEPSG;

  if FDirectTilesCopy or VIsToLatLon then begin
    Assert(FTileStorage <> nil);
    VProcessSingleGeometry := Self.ProcessSingleGeometry;
  end else begin
    Assert(FBitmapUniProvider <> nil);
    Assert(FBitmapTileSaver <> nil);
    VProcessSingleGeometry := Self.ProcessSingleGeometryUni;
  end;

  if not DirectoryExists(FExportPath) then begin
    if not ForceDirectories(FExportPath) then begin
      RaiseLastOSError;
    end;
  end;

  SetLength(VProjectedPolygons, Length(FZooms));

  VTilesToProcess := 0;

  for I := 0 to Length(FZooms) - 1 do begin
    VProjection := FProjectionSet.Zooms[FZooms[I]];
    VProjectedPolygons[I] :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        VProjection,
        PolygLL
      );
  end;

  CalcCounts(VProjectedPolygons, VRmpLayersCount, VTilesToProcess);

  VRMPWriter :=
    TRMPFileWriter.Create(
      FExportFileName,
      FImgName, FProduct, FProvider, FComments,
      VRmpLayersCount
    );
  try
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      Format('%s %d %s', [SAS_STR_AllSaves, VTilesToProcess, SAS_STR_Files])
    );
    ProgressFormUpdateOnProgress(0, VTilesToProcess);

    for I := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[I];
      if Supports(VProjectedPolygons[I], IGeometryProjectedSinglePolygon, VSingleLine) then begin
        VProcessSingleGeometry(
          VRMPWriter, VZoom, VSingleLine, VTilesToProcess, VTilesProcessed
        );
      end else if Supports(VProjectedPolygons[I], IGeometryProjectedMultiPolygon, VMultiProjected) then begin
        for J := 0 to VMultiProjected.Count - 1 do begin
          VProcessSingleGeometry(
            VRMPWriter, VZoom, VMultiProjected.Item[J], VTilesToProcess, VTilesProcessed
          );
        end;
      end else begin
        Assert(False);
        Exit;
      end;
    end;

  finally
    VRMPWriter.Free;
  end;
end;

procedure TThreadExportToRMP.ProcessSingleGeometry(
  const ARMPWriter: TRMPFileWriter;
  const AZoom: Byte;
  const APolygon: IGeometryProjectedSinglePolygon;
  const ATilesToProcess: Int64;
  var ATilesProcessed: Int64
);
var
  X, Y: Integer;
  VRmpX, VRmpY: Integer;
  VRmpStartPoint: TPoint;
  VRmpTileLon, VRmpTileLat: Double;
  VTile: Types.TPoint;
  VTilesRect, VPixelRect: Types.TRect;
  VRectLL, VAlignRectLL: TDoubleRect;
  VBounds, VRect: TDoubleRect;
  VProjection: IProjection;
  VTileInfo: ITileInfoWithData;
  VTileInfoBasic: ITileInfoBasic;
  VBitmapTile: IBitmap32Static;
  VTileData: IBinaryData;
begin
  VBounds := APolygon.Bounds;
  VProjection := FProjectionSet.Zooms[AZoom];

  VTilesRect :=
    RectFromDoubleRect(
      VProjection.PixelRectFloat2TileRectFloat(VBounds),
      rrOutside
    );

  // --- Calculate the position of the upper left tile ---
  VRectLL := VProjection.PixelRectFloat2LonLatRect(VBounds);
  VRect := VProjection.TilePos2LonLatRect(VTilesRect.TopLeft);
  librmp.CalcRmpTilePos(
    VRectLL.Left,
    VRectLL.Top,
    (VRect.Right - VRect.Left),
    (VRect.Top - VRect.Bottom),
    VRmpStartPoint.X,
    VRmpStartPoint.Y,
    VRmpTileLon,
    VRmpTileLat
  );

  if FAlignSelection then begin
    VAlignRectLL := DoubleRect(NaN, NaN, NaN, NaN);
  end else begin
    VAlignRectLL := VRectLL;
  end;

  ARMPWriter.ForceNewLayer(
    VAlignRectLL.Left, VAlignRectLL.Top,
    VAlignRectLL.Right, VAlignRectLL.Bottom
  );

  VRmpY := VRmpStartPoint.Y;

  for Y := VTilesRect.Top to VTilesRect.Bottom - 1 do begin

    ARMPWriter.ForceNewLayer(
      VAlignRectLL.Left, VAlignRectLL.Top,
      VAlignRectLL.Right, VAlignRectLL.Bottom
    );

    VRmpX := VRmpStartPoint.X;

    for X := VTilesRect.Left to VTilesRect.Right - 1 do begin
      if CancelNotifier.IsOperationCanceled(OperationID) then begin
        Exit;
      end;

      VTile := Types.Point(X, Y);
      VRect := VProjection.TilePos2PixelRectFloat(VTile);
      VRectLL := VProjection.TilePos2LonLatRect(VTile);
      VPixelRect := VProjection.TilePos2PixelRect(VTile);
      VTileData := nil;

      if APolygon.IsRectIntersectPolygon(VRect) then begin
        if FDirectTilesCopy then begin
          VTileInfoBasic :=
            FTileStorage.GetTileInfoEx(VTile, AZoom, FMapVersion, gtimWithData);
          if Supports(VTileInfoBasic, ITileInfoWithData, VTileInfo) then begin
            VTileData := VTileInfo.TileData;
          end;
        end else begin
          VBitmapTile :=
            FBitmapUniProvider.GetBitmap(
              Self.OperationID,
              Self.CancelNotifier,
              VProjection,
              VPixelRect
            );
          if Assigned(VBitmapTile) then begin
            Assert(VBitmapTile.Size.X = 256);
            Assert(VBitmapTile.Size.Y = 256);
            VTileData := FBitmapTileSaver.Save(VBitmapTile);
          end;
        end;
      end;

      if Assigned(VTileData) then begin
        ARMPWriter.AddTile(
          VRmpX, VRmpY,
          VRectLL.Left, VRectLL.Top,
          VRectLL.Right, VRectLL.Bottom,
          VTileData.Buffer, VTileData.Size
        );
      end else begin
        ARMPWriter.AddEmptyTile(
          VRmpX, VRmpY,
          VRectLL.Left, VRectLL.Top,
          VRectLL.Right, VRectLL.Bottom
        );
      end;

      Inc(ATilesProcessed);
      if ATilesProcessed mod 100 = 0 then begin
        ProgressFormUpdateOnProgress(ATilesProcessed, ATilesToProcess);
      end;
      Inc(VRmpX);
    end;
    Inc(VRmpY);
  end;
end;

procedure TThreadExportToRMP.ProcessSingleGeometryUni(
  const ARMPWriter: TRMPFileWriter;
  const AZoom: Byte;
  const APolygon: IGeometryProjectedSinglePolygon;
  const ATilesToProcess: Int64;
  var ATilesProcessed: Int64
);

  procedure _ProcessSubRect(
    const ATilesRect: Types.TRect;
    const ATileWidth, ATileHeight: Double;
    const AProjection: IProjection;
    const AResampler: TCustomResampler
  );
  var
    X, Y: Integer;
    VRmpStartPoint, VRmpEndPoint: TPoint;
    VPixelRect: Types.TRect;
    VPixelRectFloat: TDoubleRect;
    VRectLL, VSubRectLL: TDoubleRect;
    VTileData: IBinaryData;
    VBitmap: TBitmap32ByStaticBitmap;
    VBitmapTile, VRmpBitmapTile: IBitmap32Static;
  begin
    VRectLL := AProjection.TileRect2LonLatRect(ATilesRect);

    VRmpStartPoint.X := Floor((VRectLL.Left + 180) / ATileWidth);
    VRmpStartPoint.Y := Floor((-VRectLL.Top + 90) / ATileHeight);

    VRmpEndPoint.X := Round((VRectLL.Right + 180) / ATileWidth);
    VRmpEndPoint.Y := Round((-VRectLL.Bottom + 90) / ATileHeight);

    for X := VRmpStartPoint.X to VRmpEndPoint.X - 1 do begin

      // ToDo: Если колонка тайлов не вмещается в текущий слой, нужно
      // инициировать создание нового слоя через ForceNewLayer

      for Y := VRmpStartPoint.Y to VRmpEndPoint.Y - 1 do begin

        if CancelNotifier.IsOperationCanceled(OperationID) then begin
          Exit;
        end;

        VTileData := nil;

        VSubRectLL :=
          DoubleRect(
            X * ATileWidth - 180,
            -(Y * ATileHeight - 90),
            (X + 1) * ATileWidth - 180,
            -((Y + 1) * ATileHeight - 90)
          );

        VPixelRectFloat := AProjection.LonLatRect2PixelRectFloat(VSubRectLL);
        VPixelRect := RectFromDoubleRect(VPixelRectFloat, rrClosest);

        if APolygon.IsRectIntersectPolygon(VPixelRectFloat) then begin
          VBitmapTile :=
            FBitmapUniProvider.GetBitmap(
              Self.OperationID,
              Self.CancelNotifier,
              AProjection,
              VPixelRect
            );

          if Assigned(VBitmapTile) then begin
            if (VBitmapTile.Size.X = 256) and (VBitmapTile.Size.Y = 256) then begin
              VRmpBitmapTile := VBitmapTile;
            end else begin
              VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
              try
                VBitmap.SetSize(256, 256);
                VBitmap.Clear(0);
                StretchTransferFull(
                  VBitmap,
                  VBitmap.BoundsRect,
                  VBitmapTile,
                  AResampler,
                  dmOpaque
                );
                VRmpBitmapTile := VBitmap.MakeAndClear;
              finally
                VBitmap.Free;
              end;
            end;
            VTileData := FBitmapTileSaver.Save(VRmpBitmapTile);
          end;
        end;

        if Assigned(VTileData) then begin
          ARMPWriter.AddTile(
            X, Y,
            VSubRectLL.Left, VSubRectLL.Top,
            VSubRectLL.Right, VSubRectLL.Bottom,
            VTileData.Buffer, VTileData.Size
          );
        end else begin
          ARMPWriter.AddEmptyTile(
            X, Y,
            VSubRectLL.Left, VSubRectLL.Top,
            VSubRectLL.Right, VSubRectLL.Bottom
          );
        end;
      end;

      Inc(ATilesProcessed);
      if ATilesProcessed mod 100 = 0 then begin
        ProgressFormUpdateOnProgress(ATilesProcessed, ATilesToProcess);
      end;
    end;
  end;

var
  Y: Integer;
  VTilesRect: Types.TRect;
  VBounds, VAlignRectLL, VTileRectLL: TDoubleRect;
  VProjection: IProjection;
  VResampler: TCustomResampler;
begin
  VBounds := APolygon.Bounds;
  VProjection := FProjectionSet.Zooms[AZoom];

  VTilesRect :=
    RectFromDoubleRect(
      VProjection.PixelRectFloat2TileRectFloat(VBounds),
      rrOutside
    );

  VAlignRectLL := VProjection.PixelRectFloat2LonLatRect(VBounds);

  VResampler := FImageResamplerFactory.CreateResampler;
  try
    for Y := VTilesRect.Top to VTilesRect.Bottom - 1 do begin
      ARMPWriter.ForceNewLayer(
        VAlignRectLL.Left, VAlignRectLL.Top,
        VAlignRectLL.Right, VAlignRectLL.Bottom
      );

      VTileRectLL :=
        VProjection.TilePos2LonLatRect(
          Types.Point(VTilesRect.Left, Y)
        );

      _ProcessSubRect(
        Types.Rect(VTilesRect.Left, Y, VTilesRect.Right, Y+1),
        (VTileRectLL.Right - VTileRectLL.Left),
        (VTileRectLL.Top - VTileRectLL.Bottom),
        VProjection,
        VResampler
      );
    end;
  finally
    VResampler.Free;
  end;
end;

procedure TThreadExportToRMP.CalcCounts(
  const AProjectedPolygons: array of IGeometryProjectedPolygon;
  out ARmpLayersCount: Integer;
  out ATilesToProcessCount: Int64
);

const
  cMaxTilesCountPerRmpLayer = 70*(70-1);

  procedure _CalcCountsPerPolygon(
    const AZoom: Byte;
    const APolygon: IGeometryProjectedSinglePolygon
  );
  var
    X, Y: Integer;
    VTile: Types.TPoint;
    VTilesRect: Types.TRect;
    VRect, VBounds: TDoubleRect;
    VProjection: IProjection;
    VTilesInLayer: Integer;
  begin
    VBounds := APolygon.Bounds;
    VProjection := FProjectionSet.Zooms[AZoom];
    VTilesRect :=
      RectFromDoubleRect(
        VProjection.PixelRectFloat2TileRectFloat(VBounds),
        rrOutside
      );
    Inc(ARmpLayersCount);
    for Y := VTilesRect.Top to VTilesRect.Bottom - 1 do begin
      Inc(ARmpLayersCount);
      Inc(ATilesToProcessCount);
      VTilesInLayer := 0;
      for X := VTilesRect.Left to VTilesRect.Right - 1 do begin
        Inc(VTilesInLayer);
        Inc(ATilesToProcessCount);
        VTile := Types.Point(X, Y);
        VRect := VProjection.TilePos2PixelRectFloat(VTile);
        if APolygon.IsRectIntersectPolygon(VRect) then begin
          if VTilesInLayer > cMaxTilesCountPerRmpLayer then begin
            Inc(ARmpLayersCount);
          end;
        end;
      end;
    end;
  end;

var
  I, J: Integer;
  VZoom: Byte;
  VSingleLine: IGeometryProjectedSinglePolygon;
  VMultiProjected: IGeometryProjectedMultiPolygon;
begin
  for I := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[I];
    if Supports(AProjectedPolygons[I], IGeometryProjectedSinglePolygon, VSingleLine) then begin
       _CalcCountsPerPolygon(VZoom, VSingleLine);
    end else if Supports(AProjectedPolygons[I], IGeometryProjectedMultiPolygon, VMultiProjected) then begin
      for J := 0 to VMultiProjected.Count - 1 do begin
        _CalcCountsPerPolygon(VZoom, VMultiProjected.Item[J]);
      end;
    end else begin
      Assert(False);
      Exit;
    end;
  end;
end;

end.
