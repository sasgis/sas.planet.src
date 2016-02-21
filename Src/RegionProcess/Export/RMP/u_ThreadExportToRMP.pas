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
    FRowsPerRmpLayer: Integer;
    FImgName, FProduct, FProvider, FComments: AnsiString;
    procedure CalcCounts(
      const AProjectedPolygons: array of IGeometryProjectedPolygon;
      const AMaxTilesCountPerRmpLayer: Integer;
      const ACalcCountUni: Boolean;
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
  VIsExportUni: Boolean;
begin
  inherited;

  ProgressInfo.SetCaption(SAS_STR_ExportTiles);
  ProgressInfo.SetFirstLine('Initializing...');

  VIsExportUni :=
    not FDirectTilesCopy and
    (FProjectionSet.Zooms[0].ProjectionType.ProjectionEPSG <> CGELonLatProjectionEPSG);

  if not VIsExportUni then begin
    Assert(FTileStorage <> nil);
    FRowsPerRmpLayer := 1;
    VProcessSingleGeometry := Self.ProcessSingleGeometry;
  end else begin
    Assert(FBitmapUniProvider <> nil);
    Assert(FBitmapTileSaver <> nil);
    FRowsPerRmpLayer := 128;
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

  CalcCounts(
    VProjectedPolygons,
    TRMPFileWriter.MaxTilesPerLayer,
    VIsExportUni,
    VRmpLayersCount,
    VTilesToProcess
  );

  VRMPWriter :=
    TRMPFileWriter.Create(
      FExportFileName,
      FImgName, FProduct, FProvider, FComments,
      VRmpLayersCount
    );
  try
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

    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
  finally
    ProgressInfo.SetFirstLine('Finalizing...');
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

  VRectLL := VProjection.PixelRectFloat2LonLatRect(VBounds);
  VRect := VProjection.TilePos2LonLatRect(VTilesRect.TopLeft);

  librmp.LonLatToRmpXY(
    VRectLL.Left,
    VRectLL.Top,
    (VRect.Right - VRect.Left),
    (VRect.Top - VRect.Bottom),
    VRmpStartPoint.X,
    VRmpStartPoint.Y
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

{$DEFINE FULL_TILES}

procedure TThreadExportToRMP.ProcessSingleGeometryUni(
  const ARMPWriter: TRMPFileWriter;
  const AZoom: Byte;
  const APolygon: IGeometryProjectedSinglePolygon;
  const ATilesToProcess: Int64;
  var ATilesProcessed: Int64
);

  {$IFNDEF FULL_TILES}
  function _GetBitmapBounds(
    const ABitmapRectLL: TDoubleRect;
    const ASrcRectLL: TDoubleRect
  ): TRect;
  var
    I: Integer;
    VImgRect: TRect;
    VPixSize: TDoublePoint;
  begin
    VImgRect := Types.Rect(0, 0, 256, 256);
    VPixSize.X := (ABitmapRectLL.Right - ABitmapRectLL.Left) / 256;
    VPixSize.Y := (ABitmapRectLL.Top - ABitmapRectLL.Bottom) / 256;
    for I := 0 to 256 do begin
      if ABitmapRectLL.Top - I * VPixSize.Y > ASrcRectLL.Top then begin
        VImgRect.Top := I;
      end;
      if ABitmapRectLL.Bottom + I * VPixSize.Y < ASrcRectLL.Bottom then begin
        VImgRect.Bottom := 256 - I;
      end;
      if ABitmapRectLL.Left + I * VPixSize.X < ASrcRectLL.Left then begin
        VImgRect.Left := I;
      end;
      if ABitmapRectLL.Right - I * VPixSize.X > ASrcRectLL.Right then begin
        VImgRect.Right := 256 - I;
      end;
    end;
    Result :=
      Bounds(
        VImgRect.Left, VImgRect.Top,
        VImgRect.Right - VImgRect.Left,
        VImgRect.Bottom - VImgRect.Top
      );
  end;
  {$ENDIF}

  procedure _ProcessSubRect(
    const ATilesRect: Types.TRect;
    const AAlignRectLL: TDoubleRect;
    const AProjection: IProjection;
    const AResampler: TCustomResampler
  );
  var
    I: Integer;
    X, Y: Integer;
    VTilesPerCol: Integer;
    VTileWidth, VTileHeight: Double;
    VDoublePoint: TDoublePoint;
    VRmpStartPoint, VRmpEndPoint: TPoint;
    VPixelRect: Types.TRect;
    VPixelRectFloat: TDoubleRect;
    VRectLL, VSubRectLL, VLonLatRect: TDoubleRect;
    VAlignRectLL: TDoubleRect;
    VTileData: array of IBinaryData;
    VBitmap: TBitmap32ByStaticBitmap;
    VBitmapTile, VRmpBitmapTile: IBitmap32Static;
    VRMPTiles: TRMPFileWriterTileRecArray;
    VBitmapBounds: TRect;
    VProcess: Boolean;
  begin
    VRectLL := AProjection.TileRect2LonLatRect(ATilesRect);

    VTileWidth := (VRectLL.Right - VRectLL.Left) / (ATilesRect.Right - ATilesRect.Left);
    VTileHeight := (VRectLL.Top - VRectLL.Bottom) / (ATilesRect.Bottom - ATilesRect.Top);

    IntersecLonLatRect(VAlignRectLL, AAlignRectLL, VRectLL);

    ARMPWriter.ForceNewLayer(
      VAlignRectLL.Left, VAlignRectLL.Top,
      VAlignRectLL.Right, VAlignRectLL.Bottom
    );

    librmp.LonLatToRmpXYFloat(
      VRectLL.Left,
      VRectLL.Top,
      VTileWidth,
      VTileHeight,
      VDoublePoint.X,
      VDoublePoint.Y
    );
    VRmpStartPoint := PointFromDoublePoint(VDoublePoint, prToTopLeft);

    librmp.LonLatToRmpXYFloat(
      VRectLL.Right,
      VRectLL.Bottom,
      VTileWidth,
      VTileHeight,
      VDoublePoint.X,
      VDoublePoint.Y
    );
    VRmpEndPoint := PointFromDoublePoint(VDoublePoint, prToBottomRight);

    VTilesPerCol := VRmpEndPoint.Y - VRmpStartPoint.Y;

    if VTilesPerCol = 0 then begin
      Assert(False);
      Exit;
    end;

    SetLength(VRMPTiles, VTilesPerCol);
    SetLength(VTileData, VTilesPerCol);

    for X := VRmpStartPoint.X to VRmpEndPoint.X - 1 do begin

      FillChar(VRMPTiles[0], SizeOf(VRMPTiles[0]) * VTilesPerCol, 0);
      for I := Low(VTileData) to High(VTileData) do begin
        VTileData[I] := nil;
      end;
      I := 0;

      for Y := VRmpStartPoint.Y to VRmpEndPoint.Y - 1 do begin

        if CancelNotifier.IsOperationCanceled(OperationID) then begin
          Exit;
        end;

        librmp.RmpXYToLonLat(
          X, Y, VTileWidth, VTileHeight, VSubRectLL.Left, VSubRectLL.Top
        );
        librmp.RmpXYToLonLat(
          X+1, Y+1, VTileWidth, VTileHeight, VSubRectLL.Right, VSubRectLL.Bottom
        );

        VLonLatRect := VSubRectLL;
        AProjection.ProjectionType.ValidateLonLatRect(VLonLatRect);

        {$IFDEF FULL_TILES}
        VPixelRectFloat := AProjection.LonLatRect2PixelRectFloat(VLonLatRect);
        VProcess := APolygon.IsRectIntersectPolygon(VPixelRectFloat);
        {$ELSE}
        VProcess := IntersecLonLatRect(VLonLatRect, VLonLatRect, VRectLL);
        if VProcess then begin
          VPixelRectFloat := AProjection.LonLatRect2PixelRectFloat(VLonLatRect);
          VProcess := APolygon.IsRectIntersectPolygon(VPixelRectFloat);
        end;
        {$ENDIF}

        if VProcess then begin
          VPixelRect := RectFromDoubleRect(VPixelRectFloat, rrClosest);

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
                VBitmap.Clear(clBlack32);

                {$IFDEF FULL_TILES}
                VBitmapBounds := VBitmap.BoundsRect;
                {$ELSE}
                VBitmapBounds := _GetBitmapBounds(VSubRectLL, VLonLatRect);
                {$ENDIF}

                StretchTransferFull(
                  VBitmap,
                  VBitmapBounds,
                  VBitmapTile,
                  AResampler,
                  dmOpaque
                );

                VRmpBitmapTile := VBitmap.MakeAndClear;
              finally
                VBitmap.Free;
              end;
            end;
            VTileData[I] := FBitmapTileSaver.Save(VRmpBitmapTile);
          end;
        end;

        VRMPTiles[I].X := X;
        VRMPTiles[I].Y := Y;
        VRMPTiles[I].Left := VSubRectLL.Left;
        VRMPTiles[I].Top := VSubRectLL.Top;
        VRMPTiles[I].Right := VSubRectLL.Right;
        VRMPTiles[I].Bottom := VSubRectLL.Bottom;

        if Assigned(VTileData[I]) then begin
          VRMPTiles[I].Data := VTileData[I].Buffer;
          VRMPTiles[I].Size := VTileData[I].Size;
        end else begin
          VRMPTiles[I].Data := nil;
          VRMPTiles[I].Size := 0;
        end;

        Inc(I);

        Inc(ATilesProcessed);
        if ATilesProcessed mod 100 = 0 then begin
          ProgressFormUpdateOnProgress(ATilesProcessed, ATilesToProcess);
        end;
      end;

      // add tiles column
      ARMPWriter.AddTiles(VRMPTiles);
    end;
  end;

var
  VTop, VBottom: Integer;
  VTilesRect, VSubRect: Types.TRect;
  VBounds, VAlignRectLL: TDoubleRect;
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
    VTop := VTilesRect.Top;
    while VTop < VTilesRect.Bottom do begin
      VBottom := VTop + FRowsPerRmpLayer;
      if VBottom > VTilesRect.Bottom then begin
        VBottom := VTilesRect.Bottom;
      end;
      VSubRect := Types.Rect(VTilesRect.Left, VTop, VTilesRect.Right, VBottom);
      _ProcessSubRect(VSubRect, VAlignRectLL, VProjection, VResampler);
      Inc(VTop, FRowsPerRmpLayer);
    end;
  finally
    VResampler.Free;
  end;
end;

procedure TThreadExportToRMP.CalcCounts(
  const AProjectedPolygons: array of IGeometryProjectedPolygon;
  const AMaxTilesCountPerRmpLayer: Integer;
  const ACalcCountUni: Boolean;
  out ARmpLayersCount: Integer;
  out ATilesToProcessCount: Int64
);

  procedure _CalcCountsPerPolygon(
    const AZoom: Byte;
    const APolygon: IGeometryProjectedSinglePolygon
  );
  var
    X, Y: Integer;
    VTilesRect: Types.TRect;
    VBounds: TDoubleRect;
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
    for Y := VTilesRect.Top to VTilesRect.Bottom - 1 do begin
      Inc(ARmpLayersCount); // 1
      VTilesInLayer := 0;
      for X := VTilesRect.Left to VTilesRect.Right - 1 do begin
        Inc(VTilesInLayer);
        Inc(ATilesToProcessCount);
        if VTilesInLayer >= AMaxTilesCountPerRmpLayer then begin
          Inc(ARmpLayersCount); // 2
          VTilesInLayer := 0;
        end;
      end;
    end;
  end;

  procedure _CalcCountsPerPolygonUni(
    const AZoom: Byte;
    const APolygon: IGeometryProjectedSinglePolygon
  );
  var
    X: Integer;
    VTop, VBottom: Integer;
    VTileWidth, VTileHeight: Double;
    VRmpStartPoint, VRmpEndPoint: Types.TPoint;
    VTilesRect, VSubRect: Types.TRect;
    VDoublePoint: TDoublePoint;
    VBounds, VRectLL: TDoubleRect;
    VProjection: IProjection;
    VTilesInLayer, VTilesPerCol: Integer;
  begin
    VBounds := APolygon.Bounds;
    VProjection := FProjectionSet.Zooms[AZoom];
    VTilesRect :=
      RectFromDoubleRect(
        VProjection.PixelRectFloat2TileRectFloat(VBounds),
        rrOutside
      );
    VTop := VTilesRect.Top;
    while VTop < VTilesRect.Bottom do begin
      Inc(ARmpLayersCount); // 1

      VBottom := VTop + FRowsPerRmpLayer;
      if VBottom > VTilesRect.Bottom then begin
        VBottom := VTilesRect.Bottom;
      end;
      VSubRect := Types.Rect(VTilesRect.Left, VTop, VTilesRect.Right, VBottom);

      VRectLL := VProjection.TileRect2LonLatRect(VSubRect);

      VTileWidth := (VRectLL.Right - VRectLL.Left) / (VSubRect.Right - VSubRect.Left);
      VTileHeight := (VRectLL.Top - VRectLL.Bottom) / (VSubRect.Bottom - VSubRect.Top);

      librmp.LonLatToRmpXYFloat(
        VRectLL.Left,
        VRectLL.Top,
        VTileWidth,
        VTileHeight,
        VDoublePoint.X,
        VDoublePoint.Y
      );
      VRmpStartPoint := PointFromDoublePoint(VDoublePoint, prToTopLeft);

      librmp.LonLatToRmpXYFloat(
        VRectLL.Right,
        VRectLL.Bottom,
        VTileWidth,
        VTileHeight,
        VDoublePoint.X,
        VDoublePoint.Y
      );
      VRmpEndPoint := PointFromDoublePoint(VDoublePoint, prToBottomRight);

      VTilesPerCol := VRmpEndPoint.Y - VRmpStartPoint.Y;
      VTilesInLayer := 0;

      for X := VRmpStartPoint.X to VRmpEndPoint.X - 1 do begin
        Inc(ATilesToProcessCount, VTilesPerCol);
        if (VTilesInLayer + VTilesPerCol) >= AMaxTilesCountPerRmpLayer then begin
          Inc(ARmpLayersCount); // 2
          VTilesInLayer := 0;
        end;
        Inc(VTilesInLayer, VTilesPerCol);
      end;

      Inc(VTop, FRowsPerRmpLayer);
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
       if ACalcCountUni then begin
         _CalcCountsPerPolygonUni(VZoom, VSingleLine);
       end else begin
         _CalcCountsPerPolygon(VZoom, VSingleLine);
       end;
    end else if Supports(AProjectedPolygons[I], IGeometryProjectedMultiPolygon, VMultiProjected) then begin
      for J := 0 to VMultiProjected.Count - 1 do begin
        if ACalcCountUni then begin
          _CalcCountsPerPolygonUni(VZoom, VMultiProjected.Item[J]);
        end else begin
          _CalcCountsPerPolygon(VZoom, VMultiProjected.Item[J]);
        end;
      end;
    end else begin
      Assert(False);
      Exit;
    end;
  end;
end;

end.
