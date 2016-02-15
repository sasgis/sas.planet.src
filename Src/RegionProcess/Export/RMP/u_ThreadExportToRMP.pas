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
    FDirectTilesCopy: Boolean;
    FAlignSelection: Boolean;
    FImgName, FProduct, FProvider, FComments: AnsiString;
    function CalcRmpLayersCount(
      const AProjectedPolygons: array of IGeometryProjectedPolygon;
      const ATilesCountPerRmpLayer: Integer
    ): Integer;
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
      const ADirectTilesCopy: Boolean;
      const AAlignSelection: Boolean;
      const AProduct, AProvider: AnsiString
    );
  end;

implementation

uses
  Math,
  librmp,
  c_CoordConverter,
  t_GeoTypes,
  i_Projection,
  i_Bitmap32Static,
  i_TileRect,
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
  VCount: Int64;
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

    VCount := CalcTileCountInProjectedPolygon(VProjection, VProjectedPolygons[I]);
    Inc(VTilesToProcess, VCount);
  end;

  VCount := CalcRmpLayersCount(VProjectedPolygons, 70*(70-1));

  VRMPWriter :=
    TRMPFileWriter.Create(
      FExportFileName,
      FImgName, FProduct, FProvider, FComments,
      VCount
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
  RmpX, RmpY: Integer;
  RmpStartPoint: TPoint;
  VTile: Types.TPoint;
  VTilesRect, VPixelRect: Types.TRect;
  VRectLL, VAlignRectLL: TDoubleRect;
  VBounds, VRect: TDoubleRect;
  VProjection: IProjection;
  VTileInfo: ITileInfoWithData;
  VTileInfoBasic: ITileInfoBasic;
  VBitmapTile: IBitmap32Static;
  VTileData: IBinaryData;
  VProcessTile: Boolean;
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
  librmp.CalcFirstRmpTilePos(
    VRectLL.Left,
    VRectLL.Top,
    (VRect.Right - VRect.Left),
    (VRect.Top - VRect.Bottom),
    RmpStartPoint.X,
    RmpStartPoint.Y
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

  RmpY := RmpStartPoint.Y;

  for Y := VTilesRect.Top to VTilesRect.Bottom - 1 do begin

    ARMPWriter.ForceNewLayer(
      VAlignRectLL.Left, VAlignRectLL.Top,
      VAlignRectLL.Right, VAlignRectLL.Bottom
    );

    RmpX := RmpStartPoint.X;

    for X := VTilesRect.Left to VTilesRect.Right - 1 do begin
      if CancelNotifier.IsOperationCanceled(OperationID) then begin
        Exit;
      end;

      VTile := Types.Point(X, Y);
      VRect := VProjection.TilePos2PixelRectFloat(VTile);
      VRectLL := VProjection.TilePos2LonLatRect(VTile);
      VPixelRect := VProjection.TilePos2PixelRect(VTile);
      VTileData := nil;

      VProcessTile := APolygon.IsRectIntersectPolygon(VRect);

      if VProcessTile then begin
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
            Assert( VBitmapTile.Size = Types.Point(256, 256) );
            VTileData := FBitmapTileSaver.Save(VBitmapTile);
          end;
        end;
      end;

      if Assigned(VTileData) then begin
        ARMPWriter.AddTile(
          RmpX, RmpY,
          VRectLL.Left, VRectLL.Top,
          VRectLL.Right, VRectLL.Bottom,
          VTileData.Buffer, VTileData.Size
        );
      end else begin
        ARMPWriter.AddEmptyTile(
          RmpX, RmpY,
          VRectLL.Left, VRectLL.Top,
          VRectLL.Right, VRectLL.Bottom
        );
      end;

      if VProcessTile then begin
        Inc(ATilesProcessed);
        if ATilesProcessed mod 100 = 0 then begin
          ProgressFormUpdateOnProgress(ATilesProcessed, ATilesToProcess);
        end;
      end;

      Inc(RmpX);
    end;
    Inc(RmpY);
  end;
end;

procedure TThreadExportToRMP.ProcessSingleGeometryUni(
  const ARMPWriter: TRMPFileWriter;
  const AZoom: Byte;
  const APolygon: IGeometryProjectedSinglePolygon;
  const ATilesToProcess: Int64;
  var ATilesProcessed: Int64
);
var
  X, Y: Integer;
  VRmpStartPoint, VRmpEndPoint: TPoint;
  VPixelRect: Types.TRect;
  VPixelRectFloat: TDoubleRect;
  VBounds, VRectLL: TDoubleRect;
  VProjection: IProjection;
  VTileData: IBinaryData;
  VProcessTile: Boolean;
  VRmpRectLL: TDoubleRect;
  VTileWidth, VTileHeight: Double;
  VBitmapTile: IBitmap32Static;
begin
  Assert(False);
  Exit;

  VBounds := APolygon.Bounds;
  VProjection := FProjectionSet.Zooms[AZoom];
  VRectLL := VProjection.PixelRectFloat2LonLatRect(VBounds);

  VRmpRectLL := DoubleRect(VRectLL.Left, -VRectLL.Top, VRectLL.Right, -VRectLL.Bottom);

  VTileWidth := Abs(VRectLL.Right - VRectLL.Left) * 256 / (VBounds.Right - VBounds.Left);
  VTileHeight := Abs(VRectLL.Top - VRectLL.Bottom) * 256 / (VBounds.Bottom - VBounds.Top);

  VRmpStartPoint.X := Floor((VRmpRectLL.Left + 180) / VTileWidth);
  VRmpStartPoint.Y := Floor((VRmpRectLL.Top + 90) / VTileHeight);

  VRmpEndPoint.X := Ceil((VRmpRectLL.Right + 180) / VTileWidth);
  VRmpEndPoint.Y := Ceil((VRmpRectLL.Bottom + 90) / VTileHeight);

  ARMPWriter.ForceNewLayer;

  for Y := VRmpStartPoint.Y to VRmpEndPoint.Y - 1 do begin

    ARMPWriter.ForceNewLayer;

    for X := VRmpStartPoint.X to VRmpEndPoint.X - 1 do begin

      if CancelNotifier.IsOperationCanceled(OperationID) then begin
        Exit;
      end;

      VTileData := nil;

      VRectLL :=
        DoubleRect(
          X * VTileWidth - 180,
          -(Y * VTileHeight - 90),
          (X + 1) * VTileWidth - 180,
          -((Y + 1) * VTileHeight - 90)
        );

      VPixelRectFloat := VProjection.LonLatRect2PixelRectFloat(VRectLL);
      VPixelRect := RectFromDoubleRect(VPixelRectFloat, rrToTopLeft);

      VProcessTile := APolygon.IsRectIntersectPolygon(VPixelRectFloat);

      if VProcessTile then begin
        VBitmapTile :=
          FBitmapUniProvider.GetBitmap(
            Self.OperationID,
            Self.CancelNotifier,
            VProjection,
            VPixelRect
          );

        if Assigned(VBitmapTile) then begin
          VTileData := FBitmapTileSaver.Save(VBitmapTile);
        end;
      end;

      if Assigned(VTileData) then begin
        ARMPWriter.AddTile(
          X, Y,
          VRectLL.Left, VRectLL.Top,
          VRectLL.Right, VRectLL.Bottom,
          VTileData.Buffer, VTileData.Size
        );
      end else begin
        ARMPWriter.AddEmptyTile(
          X, Y,
          VRectLL.Left, VRectLL.Top,
          VRectLL.Right, VRectLL.Bottom
        );
      end;

      if VProcessTile then begin
        Inc(ATilesProcessed);
        if ATilesProcessed mod 100 = 0 then begin
          ProgressFormUpdateOnProgress(ATilesProcessed, ATilesToProcess);
        end;
      end;

    end;

  end;
end;

function TThreadExportToRMP.CalcRmpLayersCount(
  const AProjectedPolygons: array of IGeometryProjectedPolygon;
  const ATilesCountPerRmpLayer: Integer
): Integer;

  procedure _CalcCountPerPolygon(
    const AZoom: Byte;
    const APolygon: IGeometryProjectedSinglePolygon;
    var ACount: Integer
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
    Inc(ACount);
    for Y := VTilesRect.Top to VTilesRect.Bottom do begin
      Inc(ACount);
      VTilesInLayer := 0;
      for X := VTilesRect.Left to VTilesRect.Right do begin
        Inc(VTilesInLayer);
        VTile := Types.Point(X, Y);
        VRect := VProjection.TilePos2PixelRectFloat(VTile);
        if APolygon.IsRectIntersectPolygon(VRect) then begin
          if VTilesInLayer > ATilesCountPerRmpLayer then begin
            Inc(ACount);
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
  Result := 0;
  for I := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[I];
    if Supports(AProjectedPolygons[I], IGeometryProjectedSinglePolygon, VSingleLine) then begin
       _CalcCountPerPolygon(VZoom, VSingleLine, Result);
    end else if Supports(AProjectedPolygons[I], IGeometryProjectedMultiPolygon, VMultiProjected) then begin
      for J := 0 to VMultiProjected.Count - 1 do begin
        _CalcCountPerPolygon(VZoom, VMultiProjected.Item[J], Result);
      end;
    end else begin
      Assert(False);
      Exit;
    end;
  end;
end;

end.
