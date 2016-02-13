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
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FProjectionSet: IProjectionSet;
    FExportPath: string;
    FExportFileName: string;
    FTileStorage: ITileStorage;
    FMapVersion: IMapVersionRequest;
    FBitmapTileSaver: IBitmapTileSaver;
    FBitmapProvider: IBitmapTileUniProvider;
    FDirectTilesCopy: Boolean;
    FImgName, FProduct, FProvider, FComments: AnsiString;
    procedure ProcessSingleGeometry(
      const ARMPWriter: TRMPFileWriter;
      const AZoom: Byte;
      const APolygon: IGeometryProjectedSinglePolygon;
      const ADoDirectCopy: Boolean;
      const ATilesToProcess: Int64;
      var ATilesProcessed: Int64
    );
    function CalcRmpLayersCount(
      const AProjectedPolygons: array of IGeometryProjectedPolygon;
      const ATilesCountPerRmpLayer: Integer
    ): Integer;
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
      const ABitmapProvider: IBitmapTileUniProvider;
      const ADirectTilesCopy: Boolean;
      const AProduct, AProvider: AnsiString
    );
  end;

implementation

uses
  Math, // for inline functions
  librmp,
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
  const ABitmapProvider: IBitmapTileUniProvider;
  const ADirectTilesCopy: Boolean;
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
  FBitmapProvider := ABitmapProvider;
  FDirectTilesCopy := ADirectTilesCopy;

  FImgName := '1';
  FProduct := AProduct;
  FProvider := AProvider;
  FComments := 'Created with SAS.Planet';
end;

procedure TThreadExportToRMP.ProcessRegion;
var
  I, J: Integer;
  VCount: Int64;
  VZoom: Byte;
  VDoDirectCopy: Boolean;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VProjectedPolygons: array of IGeometryProjectedPolygon;
  VProjection: IProjection;
  VSingleLine: IGeometryProjectedSinglePolygon;
  VMultiProjected: IGeometryProjectedMultiPolygon;
  VRMPWriter: TRMPFileWriter;
begin
  inherited;

  VDoDirectCopy := FDirectTilesCopy and Assigned(FTileStorage);

  if not VDoDirectCopy then begin
    Assert(FBitmapProvider <> nil);
    Assert(FBitmapTileSaver <> nil);
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
      SAS_STR_AllSaves + ' ' + IntToStr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);

    for I := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[I];
      if Supports(VProjectedPolygons[I], IGeometryProjectedSinglePolygon, VSingleLine) then begin
        ProcessSingleGeometry(
          VRMPWriter,
          VZoom,
          VSingleLine,
          VDoDirectCopy,
          VTilesToProcess,
          VTilesProcessed
        );
      end else if Supports(VProjectedPolygons[I], IGeometryProjectedMultiPolygon, VMultiProjected) then begin
        for J := 0 to VMultiProjected.Count - 1 do begin
          ProcessSingleGeometry(
            VRMPWriter,
            VZoom,
            VMultiProjected.Item[J],
            VDoDirectCopy,
            VTilesToProcess,
            VTilesProcessed
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
  const ADoDirectCopy: Boolean;
  const ATilesToProcess: Int64;
  var ATilesProcessed: Int64
);
var
  X, Y: Integer;
  RmpX, RmpY: Integer;
  RmpStartPoint: TPoint;
  VTile: Types.TPoint;
  VTilesRect: Types.TRect;
  VBounds, VRect, VLonLatRect: TDoubleRect;
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
  VLonLatRect := VProjection.PixelRectFloat2LonLatRect(VBounds);
  VRect := VProjection.TilePos2LonLatRect(VTilesRect.TopLeft);
  librmp.CalcFirstRmpTilePos(
    VLonLatRect.Left,
    VLonLatRect.Top,
    (VRect.Right - VRect.Left),
    (VRect.Top - VRect.Bottom),
    RmpStartPoint.X,
    RmpStartPoint.Y
  );

  ARMPWriter.ForceNewLayer;
  RmpY := RmpStartPoint.Y;

  for Y := VTilesRect.Top to VTilesRect.Bottom - 1 do begin

    ARMPWriter.ForceNewLayer;
    RmpX := RmpStartPoint.X;

    for X := VTilesRect.Left to VTilesRect.Right - 1 do begin
      if CancelNotifier.IsOperationCanceled(OperationID) then begin
        Exit;
      end;

      VTile := Types.Point(X, Y);
      VRect := VProjection.TilePos2PixelRectFloat(VTile);
      VLonLatRect := VProjection.TilePos2LonLatRect(VTile);
      VTileData := nil;

      if APolygon.IsRectIntersectPolygon(VRect) then begin

        if ADoDirectCopy then begin
          VTileInfoBasic :=
            FTileStorage.GetTileInfoEx(VTile, AZoom, FMapVersion, gtimWithData);
          if Supports(VTileInfoBasic, ITileInfoWithData, VTileInfo) then begin
            VTileData := VTileInfo.TileData;
          end;
        end else begin
          VBitmapTile :=
            FBitmapProvider.GetTile(
              Self.OperationID,
              Self.CancelNotifier,
              VProjection,
              VTile
            );
          if Assigned(VBitmapTile) then begin
            VTileData := FBitmapTileSaver.Save(VBitmapTile);
          end;
        end;

        Inc(ATilesProcessed);
        if ATilesProcessed mod 100 = 0 then begin
          ProgressFormUpdateOnProgress(ATilesProcessed, ATilesToProcess);
        end;
      end;

      if Assigned(VTileData) then begin
        ARMPWriter.AddTile(
          RmpX, RmpY,
          VLonLatRect.Left, VLonLatRect.Top,
          VLonLatRect.Right, VLonLatRect.Bottom,
          VTileData.Buffer, VTileData.Size
        );
      end else begin
        ARMPWriter.AddEmptyTile(
          RmpX, RmpY,
          VLonLatRect.Left, VLonLatRect.Top,
          VLonLatRect.Right, VLonLatRect.Bottom
        );
      end;

      Inc(RmpX);
    end;
    Inc(RmpY);
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
