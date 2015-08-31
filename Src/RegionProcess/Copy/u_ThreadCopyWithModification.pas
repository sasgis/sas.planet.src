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

unit u_ThreadCopyWithModification;

interface

uses
  Types,
  i_MapVersionInfo,
  i_CoordConverterFactory,
  i_TileStorage,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_RegionProcessProgressInfo,
  i_ContentTypeInfo,
  i_BitmapLayerProvider,
  i_BitmapTileSaveLoad,
  i_MapType,
  u_ThreadExportAbstract;

type
  TThreadCopyWithModification = class(TThreadExportAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FTarget: ITileStorage;
    FTargetVersionInfo: IMapVersionInfo;
    FSource: IMapType;
    FLayer: IMapType;
    FProvider: IBitmapTileUniProvider;
    FBitmapTileSaver: IBitmapTileSaver;
    FContentType: IContentTypeInfoBasic;
    FIsOverwriteDestTiles: Boolean;
    FIsProcessTne: Boolean;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const APolygon: IGeometryLonLatPolygon;
      const ATarget: ITileStorage;
      const ATargetVersionForce: IMapVersionInfo;
      const ASource: IMapType;
      const ALayer: IMapType;
      const AProvider: IBitmapTileUniProvider;
      const ABitmapTileSaver: IBitmapTileSaver;
      const AZoomArr: TByteDynArray;
      const AContentType: IContentTypeInfoBasic;
      const AIsProcessTne: Boolean;
      const AIsOverwriteDestTiles: Boolean
    );
  end;

implementation

uses
  SysUtils,
  i_TileIterator,
  i_Bitmap32Static,
  i_BinaryData,
  i_CoordConverter,
  i_ProjectionInfo,
  i_GeometryProjected,
  u_TileIteratorByPolygon,
  u_ResStrings;

{ TThreadCopyWithModification }

constructor TThreadCopyWithModification.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const APolygon: IGeometryLonLatPolygon;
  const ATarget: ITileStorage;
  const ATargetVersionForce: IMapVersionInfo;
  const ASource: IMapType;
  const ALayer: IMapType;
  const AProvider: IBitmapTileUniProvider;
  const ABitmapTileSaver: IBitmapTileSaver;
  const AZoomArr: TByteDynArray;
  const AContentType: IContentTypeInfoBasic;
  const AIsProcessTne: Boolean;
  const AIsOverwriteDestTiles: Boolean
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    AZoomArr,
    Self.ClassName
  );
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FIsProcessTne := AIsProcessTne;
  FTarget := ATarget;
  FTargetVersionInfo := ATargetVersionForce;
  FSource := ASource;
  FLayer := ALayer;
  FProvider := AProvider;
  FBitmapTileSaver := ABitmapTileSaver;
  FContentType := AContentType;
  FIsOverwriteDestTiles := AIsOverwriteDestTiles;
end;

procedure TThreadCopyWithModification.ProcessRegion;

  procedure ProcessTile(
    const AProjection: IProjectionInfo;
    const ATile: TPoint;
    const AZoom: Byte;
    const ALoadDate: TDateTime
  );
  var
    VBitmapTile: IBitmap32Static;
    VTileData: IBinaryData;
  begin
    VBitmapTile :=
      FProvider.GetTile(
        Self.OperationID,
        Self.CancelNotifier,
        AProjection,
        ATile
      );
    if Assigned(VBitmapTile) then begin
      VTileData := FBitmapTileSaver.Save(VBitmapTile);
      FTarget.SaveTile(
        ATile,
        AZoom,
        FTargetVersionInfo,
        ALoadDate,
        FContentType,
        VTileData,
        FIsOverwriteDestTiles
      );
    end;
  end;

var
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VTileIterators: array of ITileIterator;
  VTileProjections: array of IProjectionInfo;
  I: Integer;
  VZoom: Byte;
  VGeoConvert: ICoordConverter;
  VProjection: IProjectionInfo;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VTileIterator: ITileIterator;
  VTile: TPoint;
begin
  inherited;
  VTilesToProcess := 0;
  VTilesProcessed := 0;

  SetLength(VTileIterators, Length(FZooms));
  SetLength(VTileProjections, Length(FZooms));
  for I := 0 to High(FZooms) do begin
    VZoom := FZooms[I];
    VGeoConvert := FTarget.CoordConverter;
    VProjection :=
      FProjectionFactory.GetByConverterAndZoom(
        VGeoConvert,
        VZoom
      );
    VProjectedPolygon :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        VProjection,
        PolygLL
      );
    VTileIterators[I] :=
      TTileIteratorByPolygon.Create(
        VProjection,
        VProjectedPolygon
      );
    VTileProjections[I] := VProjection;
    VTilesToProcess := VTilesToProcess + VTileIterators[I].TilesTotal;
  end;

  ProgressInfo.SetCaption(SAS_STR_ExportTiles);
  ProgressInfo.SetFirstLine(
    SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
  );
  ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);

  for I := 0 to High(FZooms) do begin
    VZoom := FZooms[I];
    VProjection := VTileProjections[I];
    VTileIterator := VTileIterators[I];
    while VTileIterator.Next(VTile) do begin
      if CancelNotifier.IsOperationCanceled(OperationID) then begin
        Exit;
      end;

      ProcessTile(VProjection, VTile, VZoom, Now);

      Inc(VTilesProcessed);
      if VTilesProcessed mod 100 = 0 then begin
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
      end;
    end;
  end;
end;

end.
