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

unit u_ThreadCopyFromStorageToStorage;

interface

uses
  Types,
  i_MapVersionInfo,
  i_MapVersionRequest,
  i_CoordConverterFactory,
  i_TileStorage,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_RegionProcessProgressInfo,
  u_ThreadExportAbstract;

type
  TCopyTask = record
    FSource: ITileStorage;
    FSourceVersion: IMapVersionRequest;
    FTarget: ITileStorage;
    FTargetVersionForce: IMapVersionInfo;
  end;
  TCopyTaskArray = array of TCopyTask;

  TThreadCopyFromStorageToStorage = class(TThreadExportAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FTasks: TCopyTaskArray;
    FIsMove: Boolean;
    FIsOverwriteDestTiles: Boolean;
    FIsProcessTne: Boolean;
    procedure ProcessTile(
      const ATask: TCopyTask;
      const AXY: TPoint;
      const AZoom: Byte
    ); inline;
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const APolygon: IGeometryLonLatPolygon;
      const ATasks: TCopyTaskArray;
      const AZoomArr: TByteDynArray;
      const AIsProcessTne: Boolean;
      const AIsMove: Boolean;
      const AIsOverwriteDestTiles: Boolean
    );
  end;

implementation

uses
  SysUtils,
  i_TileIterator,
  i_ProjectionSet,
  i_ProjectionInfo,
  i_GeometryProjected,
  u_TileIteratorByPolygon,
  i_MapVersionListStatic,
  i_TileInfoBasic,
  u_ResStrings;

{ TThreadCopyFromStorageToStorage }

constructor TThreadCopyFromStorageToStorage.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const APolygon: IGeometryLonLatPolygon;
  const ATasks: TCopyTaskArray;
  const AZoomArr: TByteDynArray;
  const AIsProcessTne: Boolean;
  const AIsMove, AIsOverwriteDestTiles: Boolean
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
  FIsMove := AIsMove;
  FIsOverwriteDestTiles := AIsOverwriteDestTiles;
  FTasks := ATasks;
end;

procedure TThreadCopyFromStorageToStorage.ProcessRegion;
var
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VTileIterators: array of array of ITileIterator;
  I, J: Integer;
  VZoom: Byte;
  VProjectionSet: IProjectionSet;
  VProjection: IProjectionInfo;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VTileIterator: ITileIterator;
  VTile: TPoint;
begin
  inherited;
  VTilesToProcess := 0;
  VTilesProcessed := 0;
  SetLength(VTileIterators, Length(FTasks), Length(FZooms));
  for I := 0 to Length(FTasks) - 1 do begin
    for J := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[J];
      VProjectionSet := FTasks[I].FSource.ProjectionSet;
      VProjection := VProjectionSet.Zooms[VZoom];
      VProjectedPolygon :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
          VProjection,
          PolygLL
        );
      VTileIterators[I, J] :=
        TTileIteratorByPolygon.Create(
          VProjection,
          VProjectedPolygon
        );
      VTilesToProcess := VTilesToProcess + VTileIterators[I, J].TilesTotal;
    end;
  end;
  try
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    ProgressInfo.SetFirstLine(
      SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
    );
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
    for I := 0 to Length(FTasks) - 1 do begin
      for J := 0 to Length(FZooms) - 1 do begin
        VZoom := FZooms[J];
        VTileIterator := VTileIterators[I, J];
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            Exit;
          end;

          ProcessTile(FTasks[I], VTile, VZoom);

          Inc(VTilesProcessed);
          if VTilesProcessed mod 100 = 0 then begin
            ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
          end;
        end;
      end;
    end;
  finally
    for I := 0 to Length(FTasks) - 1 do begin
      for J := 0 to Length(FZooms) - 1 do begin
        VTileIterators[I, J] := nil;
      end;
    end;
    VTileIterators := nil;
  end;
end;

procedure TThreadCopyFromStorageToStorage.ProcessTile(
  const ATask: TCopyTask;
  const AXY: TPoint;
  const AZoom: Byte
);
var
  VSrcTileInfo: ITileInfoBasic;
  VTileInfoWithData: ITileInfoWithData;
  VSrcVersionInfo: IMapVersionInfo;
  VTargetVersionInfo: IMapVersionInfo;
  VMapVersionList: IMapVersionListStatic;
  VVersionIndex: Integer;
  VResult: Boolean;
begin
  if Assigned(ATask.FSourceVersion) then begin
    // will copy only one (current) version from source and save it with a ATargetVersionInfo
    VSrcTileInfo := ATask.FSource.GetTileInfoEx(AXY, AZoom, ATask.FSourceVersion, gtimWithData);
    if Assigned(VSrcTileInfo) then begin
      if VSrcTileInfo.IsExists then begin
        if Supports(VSrcTileInfo, ITileInfoWithData, VTileInfoWithData) then begin
          VSrcVersionInfo := VSrcTileInfo.VersionInfo;
          if Assigned(ATask.FTargetVersionForce) then begin
            VTargetVersionInfo := ATask.FTargetVersionForce;
          end else begin
            VTargetVersionInfo := VSrcVersionInfo;
          end;
          VResult :=
            ATask.FTarget.SaveTile(
              AXY,
              AZoom,
              VTargetVersionInfo,
              VTileInfoWithData.LoadDate,
              VTileInfoWithData.ContentType,
              VTileInfoWithData.TileData,
              FIsOverwriteDestTiles
            );
          if FIsMove and VResult then begin
            ATask.FSource.DeleteTile(AXY, AZoom, VSrcVersionInfo);
          end;
        end;
      end else if VSrcTileInfo.IsExistsTNE and FIsProcessTne then begin
        VSrcVersionInfo := VSrcTileInfo.VersionInfo;
        if Assigned(ATask.FTargetVersionForce) then begin
          VTargetVersionInfo := ATask.FTargetVersionForce;
        end else begin
          VTargetVersionInfo := VSrcVersionInfo;
        end;
        VResult :=
          ATask.FTarget.SaveTile(
            AXY,
            AZoom,
            VTargetVersionInfo,
            VSrcTileInfo.LoadDate,
            nil,
            nil,
            FIsOverwriteDestTiles
          );
        if FIsMove and VResult then begin
          ATask.FSource.DeleteTile(AXY, AZoom, VSrcVersionInfo);
        end;
      end;
    end;
  end else begin
    // will try copy all versions from source and ingnore ATargetVersionInfo
    VMapVersionList := ATask.FSource.GetListOfTileVersions(AXY, AZoom, ATask.FSourceVersion);
    if Assigned(VMapVersionList) then begin
      for VVersionIndex := 0 to VMapVersionList.Count - 1 do begin
        VSrcVersionInfo := VMapVersionList.Item[VVersionIndex];
        VSrcTileInfo := ATask.FSource.GetTileInfo(AXY, AZoom, VSrcVersionInfo, gtimWithData);
        if Assigned(VSrcTileInfo) then begin
          if VSrcTileInfo.IsExists then begin
            if Supports(VSrcTileInfo, ITileInfoWithData, VTileInfoWithData) then begin
              VTargetVersionInfo := VSrcTileInfo.VersionInfo;
              if Assigned(ATask.FTargetVersionForce) then begin
                VTargetVersionInfo := ATask.FTargetVersionForce;
              end;
              VResult :=
                ATask.FTarget.SaveTile(
                  AXY,
                  AZoom,
                  VTargetVersionInfo,
                  VTileInfoWithData.LoadDate,
                  VTileInfoWithData.ContentType,
                  VTileInfoWithData.TileData,
                  FIsOverwriteDestTiles
                );
              if FIsMove and VResult then begin
                ATask.FSource.DeleteTile(AXY, AZoom, VSrcVersionInfo);
              end;
            end;
          end else if VSrcTileInfo.IsExistsTNE and FIsProcessTne then begin
            VResult :=
              ATask.FTarget.SaveTile(
                AXY,
                AZoom,
                VTargetVersionInfo,
                VSrcTileInfo.LoadDate,
                nil,
                nil,
                FIsOverwriteDestTiles
              );
            if FIsMove and VResult then begin
              ATask.FSource.DeleteTile(AXY, AZoom, VSrcVersionInfo);
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.                                 