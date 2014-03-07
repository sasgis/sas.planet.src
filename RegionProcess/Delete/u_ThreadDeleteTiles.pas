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

unit u_ThreadDeleteTiles;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_MapVersionRequest,
  i_PredicateByTileInfo,
  i_TileStorage,
  u_ThreadRegionProcessAbstract;

type
  TThreadDeleteTiles = class(TThreadRegionProcessAbstract)
  private
    FZoom: byte;
    FTileStorage: ITileStorage;
    FVersion: IMapVersionRequest;
    FPolyProjected: IGeometryProjectedMultiPolygon;
    FPredicate: IPredicateByTileInfo;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress(
      const AProcessed, AToProcess, ADeleted: Int64
    );
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolyLL: IGeometryLonLatMultiPolygon;
      const AProjectedPolygon: IGeometryProjectedMultiPolygon;
      AZoom: byte;
      const ATileStorage: ITileStorage;
      const AVersion: IMapVersionRequest;
      const APredicate: IPredicateByTileInfo
    );
  end;

implementation

uses
  i_TileIterator,
  i_TileInfoBasic,
  u_TileIteratorByPolygon,
  u_ResStrings;

constructor TThreadDeleteTiles.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolyLL: IGeometryLonLatMultiPolygon;
  const AProjectedPolygon: IGeometryProjectedMultiPolygon;
  AZoom: byte;
  const ATileStorage: ITileStorage;
  const AVersion: IMapVersionRequest;
  const APredicate: IPredicateByTileInfo
);
begin
  inherited Create(
    AProgressInfo,
    APolyLL,
    Self.ClassName
  );
  FPolyProjected := AProjectedPolygon;
  FZoom := AZoom;
  FTileStorage := ATileStorage;
  FPredicate := APredicate;
  FVersion := AVersion;
end;

procedure TThreadDeleteTiles.ProcessRegion;
var
  VTile: TPoint;
  VTileIterator: ITileIterator;
  VDeletedCount: integer;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VTileInfo: ITileInfoBasic;
begin
  inherited;
  VTileIterator := TTileIteratorByPolygon.Create(FPolyProjected);
  VTilesToProcess := VTileIterator.TilesTotal;
  ProgressInfo.SetCaption(
    SAS_STR_Deleted + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files + ' (x' + inttostr(FZoom + 1) + ')'
  );
  VTilesProcessed := 0;
  VDeletedCount := 0;
  ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess, VDeletedCount);

  // foreach selected tile
  while VTileIterator.Next(VTile) do begin
    if CancelNotifier.IsOperationCanceled(OperationID) then begin
      exit;
    end;
    VTileInfo := FTileStorage.GetTileInfoEx(VTile, FZoom, FVersion, gtimWithoutData);
    if (VTileInfo <> nil) then begin
      if FPredicate.Check(VTileInfo, FZoom, VTile) then begin
        if FTileStorage.DeleteTile(VTile, FZoom, VTileInfo.VersionInfo) then begin
          inc(VDeletedCount);
        end;
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess, VDeletedCount);
      end;
    end;
    inc(VTilesProcessed);
  end;
end;

procedure TThreadDeleteTiles.ProgressFormUpdateOnProgress(
  const AProcessed, AToProcess, ADeleted: Int64
);
begin
  ProgressInfo.SetProcessedRatio(AProcessed / AToProcess);
  ProgressInfo.SetSecondLine(SAS_STR_Processed + ' ' + inttostr(AProcessed));
  ProgressInfo.SetFirstLine(SAS_STR_AllDelete + ' ' + inttostr(ADeleted) + ' ' + SAS_STR_Files);
end;

end.
