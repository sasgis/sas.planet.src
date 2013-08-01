{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ThreadDeleteTiles;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  i_MapVersionInfo,
  i_PredicateByTileInfo,
  u_MapType,
  u_ThreadRegionProcessAbstract,
  u_ResStrings;

type
  TThreadDeleteTiles = class(TThreadRegionProcessAbstract)
  private
    FZoom: byte;
    FMapType: TMapType;
    FVersion: IMapVersionInfo;
    FPolyProjected: IProjectedPolygon;
    FPredicate: IPredicateByTileInfo;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress(
      const AProcessed, AToProcess, ADeleted: Int64
    );
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const APolyLL: ILonLatPolygon;
      const AProjectedPolygon: IProjectedPolygon;
      AZoom: byte;
      AMapType: TMapType;
      const AVersion: IMapVersionInfo;
      const APredicate: IPredicateByTileInfo
    );
  end;

implementation

uses
  i_TileIterator,
  i_TileInfoBasic,
  i_TileStorage,
  u_TileIteratorByPolygon;

constructor TThreadDeleteTiles.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const APolyLL: ILonLatPolygon;
  const AProjectedPolygon: IProjectedPolygon;
  AZoom: byte;
  AMapType: TMapType;
  const AVersion: IMapVersionInfo;
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
  FMapType := AMapType;
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
    VTileInfo := FMapType.TileStorage.GetTileInfo(VTile, FZoom, FVersion, gtimWithoutData);
    if (VTileInfo <> nil) then begin
      if FPredicate.Check(VTileInfo, FZoom, VTile) then begin
        if FMapType.TileStorage.DeleteTile(VTile, FZoom, VTileInfo.VersionInfo) then begin
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
