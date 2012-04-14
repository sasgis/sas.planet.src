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
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  u_MapType,
  u_ThreadRegionProcessAbstract,
  u_ResStrings;

type
  TThreadDeleteTiles = class(TThreadRegionProcessAbstract)
  private
    FZoom: byte;
    FMapType: TMapType;
    FPolyProjected: IProjectedPolygon;
    DelBytes: boolean;
    DelBytesNum: integer;
    FForAttachments: Boolean;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress(
      const AProcessed, AToProcess, ADeleted: Int64
    );
  public
    constructor Create(
      const ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      const AProgressInfo: IRegionProcessProgressInfo;
      const APolyLL: ILonLatPolygon;
      const AProjectedPolygon: IProjectedPolygon;
      Azoom: byte;
      Atypemap: TMapType;
      ADelByte: boolean;
      ADelBytesNum: integer;
      AForAttachments: Boolean
    );
  end;

implementation

uses
  i_TileIterator,
  u_TileIteratorByPolygon;

constructor TThreadDeleteTiles.Create(
  const ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  const AProgressInfo: IRegionProcessProgressInfo;
  const APolyLL: ILonLatPolygon;
  const AProjectedPolygon: IProjectedPolygon;
  Azoom: byte;
  Atypemap: TMapType;
  ADelByte: boolean;
  ADelBytesNum: integer;
  AForAttachments: Boolean
);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolyLL
  );
  FPolyProjected := AProjectedPolygon;
  FZoom := Azoom;
  FMapType := Atypemap;
  DelBytes := ADelByte;
  DelBytesNum := ADelBytesNum;
  FForAttachments := AForAttachments;
end;

procedure TThreadDeleteTiles.ProcessRegion;
var
  VTile: TPoint;
  VTileIterator: ITileIterator;
  VDeletedCount: integer;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
begin
  inherited;
  VTileIterator := TTileIteratorByPolygon.Create(FPolyProjected);
  try
    VTilesToProcess := VTileIterator.TilesTotal;
    ProgressInfo.Caption :=
      SAS_STR_Deleted + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_files + ' (x' + inttostr(FZoom + 1) + ')';
    VTilesProcessed := 0;
    VDeletedCount := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess, VDeletedCount);

    // foreach selected tile
    while VTileIterator.Next(VTile) do begin
      if CancelNotifier.IsOperationCanceled(OperationID) then begin
        exit;
      end;
      // try to delete
      if (FForAttachments or (not DelBytes) or (DelBytesNum = FMapType.TileSize(VTile, FZoom))) then begin
        // for attachments - may delete many files
        if FForAttachments then
          VDeletedCount := VDeletedCount + FMapType.DeleteAttachments(VTile, FZoom, DelBytes, DelBytesNum)
        else if FMapType.DeleteTile(VTile, FZoom) then begin
          // for map - delete single file
          inc(VDeletedCount);
        end;
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess, VDeletedCount);
      end;
      inc(VTilesProcessed);
    end;
  finally
    VTileIterator := nil;
  end;
end;

procedure TThreadDeleteTiles.ProgressFormUpdateOnProgress(
  const AProcessed, AToProcess, ADeleted: Int64
);
begin
  ProgressInfo.Processed := AProcessed/AToProcess;
  ProgressInfo.SecondLine := SAS_STR_Processed + ' ' + inttostr(AProcessed);
  ProgressInfo.FirstLine := SAS_STR_AllDelete + ' ' + inttostr(ADeleted) + ' ' + SAS_STR_files;
end;

end.
