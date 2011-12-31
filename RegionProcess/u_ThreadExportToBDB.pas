{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_ThreadExportToBDB;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_TileFileNameGenerator,
  u_MapType,
  u_ResStrings,
  t_GeoTypes,
  u_BerkeleyDB,
  u_BerkeleyDBPool,
  u_ThreadExportAbstract;

type
  TThreadExportToBDB = class(TThreadExportAbstract)
  private
    FBDBPool: TBerkeleyDBPool;
    FMapTypeArr: array of TMapType;
    FTileNameGen: ITileFileNameGenerator;
    FStream: TMemoryStream;
    FIsMove: boolean;
    FIsReplace: boolean;
    FPathExport: string;
    function TileExportToRemoteBDB(
      AMapType: TMapType;
      AXY: TPoint;
      AZoom: Byte;
      ARemotePath: string
    ): Boolean;
    procedure CreateDirIfNotExists(APath: string);
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      APath: string;
      APolygon: TArrayOfDoublePoint;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Amove: boolean;
      Areplace: boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_CoordConverter,
  i_TileIterator,
  i_TileInfoBasic,
  u_TileFileNameBDB,
  u_TileStorageBerkeleyDB,
  u_TileIteratorStuped;

constructor TThreadExportToBDB.Create(
  APath: string;
  APolygon: TArrayOfDoublePoint;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Amove, Areplace: boolean
);
var
  i: integer;
begin
  inherited Create(APolygon, Azoomarr);
  FPathExport := APath;
  FIsMove := AMove;
  FTileNameGen := TTileFileNameBDB.Create;
  FIsReplace := AReplace;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 0 to length(Atypemaparr) - 1 do begin
    FMapTypeArr[i] := Atypemaparr[i];
  end;
  FBDBPool := TBerkeleyDBPool.Create;
  FStream := TMemoryStream.Create;
end;

destructor TThreadExportToBDB.Destroy;
begin
  FreeAndNil(FBDBPool);
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TThreadExportToBDB.CreateDirIfNotExists(APath: string);
var
  i: integer;
begin
  i := LastDelimiter(PathDelim, Apath);
  Apath := copy(Apath, 1, i);
  if not(DirectoryExists(Apath)) then begin
    ForceDirectories(Apath);
  end;
end;

function TThreadExportToBDB.TileExportToRemoteBDB(
  AMapType: TMapType;
  AXY: TPoint;
  AZoom: Byte;
  ARemotePath: string
): Boolean;
var
  VBDB: TBerkeleyDB;
  VPath: string;
  VTileInfo: ITileInfoBasic;
  VKey: TBDBKey;
  VData: TBDBData;
  VMemStream: TMemoryStream;
  VExists: Boolean;
begin
  Result := False;
  VPath := FPathExport + FTileNameGen.GetTileFileName(AXY, AZoom) + '.sdb';
  CreateDirIfNotExists(VPath);
  VBDB := FBDBPool.Acquire(VPath);
  try
    if Assigned(VBDB) and VBDB.Open(VPath, CPageSize, CCacheSize) then begin
      VKey.TileX := AXY.X;
      VKey.TileY := AXY.Y;
      VExists := VBDB.Exists(@VKey, SizeOf(TBDBKey));
      if not VExists or (VExists and FIsReplace) then begin
        FStream.Clear;
        if AMapType.TileStorage.LoadTile(AXY, AZoom, nil, FStream, VTileInfo) then begin
          VData.BDBRecVer := CBDBRecVerCur;
          VData.TileSize := FStream.Size;
          VData.TileDate := Now;
          VData.TileVer := ''; // TODO
          VData.TileMIME := PWideChar(AMapType.TileStorage.GetMainContentType.GetContentType);
          FStream.Position := 0;
          VData.TileBody := FStream.Memory; 
          VMemStream := TMemoryStream.Create;
          try
            PBDBDataToMemStream(@VData, VMemStream);
            VMemStream.Position := 0;
            Result := VBDB.Write(@VKey, SizeOf(TBDBKey), VMemStream.Memory, VMemStream.Size);
          finally
            VMemStream.Free;
          end;
        end;
      end;
    end;
  finally
    FBDBPool.Release(VBDB);
  end;
end;

procedure TThreadExportToBDB.ProcessRegion;
var
  i, j: integer;
  VZoom: Byte;
  VPath: string;
  VTile: TPoint;
  VMapType: TMapType;
  VGeoConvert: ICoordConverter;
  VTileIterators: array of array of ITileIterator;
  VTileIterator: ITileIterator;
begin
  inherited;
  SetLength(VTileIterators, length(FMapTypeArr), Length(FZooms));
  FTilesToProcess := 0;
  for j := 0 to length(FMapTypeArr) - 1 do begin
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      VGeoConvert := FMapTypeArr[j].GeoConvert;
      VTileIterators[j, i] := TTileIteratorStuped.Create(VZoom, FPolygLL, VGeoConvert);
      FTilesToProcess := FTilesToProcess + VTileIterators[j, i].TilesTotal;
    end;
  end;
  try
    ProgressFormUpdateCaption(
      SAS_STR_ExportTiles,
      SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_Files
      );
    FTilesProcessed := 0;
    ProgressFormUpdateOnProgress;
    for i := 0 to Length(FZooms) - 1 do begin
      VZoom := FZooms[i];
      for j := 0 to length(FMapTypeArr) - 1 do begin
        VMapType := FMapTypeArr[j];
        VGeoConvert := VMapType.GeoConvert;
        VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) + VMapType.GetShortFolderName);
        VTileIterator := VTileIterators[j, i];
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            exit;
          end;
          if VMapType.TileExists(VTile, VZoom) then begin
            if TileExportToRemoteBDB(VMapType, VTile, VZoom, VPath) then begin
              if FIsMove then begin
                VMapType.DeleteTile(VTile, VZoom);
              end;
            end;
          end;
          inc(FTilesProcessed);
          if FTilesProcessed mod 100 = 0 then begin
            ProgressFormUpdateOnProgress;
          end;
        end;
      end;
    end;
  finally
    for j := 0 to length(FMapTypeArr) - 1 do begin
      for i := 0 to Length(FZooms) - 1 do begin
        VTileIterators[j, i] := nil;
      end;
    end;
    VTileIterators := nil;
  end;
  ProgressFormUpdateOnProgress;
  FTileNameGen := nil;
end;

end.
