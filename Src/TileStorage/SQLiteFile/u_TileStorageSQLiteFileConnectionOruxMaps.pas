{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_TileStorageSQLiteFileConnectionOruxMaps;

interface

uses
  Types,
  Math,
  SysUtils,
  StrUtils,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_ProjectionSet,
  i_TileStorageSQLiteFileInfo,
  u_TileStorageSQLiteFileConnection;

type
  TTileStorageSQLiteFileConnectionOruxMaps = class(TTileStorageSQLiteFileConnection)
  protected
    procedure CreateTables; override;
    procedure FetchMetadata; override;
    procedure UpdateMetadata(const AXY: TPoint; const AZoom: Byte); override;
  public
    constructor Create(
      const AFileName: string;
      const AFileInfo: ITileStorageSQLiteFileInfo;
      const AMainContentType: IContentTypeInfoBasic;
      const AContentTypeManager: IContentTypeManager;
      const AProjectionSet: IProjectionSet
    );
  end;

implementation

uses
  IOUtils,
  OruxMapsXmlFile,
  libsqlite3,
  t_GeoTypes,
  i_BinaryData,
  u_GeoFunc,
  u_GeoToStrFunc,
  u_SQLite3Handler;

type
  TBasePointsArray = array[0..23] of TPoint;

  TTileDataConnectionStatementOruxMaps = class(TTileDataConnectionStatement)
  private
    FBits: Cardinal;
    FBasePoints: TBasePointsArray;
  public
    function BindParams(X, Y, Z: Integer): Boolean; override;
    procedure GetResult(out ABlob: IBinaryData); override;
    constructor Create(const ABits: Cardinal; const ABasePoints: TBasePointsArray);
  end;

  TTileInfoConnectionStatementOruxMaps = class(TTileInfoConnectionStatement)
  private
    FBits: Cardinal;
    FBasePoints: TBasePointsArray;
  public
    function BindParams(X, Y, Z: Integer): Boolean; override;
    procedure GetResult(out ABlobSize: Integer); override;
    constructor Create(const ABits: Cardinal; const ABasePoints: TBasePointsArray);
  end;

  TRectInfoConnectionStatementOruxMaps = class(TRectInfoConnectionStatement)
  private
    FBits: Cardinal;
    FBasePoints: TBasePointsArray;
  public
    function BindParams(const ARect: TRect; Z: Integer): Boolean; override;
    procedure GetResult(out X, Y: Integer; out ASize: Integer); override;
    constructor Create(const ABits: Cardinal; const ABasePoints: TBasePointsArray);
  end;

  TEnumTilesConnectionStatementOruxMaps = class(TEnumTilesConnectionStatement)
  private
    FBits: Cardinal;
    FBasePoints: TBasePointsArray;
  public
    procedure GetResult(out X, Y, Z: Integer; out ABlob: IBinaryData); override;
    constructor Create(const ABits: Cardinal; const ABasePoints: TBasePointsArray);
  end;

const
  CBasePointsKey = 'base_points';

procedure SetBit(var ABits: Cardinal; const ABitNum: Cardinal); inline;
begin
  ABits := ABits or (1 shl ABitNum);
end;

function IsBitSet(const ABits: Cardinal; const ABitNum: Cardinal): Boolean; inline;
begin
  Result := (ABits and (1 shl ABitNum)) > 0;
end;

function BasePointsStrToArray(
  const AStr: string;
  const AProjectionSet: IProjectionSet;
  out ABits: Cardinal
): TBasePointsArray;
var
  I: Integer;
  VZoom: Integer;
  VLon, VLat: Double;
  VItems, VSubItems: TStringDynArray;
begin
  ABits := 0;

  VItems := SplitString(AStr, ' ');

  for I := 0 to Length(VItems) - 1 do begin
    VSubItems := SplitString(VItems[I], ';');

    if Length(VSubItems) <> 3 then begin
      Assert(False, 'OruxMaps: Invalid Items string "' + VItems[I] + '"');
      Continue;
    end;

    VZoom := StrToInt(VSubItems[0]);
    VLon := StrPointToFloat(VSubItems[1]);
    VLat := StrPointToFloat(VSubItems[2]);

    if not AProjectionSet.CheckZoom(VZoom) then begin
      Assert(False, 'OruxMaps: Invalid zoom value = ' + VItems[I]);
      Continue;
    end;

    Result[VZoom] :=
      PointFromDoublePoint(
        AProjectionSet.Zooms[VZoom].LonLat2TilePosFloat(DoublePoint(VLon, VLat)),
        prToTopLeft
      );

    SetBit(ABits, VZoom);
  end;
end;

{ TTileStorageSQLiteFileConnectionOruxMaps }

constructor TTileStorageSQLiteFileConnectionOruxMaps.Create(
  const AFileName: string;
  const AFileInfo: ITileStorageSQLiteFileInfo;
  const AMainContentType: IContentTypeInfoBasic;
  const AContentTypeManager: IContentTypeManager;
  const AProjectionSet: IProjectionSet
);
var
  VValue: string;
  VBits: Cardinal;
  VBasePoints: TBasePointsArray;
begin
  inherited Create(True, AFileName, AFileInfo, AMainContentType, AContentTypeManager, AProjectionSet);

  Assert(FFileInfo <> nil);

  if not FFileInfo.TryGetMetadataValue(CBasePointsKey, VValue) then begin
    raise Exception.Create('OruxMaps: BasePoints metadata not found!');
  end;

  VBasePoints := BasePointsStrToArray(VValue, AProjectionSet, VBits);

  // read access
  FTileDataStmt := TTileDataConnectionStatementOruxMaps.Create(VBits, VBasePoints);
  FTileInfoStmt := TTileInfoConnectionStatementOruxMaps.Create(VBits, VBasePoints);
  FRectInfoStmt := TRectInfoConnectionStatementOruxMaps.Create(VBits, VBasePoints);
  FEnumTilesStmt := TEnumTilesConnectionStatementOruxMaps.Create(VBits, VBasePoints);

  // write access
  FInsertOrReplaceStmt := nil;
  FInsertOrIgnoreStmt := nil;
  FDeleteTileStmt := nil;

  FEnabled :=
    FTileDataStmt.CheckPrepared(FSQLite3DB) and
    FTileInfoStmt.CheckPrepared(FSQLite3DB);
end;

procedure TTileStorageSQLiteFileConnectionOruxMaps.CreateTables;
begin
  raise Exception.Create('OruxMaps: Write access is not supported!');
end;

procedure TTileStorageSQLiteFileConnectionOruxMaps.FetchMetadata;
const
  CXmlFileMask = '*.otrk2.xml';
var
  VXmlFiles: TStringDynArray;
  VBasePoints: string;
begin
  VBasePoints := '';

  VXmlFiles := TDirectory.GetFiles(ExtractFilePath(FFileInfo.FileName), CXmlFileMask);
  if Length(VXmlFiles) > 0 then begin
    VBasePoints := TOruxMapsXmlFile.Parse(VXmlFiles[0]);
  end else begin
    raise Exception.Create('OruxMaps: .otrk2.xml file not found!');
  end;

  if VBasePoints <> '' then begin
    FFileInfo.AddOrSetMetadataValue(CBasePointsKey, VBasePoints);
  end else begin
    raise Exception.Create('OruxMaps: Error reading XML file!');
  end;
end;

procedure TTileStorageSQLiteFileConnectionOruxMaps.UpdateMetadata(const AXY: TPoint; const AZoom: Byte);
begin
  raise Exception.Create('OruxMaps: Write access is not supported!');
end;

{ TTileDataConnectionStatementOruxMaps }

constructor TTileDataConnectionStatementOruxMaps.Create(const ABits: Cardinal; const ABasePoints: TBasePointsArray);
begin
  inherited Create(False, False);

  FBits := ABits;
  FBasePoints := ABasePoints;

  FText := 'SELECT image FROM tiles WHERE x = ? AND y = ? AND z = ?';
end;

function TTileDataConnectionStatementOruxMaps.BindParams(X, Y, Z: Integer): Boolean;
begin
  if IsBitSet(FBits, Z) then begin
    Dec(X, FBasePoints[Z].X);
    Dec(Y, FBasePoints[Z].Y);
  end else begin
    Z := -1;
  end;

  Result :=
    FStmt.BindInt(1, X) and
    FStmt.BindInt(2, Y) and
    FStmt.BindInt(3, Z);
end;

procedure TTileDataConnectionStatementOruxMaps.GetResult(out ABlob: IBinaryData);
begin
  ABlob := BlobToBinaryData(0);
end;

{ TTileInfoConnectionStatementOruxMaps }

constructor TTileInfoConnectionStatementOruxMaps.Create(const ABits: Cardinal; const ABasePoints: TBasePointsArray);
begin
  inherited Create(False, False);

  FBits := ABits;
  FBasePoints := ABasePoints;

  FText := 'SELECT length(image) FROM tiles WHERE x = ? AND y = ? AND z = ?';
end;

function TTileInfoConnectionStatementOruxMaps.BindParams(X, Y, Z: Integer): Boolean;
begin
  if IsBitSet(FBits, Z) then begin
    Dec(X, FBasePoints[Z].X);
    Dec(Y, FBasePoints[Z].Y);
  end else begin
    Z := -1;
  end;

  Result :=
    FStmt.BindInt(1, X) and
    FStmt.BindInt(2, Y) and
    FStmt.BindInt(3, Z);
end;

procedure TTileInfoConnectionStatementOruxMaps.GetResult(out ABlobSize: Integer);
begin
  ABlobSize := FStmt.ColumnInt(0);
end;

{ TRectInfoConnectionStatementOruxMaps }

constructor TRectInfoConnectionStatementOruxMaps.Create(const ABits: Cardinal; const ABasePoints: TBasePointsArray);
begin
  inherited Create(False, False);

  FBits := ABits;
  FBasePoints := ABasePoints;

  FText :=
    'SELECT x, y, length(image) FROM tiles WHERE x >= ? AND x < ? AND ' +
    'y >= ? AND y < ? AND z = ?';
end;

function TRectInfoConnectionStatementOruxMaps.BindParams(const ARect: TRect; Z: Integer): Boolean;
var
  VRect: TRect;
begin
  FZoom := Z;

  VRect := ARect;

  if IsBitSet(FBits, Z) then begin
    Dec(VRect.Left, FBasePoints[Z].X);
    Dec(VRect.Right, FBasePoints[Z].X);

    Dec(VRect.Top, FBasePoints[Z].Y);
    Dec(VRect.Bottom, FBasePoints[Z].Y);
  end else begin
    Z := -1;
  end;

  Result :=
    FStmt.BindInt(1, VRect.Left) and
    FStmt.BindInt(2, VRect.Right) and
    FStmt.BindInt(3, VRect.Top) and
    FStmt.BindInt(4, VRect.Bottom) and
    FStmt.BindInt(5, Z);
end;

procedure TRectInfoConnectionStatementOruxMaps.GetResult(out X, Y, ASize: Integer);
begin
  X := FStmt.ColumnInt(0);
  Y := FStmt.ColumnInt(1);
  ASize := FStmt.ColumnInt(2);

  if IsBitSet(FBits, FZoom) then begin
    Inc(X, FBasePoints[FZoom].X);
    Inc(Y, FBasePoints[FZoom].Y);
  end else begin
    Assert(False);
  end;
end;

{ TEnumTilesConnectionStatementOruxMaps }

constructor TEnumTilesConnectionStatementOruxMaps.Create(const ABits: Cardinal; const ABasePoints: TBasePointsArray);
begin
  inherited Create(False, False);

  FBits := ABits;
  FBasePoints := ABasePoints;

  FText := 'SELECT x, y, z, image FROM tiles';
end;

procedure TEnumTilesConnectionStatementOruxMaps.GetResult(out X, Y, Z: Integer; out ABlob: IBinaryData);
begin
  X := FStmt.ColumnInt(0);
  Y := FStmt.ColumnInt(1);
  Z := FStmt.ColumnInt(2);
  ABlob := BlobToBinaryData(3);

  if IsBitSet(FBits, Z) then begin
    Inc(X, FBasePoints[Z].X);
    Inc(Y, FBasePoints[Z].Y);
  end else begin
    Assert(False);
  end;
end;

end.
