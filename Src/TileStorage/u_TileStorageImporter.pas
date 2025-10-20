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

unit u_TileStorageImporter;

interface

uses
  Types,
  t_GeoTypes,
  i_MapTypeSet,
  i_Projection,
  i_ConfigDataProvider,
  i_ContentTypeManager,
  i_ActiveMapsConfig,
  i_ArchiveReadWriteFactory;

type
  TTileStorageImporterGoToInfo = record
    FLonLatPoint: TDoublePoint;
    FProjection: IProjection;
  end;

  TTileStorageImporterGoToResult = (gtrError, gtrPointOk, gtrLonLatOk);

  TTileStorageImporterFileInfo = record
    FCacheTypeCode: Integer;
    FContentType: string;
    FProjectionEpsg: Integer;
    FIsBitmapTile: Boolean;
    FIsLayer: Boolean;
    FExt: string;
    FNameInCache: string;
    FName: string;
    FParentSubMenu: string;

    FGotoResult: TTileStorageImporterGoToResult;
    FGotoPoint: TPoint;
    FGotoLonLat: TDoublePoint;
    FGotoZoom: Byte;
  end;

  TTileStorageImporter = class
  private
    FAllMapsSet:  IMapTypeSet;
    FMainMapConfig: IActiveMapConfig;
    FMainLayersConfig: IActiveLayersConfig;
    FContentTypeManager: IContentTypeManager;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;

    function GetFileInfo(
      const AFileName: string;
      const AShowImportDlg: Boolean;
      out AFileInfo: TTileStorageImporterFileInfo
    ): Boolean;

    function MakeZmpMapConfig(
      const AGuid: string;
      const AFileInfo: TTileStorageImporterFileInfo
    ): IConfigDataProvider;
  public
    function ProcessFile(
      const AFileName: string;
      const AShowImportDlg: Boolean;
      out AGoToInfo: TTileStorageImporterGoToInfo
    ): Boolean;

    class function IsSupportedFileExt(const AExt: string): Boolean;
  public
    constructor Create(
      const AAllMapsSet:  IMapTypeSet;
      const AMainMapConfig: IActiveMapConfig;
      const AMainLayersConfig: IActiveLayersConfig;
      const AContentTypeManager: IContentTypeManager;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  Classes,
  IOUtils,
  Generics.Collections,
  libsqlite3,
  superobject,
  c_CacheTypeCodes,
  i_ArchiveReadWrite,
  i_BinaryData,
  i_ContentTypeInfo,
  i_MapType,
  i_ZmpInfo,
  u_BinaryData,
  u_GeoFunc,
  u_GeoToStrFunc,
  u_SQLite3Handler,
  u_ContentDetecter,
  u_ConfigDataProviderByZip;

type
  TTablesInfo = TDictionary<string, TStringDynArray>;
  TMetadataInfo = TDictionary<string, string>;

{ TTileStorageImporter }

constructor TTileStorageImporter.Create(
  const AAllMapsSet: IMapTypeSet;
  const AMainMapConfig: IActiveMapConfig;
  const AMainLayersConfig: IActiveLayersConfig;
  const AContentTypeManager: IContentTypeManager;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
begin
  inherited Create;

  FAllMapsSet := AAllMapsSet;
  FMainMapConfig := AMainMapConfig;
  FMainLayersConfig := AMainLayersConfig;
  FContentTypeManager := AContentTypeManager;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

function TTileStorageImporter.ProcessFile(
  const AFileName: string;
  const AShowImportDlg: Boolean;
  out AGoToInfo: TTileStorageImporterGoToInfo
): Boolean;
var
  I: Integer;
  VMapType: IMapType;
  VMapTypeProxy: IMapTypeProxy;
  VZmpInfoProxy: IZmpInfoProxy;
  VFileInfo: TTileStorageImporterFileInfo;
  VZmpMapConfig: IConfigDataProvider;
begin
  Result := False;

  AGoToInfo.FLonLatPoint := CEmptyDoublePoint;
  AGoToInfo.FProjection := nil;

  if not GetFileInfo(AFileName, AShowImportDlg, VFileInfo) then begin
    // usupported file format
    Exit;
  end;

  for I := 0 to FAllMapsSet.Count - 1 do begin
    VMapType := FAllMapsSet.Items[I];
    if Supports(VMapType, IMapTypeProxy, VMapTypeProxy) and
       not VMapTypeProxy.IsInitialized
    then begin
      VZmpInfoProxy := VMapTypeProxy.Zmp as IZmpInfoProxy;
      if (VFileInfo.FIsBitmapTile = VZmpInfoProxy.GetIsBitmapTiles) and
         (VFileInfo.FIsLayer = VZmpInfoProxy.IsLayer)
      then begin
        VZmpMapConfig := MakeZmpMapConfig(VZmpInfoProxy.GUID.ToString, VFileInfo);
        VMapTypeProxy.Initialize(VZmpMapConfig);

        if VMapTypeProxy.Zmp.IsLayer then begin
          FMainLayersConfig.SelectLayerByGUID(VMapTypeProxy.GUID);
        end else begin
          FMainMapConfig.MainMapGUID := VMapTypeProxy.GUID;
        end;

        if VFileInfo.FGotoResult <> gtrError then begin
          AGoToInfo.FProjection := VMapTypeProxy.ProjectionSet.Zooms[VFileInfo.FGotoZoom];
        end;
        if VFileInfo.FGotoResult = gtrLonLatOk then begin
          AGoToInfo.FLonLatPoint := VFileInfo.FGotoLonLat;
        end else
        if VFileInfo.FGotoResult = gtrPointOk then begin
          AGoToInfo.FLonLatPoint := AGoToInfo.FProjection.TilePos2LonLat(VFileInfo.FGotoPoint);
        end;

        Result := True;
        Break;
      end;
    end;
  end;

  if not Result then begin
    // error: no free slots for this map type
  end;
end;

procedure PrepareStmt(const ASQLite3: TSQLite3DbHandler; const AStmtData: TSQLite3StmtData;
  const ASqlText: UTF8String);
begin
  if not ASQLite3.PrepareStatement(@AStmtData, ASqlText) then begin
    ASQLite3.RaiseSQLite3Error;
  end;
end;

procedure GetTablesInfo(const ASQLite3: TSQLite3DbHandler; var AInfo: TTablesInfo);
var
  I: Integer;
  VStmtData: TSQLite3StmtData;
  VKey, VValue: string;
  VValueArr: TStringDynArray;
begin
  PrepareStmt(ASQLite3, VStmtData, 'SELECT name FROM sqlite_master WHERE type="table"');
  try
    while sqlite3_step(VStmtData.Stmt) = SQLITE_ROW do begin
      VKey := LowerCase(VStmtData.ColumnAsString(0));
      if VKey <> '' then begin
        AInfo.AddOrSetValue(VKey, nil);
      end;
    end;
  finally
    VStmtData.Fin;
  end;

  for VKey in AInfo.Keys do begin
    PrepareStmt(ASQLite3, VStmtData, 'SELECT * FROM ' + QuotedStr(VKey) + ' LIMIT 1');
    try
      if sqlite3_step(VStmtData.Stmt) = SQLITE_ROW then begin
        VValueArr := nil;
        for I := 0 to VStmtData.ColumnCount - 1 do begin
          VValue := LowerCase(VStmtData.ColumnName(I));
          if VValue <> '' then begin
            VValueArr := VValueArr + [VValue];
          end;
        end;
        if Length(VValueArr) > 0 then begin
          TArray.Sort<string>(VValueArr); // for binary search
          AInfo.AddOrSetValue(VKey, Copy(VValueArr));
        end;
      end;
    finally
      VStmtData.Fin;
    end;
  end;
end;

function IsContainsAll(const AArr: TStringDynArray; const AItems: array of string): Boolean;
var
  I: Integer;
  VIndex: Integer;
begin
  Result := True;
  for I := 0 to Length(AItems) - 1 do begin
    if not TArray.BinarySearch<string>(AArr, AItems[I], VIndex) then begin
      Result := False;
      Exit;
    end;
  end;
end;

function IsContainsAny(const AArr: TStringDynArray; const AItems: array of string): Boolean;
var
  I: Integer;
  VIndex: Integer;
begin
  Result := False;
  for I := 0 to Length(AItems) - 1 do begin
    if TArray.BinarySearch<string>(AArr, AItems[I], VIndex) then begin
      Result := True;
      Exit;
    end;
  end;
end;

function TryDetectCacheTypeCode(
  const AFileName: string;
  const AInfo: TTablesInfo
): Integer;
var
  VItems: TStringDynArray;
  VFileExt: string;
begin
  Result := 0;
  VFileExt := LowerCase(ExtractFileExt(AFileName));

  if (VFileExt = '.mbtiles') and
     AInfo.TryGetValue('metadata', VItems) and
     IsContainsAll(VItems, ['name','value']) and
     AInfo.TryGetValue('tiles', VItems) and
     IsContainsAll(VItems, ['zoom_level','tile_column','tile_row','tile_data'])
  then begin
    Result := c_File_Cache_Id_SQLite_MBTiles;
    Exit;
  end;

  if (VFileExt = '.rmaps') and
     AInfo.TryGetValue('info', VItems) and
     IsContainsAll(VItems, ['minzoom','maxzoom']) and
     AInfo.TryGetValue('tiles', VItems) and
     IsContainsAll(VItems, ['x','y','z','s','image'])
  then begin
    Result := c_File_Cache_Id_SQLite_RMaps;
    Exit;
  end;

  if (VFileExt = '.sqlitedb') and
     AInfo.TryGetValue('tiles', VItems) and
     IsContainsAll(VItems, ['x','y','z','s','image'])
  then begin
    Result := c_File_Cache_Id_SQLite_RMaps;
    if AInfo.TryGetValue('info', VItems) then begin
      if IsContainsAny(VItems, ['zooms','center_x','center_y','provider']) then begin
        Result := c_File_Cache_Id_SQLite_Locus;
      end else
      if IsContainsAny(VItems, ['ellipsoid','timecolumn','expireminutes','tilenumbering','tilesize']) then begin
        Result := c_File_Cache_Id_SQLite_OsmAnd;
      end;
    end;
    Exit;
  end;

  if (VFileExt = '.db') and
     AInfo.TryGetValue('tiles', VItems) and
     IsContainsAll(VItems, ['x','y','z','image'])
  then begin
    VItems := TDirectory.GetFiles(ExtractFileDir(AFileName), '*.otrk2.xml');
    if Length(VItems) > 0 then begin
      Result := c_File_Cache_Id_SQLite_OruxMaps;
    end;
  end;
end;

procedure ReadMetadata(
  const ASQLite3: TSQLite3DbHandler;
  const ACacheTypeCode: Integer;
  var AInfo: TMetadataInfo
);

  procedure TryParseSasGisJson(const AJson: string);
  var
    VKey, VVal: string;
    VJsonObject: ISuperObject;
  begin
    VJsonObject := SO(AJson);
    for VKey in ['epsg', 'format'] do begin
      VVal := VJsonObject.S[VKey];
      if VVal <> '' then begin
        AInfo.AddOrSetValue('sasgis_' + VKey, VVal);
      end;
    end;
  end;

var
  I: Integer;
  VKey, VValue: string;
  VStmtData: TSQLite3StmtData;
begin
  AInfo.Clear;

  if ACacheTypeCode = c_File_Cache_Id_SQLite_MBTiles then begin
    PrepareStmt(ASQLite3, VStmtData, 'SELECT name, value FROM metadata');
    try
      while sqlite3_step(VStmtData.Stmt) = SQLITE_ROW do begin
        VKey := LowerCase(VStmtData.ColumnAsString(0));
        VValue := VStmtData.ColumnAsString(1);
        if VKey <> '' then begin
          AInfo.AddOrSetValue(VKey, VVAlue);
        end;
      end;
    finally
      VStmtData.Fin;
    end;
  end else
  if ACacheTypeCode in [c_File_Cache_Id_SQLite_OsmAnd, c_File_Cache_Id_SQLite_Locus, c_File_Cache_Id_SQLite_RMaps] then begin
    PrepareStmt(ASQLite3, VStmtData, 'SELECT * FROM info LIMIT 1');
    try
      if sqlite3_step(VStmtData.Stmt) = SQLITE_ROW then begin
        for I := 0 to VStmtData.ColumnCount - 1 do begin
          VKey := LowerCase(VStmtData.ColumnName(I));
          VValue := VStmtData.ColumnAsString(I);
          if VKey <> '' then begin
            AInfo.AddOrSetValue(VKey, VVAlue);
          end;
        end;
      end;
    finally
      VStmtData.Fin;
    end;
  end else
  if ACacheTypeCode = c_File_Cache_Id_SQLite_OruxMaps then begin
    // todo
  end else begin
    Assert(False);
  end;

  if AInfo.TryGetValue('sasgis', VValue) then begin
    TryParseSasGisJson(VValue);
  end;
end;

function TryDetectContentType(
  const ASQLite3: TSQLite3DbHandler;
  const ACacheTypeCode: Integer;
  const ADefault: RawByteString
): RawByteString;
var
  VData: Pointer;
  VSize: Integer;
  VStmtData: TSQLite3StmtData;
  VSqlText: UTF8String;
begin
  Result := ADefault;

  case ACacheTypeCode of
    c_File_Cache_Id_SQLite_MBTiles: begin
      VSqlText := 'SELECT tile_data FROM tiles LIMIT 1';
    end;
    c_File_Cache_Id_SQLite_OsmAnd,
    c_File_Cache_Id_SQLite_Locus,
    c_File_Cache_Id_SQLite_RMaps,
    c_File_Cache_Id_SQLite_OruxMaps: begin
      VSqlText := 'SELECT image FROM tiles LIMIT 1';
    end;
  else
    raise Exception.CreateFmt('Unexpected CacheTypeCode: %d', [ACacheTypeCode]);
  end;

  PrepareStmt(ASQLite3, VStmtData, VSqlText);
  try
    if sqlite3_step(VStmtData.Stmt) = SQLITE_ROW then begin
      VData := VStmtData.ColumnBlobData(0);
      VSize := VStmtData.ColumnBlobSize(0);
      if (VData <> nil) and (VSize > 0) then begin
        Result := u_ContentDetecter.TryDetectContentType(VData, VSize, Result);
      end;
    end;
  finally
    VStmtData.Fin;
  end;
end;

procedure TryDetectGoToPoint(
  const ASQLite3: TSQLite3DbHandler;
  const AMetadataInfo: TMetadataInfo;
  var AFileInfo: TTileStorageImporterFileInfo
);

  function InvertY(Y, Z: Integer): Integer;
  begin
    Result := (1 shl Z) - Y - 1;
  end;

  function InvertZ(Z: Integer): Integer;
  begin
    Result := 17 - Z;
  end;

  function SelectMinZoom(const AMetadataKey: string; const ASqlText: UTF8String; out AZoom: Integer;
    const AInvertZ: Boolean): Boolean;
  var
    VValue: string;
    VStmtData: TSQLite3StmtData;
  begin
    // read metadata (if any)
    Result :=
      (AMetadataKey <> '') and
      AMetadataInfo.TryGetValue(AMetadataKey, VValue) and
      TryStrToInt(VValue, AZoom);

    if Result then begin
      if AInvertZ then begin
        AZoom := InvertZ(AZoom);
      end;
      Exit;
    end;

    // fetch from db
    PrepareStmt(ASQLite3, VStmtData, ASqlText);
    try
      Result := sqlite3_step(VStmtData.Stmt) = SQLITE_ROW;

      if not Result then begin
        Exit;
      end;

      AZoom := VStmtData.ColumnInt(0);

      if AInvertZ then begin
        AZoom := InvertZ(AZoom);
      end;
    finally
      VStmtData.Fin;
    end;
  end;

  function SelectXYZ(const ASqlText: UTF8String; out APoint: TPoint; out AZoom: Integer;
    const AInvertY: Boolean; const AInvertZ: Boolean): Boolean;
  var
    VStmtData: TSQLite3StmtData;
  begin
    PrepareStmt(ASQLite3, VStmtData, ASqlText);
    try
      Result := sqlite3_step(VStmtData.Stmt) = SQLITE_ROW;

      if not Result then begin
        Exit;
      end;

      APoint.X := VStmtData.ColumnInt(0);
      APoint.Y := VStmtData.ColumnInt(1);
      AZoom    := VStmtData.ColumnInt(2);

      if AInvertZ then begin
        AZoom := InvertZ(AZoom);
      end;

      if AInvertY then begin
        APoint.Y := InvertY(APoint.Y, AZoom);
      end;
    finally
      VStmtData.Fin;
    end;
  end;

var
  VValue: string;
  VPoint: TPoint;
  VLonLat: TDoublePoint;
  VBounds: TDoubleRect;
  VZoom: Integer;
  VItems: TStringDynArray;
  VSqlText: UTF8String;
  VInvertY: Boolean;
  VInvertZ: Boolean;
begin
  // MBTiles
  if AFileInfo.FCacheTypeCode = c_File_Cache_Id_SQLite_MBTiles then begin

    // using center (lon, lat, zoom)
    if AMetadataInfo.TryGetValue('center', VValue) and (VValue <> '') then begin
      VItems := SplitString(VValue, ',');
      if Length(VItems) = 3 then begin
        if TryStrPointToFloat(Trim(VItems[0]), VLonLat.X) and
           TryStrPointToFloat(Trim(VItems[1]), VLonLat.Y) and
           TryStrToInt(Trim(VItems[2]), VZoom)
        then begin
          AFileInfo.FGotoLonLat := VLonLat;
          AFileInfo.FGotoZoom := VZoom;
          AFileInfo.FGotoResult := gtrLonLatOk;
          Exit;
        end;
      end;
    end;

    // using bounds (left, bottom, right, top) and minzoom
    if AMetadataInfo.TryGetValue('bounds', VValue) and (VValue <> '') then begin
      VItems := SplitString(VValue, ',');
      if Length(VItems) = 4 then begin
        if TryStrPointToFloat(Trim(VItems[0]), VBounds.Left) and
           TryStrPointToFloat(Trim(VItems[1]), VBounds.Bottom) and
           TryStrPointToFloat(Trim(VItems[2]), VBounds.Right) and
           TryStrPointToFloat(Trim(VItems[3]), VBounds.Top)
        then begin
          VSqlText := 'SELECT DISTINCT zoom_level FROM tiles ORDER BY zoom_level ASC LIMIT 1';
          if SelectMinZoom('minzoom', VSqlText, VZoom, False) then begin
            AFileInfo.FGotoLonLat := RectCenter(VBounds);
            AFileInfo.FGotoZoom := VZoom;
            AFileInfo.FGotoResult := gtrLonLatOk;
            Exit;
          end;
        end;
      end;
    end;

    // using the first available tile's coordinates
    VSqlText := 'SELECT tile_column, tile_row, zoom_level FROM tiles LIMIT 1';
    VInvertY := not (AMetadataInfo.TryGetValue('scheme', VValue) and SameText(VValue, 'xyz'));
    if SelectXYZ(VSqlText, VPoint, VZoom, VInvertY, False) then begin
      AFileInfo.FGotoPoint := VPoint;
      AFileInfo.FGotoZoom := VZoom;
      AFileInfo.FGotoResult := gtrPointOk;
      Exit;
    end;
  end else // OsmAnd, Locus, RMaps
  if AFileInfo.FCacheTypeCode in [c_File_Cache_Id_SQLite_OsmAnd, c_File_Cache_Id_SQLite_Locus, c_File_Cache_Id_SQLite_RMaps] then begin

    if AFileInfo.FCacheTypeCode = c_File_Cache_Id_SQLite_OsmAnd then begin
      VInvertZ := AMetadataInfo.TryGetValue('tilenumbering', VValue) and SameText(VValue, 'BigPlanet');
    end else begin
      VInvertZ := True;
    end;

    // using center_x, center_y and minzoom
    if AMetadataInfo.TryGetValue('center_x', VValue) and TryStrPointToFloat(VValue, VLonLat.Y) and
       AMetadataInfo.TryGetValue('center_y', VValue) and TryStrPointToFloat(VValue, VLonLat.X)
    then begin
      if VInvertZ then begin
        VSqlText := 'SELECT DISTINCT z FROM tiles ORDER BY z DESC LIMIT 1';
      end else begin
        VSqlText := 'SELECT DISTINCT z FROM tiles ORDER BY z ASC LIMIT 1';
      end;
      if SelectMinZoom('', VSqlText, VZoom, VInvertZ) then begin
        AFileInfo.FGotoLonLat := VLonLat;
        AFileInfo.FGotoZoom := VZoom;
        AFileInfo.FGotoResult := gtrLonLatOk;
        Exit;
      end;
    end;

    // using the first available tile's coordinates
    VSqlText := 'SELECT x, y, z FROM tiles LIMIT 1';
    if SelectXYZ(VSqlText, VPoint, VZoom, False, VInvertZ) then begin
      AFileInfo.FGotoPoint := VPoint;
      AFileInfo.FGotoZoom := VZoom;
      AFileInfo.FGotoResult := gtrPointOk;
      Exit;
    end;
  end else // OruxMaps
  if AFileInfo.FCacheTypeCode = c_File_Cache_Id_SQLite_OruxMaps then begin
    // todo
  end;
end;

function TTileStorageImporter.GetFileInfo(
  const AFileName: string;
  const AShowImportDlg: Boolean;
  out AFileInfo: TTileStorageImporterFileInfo
): Boolean;
var
  I: Integer;
  VValue: string;
  VIsLayer: Boolean;
  VContentType: AnsiString;
  VContentTypeInfo: IContentTypeInfoBasic;
  VSQLite3: TSQLite3DbHandler;
  VTablesInfo: TTablesInfo;
  VMetadataInfo: TMetadataInfo;
begin
  Result := False;

  AFileInfo.FCacheTypeCode := 0;
  AFileInfo.FProjectionEpsg := 3857;
  AFileInfo.FContentType := '';
  AFileInfo.FIsBitmapTile := False;
  AFileInfo.FExt := '';

  AFileInfo.FGotoResult := gtrError;

  VIsLayer := False;
  VContentType := 'image/jpg';

  if not VSQLite3.Init then begin
    VSQLite3.RaiseSQLite3Error;
  end;

  VMetadataInfo := TMetadataInfo.Create;
  try
    VSQLite3.Open('file:///' + AFileName + '?immutable=1',
      SQLITE_OPEN_READONLY or SQLITE_OPEN_URI or SQLITE_OPEN_NOMUTEX);
    try
      VTablesInfo := TTablesInfo.Create;
      try
        GetTablesInfo(VSQLite3, VTablesInfo);
        AFileInfo.FCacheTypeCode := TryDetectCacheTypeCode(AFileName, VTablesInfo);
      finally
        VTablesInfo.Free;
      end;

      if AFileInfo.FCacheTypeCode <> 0 then begin
        ReadMetadata(VSQLite3, AFileInfo.FCacheTypeCode, VMetadataInfo);
      end else begin
        Exit;
      end;

      // projection
      if VMetadataInfo.TryGetValue('sasgis_epsg', VValue) and (VValue <> '') then begin
        AFileInfo.FProjectionEpsg := StrToInt(VValue);
      end else
      if VMetadataInfo.TryGetValue('crs', VValue) and (VValue <> '') then begin
        I := Pos(':', VValue);
        if I > 0 then begin
          VValue := Copy(VValue, I+1);
        end;
        AFileInfo.FProjectionEpsg := StrToInt(VValue);
      end else
      if VMetadataInfo.TryGetValue('ellipsoid', VValue) and (VValue = '1') then begin
        AFileInfo.FProjectionEpsg := 3395;
      end;

      // content-type
      if VMetadataInfo.TryGetValue('sasgis_format', VValue) and (VValue <> '') then begin
        VContentType := AnsiString(VValue);
      end else
      if VMetadataInfo.TryGetValue('format', VValue) and (VValue <> '') then begin
        if Pos('/', VValue) > 0 then begin
          VContentType := AnsiString(VValue);
        end else begin
          VContentTypeInfo := FContentTypeManager.GetInfoByExt('.' + AnsiString(VValue));
          if VContentTypeInfo <> nil then begin
            VContentType := VContentTypeInfo.GetContentType;
          end else begin
            VContentType := ''; // unsupported content-type
          end;
        end;
      end else begin
        VContentType := TryDetectContentType(VSQLite3, AFileInfo.FCacheTypeCode, VContentType);
      end;

      if VContentType <> '' then begin
        AFileInfo.FContentType := VContentType;
        AFileInfo.FIsBitmapTile := FContentTypeManager.GetIsBitmapType(VContentType);

        VContentTypeInfo := FContentTypeManager.GetInfo(VContentType);
        if VContentTypeInfo <> nil then begin
          AFileInfo.FExt := VContentTypeInfo.GetDefaultExt;
        end else begin
          AFileInfo.FExt := '';
        end;
      end;

      // base layer / overlay
      if (AFileInfo.FCacheTypeCode = c_File_Cache_Id_SQLite_MBTiles) and
        VMetadataInfo.TryGetValue('type', VValue)
      then begin
        VIsLayer := SameText(VValue, 'overlay');
      end;

      // goto location
      TryDetectGoToPoint(VSQLite3, VMetadataInfo, AFileInfo);
    finally
      VSQLite3.Close;
    end;

    AFileInfo.FNameInCache := AFileName;
    AFileInfo.FIsLayer := VIsLayer;
    AFileInfo.FName := TPath.GetFileNameWithoutExtension(AFileName);
    AFileInfo.FParentSubMenu := '';

    if AShowImportDlg then begin
      // todo: show import dialog
    end;

    Result := True;
  finally
    VMetadataInfo.Free;
  end;
end;

function TTileStorageImporter.MakeZmpMapConfig(
  const AGuid: string;
  const AFileInfo: TTileStorageImporterFileInfo
): IConfigDataProvider;
const
  CParamsTxtFmt =
    '[PARAMS]'         + #13#10 +
    'GUID=%s'          + #13#10 +
    'asLayer=%s'       + #13#10 +
    'Name=%s'          + #13#10 +
    'ParentSubMenu=%s' + #13#10 +
    'NameInCache=%s'   + #13#10 +
    'ContentType=%s'   + #13#10 +
    'Ext=%s'           + #13#10 +
    'Epsg=%d'          + #13#10 +
    'CacheType=%d'     + #13#10 +
    'UseDwn=0'         + #13#10 +
    'IsReadOnly=1'     + #13#10;
var
  VParamsTxt: string;
  VBinaryData: IBinaryData;
  VZipStream: TStream;
  VZip: IArchiveType;
  VZipWriter: IArchiveWriter;
  VZipReader: IArchiveReader;
begin
  VZip := FArchiveReadWriteFactory.Zip;

  VZipStream := TMemoryStream.Create;
  try
    VParamsTxt := Format(CParamsTxtFmt, [AGuid, AFileInfo.FIsLayer.ToString,
      AFileInfo.FName, AFileInfo.FParentSubMenu, AFileInfo.FNameInCache,
      AFileInfo.FContentType, AFileInfo.FExt, AFileInfo.FProjectionEpsg,
      AFileInfo.FCacheTypeCode]
    );

    VBinaryData := TBinaryData.CreateByAnsiString(UTF8Encode(VParamsTxt));

    VZipWriter :=  VZip.WriterFactory.BuildByStream(VZipStream);
    VZipWriter.AddFile(VBinaryData, 'params.txt', Now);
    VZipWriter := nil;

    VZipStream.Position := 0;
    VZipReader := VZip.ReaderFactory.BuildByStreamWithOwn(VZipStream);
    Result := TConfigDataProviderByArchive.Create('', VZipReader);

    VZipStream := nil;
  finally
    VZipStream.Free;
  end;
end;

class function TTileStorageImporter.IsSupportedFileExt(const AExt: string): Boolean;
const
  CSupportedExt: array [0..3] of string = (
    '.mbtiles', '.sqlietedb', '.db', '.rmaps'
  );
var
  VExt, VExtLower: string;
begin
  Result := False;
  VExtLower := LowerCase(AExt);
  for VExt in CSupportedExt do  begin
    if VExtLower = VExt then begin
      Result := True;
      Exit;
    end;
  end;
end;

end.
