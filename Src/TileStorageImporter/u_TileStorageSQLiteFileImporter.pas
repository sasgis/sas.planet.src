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

unit u_TileStorageSQLiteFileImporter;

interface

uses
  Types,
  SysUtils,
  Math,
  t_GeoTypes,
  t_TileStorageImporter,
  i_MapTypeSet,
  i_Projection,
  i_ConfigDataProvider,
  i_ContentTypeManager,
  i_ArchiveReadWriteFactory,
  i_TileStorageImporter,
  u_BaseInterfacedObject;

type
  TTileStorageSQLiteFileImporter = class(TBaseInterfacedObject, ITileStorageImporter)
  private
    FContentTypeManager: IContentTypeManager;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;

    function GetFileInfo(
      const AFileInfo: TTileStorageImporterFileInfo
    ): Boolean;
  private
    { ITileStorageImporter }
    function ProcessFile(
      const AFileName: string;
      const AAllMapsSet: IMapTypeSet;
      const AShowImportDialod: Boolean = False;
      const ADoShowImportDialog: TDoShowImportDialog = nil
    ): TTileStorageImportResult;
  public
    constructor Create(
      const AContentTypeManager: IContentTypeManager;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory
    );
  end;

  ETileStorageSQLiteFileImporter = class(Exception);

implementation

uses
  Classes,
  StrUtils,
  IOUtils,
  Generics.Collections,
  gnugettext,
  libsqlite3,
  superobject,
  OruxMapsXmlFile,
  c_CacheTypeCodes,
  i_ContentTypeInfo,
  i_MapType,
  i_ZmpInfo,
  u_Dialogs,
  u_GeoFunc,
  u_GeoToStrFunc,
  u_SQLite3Handler,
  u_ContentDetecter,
  u_TileStorageImporterFunc;

type
  TTablesInfo = TDictionary<string, TStringDynArray>;

{ TTileStorageSQLiteFileImporter }

constructor TTileStorageSQLiteFileImporter.Create(
  const AContentTypeManager: IContentTypeManager;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
begin
  inherited Create;

  FContentTypeManager := AContentTypeManager;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

function TTileStorageSQLiteFileImporter.ProcessFile(
  const AFileName: string;
  const AAllMapsSet: IMapTypeSet;
  const AShowImportDialod: Boolean;
  const ADoShowImportDialog: TDoShowImportDialog
): TTileStorageImportResult;
var
  I: Integer;
  VMsg: string;
  VMapType: IMapType;
  VMapTypeProxy: IMapTypeProxy;
  VZmpInfoProxy: IZmpInfoProxy;
  VProjection: IProjection;
  VFileInfo: TTileStorageImporterFileInfo;
  VZmpMapConfig: IConfigDataProvider;
  VIsCanceled: Boolean;
begin
  Result.Status := tsiInternalError;
  Result.MapType := nil;
  Result.GoToPoint := CEmptyDoublePoint;

  VFileInfo := TTileStorageImporterFileInfo.Create(AFileName);
  try
    // try to open and read the contents of the file
    if not GetFileInfo(VFileInfo) then begin
      // unsupported file format
      Result.Status := tsiUnsupportedFormat;
      Exit;
    end;

    if (VFileInfo.FContentType = '') or (VFileInfo.FExt = '') then begin
      // unsupported tile format
      VMsg := IfThen(VFileInfo.FContentType <> '', VFileInfo.FContentType, VFileInfo.FExt);
      ShowErrorMessageSync(Format(_('Error: Unsupported tile format: "%s"!'), [VMsg]));
      Result.Status := tsiUnsupportedContentType;
      Exit;
    end;

    if AShowImportDialod and Assigned(ADoShowImportDialog) then begin
      TThread.Synchronize(nil,
        procedure
        begin
          ADoShowImportDialog(VFileInfo, VIsCanceled);
        end
      );
      if VIsCanceled then begin
        // canceled by user
        Result.Status := tsiCanceled;
        Exit;
      end;
    end;

    for I := 0 to AAllMapsSet.Count - 1 do begin
      VMapType := AAllMapsSet.Items[I];
      if Supports(VMapType, IMapTypeProxy, VMapTypeProxy) and
         not VMapTypeProxy.IsInitialized
      then begin
        VZmpInfoProxy := VMapTypeProxy.Zmp as IZmpInfoProxy;
        if (VFileInfo.FIsBitmapTile = VZmpInfoProxy.GetIsBitmapTiles) and
           (VFileInfo.FIsLayer = VZmpInfoProxy.IsLayer)
        then begin
          VZmpMapConfig := MakeZmpMapConfig(VZmpInfoProxy.GUID.ToString, VFileInfo, FArchiveReadWriteFactory);
          VMapTypeProxy.Initialize(VZmpMapConfig);

          Result.MapType := VMapTypeProxy;

          case VFileInfo.FGotoInfo.Status of
            gtsError: begin
              Result.GoToPoint := CEmptyDoublePoint;
            end;

            gtsTilePos: begin
              Result.GoToZoom := VFileInfo.FGotoInfo.Zoom;
              VProjection := VMapTypeProxy.ProjectionSet.Zooms[VFileInfo.FGotoInfo.Zoom];
              Result.GoToPoint := VProjection.TilePos2LonLat(VFileInfo.FGotoInfo.TilePos);
            end;

            gtsLonLat: begin
              Result.GoToZoom := VFileInfo.FGotoInfo.Zoom;
              Result.GoToPoint := VFileInfo.FGotoInfo.LonLat;
            end;
          else
            raise ETileStorageSQLiteFileImporter.CreateFmt(
              'Unexpected GotoInfo status: %d', [Integer(VFileInfo.FGotoInfo.Status)]
            );
          end;

          Result.Status := tsiOk;
          Exit; // done
        end;
      end;
    end;

    if Result.Status = tsiInternalError then begin
      if VFileInfo.FIsLayer then begin
        VMsg := IfThen(VFileInfo.FIsBitmapTile, _('raster layer'), _('vector layer'));
      end else begin
        VMsg := _('base map');
      end;
      ShowErrorMessageSync(Format(_('Error: No available slots for the offline maps (%s)!'), [VMsg]));
    end;
  finally
    VFileInfo.Free;
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

function TryGetOruxMapsXmlFileName(const AFileName: string): string;
var
  VItems: TStringDynArray;
begin
  VItems := TDirectory.GetFiles(ExtractFileDir(AFileName), '*.otrk2.xml');
  if Length(VItems) > 0 then begin
    Result := VItems[0];
  end else begin
    Result := '';
  end;
end;

function TryDetectCacheTypeCode(
  const AFileName: string;
  const ATablesInfo: TTablesInfo
): Integer;
var
  VItems: TStringDynArray;
  VFileExt: string;
begin
  Result := 0;
  VFileExt := LowerCase(ExtractFileExt(AFileName));

  if (VFileExt = '.mbtiles') and
     ATablesInfo.TryGetValue('metadata', VItems) and
     IsContainsAll(VItems, ['name','value']) and
     ATablesInfo.TryGetValue('tiles', VItems) and
     IsContainsAll(VItems, ['zoom_level','tile_column','tile_row','tile_data'])
  then begin
    Result := c_File_Cache_Id_SQLite_MBTiles;
    Exit;
  end;

  if (VFileExt = '.rmaps') and
     ATablesInfo.TryGetValue('info', VItems) and
     IsContainsAll(VItems, ['minzoom','maxzoom']) and
     ATablesInfo.TryGetValue('tiles', VItems) and
     IsContainsAll(VItems, ['x','y','z','s','image'])
  then begin
    Result := c_File_Cache_Id_SQLite_RMaps;
    Exit;
  end;

  if (VFileExt = '.sqlitedb') and
     ATablesInfo.TryGetValue('tiles', VItems) and
     IsContainsAll(VItems, ['x','y','z','s','image'])
  then begin
    Result := c_File_Cache_Id_SQLite_RMaps;
    if ATablesInfo.TryGetValue('info', VItems) then begin
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
     ATablesInfo.TryGetValue('tiles', VItems) and
     IsContainsAll(VItems, ['x','y','z','image']) and
     (TryGetOruxMapsXmlFileName(AFileName) <> '')
  then begin
    Result := c_File_Cache_Id_SQLite_OruxMaps;
  end;
end;

procedure ReadMetadata(
  const ASQLite3: TSQLite3DbHandler;
  const AFileInfo: TTileStorageImporterFileInfo
);

  procedure TryParseSasGisJson(const AJson: string; const AMetadata: TTileStorageImporterMetadataInfo);
  var
    VKey, VVal: string;
    VJsonObject: ISuperObject;
  begin
    VJsonObject := SO(AJson);
    for VKey in ['epsg', 'format'] do begin
      VVal := VJsonObject.S[VKey];
      if VVal <> '' then begin
        AMetadata.AddOrSetValue('sasgis_' + VKey, VVal);
      end;
    end;
  end;

  function GetOruxBoundsStr(const AOruxLayer: TOruxMapsLayer): string;
  begin
    Result :=
      RoundEx(AOruxLayer.MinLon, 8) + ',' + RoundEx(AOruxLayer.MinLat, 8) + ',' +
      RoundEx(AOruxLayer.MaxLon, 8) + ',' + RoundEx(AOruxLayer.MaxLat, 8);
  end;

  function GetOruxCenterStr(const AOruxLayers: TOruxMapsLayers): string;
  var
    I: Integer;
    VLayer: POruxMapsLayer;
  begin
    if Length(AOruxLayers) = 0 then begin
      Result := '';
      Exit;
    end;

    VLayer := @AOruxLayers[0];
    for I := 1 to Length(AOruxLayers) - 1 do begin
      if AOruxLayers[I].Zoom < VLayer.Zoom then begin
        VLayer := @AOruxLayers[I];
      end;
    end;

    Result :=
      RoundEx((VLayer.MinLon + VLayer.MaxLon) / 2, 8) + ',' +
      RoundEx((VLayer.MinLat + VLayer.MaxLat) / 2, 8) + ',' +
      IntToStr(VLayer.Zoom);
  end;

var
  I: Integer;
  VKey, VValue: string;
  VStmtData: TSQLite3StmtData;
  VMetadata: TTileStorageImporterMetadataInfo;
  VCacheTypeCode: Integer;
  VOruxXmlFileName: string;
  VOruxInfo: TOruxMapsCalibrationInfo;
begin
  VMetadata := AFileInfo.FMetadata;
  VCacheTypeCode := AFileInfo.FCacheTypeCode;

  VMetadata.Clear;

  if VCacheTypeCode = c_File_Cache_Id_SQLite_MBTiles then begin
    PrepareStmt(ASQLite3, VStmtData, 'SELECT name, value FROM metadata');
    try
      while sqlite3_step(VStmtData.Stmt) = SQLITE_ROW do begin
        VKey := LowerCase(VStmtData.ColumnAsString(0));
        VValue := VStmtData.ColumnAsString(1);
        if VKey <> '' then begin
          VMetadata.AddOrSetValue(VKey, VVAlue);
        end;
      end;
    finally
      VStmtData.Fin;
    end;
  end else
  if VCacheTypeCode in [c_File_Cache_Id_SQLite_OsmAnd, c_File_Cache_Id_SQLite_Locus, c_File_Cache_Id_SQLite_RMaps] then begin
    PrepareStmt(ASQLite3, VStmtData, 'SELECT * FROM info LIMIT 1');
    try
      if sqlite3_step(VStmtData.Stmt) = SQLITE_ROW then begin
        for I := 0 to VStmtData.ColumnCount - 1 do begin
          VKey := LowerCase(VStmtData.ColumnName(I));
          VValue := VStmtData.ColumnAsString(I);
          if VKey <> '' then begin
            VMetadata.AddOrSetValue(VKey, VVAlue);
          end;
        end;
      end;
    finally
      VStmtData.Fin;
    end;
  end else
  if VCacheTypeCode = c_File_Cache_Id_SQLite_OruxMaps then begin
    VOruxXmlFileName := TryGetOruxMapsXmlFileName(AFileInfo.FFileName);
    if (VOruxXmlFileName <> '') and TOruxMapsXmlFile.Parse(VOruxXmlFileName, VOruxInfo) then begin
      VMetadata.AddOrSetValue('map_name', Trim(VOruxInfo.MapName));
      VMetadata.AddOrSetValue('center', GetOruxCenterStr(VOruxInfo.Layers));
      VMetadata.AddOrSetValue('layers_count', IntToStr(Length(VOruxInfo.Layers)));
      for I := 0 to Length(VOruxInfo.Layers) - 1 do begin
        VKey := 'layer_' + IntToStr(I) + '_';
        VMetadata.AddOrSetValue(VKey + 'datum', Trim(VOruxInfo.Layers[I].Datum));
        VMetadata.AddOrSetValue(VKey + 'projection', Trim(VOruxInfo.Layers[I].Projection));
        VMetadata.AddOrSetValue(VKey + 'zoom', IntToStr(VOruxInfo.Layers[I].Zoom));
        VMetadata.AddOrSetValue(VKey + 'bounds', GetOruxBoundsStr(VOruxInfo.Layers[I]));
      end;
    end;
  end else begin
    Assert(False);
  end;

  if VMetadata.TryGetValue('sasgis', VValue) then begin
    TryParseSasGisJson(VValue, VMetadata);
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
  const AMetadataInfo: TTileStorageImporterMetadataInfo;
  const AFileInfo: TTileStorageImporterFileInfo
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

  function GetGotoInfoByCenter(out AGotoInfo: TTileStorageImporterGotoInfo): Boolean;
  var
    VValue: string;
    VItems: TStringDynArray;
  begin
    Result := False;
    if AMetadataInfo.TryGetValue('center', VValue) and (VValue <> '') then begin
      VItems := SplitString(VValue, ',');
      Result :=
        (Length(VItems) = 3) and
        TryStrPointToFloat(Trim(VItems[0]), AGotoInfo.LonLat.X) and
        TryStrPointToFloat(Trim(VItems[1]), AGotoInfo.LonLat.Y) and
        TryStrToInt(Trim(VItems[2]), AGotoInfo.Zoom);
      if Result then begin
        AGotoInfo.Status := gtsLonLat;
      end;
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
    if GetGotoInfoByCenter(AFileInfo.FGotoInfo) then begin
      Exit;
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
            AFileInfo.FGotoInfo.LonLat := RectCenter(VBounds);
            AFileInfo.FGotoInfo.Zoom := VZoom;
            AFileInfo.FGotoInfo.Status := gtsLonLat;
            Exit;
          end;
        end;
      end;
    end;

    // using the first available tile's coordinates
    VSqlText := 'SELECT tile_column, tile_row, zoom_level FROM tiles LIMIT 1';
    VInvertY := not (AMetadataInfo.TryGetValue('scheme', VValue) and SameText(VValue, 'xyz'));
    if SelectXYZ(VSqlText, VPoint, VZoom, VInvertY, False) then begin
      AFileInfo.FGotoInfo.TilePos := VPoint;
      AFileInfo.FGotoInfo.Zoom := VZoom;
      AFileInfo.FGotoInfo.Status := gtsTilePos;
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
        AFileInfo.FGotoInfo.LonLat := VLonLat;
        AFileInfo.FGotoInfo.Zoom := VZoom;
        AFileInfo.FGotoInfo.Status := gtsLonLat;
        Exit;
      end;
    end;

    // using the first available tile's coordinates
    VSqlText := 'SELECT x, y, z FROM tiles LIMIT 1';
    if SelectXYZ(VSqlText, VPoint, VZoom, False, VInvertZ) then begin
      AFileInfo.FGotoInfo.TilePos := VPoint;
      AFileInfo.FGotoInfo.Zoom := VZoom;
      AFileInfo.FGotoInfo.Status := gtsTilePos;
      Exit;
    end;
  end else // OruxMaps
  if AFileInfo.FCacheTypeCode = c_File_Cache_Id_SQLite_OruxMaps then begin
    // using center (lon, lat, zoom)
    if GetGotoInfoByCenter(AFileInfo.FGotoInfo) then begin
      Exit;
    end;
  end;
end;

function TTileStorageSQLiteFileImporter.GetFileInfo(
  const AFileInfo: TTileStorageImporterFileInfo
): Boolean;
var
  I: Integer;
  VValue: string;
  VContentType: AnsiString;
  VContentTypeInfo: IContentTypeInfoBasic;
  VSQLite3: TSQLite3DbHandler;
  VTablesInfo: TTablesInfo;
  VMetadata: TTileStorageImporterMetadataInfo;
  VFileName: string;
begin
  Result := False;

  VContentType := 'image/jpg';

  VFileName := AFileInfo.FFileName;
  VMetadata := AFileInfo.FMetadata;

  if not VSQLite3.Init then begin
    VSQLite3.RaiseSQLite3Error;
  end;

  VSQLite3.Open('file:///' + VFileName + '?immutable=1',
    SQLITE_OPEN_READONLY or SQLITE_OPEN_URI or SQLITE_OPEN_NOMUTEX);
  try
    VTablesInfo := TTablesInfo.Create;
    try
      GetTablesInfo(VSQLite3, VTablesInfo);
      AFileInfo.FCacheTypeCode := TryDetectCacheTypeCode(VFileName, VTablesInfo);
    finally
      VTablesInfo.Free;
    end;

    if AFileInfo.FCacheTypeCode <> 0 then begin
      ReadMetadata(VSQLite3, AFileInfo);
    end else begin
      Exit;
    end;

    // projection
    if VMetadata.TryGetValue('sasgis_epsg', VValue) and (VValue <> '') then begin
      AFileInfo.FProjectionEpsg := StrToInt(VValue);
    end else
    if VMetadata.TryGetValue('crs', VValue) and (VValue <> '') then begin
      I := Pos(':', VValue);
      if I > 0 then begin
        VValue := Copy(VValue, I+1);
      end;
      AFileInfo.FProjectionEpsg := StrToInt(VValue);
    end else
    if VMetadata.TryGetValue('ellipsoid', VValue) and (VValue = '1') then begin
      AFileInfo.FProjectionEpsg := 3395;
    end;

    // content-type
    if VMetadata.TryGetValue('sasgis_format', VValue) and (VValue <> '') then begin
      VContentType := AnsiString(VValue);
    end else
    if VMetadata.TryGetValue('format', VValue) and (VValue <> '') then begin
      if Pos('/', VValue) > 0 then begin
        VContentType := AnsiString(VValue);
      end else begin
        VContentTypeInfo := FContentTypeManager.GetInfoByExt('.' + AnsiString(VValue));
        if VContentTypeInfo <> nil then begin
          VContentType := VContentTypeInfo.GetContentType;
        end else begin
          VContentType := ''; // unsupported content-type
          AFileInfo.FExt := '.' + VValue;
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
       VMetadata.TryGetValue('type', VValue)
    then begin
      AFileInfo.FIsLayer := SameText(VValue, 'overlay');
    end;

    // goto location
    TryDetectGoToPoint(VSQLite3, VMetadata, AFileInfo);
  finally
    VSQLite3.Close;
  end;

  // map name
  if AFileInfo.FCacheTypeCode = c_File_Cache_Id_SQLite_OruxMaps then begin
    if VMetadata.TryGetValue('map_name', VValue) and (VValue <> '') then begin
      AFileInfo.FMapName := VValue;
    end else begin
      AFileInfo.FMapName := ExtractFileName(TPath.GetDirectoryName(VFileName));
      if AFileInfo.FMapName = '' then begin
        AFileInfo.FMapName := TPath.GetFileNameWithoutExtension(VFileName);
      end;
    end;
  end else begin
    AFileInfo.FMapName := TPath.GetFileNameWithoutExtension(VFileName);
  end;

  AFileInfo.FParentSubMenu := '';
  AFileInfo.FNameInCache := VFileName;

  Result := True;
end;

end.
