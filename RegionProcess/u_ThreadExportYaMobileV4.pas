unit u_ThreadExportYaMobileV4;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  YaMobileCache,
  i_VectorItemLonLat,
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  u_MapType,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TThreadExportYaMobileV4 = class(TThreadExportAbstract)
  private
    FMapTypeArr: array of TMapType;
    FIsReplace: Boolean;
    FExportPath: string;
    cSat, cMap: Byte;
    FCoordConverterFactory: ICoordConverterFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FCacheFile: array [0..7] of TYaMobileCacheFile;
    FCacheCount: Byte;
    procedure GenUserXml(const AMapID, AMapName: string);
    function OpenCacheFile(
      const ACachePath: string;
      out ACacheFile: TYaMobileCacheFile
    ): Boolean;
    procedure CloseCacheFiles;
    procedure AddTileToCache(
      ATileData: TMemoryStream;
      ATilePoint: TPoint;
      AZoom: Byte;
      AMapID: Integer
    );
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      AProgressInfo: IRegionProcessProgressInfo;
      ACoordConverterFactory: ICoordConverterFactory;
      AProjectionFactory: IProjectionInfoFactory;
      AVectorItmesFactory: IVectorItmesFactory;
      APath: string;
      APolygon: ILonLatPolygon;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Areplace: boolean;
      Acsat: byte;
      Acmap: byte
    );
  end;

implementation

uses
  GR32_Resamplers,
  c_CoordConverter,
  i_CoordConverter,
  i_VectorItemProjected,
  i_BitmapTileSaveLoad,
  i_TileIterator,
  u_TileIteratorByPolygon,
  u_ARGBToPaletteConverter,
  u_BitmapTileVampyreSaver;

{ TThreadExportYaMobileV4 }

constructor TThreadExportYaMobileV4.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  AProgressInfo: IRegionProcessProgressInfo;
  ACoordConverterFactory: ICoordConverterFactory;
  AProjectionFactory: IProjectionInfoFactory;
  AVectorItmesFactory: IVectorItmesFactory;
  APath: string;
  APolygon: ILonLatPolygon;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Areplace: boolean;
  Acsat, Acmap: byte
);
var
  i: integer;
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygon,
    Azoomarr
  );
  FCoordConverterFactory := ACoordConverterFactory;
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  cSat := Acsat;
  cMap := Acmap;
  FExportPath := APath;
  FIsReplace := AReplace;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 1 to length(Atypemaparr) do begin
    FMapTypeArr[i - 1] := Atypemaparr[i - 1];
  end;
  for I := 0 to Length(FCacheFile) - 1 do begin
    FCacheFile[I] := nil;
  end;
  FCacheCount := 0;
end;

function TThreadExportYaMobileV4.OpenCacheFile(
  const ACachePath: string;
  out ACacheFile: TYaMobileCacheFile
): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(FCacheFile) - 1 do begin
    if Assigned(FCacheFile[I]) then begin
      if ACachePath = FCacheFile[I].FilePath then begin
        ACacheFile := FCacheFile[I];
        Result := True;
        Exit;
      end;
    end;
  end;
  if Assigned(FCacheFile[FCacheCount]) then begin
    FreeAndNil(FCacheFile[FCacheCount]);
  end;
  FCacheFile[FCacheCount] := TYaMobileCacheFile.Create(ACachePath, FIsReplace);
  FCacheCount := FCacheCount + 1;
  if FCacheCount > Length(FCacheFile) - 1 then begin
    FCacheCount := 0;
  end;
  Result := Assigned(ACacheFile);
end;

procedure TThreadExportYaMobileV4.CloseCacheFiles;
var
  I: Integer;
begin
  for I := 0 to Length(FCacheFile) - 1 do begin
    if Assigned(FCacheFile[I]) then begin
      FreeAndNil(FCacheFile[I]);
    end;
  end;
end;

procedure TThreadExportYaMobileV4.AddTileToCache(
  ATileData: TMemoryStream;
  ATilePoint: TPoint;
  AZoom: Byte;
  AMapID: Integer
);
var
  VCacheFilePath: string;
  VCacheFile: TYaMobileCacheFile;
  VTileData: TTileStream;
begin
  if Assigned(ATileData) then begin
    VCacheFile := nil;
    VCacheFilePath := GetFilePath(FExportPath, ATilePoint, AZoom, AMapID);
    if OpenCacheFile(VCacheFilePath, VCacheFile) then begin
      ATileData.Position := 0;
      VTileData.Data := ATileData;
      VTileData.Point := Types.Point(ATilePoint.X mod 128, ATilePoint.Y mod 128);
      VTileData.Zoom := AZoom;
      VTileData.MapVersion := 1;
      VCacheFile.AddTile(VTileData);
    end;
  end;
end;

procedure TThreadExportYaMobileV4.GenUserXml(const AMapID, AMapName: string);
var
  VUserXml: string;
  VUserXmlPath: string;
  VStream: TMemoryStream;
  VAddStr: string;
  BOM: array[0..2] of Byte;
  VSize: Integer;
begin
  VStream := TMemoryStream.Create;
  try
    VUserXmlPath := FExportPath + 'config' + PathDelim + 'user.xml';
    if ForceDirectories(ExtractFilePath(VUserXmlPath)) then begin
      VAddStr := '    <l id="' + AMapID + '" request="" name="' + AMapName + '" service="0" size_in_pixels="128" ver="1" />' + #10 + '</map_layers>' + #10;
      if not FileExists(VUserXmlPath) then begin
        VUserXml := '<?xml version="1.0" encoding="utf-8" ?>' + #10 + '<map_layers>' + #10 + VAddStr;
      end else begin
        VStream.LoadFromFile(VUserXmlPath);
        VStream.Position := 0;
        VStream.Read(BOM[0], 3);
        if (BOM[0] = $EF) and (BOM[1] = $BB) and (BOM[2] = $BF) then begin
          VSize := VStream.Size - 3;
          VStream.Position := 3;
        end else begin
          VSize := VStream.Size;
          VStream.Position := 0;
        end;
        SetLength(VUserXml, VSize);
        VStream.Read(Pointer(VUserXml)^, Length(VUserXml));
        VUserXml := Utf8ToAnsi(VUserXml);
        VUserXml := StringReplace(VUserXml, '</map_layers>'#10, '', [rfIgnoreCase, rfReplaceAll]);
        if VUserXml <> '' then begin
          VUserXml := VUserXml + VAddStr;
        end;
      end;
      if VUserXml <> '' then begin
        VUserXml := #239#187#191 + AnsiToUtf8(VUserXml);
        VStream.Clear;
        VStream.Position := 0;
        VStream.Write(Pointer(VUserXml)^, Length(VUserXml));
        VStream.SaveToFile(VUserXmlPath);
      end;
    end;
  finally
    FreeAndNil(VStream);
  end;
end;

procedure TThreadExportYaMobileV4.ProcessRegion;
var
  i, j, xi, yi, hxyi, sizeim: integer;
  VZoom: Byte;
  bmp32, bmp322, bmp32crop: TCustomBitmap32;
  TileStream: TMemoryStream;
  tc: cardinal;
  VGeoConvert: ICoordConverter;
  JPGSaver, PNGSaver: IBitmapTileSaver;
  VTile: TPoint;
  VMapType: TMapType;
  VSaver: IBitmapTileSaver;
  VTileIterators: array of ITileIterator;
  VMapID: Integer;
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
begin
  inherited;
  if (FMapTypeArr[0] = nil) and (FMapTypeArr[1] = nil) and (FMapTypeArr[2] = nil) then begin
    exit;
  end;
  bmp32 := TCustomBitmap32.Create;
  bmp322 := TCustomBitmap32.Create;
  try
    hxyi := 1;
    sizeim := 128;
    JPGSaver := TVampyreBasicBitmapTileSaverJPG.create(cSat);
    PNGSaver := TVampyreBasicBitmapTileSaverPNGPalette.create(TARGBToPaletteConverter.Create, cMap);
    TileStream := TMemoryStream.Create;
    try
      bmp32.DrawMode := dmBlend;
      bmp322.DrawMode := dmBlend;
      bmp32crop := TCustomBitmap32.Create;
      try
        bmp32crop.Width := sizeim;
        bmp32crop.Height := sizeim;
        VGeoConvert := FCoordConverterFactory.GetCoordConverterByCode(CYandexProjectionEPSG, CTileSplitQuadrate256x256);
        VTilesToProcess := 0;
        SetLength(VTileIterators,Length(FZooms));

        for i := 0 to Length(FZooms) - 1 do begin
          VZoom := FZooms[i];
          VProjectedPolygon :=
            FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
              FProjectionFactory.GetByConverterAndZoom(
                VGeoConvert,
                VZoom
              ),
              PolygLL
            );
          VTileIterators[i] := TTileIteratorByPolygon.Create(VProjectedPolygon);
          for j := 0 to 2 do begin
            if (FMapTypeArr[j] <> nil) and (not ((j = 0) and (FMapTypeArr[2] <> nil))) then begin
              VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal;
            end;
          end;
        end;
        try
          VTilesProcessed := 0;

          ProgressInfo.Caption := SAS_STR_ExportTiles;
          ProgressInfo.FirstLine := SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files;
          ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);

          tc := GetTickCount;
          try
            for j := 0 to length(FMapTypeArr)-1 do begin
              VMapType := FMapTypeArr[j];
              if VMapType <> nil then begin
                VMapID := 10 + j;
                GenUserXml(IntToStr(VMapID), VMapType.GUIConfig.Name.Value);
                for i := 0 to Length(FZooms) - 1 do begin
                  VZoom := FZooms[i];
                  VTileIterators[i].Reset;
                  while VTileIterators[i].Next(VTile) do begin
                    if CancelNotifier.IsOperationCanceled(OperationID) then begin
                      exit;
                    end;
                    if not ( (j = 0) and (FMapTypeArr[2] <> nil) ) then begin
                      bmp322.Clear;
                      if (j = 2) and (FMapTypeArr[0] <> nil) then begin
                        FMapTypeArr[0].LoadTileUni(bmp322, VTile, VZoom, VGeoConvert, False, False, True);
                      end;
                      bmp32.Clear;
                      if VMapType.LoadTileUni(bmp32, VTile, VZoom, VGeoConvert, False, False, True) then begin
                        if (j = 2) and (FMapTypeArr[0] <> nil) then begin
                          bmp322.Draw(0, 0, bmp32);
                          bmp32.Draw(0, 0, bmp322);
                        end;
                        if (j = 2) or (j = 0) then begin
                          VSaver := JPGSaver;
                        end else begin
                          VSaver := PNGSaver;
                        end;
                        for xi := 0 to hxyi do begin
                          for yi := 0 to hxyi do begin
                            bmp32crop.Clear;
                            BlockTransfer(
                              bmp32crop,
                              0,
                              0,
                              bmp32crop.ClipRect,
                              bmp32,
                              bounds(sizeim * xi, sizeim * yi, sizeim, sizeim),
                              dmOpaque
                            );
                            TileStream.Clear;
                            VSaver.SaveToStream(bmp32crop, TileStream);
                            AddTileToCache(
                              TileStream,
                              Types.Point(2*VTile.X + Xi, 2*VTile.Y + Yi),
                              VZoom,
                              VMapID
                            );
                          end;
                        end;
                      end;
                      inc(VTilesProcessed);
                      if (GetTickCount - tc > 1000) then begin
                        tc := GetTickCount;
                        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
                      end;
                    end;
                  end;
                end;
              end;
            end;
          finally
            CloseCacheFiles;
          end;
        finally
          for i := 0 to Length(FZooms)-1 do begin
            VTileIterators[i] := nil;
          end;
        end;
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
      finally
        bmp32crop.Free;
      end;
    finally
      TileStream.Free;
    end;
  finally
    bmp32.Free;
    bmp322.Free;
  end;
end;


end.

