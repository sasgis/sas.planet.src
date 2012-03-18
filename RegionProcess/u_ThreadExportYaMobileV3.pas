unit u_ThreadExportYaMobileV3;

interface

uses
  Windows,
  SysUtils,
  Classes,
  GR32,
  u_MapType,
  u_ResStrings,
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_VectorItemLonLat,
  u_ThreadExportAbstract;

type
  TThreadExportYaMobileV3 = class(TThreadExportAbstract)
  private
    FMapTypeArr: array of TMapType;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FIsReplace: boolean;
    FExportPath: string;
    csat, cmap: byte;
    FCoordConverterFactory: ICoordConverterFactory;
    function GetMobileFile(X,Y: Integer; Z: Byte; AMapType: Byte): string;
    function TileToTablePos(ATile: TPoint): Integer;
    procedure CreateNilFile(AFileName: string; ATableSize: Integer);
    procedure WriteTileToYaCache(
      ATile: TPoint;
      AZoom, AMapType, sm_xy: Byte;
      AExportPath: string;
      ATileStream: TMemoryStream;
      AReplace: Boolean
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
  i_TileIterator,
  u_TileIteratorByPolygon,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,
  u_ARGBToPaletteConverter;

const
  YaHeaderSize: integer = 1024;

constructor TThreadExportYaMobileV3.Create(
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
end;

function TThreadExportYaMobileV3.GetMobileFile(X,Y: Integer; Z: Byte; AMapType: Byte): string;
var
  Mask, Num: Integer;
begin
  Result := IntToStr(Z) + PathDelim;
  if(Z > 15) then
  begin
    Mask := (1 shl (Z-15))-1;
    Num  := (((X shr 15) and Mask) shl 4) + ((Y shr 15) and Mask);
    Result := Result + IntToHex(Num, 2) + PathDelim;
  end;
  if(Z > 11) then
  begin
    Mask := (1 shl (Z-11))-1;
    Mask := Mask and $F;
    Num  := (((X shr 11) and Mask) shl 4) + ((Y shr 11) and Mask);
    Result := Result + IntToHex(Num, 2) + PathDelim;
  end;
  if(Z > 7) then
  begin
    Mask := (1 shl (Z-7))-1;
    Mask := Mask and $F;
    Num  := (((X shr 7) and Mask) shl 8) + (((Y shr 7) and Mask) shl 4) + AMapType;
  end
  else
    Num := AMapType;    
  Result := LowerCase(Result + IntToHex(Num, 3));
end;

function TThreadExportYaMobileV3.TileToTablePos(ATile: TPoint): Integer;
var
  X,Y: Integer;
begin
  X := ATile.X and $7F;
  Y := ATile.Y and $7F;
  Result := ((Y and $40) shl 9) +
            ((X and $40) shl 8) +
            ((Y and $20) shl 8) +
            ((X and $20) shl 7) +
            ((Y and $10) shl 7) +
            ((X and $10) shl 6) +
            ((Y and $08) shl 6) +
            ((X and $08) shl 5) +
            ((Y and $04) shl 5) +
            ((X and $04) shl 4) +
            ((Y and $02) shl 4) +
            ((X and $02) shl 3) +
            ((Y and $01) shl 3) +
            ((X and $01) shl 2);
end;

procedure TThreadExportYaMobileV3.CreateNilFile(AFileName: string; ATableSize: Integer);
var
  VYaMob: TMemoryStream;
  VInitSize: Integer;
  VPath: string;
begin
  VYaMob := TMemoryStream.Create;
  try
    VPath := copy(AFileName, 1, LastDelimiter(PathDelim, AFileName));
    if not(DirectoryExists(VPath)) then
      if not ForceDirectories(VPath) then
        Exit;
    VInitSize := YaHeaderSize + 6*(sqr(ATableSize));
    VYaMob.SetSize(VInitSize);
    FillChar(VYaMob.Memory^, VInitSize, 0);
    VYaMob.Position := 0;
    VYaMob.Write('YNDX', 4);        // Magic = "YNDX"
    VYaMob.Write(#01#00, 2);        // Reserved
    VYaMob.Write(YaHeaderSize, 4);  // HeadSize = 1024 byte
    VYaMob.Write(#00#00#00, 3);     // "Author"
    VYaMob.SaveToFile(AFileName);
  finally
    VYaMob.Free;
  end;
end;

procedure TThreadExportYaMobileV3.WriteTileToYaCache(
  ATile: TPoint;
  AZoom, AMapType, sm_xy: Byte;
  AExportPath: string;
  ATileStream: TMemoryStream;
  AReplace: Boolean
);
var
  VYaMobileFile: string;
  VYaMobileStream: TFileStream;
  VTablePos: Integer;
  VTableOffset: Integer;
  VTableSize: Integer;
  VTileOffset: Integer;
  VExistsTileOffset: Integer;
  VTileSize: SmallInt;
  VHead: array [0..12] of byte;
begin
  if AZoom > 7 then
    VTableSize := 256
  else
    VTableSize := 2 shl AZoom;

  VYaMobileFile := AExportPath + GetMobileFile(ATile.X, ATile.Y, AZoom, AMapType);

  if not FileExists(VYaMobileFile) then
    CreateNilFile(VYaMobileFile, VTableSize);

  VYaMobileStream := TFileStream.Create(VYaMobileFile, fmOpenReadWrite or fmShareExclusive);
  try
    VYaMobileStream.Read(VHead, Length(VHead));
    VTableOffset := ( VHead[6] or (VHead[7] shl 8) or (VHead[8] shl 16) or (VHead[9] shl 24) );
    VTablePos := TileToTablePos(ATile)*6 + sm_xy*6;
    VTileOffset := VYaMobileStream.Size;
    VTileSize := ATileStream.Size;
    VYaMobileStream.Position := VTableOffset + VTablePos;
    VYaMobileStream.Read(VExistsTileOffset, 4);
    if (VExistsTileOffset = 0) or AReplace then
    begin
      VYaMobileStream.Position := VTableOffset + VTablePos;
      VYaMobileStream.Write(VTileOffset, 4);
      VYaMobileStream.Write(VTileSize, 2);
      VYaMobileStream.Position := VYaMobileStream.Size;
      VYaMobileStream.Write(ATileStream.Memory^, VTileSize);
    end;
  finally
    VYaMobileStream.Free;
  end;
end;

procedure TThreadExportYaMobileV3.ProcessRegion;
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
  Vmt: Byte;
  VTileIterators: array of ITileIterator;
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
  bmp32crop := TCustomBitmap32.Create;
  try
    hxyi := 1;
    sizeim := 128;
    JPGSaver := TVampyreBasicBitmapTileSaverJPG.create(cSat);
    PNGSaver := TVampyreBasicBitmapTileSaverPNGPalette.create(TARGBToPaletteConverter.Create, cMap);
    TileStream := TMemoryStream.Create;
    try
      bmp32.DrawMode := dmBlend;
      bmp322.DrawMode := dmBlend;
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
        ProgressInfo.Caption := SAS_STR_ExportTiles;
        ProgressInfo.FirstLine := SAS_STR_AllSaves + ' ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files;

        VTilesProcessed := 0;
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
        tc := GetTickCount;
        for i := 0 to Length(FZooms) - 1 do begin
          VZoom := FZooms[i];
          while VTileIterators[i].Next(VTile) do begin
            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              exit;
            end;
            for j := 0 to 2 do begin
              VMapType := FMapTypeArr[j];
              if (VMapType <> nil) and (not ((j = 0) and (FMapTypeArr[2] <> nil))) then begin
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
                    Vmt := 2;
                  end else begin
                    VSaver := PNGSaver;
                    Vmt := 1;
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
                      WriteTileToYaCache(VTile, VZoom, Vmt, (yi * 2) + xi, FExportPath, TileStream, FIsReplace);
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
      finally
        for i := 0 to Length(FZooms)-1 do begin
          VTileIterators[i] := nil;
        end;
      end;
      ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
    finally
      TileStream.Free;
    end;
  finally
    bmp32.Free;
    bmp322.Free;
    bmp32crop.Free;
  end;
end;

end.
