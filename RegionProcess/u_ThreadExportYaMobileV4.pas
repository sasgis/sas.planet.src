unit u_ThreadExportYaMobileV4;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  MD5,
  t_GeoTypes,
  u_MapType,
  u_ResStrings,
  u_ThreadExportAbstract;

type
  TTileStream = record
    Data: TMemoryStream;
    X: Word;
    Y: Word;
  end;

  TThreadExportYaMobileV4 = class(TThreadExportAbstract)
  private
    FMapTypeArr: array of TMapType;
    FIsReplace: Boolean;
    FExportPath: string;
    FCurrentFilePath: string;
    FTiles2Block: array of TTileStream;
    cSat, cMap: Byte;
    procedure WriteHeader(ACacheFile: TFileStream);
    procedure WriteTile(
      ACacheFile: TFileStream;
      ATileData: TMemoryStream;
      ARecordPos: Integer
    );
    procedure WriteBlock(
      ACacheFile: TFileStream;
      ATilesArray: array of TTileStream
    );
    function GetFilePath(APoint: TPoint; AZoom, AMapID: Integer): string;
    procedure AddTileToCache(
      ATileData: TMemoryStream;
      ATilePoint: TPoint;
      AZoom: Byte;
      AMapID: Integer;
      AFinalize: Boolean
    );
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      APath: string;
      APolygon: TArrayOfDoublePoint;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Areplace: boolean;
      Acsat: byte;
      Acmap: byte
    );
  end;

implementation

uses
  c_CoordConverter,
  i_CoordConverter,
  i_BitmapTileSaveLoad,
  i_TileIterator,
  u_TileIteratorStuped,
  u_ARGBToPaletteConverter,
  u_BitmapTileVampyreSaver,
  u_GlobalState;


constructor TThreadExportYaMobileV4.Create(
  APath: string;
  APolygon: TArrayOfDoublePoint;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Areplace: boolean;
  Acsat, Acmap: byte
);
var
  i: integer;
begin
  inherited Create(APolygon, Azoomarr);
  cSat := Acsat;
  cMap := Acmap;
  FExportPath := APath;
  FIsReplace := AReplace;
  setlength(FMapTypeArr, length(Atypemaparr));
  for i := 1 to length(Atypemaparr) do begin
    FMapTypeArr[i - 1] := Atypemaparr[i - 1];
  end;
  SetLength(FTiles2Block, 0);
  FCurrentFilePath := '';
end;

function TThreadExportYaMobileV4.GetFilePath(APoint: TPoint; AZoom, AMapID: Integer): string;

  function GetHeightTreeForZoom(Zoom: Integer): Integer;
  var
    VHeightZoom: Integer;
    VTilesInZoom, I: Int64;
  begin
    VHeightZoom := 0;
    VTilesInZoom := Int64(4) shl (Zoom shl 1);
    I := 1;
    while I < VTilesInZoom do begin
      Inc(VHeightZoom);
      I := I shl 8;
    end;
    Result := VHeightZoom;
  end;

const
  HexSymbol: array [0..15] of PChar =
    ( '0', '1', '2', '3', '4', '5', '6', '7',
      '8', '9', 'a', 'b', 'c', 'd', 'e', 'f' );
var
  VFilePath: string;
  VHeightTreeForZoom: Integer;
  VNode, VChild, VFile: TPoint;
  VNodeSize: Integer;
  VBlock127: Integer;
  I: Integer;
begin
  VFilePath := FExportPath + IntToStr(AMapID) + PathDelim + IntToStr(AZoom) + PathDelim;
  VHeightTreeForZoom := GetHeightTreeForZoom(AZoom);
  VNodeSize := 1 shl (4*(VHeightTreeForZoom - 1) {- 1}); // ?? -1 - опечатка в даташите ??
  VNode.X := 0;
  VNode.Y := 0;
  VChild.X := 0;
  VChild.Y := 0;
  I := 0;
  while ( I < (VHeightTreeForZoom - 2) ) do begin
    VChild.X := (APoint.X - VNode.X) div VNodeSize;
    VChild.Y := (APoint.Y - VNode.Y) div VNodeSize;
    VNode.X := VNode.X + VNodeSize * VChild.X;
    VNode.Y := VNode.Y + VNodeSize * VChild.Y;
    VNodeSize := VNodeSize shr 4;
    if ( I < (VHeightTreeForZoom - 3) ) then begin
      if (VChild.X > 15) or (VChild.X < 0) or (VChild.Y > 15) or (VChild.Y < 0) then begin
        raise Exception.Create(
                'Child value overflow: ' + #13#10 +
                'X = ' + IntToStr(VChild.X) + #13#10 +
                'Y = ' + IntToStr(VChild.Y)
              );
      end;
      VFilePath := VFilePath + HexSymbol[VChild.X] + HexSymbol[VChild.Y] + PathDelim;
    end;
    Inc(I);
  end;
  VFile.X := APoint.X - VNode.X;
  VFile.Y := APoint.Y - VNode.Y;
  VBlock127 := ( (VFile.X shr 7) shl 1 ) or (VFile.Y shr 7);
  if (VChild.X > 15) or (VChild.X < 0) or (VChild.Y > 15) or (VChild.Y < 0) then begin
    raise Exception.Create(
            'Child value overflow: ' + #13#10 +
            'X = ' + IntToStr(VChild.X) + #13#10 +
            'Y = ' + IntToStr(VChild.Y)
          );
  end;
  if (VBlock127 > 15) or (VBlock127 < 0) then begin
    raise Exception.Create('Block127 value overflow: ' + IntToStr(VBlock127));
  end;
  Result := VFilePath + HexSymbol[VChild.X] + HexSymbol[VChild.Y] + HexSymbol[VBlock127];
end;

procedure TThreadExportYaMobileV4.WriteHeader(ACacheFile: TFileStream);
begin
  ACacheFile.Size := 2*32*1024;      // Заголовок (32k) + таблица смещений (32k)
  ACacheFile.Position := 0;
  ACacheFile.Write('YMCF', 4);       // YMCF -> Yandex Mobile Cache File
  ACacheFile.Write(#32#00, 2);       // Размер заголовка, в килобайтах = 32
  ACacheFile.Write(#01#00, 2);       // Версия формата = 1
  ACacheFile.Write(#00#00#00#00, 4); // Платформа, на которой создали: SYMB, ANDR, IOSX
  ACacheFile.Write(#32#00, 2);       // Размер блока регулярных данных, в килобайтах = 32
  ACacheFile.Write(#00#00, 2);       // Размер фрагмента блока остаточных данных, в килобайтах = 1
  ACacheFile.Position := 9*1024;
  ACacheFile.Write('YBLK', 4);
  ACacheFile.Write(#01#00, 2);
  ACacheFile.Write(#00#00, 1);
  ACacheFile.Position := ACacheFile.Size;
end;

procedure TThreadExportYaMobileV4.WriteTile(
  ACacheFile: TFileStream;
  ATileData: TMemoryStream;
  ARecordPos: Integer
);
var
  VTileSize: Integer;
  MD5: TMD5Digest;
begin
  VTileSize := ATileData.Size;
  ATileData.Position:=0;
  MD5 := MD5Buffer(ATileData.Memory, VTileSize);
  ACacheFile.Position := ARecordPos;
  ACacheFile.Write('YTLD', 4);                    // YTLD -> Yandex Tile Data
  ACacheFile.Write(#01#00, 2);                    // количество записей в таблице разметки данных (пока всегда = 1)
  ACacheFile.Write(#01#00, 2);                    // номер версии формата = 1
  ACacheFile.Write(MD5.v, 16);                    // MD5-чексумма
  ACacheFile.Write(#01#00, 2);                    // номер версии карт-основы
  ACacheFile.Write(#00#00#00#00, 4);              // время (пока не используется = 0)
  ACacheFile.Write(#00#00#00#00, 4);              // идентификатор данных (сейчас всегда 0)
  ACacheFile.Write(VTileSize, 4);                 // размер данных
  ACacheFile.Write(ATileData.Memory^, VTileSize); // данные
end;

procedure TThreadExportYaMobileV4.WriteBlock(
  ACacheFile: TFileStream;
  ATilesArray: array of TTileStream
);

  function CalcTileIndex(X, Y: Word): Word; inline;
  begin
    Result := X or (Y shl 7);
  end;

var records:word;
    version:word;
    flag:byte;
    nexttablesize:byte;
    datasize:integer;
    dataid:word;
    freeblocknum:Smallint;
    blocks8indexes:byte;
    blockindexpos:integer;
    blockPos:integer;
    i:integer;
begin
  ACacheFile.Position := 16;
  freeblocknum:=0;
  blockindexpos:=0;
  while (blockindexpos<=8192) do begin
    if (blockindexpos mod 8)=0 then begin
      ACacheFile.Read(blocks8indexes,1);
    end;
    if ((blocks8indexes shr (7-(blockindexpos mod 8))) mod 2) = 0 then begin
      freeblocknum:=blockindexpos+1;
      Break;
    end;
    inc(blockindexpos);
  end;

  ACacheFile.Position:=16+((freeblocknum-1) div 8);
  ACacheFile.read(blocks8indexes,1);
  blocks8indexes:=blocks8indexes or ($01 shl (7-(blockindexpos mod 8)));
  ACacheFile.Position:=16+((freeblocknum-1) div 8);
  ACacheFile.write(blocks8indexes,1);

  blockPos:=32768*(freeblocknum+1);

  version:=1;
  flag:=3;
  nexttablesize:=0;
  records:=length(ATilesArray);
  if ACacheFile.size<blockPos+32768 then begin
     ACacheFile.size:=blockPos+32768;               //заполняем нулями весь блок
  end;
  ACacheFile.Position:=blockPos;

  ACacheFile.Write('YBLK',4);
  ACacheFile.Write(version,2); //Версия формата блока = 1
  ACacheFile.Write(flag,1); //Флаги (см. ниже)
  ACacheFile.Write(nexttablesize,1); //Размер таблицы размещения следующих блоков
  ACacheFile.Write(records,2); //Количество ячеек данных

  for i := 0 to records - 1 do begin
    datasize:=ATilesArray[i].data.Size+38; //38-байт на заголовок окромя тайла
    dataid:=calcTileIndex(ATilesArray[i].x,ATilesArray[i].y);
    ACacheFile.Write(datasize,4);  //Размер элемента
    ACacheFile.Write(dataid,2);   //Индекс тайла
  end;
  for i := 0 to records - 1 do begin
    WriteTile(ACacheFile,ATilesArray[i].data,ACacheFile.Position);
  end;
  for i := 0 to records - 1 do begin
    dataid:=calcTileIndex(ATilesArray[i].x,ATilesArray[i].y);
    ACacheFile.Position:=32768+(dataid*2);
    ACacheFile.Write(freeblocknum,2);
  end;
end;

procedure TThreadExportYaMobileV4.AddTileToCache(
  ATileData: TMemoryStream;
  ATilePoint: TPoint;
  AZoom: Byte;
  AMapID: Integer;
  AFinalize: Boolean
);
var
  VNewFilePath: string;
  VTilesSize: Int64;
  VCacheFile: TFileStream;
  VPath: string;
  I:Integer;
begin
  VNewFilePath := '';
  VCacheFile := nil;
  try
    if AFinalize then begin
      VNewFilePath := FCurrentFilePath;
    end else begin
      VNewFilePath := GetFilePath(ATilePoint, AZoom, AMapID);
    end;

    if Length(FTiles2Block) = 0 then begin
      FCurrentFilePath := VNewFilePath;
    end;

    VTilesSize:=0;
    for I := 0 to Length(FTiles2Block) - 1 do begin
      VTilesSize := VTilesSize + FTiles2Block[I].Data.Size;
    end;

    if FileExists(FCurrentFilePath) then begin
      VCacheFile := TFileStream.Create(FCurrentFilePath, fmOpenReadWrite);
    end else begin
      VPath := ExtractFilePath(FCurrentFilePath);
      if VPath <> '' then begin
        if not DirectoryExists(VPath) then begin
          if not ForceDirectories(VPath) then begin
            Exit;
          end;
        end;
        VCacheFile := TFileStream.Create(FCurrentFilePath, fmCreate);
      end;
    end;
    if not Assigned(VCacheFile) then begin
      raise Exception.Create('Can''t open cache file: ' + FCurrentFilePath);
      Exit;
    end;
    try
      if VCacheFile.Size = 0 then begin
        WriteHeader(VCacheFile);
      end;
      if ( AFinalize or
          ((VTilesSize + ATileData.Size + 10 + 6*Length(FTiles2Block) ) > 32768 ) or
          (FCurrentFilePath <> VNewFilePath)
         ) and (Length(FTiles2Block) > 0) then
      try
        WriteBlock(VCacheFile, FTiles2Block);
      finally
        try
          for I := 0 to Length(FTiles2Block) - 1 do begin
            FTiles2Block[I].Data.Free;
          end;
        finally
          SetLength(FTiles2Block, 0);
        end;
      end;
      if ( not AFinalize and Assigned(ATileData) ) then begin
        ATileData.Position := 0;
        SetLength(FTiles2Block, Length(FTiles2Block) + 1);
        FTiles2Block[Length(FTiles2Block) - 1].Data := TMemoryStream.Create;
        FTiles2Block[Length(FTiles2Block) - 1].Data.CopyFrom(ATileData, ATileData.Size);
        FTiles2Block[Length(FTiles2Block) - 1].X := ATilePoint.X mod 128;
        FTiles2Block[Length(FTiles2Block) - 1].Y := ATilePoint.Y mod 128;
      end;
    finally
      FreeAndNil(VCacheFile);
    end;
  finally
    FCurrentFilePath := VNewFilePath;
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
        VGeoConvert := GState.CoordConverterFactory.GetCoordConverterByCode(CYandexProjectionEPSG, CTileSplitQuadrate256x256);
        FTilesToProcess := 0;
        SetLength(VTileIterators,Length(FZooms));

        for i := 0 to Length(FZooms) - 1 do begin
          VZoom := FZooms[i];
          VTileIterators[i] := TTileIteratorStuped.Create(VZoom, FPolygLL, VGeoConvert);
          for j := 0 to 2 do begin
            if (FMapTypeArr[j] <> nil) and (not ((j = 0) and (FMapTypeArr[2] <> nil))) then begin
              FTilesToProcess := FTilesToProcess + VTileIterators[i].TilesTotal;
            end;
          end;
        end;
        try
          FTilesProcessed := 0;

          ProgressFormUpdateCaption(SAS_STR_ExportTiles, SAS_STR_AllSaves + ' ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_files);
          ProgressFormUpdateOnProgress;

          tc := GetTickCount;
          for j := 0 to length(FMapTypeArr)-1 do begin
            VMapType:=FMapTypeArr[j];
            for i := 0 to Length(FZooms) - 1 do begin
              VZoom := FZooms[i];
              VTileIterators[i].Reset;
              while VTileIterators[i].Next(VTile) do begin
                if CancelNotifier.IsOperationCanceled(OperationID) then begin
                  exit;
                end;
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
                    end else begin
                      VSaver := PNGSaver;
                    end;
                    for xi := 0 to hxyi do begin
                      for yi := 0 to hxyi do begin
                        bmp32crop.Clear;
                        bmp32crop.Draw(0, 0, bounds(sizeim * xi, sizeim * yi, sizeim, sizeim), bmp32);
                        TileStream.Clear;
                        VSaver.SaveToStream(bmp32crop, TileStream);
                        AddTileToCache(
                          TileStream,
                          Types.Point(2*VTile.X + Xi, 2*VTile.Y + Yi),
                          VZoom,
                          (10 + j),
                          False
                        );
                     end;
                    end;
                  end;
                  inc(FTilesProcessed);
                  if (GetTickCount - tc > 1000) then begin
                    tc := GetTickCount;
                    ProgressFormUpdateOnProgress;
                  end;
                end;
              end;
            end;
          end;
          AddTileToCache(nil, Types.Point(0,0), 0, 0, True);
        finally
          for i := 0 to Length(FZooms)-1 do begin
            VTileIterators[i] := nil;
          end;
        end;
        ProgressFormUpdateOnProgress
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
