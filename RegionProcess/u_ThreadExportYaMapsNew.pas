unit u_ThreadExportYaMapsNew;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  MD5,
  u_MapType,
  u_ResStrings,
  t_GeoTypes,
  u_ThreadExportAbstract;

type
  TTileStream = record
    data:TMemoryStream;
    x:word;
    y:word;
  end;

  TThreadExportYaMapsNew = class(TThreadExportAbstract)
  private
    FMapTypeArr: array of TMapType;
    FIsReplace: boolean;
    FExportPath: string;
    csat, cmap: byte;
    CurrentFilePath:string;
    Tiles2Block:array of TTileStream;
  protected
    procedure ProcessRegion; override;
    procedure WriteHeader(FilesStream:TFileStream);
    procedure WriteTile(FilesStream:TFileStream;TileStream:TMemoryStream;recordpos:integer);
    procedure WriteBlock(FilesStream:TFileStream;TileStreams:array of TTileStream);
    function GetFilePath(Ax,Ay,Azoom,Aid:integer): string;
    function calcTileIndex(x,y:word):word;
    procedure AddTileToCache(TileStream:TMemoryStream;x,y:integer;z:byte;cacheid:integer;last:boolean);
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
  i_TileIterator,
  u_TileIteratorStuped,
  i_BitmapTileSaveLoad,
  u_BitmapTileVampyreSaver,
  u_GlobalState;


constructor TThreadExportYaMapsNew.Create(
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
end;

function TThreadExportYaMapsNew.GetFilePath(Ax,Ay,Azoom,Aid:integer): string;
 function heightTreeForZooms(i:integer):integer;
 var heightZoom:integer;
     j,tilesInZoom:Int64;
 begin
    heightZoom:=0;
    tilesInZoom:=4 shl (i shl 1);
    j:=1;
    while j<tilesInZoom do begin
      inc(heightZoom);
      j:=j shl 8;
    end;
    result:=heightZoom;
 end;

var path:string;
    xNode,yNode:integer;
    xChild,yChild:integer;
    fileX,fileY:integer;
    sizeNode:integer;
    i:integer;
    heightTreeForZoom:integer;
    block127:integer;
begin
  heightTreeForZoom:=heightTreeForZooms(Azoom);

  path:=FExportPath;
  path:=path+inttostr(Aid)+PathDelim;
  path:=path+inttostr(Azoom)+PathDelim;

  xNode:=0;
  yNode:=0;
  sizeNode:=1 shl ((4 * (heightTreeForZoom - 1) {- 1}));

  xChild:=0;
  yChild:=0;

  for i:=0 to heightTreeForZoom - 3 do begin
    xChild:=(AX-xNode)div sizeNode;
    yChild:=(AY-yNode)div sizeNode;
    xNode:=xNode+sizeNode*xChild;
    yNode:=yNode+sizeNode*yChild;
    sizeNode:=sizeNode shr 4;
    if (i < heightTreeForZoom - 3) then begin
      path:=path+IntToHex(xChild,1);
      path:=path+IntToHex(yChild,1);
      path:=path+PathDelim;
    end;
  end;
  path:=path+IntToHex(xChild,1);
  path:=path+IntToHex(yChild,1);
 
  fileX:=AX-xNode;
  fileY:=AY-yNode;

  block127:=((fileX shr 7) shl 1) or (fileY shr 7);

  if ((fileX shr 7)>0) then begin
    fileX:=fileX-128;
  end;
  if ((fileY shr 7)>0) then begin
    fileY:=fileY-128;
  end;

 path:=path+IntToHex(block127,1);
 result:=path;
end;

procedure TThreadExportYaMapsNew.WriteHeader(FilesStream:TFileStream);
var headersize:word;
    blocksize,ostblocksize:word;
    version:word;
    flag:byte;
begin
  FilesStream.Write('YMCF',4);
  headersize:=32;
  flag:=0;
  version:=1;
  blocksize:=32;
  ostblocksize:=0;
  FilesStream.Write(headersize,2); //Размер заголовка, в килобайтах = 32
  FilesStream.Write(version,2); //Версия формата = 1
  FilesStream.Size:=FilesStream.Size+4; //Платформа, на которой создали
  FilesStream.Position:=FilesStream.Size;
  FilesStream.Write(blocksize,2); //Размер блока регулярных данных, в килобайтах = 32
  FilesStream.Write(ostblocksize,2);   //Размер фрагмента блока остаточных данных, в килобайтах = 1
  FilesStream.Size:=FilesStream.Size+8192; //Битовая таблица свободных блоков данных
  FilesStream.Size:=FilesStream.Size+512; //Таблица номеров остаточных блоков данных
  FilesStream.Size:=FilesStream.Size+496; //Резерв
  FilesStream.Position:=FilesStream.Size;
  FilesStream.Write('YBLK',4);
  FilesStream.Write(version,1);
  FilesStream.Write(flag,1);
  FilesStream.Size:=32768*2;
end;

procedure TThreadExportYaMapsNew.WriteTile(FilesStream:TFileStream;TileStream:TMemoryStream;recordpos:integer);
var records:word;
    version:word;
    time:Integer;
    datasize:integer;
    dataid:integer;
    MD5arr:TMD5Digest;
begin
  records:=1;
  version:=1;
  time:=0;
  dataid:=0;
  datasize:=TileStream.Size;
  FilesStream.Position:=recordpos;
  FilesStream.Write('YTLD',4);
  FilesStream.Write(records,2);//количество записей в таблице разметки данных (пока всегда = 1)
  FilesStream.Write(version,2);//номер версии формата = 1
  MD5arr:=MD5Buffer(TileStream.Memory,TileStream.Size);
  FilesStream.Write(MD5arr.v,16);//MD5-чексумма
  version:=5;
  FilesStream.Write(version,2);//номер версии карт-основы
  FilesStream.Write(time,4);//время (пока не используется = 0)
  FilesStream.Write(dataid,4);//идентификатор данных (сейчас всегда 0)
  FilesStream.Write(datasize,4);//размер данных
  TileStream.Position:=0;
  FilesStream.Write(TileStream.Memory^,datasize); //данные
end;

function TThreadExportYaMapsNew.calcTileIndex(x,y:word):word;
begin
  result:= x or (y shl 7);
end;

procedure TThreadExportYaMapsNew.WriteBlock(FilesStream:TFileStream;TileStreams:array of TTileStream);
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
  FilesStream.Position:=16;
  freeblocknum:=0;
  blockindexpos:=0;
  while (blockindexpos<=8192) do begin
    if (blockindexpos mod 8)=0 then begin
      FilesStream.Read(blocks8indexes,1);
    end;
    if ((blocks8indexes shr (7-(blockindexpos mod 8))) mod 2) = 0 then begin
      freeblocknum:=blockindexpos+1;
      Break;
    end;
    inc(blockindexpos);
  end;

  FilesStream.Position:=16+((freeblocknum-1) div 8);
  FilesStream.read(blocks8indexes,1);
  blocks8indexes:=blocks8indexes or ($01 shl (7-(blockindexpos mod 8)));
  FilesStream.Position:=16+((freeblocknum-1) div 8);
  FilesStream.write(blocks8indexes,1);

  blockPos:=32768*(freeblocknum+1);

  version:=1;
  flag:=3;
  nexttablesize:=0;
  records:=length(TileStreams);
  if FilesStream.size<blockPos+32768 then begin
     FilesStream.size:=blockPos+32768;               //заполняем нулями весь блок
  end;
  FilesStream.Position:=blockPos;

  FilesStream.Write('YBLK',4);
  FilesStream.Write(version,2); //Версия формата блока = 1
  FilesStream.Write(flag,1); //Флаги (см. ниже)
  FilesStream.Write(nexttablesize,1); //Размер таблицы размещения следующих блоков
  FilesStream.Write(records,2); //Количество ячеек данных

  for i := 0 to records - 1 do begin
    datasize:=TileStreams[i].data.Size+38; //38-байт на заголовок окромя тайла
    dataid:=calcTileIndex(TileStreams[i].x,TileStreams[i].y);
    FilesStream.Write(datasize,4);  //Размер элемента
    FilesStream.Write(dataid,2);   //Индекс тайла
  end;
  for i := 0 to records - 1 do begin
    WriteTile(FilesStream,TileStreams[i].data,FilesStream.Position);
  end;
  for i := 0 to records - 1 do begin
    dataid:=calcTileIndex(TileStreams[i].x,TileStreams[i].y);
    FilesStream.Position:=32768+(dataid*2);
    FilesStream.Write(freeblocknum,2);
  end;
end;

procedure TThreadExportYaMapsNew.AddTileToCache(TileStream:TMemoryStream;x,y:integer;z:byte;cacheid:integer;last:boolean);
  procedure createdirif(path:string);
  begin
   path:=copy(path, 1, LastDelimiter(PathDelim, path));
   if not(DirectoryExists(path)) then ForceDirectories(path);
  end;

var newFilePath:string;
    TilesSize:integer;
    i:Integer;
    filestream:TFileStream;
begin
  if last then begin
    newFilePath:=CurrentFilePath;
  end else begin
    newFilePath:=GetFilePath(x,y,z,cacheid);
  end;

  if length(Tiles2Block)=0 then begin
    CurrentFilePath:=newFilePath;
  end;

  TilesSize:=0;
  for i := 0 to length(Tiles2Block) - 1 do begin
    TilesSize:=TilesSize+Tiles2Block[i].data.size;
  end;

  if FileExists(CurrentFilePath) then begin
    filestream:=TFileStream.Create(CurrentFilePath,fmOpenReadWrite);
  end else begin
    createdirif(CurrentFilePath);
    filestream:=TFileStream.Create(CurrentFilePath,fmCreate);
  end;
  try
    if filestream.Size=0 then begin
      WriteHeader(filestream);
    end;

    if (
       (last)or
       (TilesSize+TileStream.Size+10+6*(length(Tiles2Block))>32768)or
       (CurrentFilePath<>newFilePath)
       )
       and(length(Tiles2Block)>0) then begin
      WriteBlock(filestream,Tiles2Block);
      SetLength(Tiles2Block,0);
    end;

    if (not(last))and(TileStream<>nil) then begin
      SetLength(Tiles2Block,length(Tiles2Block)+1);
      Tiles2Block[length(Tiles2Block)-1].data:=TMemoryStream.Create;
      TileStream.Position:=0;
      Tiles2Block[length(Tiles2Block)-1].data.CopyFrom(TileStream,TileStream.Size);
      Tiles2Block[length(Tiles2Block)-1].x:=x mod 128;
      Tiles2Block[length(Tiles2Block)-1].y:=y mod 128;
    end;
  finally
    filestream.Free;
  end;
  CurrentFilePath:=newFilePath;
end;

procedure TThreadExportYaMapsNew.ProcessRegion;
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
    PNGSaver := TVampyreBasicBitmapTileSaverPNGPalette.create(cMap);
    TileStream := TMemoryStream.Create;
    try
      bmp32.DrawMode := dmBlend;
      bmp322.DrawMode := dmBlend;
      bmp32crop := TCustomBitmap32.Create;
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
              if IsCancel then begin
                exit;
              end;
              if (VMapType <> nil) and (not ((j = 0) and (FMapTypeArr[2] <> nil))) then begin
                bmp322.Clear;
                if (j = 2) and (FMapTypeArr[0] <> nil) then begin
                  FMapTypeArr[0].LoadTileUni(bmp322, VTile, VZoom, False, VGeoConvert, False, False, True);
                end;
                bmp32.Clear;
                if VMapType.LoadTileUni(bmp32, VTile, VZoom, False, VGeoConvert, False, False, True) then begin
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
                      bmp32crop.Draw(0, 0, bounds(sizeim * xi, sizeim * yi, sizeim, sizeim), bmp32);
                      TileStream.Clear;
                      VSaver.SaveToStream(bmp32crop, TileStream);
                      AddTileToCache(TileStream,VTile.X*2+Xi,VTile.Y*2+Yi,VZoom,10+j,false);
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
        AddTileToCache(nil,0,0,0,0,true);
      finally
        for i := 0 to Length(FZooms)-1 do begin
          VTileIterators[i] := nil;
        end;
      end;
      ProgressFormUpdateOnProgress
    finally
      TileStream.Free;
    end;
  finally
    bmp32.Free;
    bmp322.Free;
  end;
end;



end.
