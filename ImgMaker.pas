unit ImgMaker;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Graphics,
  Math,
  Jpeg,
  GR32,
  GR32_Resamplers,
  UMapType,
  UGeoFun,
  u_CoordConverterAbstract;

type
TExtended = record Lat: Extended;
					         Lon: Extended; end;

TBMPbuf = record BMPTile: array [1..2] of TBitmap;
                   UpLatLon: array [1..2] of TExtendedPoint;
                   DownLatLon: array [1..2] of TExtendedPoint;
                   TileRez: array [1..2] of Extended;
                   MerkatorSourceXY: array [1..2] of TPoint;
                   count:byte; end;
TRGBTripleArr = array [0..255] of TRGBTriple;

function GetMerkatorGETile(var ResTile:TMemoryStream;cachepath:string;x,y:integer;z:byte;MT:TMapType):boolean;
function GEGetTile(cachepath:string;x,y:integer;z:byte):TMemoryStream;
function GEXYZtoTileName(x,y:integer;z:byte):string;

var BMP_Bufer: TBMPbuf;
    Tile:integer;
    key: array[0..1023] of byte = (
	$94, $64, $87, $4E, $66, $00, $72, $42, $45, $F4, $BD, $0B, $79, $E2, $6A, $45,
	$22, $05, $92, $2C, $17, $CD, $06, $71, $F8, $49, $10, $46, $67, $51, $00, $42,
	$25, $C6, $E8, $61, $2C, $66, $29, $08, $C6, $34, $DC, $6A, $62, $25, $79, $0A,
	$77, $1D, $6D, $69, $D6, $F0, $9C, $6B, $93, $A1, $BD, $4E, $75, $E0, $41, $04,
	$5B, $DF, $40, $56, $0C, $D9, $BB, $72, $9B, $81, $7C, $10, $33, $53, $EE, $4F,
	$6C, $D4, $71, $05, $B0, $7B, $C0, $7F, $45, $03, $56, $5A, $AD, $77, $55, $65,
	$0B, $33, $92, $2A, $AC, $19, $6C, $35, $14, $C5, $1D, $30, $73, $F8, $33, $3E,
	$6D, $46, $38, $4A, $B4, $DD, $F0, $2E, $DD, $17, $75, $16, $DA, $8C, $44, $74,
	$22, $06, $FA, $61, $22, $0C, $33, $22, $53, $6F, $AF, $39, $44, $0B, $8C, $0E,
	$39, $D9, $39, $13, $4C, $B9, $BF, $7F, $AB, $5C, $8C, $50, $5F, $9F, $22, $75,
	$78, $1F, $E9, $07, $71, $91, $68, $3B, $C1, $C4, $9B, $7F, $F0, $3C, $56, $71,
	$48, $82, $05, $27, $55, $66, $59, $4E, $65, $1D, $98, $75, $A3, $61, $46, $7D,
	$61, $3F, $15, $41, $00, $9F, $14, $06, $D7, $B4, $34, $4D, $CE, $13, $87, $46,
	$B0, $1A, $D5, $05, $1C, $B8, $8A, $27, $7B, $8B, $DC, $2B, $BB, $4D, $67, $30,
	$C8, $D1, $F6, $5C, $8F, $50, $FA, $5B, $2F, $46, $9B, $6E, $35, $18, $2F, $27,
	$43, $2E, $EB, $0A, $0C, $5E, $10, $05, $10, $A5, $73, $1B, $65, $34, $E5, $6C,
	$2E, $6A, $43, $27, $63, $14, $23, $55, $A9, $3F, $71, $7B, $67, $43, $7D, $3A,
	$AF, $CD, $E2, $54, $55, $9C, $FD, $4B, $C6, $E2, $9F, $2F, $28, $ED, $CB, $5C,
	$C6, $2D, $66, $07, $88, $A7, $3B, $2F, $18, $2A, $22, $4E, $0E, $B0, $6B, $2E,
	$DD, $0D, $95, $7D, $7D, $47, $BA, $43, $B2, $11, $B2, $2B, $3E, $4D, $AA, $3E,
	$7D, $E6, $CE, $49, $89, $C6, $E6, $78, $0C, $61, $31, $05, $2D, $01, $A4, $4F,
	$A5, $7E, $71, $20, $88, $EC, $0D, $31, $E8, $4E, $0B, $00, $6E, $50, $68, $7D,
	$17, $3D, $08, $0D, $17, $95, $A6, $6E, $A3, $68, $97, $24, $5B, $6B, $F3, $17,
	$23, $F3, $B6, $73, $B3, $0D, $0B, $40, $C0, $9F, $D8, $04, $51, $5D, $FA, $1A,
	$17, $22, $2E, $15, $6A, $DF, $49, $00, $B9, $A0, $77, $55, $C6, $EF, $10, $6A,
	$BF, $7B, $47, $4C, $7F, $83, $17, $05, $EE, $DC, $DC, $46, $85, $A9, $AD, $53,
	$07, $2B, $53, $34, $06, $07, $FF, $14, $94, $59, $19, $02, $E4, $38, $E8, $31, 
	$83, $4E, $B9, $58, $46, $6B, $CB, $2D, $23, $86, $92, $70, $00, $35, $88, $22, 
	$CF, $31, $B2, $26, $2F, $E7, $C3, $75, $2D, $36, $2C, $72, $74, $B0, $23, $47,
	$B7, $D3, $D1, $26, $16, $85, $37, $72, $E2, $00, $8C, $44, $CF, $10, $DA, $33, 
	$2D, $1A, $DE, $60, $86, $69, $23, $69, $2A, $7C, $CD, $4B, $51, $0D, $95, $54, 
	$39, $77, $2E, $29, $EA, $1B, $A6, $50, $A2, $6A, $8F, $6F, $50, $99, $5C, $3E,
	$54, $FB, $EF, $50, $5B, $0B, $07, $45, $17, $89, $6D, $28, $13, $77, $37, $1D,
	$DB, $8E, $1E, $4A, $05, $66, $4A, $6F, $99, $20, $E5, $70, $E2, $B9, $71, $7E, 
	$0C, $6D, $49, $04, $2D, $7A, $FE, $72, $C7, $F2, $59, $30, $8F, $BB, $02, $5D, 
	$73, $E5, $C9, $20, $EA, $78, $EC, $20, $90, $F0, $8A, $7F, $42, $17, $7C, $47,
	$19, $60, $B0, $16, $BD, $26, $B7, $71, $B6, $C7, $9F, $0E, $D1, $33, $82, $3D,
	$D3, $AB, $EE, $63, $99, $C8, $2B, $53, $A0, $44, $5C, $71, $01, $C6, $CC, $44, 
	$1F, $32, $4F, $3C, $CA, $C0, $29, $3D, $52, $D3, $61, $19, $58, $A9, $7D, $65,
	$B4, $DC, $CF, $0D, $F4, $3D, $F1, $08, $A9, $42, $DA, $23, $09, $D8, $BF, $5E, 
	$50, $49, $F8, $4D, $C0, $CB, $47, $4C, $1C, $4F, $F7, $7B, $2B, $D8, $16, $18,
	$C5, $31, $92, $3B, $B5, $6F, $DC, $6C, $0D, $92, $88, $16, $D1, $9E, $DB, $3F,
	$E2, $E9, $DA, $5F, $D4, $84, $E2, $46, $61, $5A, $DE, $1C, $55, $CF, $A4, $00,
	$BE, $FD, $CE, $67, $F1, $4A, $69, $1C, $97, $E6, $20, $48, $D8, $5D, $7F, $7E,
	$AE, $71, $20, $0E, $4E, $AE, $C0, $56, $A9, $91, $01, $3C, $82, $1D, $0F, $72,
	$E7, $76, $EC, $29, $49, $D6, $5D, $2D, $83, $E3, $DB, $36, $06, $A9, $3B, $66,
	$13, $97, $87, $6A, $D5, $B6, $3D, $50, $5E, $52, $B9, $4B, $C7, $73, $57, $78,
	$C9, $F4, $2E, $59, $07, $95, $93, $6F, $D0, $4B, $17, $57, $19, $3E, $27, $27, 
	$C7, $60, $DB, $3B, $ED, $9A, $0E, $53, $44, $16, $3E, $3F, $8D, $92, $6D, $77,
	$A2, $0A, $EB, $3F, $52, $A8, $C6, $55, $5E, $31, $49, $37, $85, $F4, $C5, $1F,
	$26, $2D, $A9, $1C, $BF, $8B, $27, $54, $DA, $C3, $6A, $20, $E5, $2A, $78, $04,
	$B0, $D6, $90, $70, $72, $AA, $8B, $68, $BD, $88, $F7, $02, $5F, $48, $B1, $7E, 
	$C0, $58, $4C, $3F, $66, $1A, $F9, $3E, $E1, $65, $C0, $70, $A7, $CF, $38, $69,
	$AF, $F0, $56, $6C, $64, $49, $9C, $27, $AD, $78, $74, $4F, $C2, $87, $DE, $56, 
	$39, $00, $DA, $77, $0B, $CB, $2D, $1B, $89, $FB, $35, $4F, $02, $F5, $08, $51,
	$13, $60, $C1, $0A, $5A, $47, $4D, $26, $1C, $33, $30, $78, $DA, $C0, $9C, $46,
	$47, $E2, $5B, $79, $60, $49, $6E, $37, $67, $53, $0A, $3E, $E9, $EC, $46, $39,
	$B2, $F1, $34, $0D, $C6, $84, $53, $75, $6E, $E1, $0C, $59, $D9, $1E, $DE, $29,
	$85, $10, $7B, $49, $49, $A5, $77, $79, $BE, $49, $56, $2E, $36, $E7, $0B, $3A,
	$BB, $4F, $03, $62, $7B, $D2, $4D, $31, $95, $2F, $BD, $38, $7B, $A8, $4F, $21,
	$E1, $EC, $46, $70, $76, $95, $7D, $29, $22, $78, $88, $0A, $90, $DD, $9D, $5C,
	$DA, $DE, $19, $51, $CF, $F0, $FC, $59, $52, $65, $7C, $33, $13, $DF, $F3, $48,
	$DA, $BB, $2A, $75, $DB, $60, $B2, $02, $15, $D4, $FC, $19, $ED, $1B, $EC, $7F,
	$35, $A8, $FF, $28, $31, $07, $2D, $12, $C8, $DC, $88, $46, $7C, $8A, $5B, $22);

implementation
uses unit1;

function GPos2LonLat3(XY:TPoint;Level:byte):TExtendedPoint;
begin
    result.x:=((XY.x)-zoom[Level]/2)/(zoom[Level]/360);
    result.y:=-((XY.y)-zoom[Level]/2)/(zoom[Level]/360);      // ƒолгота/широта
end;

function FillBuff(cachepath:string;var x,y:integer;var z:byte;SourcePoint:TPoint;MT:TMapType):byte;
var TileStream:TmemoryStream;
JpegImg: TJpegImage;
XY:TPoint;
LatLon:TExtendedPoint;
i:byte;
inBuf:Boolean;
begin
     result:=0;
     inBuf:=false;
     try
         if not inBuf then
         begin
             TileStream:=GEGetTile(cachepath,x*256,y*256,z);
             if (TileStream<>nil) then
             begin
               JpegImg:= TjpegImage.Create;
               TileStream.Position:=0;
               JpegImg.LoadFromStream(TileStream);
               TileStream.Free;

               i:=BMP_Bufer.Count+1;
               if i=3 then i:=1;

               if BMP_Bufer.BMPTile[i]<>nil then
               begin
                 BMP_Bufer.BMPTile[i].Canvas.Unlock;
                 FreeAndNil(BMP_Bufer.BMPTile[i]);

               end;

               BMP_Bufer.BMPTile[i]:=TBitmap.Create;
               BMP_Bufer.BMPTile[i].Canvas.Lock;
               BMP_Bufer.BMPTile[i].Width:=256;
               BMP_Bufer.BMPTile[i].Height:=256;
               BMP_Bufer.BMPTile[i].Assign(JpegImg);

               JpegImg.Destroy;

               BMP_Bufer.MerkatorSourceXY[i].X:=SourcePoint.X;
               BMP_Bufer.MerkatorSourceXY[i].Y:=SourcePoint.Y;

               XY.X:=X;
               XY.Y:=Y;
               LatLon:=GPos2LonLat3(Point(XY.x*256,XY.y*256),Z);
               BMP_Bufer.UpLatLon[i].y:=LatLon.y;
               BMP_Bufer.UpLatLon[i].x:=LatLon.x;

               inc(XY.Y);
               LatLon:=GPos2LonLat3(Point(XY.x*256,XY.y*256),Z);
               BMP_Bufer.DownLatLon[i].y:=LatLon.y;
               BMP_Bufer.DownLatLon[i].x:=LatLon.x;

               BMP_Bufer.TileRez[i]:=(BMP_Bufer.UpLatLon[i].y-BMP_Bufer.DownLatLon[i].y)/256;
               BMP_Bufer.Count:=i;

               result:=i;
             end;
         end;
     finally
     end;
end;

function GetPixNum(var PixCoord:TExtendedPoint;var ID:byte):TPoint;
begin
  if (PixCoord.x=BMP_Bufer.UpLatLon[ID].x) and
     (PixCoord.y<=BMP_Bufer.UpLatLon[ID].y) and
     (PixCoord.y>=BMP_Bufer.DownLatLon[ID].y) then
  begin
    result.Y:=floor((BMP_Bufer.UpLatLon[ID].y-PixCoord.y)/BMP_Bufer.TileRez[ID]);
    result.X:=ID;
  end
  else
    result:=Point(0,0);
end;

function MakeInterlaseTile(UpXY:TPoint;Level,id1,id2:byte;MT:TMapType):TMemoryStream;
var LineCoord:TExtendedPoint;
    bitmap:Tbitmap;
    Jpeg2:TjpegImage;
    J_GE:Tpoint;
    J_GM:byte;
begin
     result:=nil;
     if id1<>0 then
     begin
       bitmap:=TBitmap.Create;
       bitmap.PixelFormat:=pf24bit;
       bitmap.Canvas.Lock;
       bitmap.Width:=256;
       bitmap.Height:=256;
       UpXY.X:=UpXY.X*256;
       UpXY.Y:=UpXY.Y*256;
       for J_GM := 0 to 255 do
       begin
          LineCoord:=MT.GeoConvert.Pos2LonLat(UpXY,(Level - 1) + 8);
          J_GE:=GetPixNum(LineCoord,id1); // номер строки на GE тайле
          if (J_GE.X=0) and (id2<>0) then
          J_GE:=GetPixNum(LineCoord,id2);
          if J_GE.X<>0 then
           begin
              TRGBTripleArr(Bitmap.ScanLine[J_GM]^):=TRGBTripleArr(BMP_Bufer.BMPTile[J_GE.X].ScanLine[J_GE.Y]^);
           end;
          inc(UpXY.Y);
       end;
        result:=TMemoryStream.Create;
        try
          Jpeg2:= TjpegImage.Create;
          jpeg2.CompressionQuality := 100;
          Jpeg2.PixelFormat := jf24Bit;
          jpeg2.Compress;
          Jpeg2.Assign(bitmap);
          Jpeg2.SaveToStream(result);
        finally
           Jpeg2.Free;
           bitmap.Canvas.UnLock;
           Bitmap.Free;
           if result.Size=0 then FreeAndNil(result);
        end;
      end;
end;

function GLonLat2Pos3(Point:TExtendedPoint;Level:byte):TPoint;
var
i:longint;
begin
  i:=zoom[Level];
  result.X:=round(i/2+Point.x*(i/360));
  result.Y:=round(i/2-Point.y*(i/360));
end;

function GetMerkatorGETile(var ResTile:TMemoryStream;cachepath:string;x,y:integer;z:byte;MT:TMapType):boolean;
var Up,Down:TExtendedPoint;
UpXY,DownXY,gXY1,gXY2:TPoint;
id1,id2:byte;
abort:boolean;
begin
   if ResTile=nil then ResTile:=TMemoryStream.Create;
   abort:=false;
   id2:=0;
   UpXY.X:=X;
   UpXY.Y:=Y;
   Up:=mt.GeoConvert.Pos2LonLat(Point(UpXY.x*256,UpXY.y*256),(Z - 1) + 8);     // долота/штрота верхней левой точки
   DownXY.X:=X;
   DownXY.Y:=Y+1;
   Down:=mt.GeoConvert.Pos2LonLat(Point(DownXY.x*256,DownXY.y*256),(Z - 1) + 8);   // долота/штрота Ќ»∆Ќ≈… левой точки
   gXY1:=GLonLat2Pos3(Up,Z);   // x,y тайла GE куда попала верхн€€ точка
   gXY2:=GLonLat2Pos3(Down,Z); // x,y тайла GE куда попала Ќ»∆Ќяя точка
   gXY1:=Point(gXY1.x div 256,gXY1.y div 256);
   gXY2:=Point(gXY2.x div 256,gXY2.y div 256);
   X:=gXY1.X;
   Y:=gXY1.Y;
   id1:=FillBuff(cachepath,x,y,z,UpXY,MT);
   if id1=0 then abort:=true;
   if (gXY1.X<>gXY2.X)or(gXY1.Y<>gXY2.Y) then
   begin
      X:=gXY2.X;
      Y:=gXY2.Y;
      id2:=FillBuff(cachepath,x,y,z,DownXY,MT);
      if id2=0 then abort:=true;
   end;
   if not abort then begin
                      ResTile:=MakeInterlaseTile(UpXY,Z,id1,id2,MT);
                      Result:=true;
                     end
                else begin
                      result:=false;
                     end;
end;


//----------------------

function HexToInt(HexStr : string) : Int64;
var RetVar : Int64;
    i : byte;
begin
  HexStr := UpperCase(HexStr);
  if HexStr[length(HexStr)] = 'H' then  Delete(HexStr,length(HexStr),1);
  RetVar := 0;
  for i := 1 to length(HexStr) do
    begin
      RetVar := RetVar shl 4;
      if HexStr[i] in ['0'..'9'] then
         RetVar := RetVar + (byte(HexStr[i]) - 48)
      else if HexStr[i] in ['A'..'F'] then RetVar := RetVar + (byte(HexStr[i]) - 55)
             else begin
                    Retvar := 0;
                    break;
                  end;
    end;
  Result := RetVar;
end;


function GEDecrypt(Mem_Orig:TMemoryStream):TMemoryStream;
var i,j,keystart,keylen,Orig_File_Size: integer;
    lBuf,fBuf: byte;
    Mem_Dcr:TMemoryStream;
begin
  Mem_Dcr:=TMemoryStream.Create;
  Orig_File_Size:=Mem_Orig.Size;
  keystart:=16;
  keylen:=$3f8;
  j:=16;
  for i:=0 to Orig_File_Size-1 do
  begin
       Mem_Orig.Seek(i,0);
       Mem_Dcr.Seek(i,0);
       Mem_Orig.ReadBuffer(lBuf,1);
       fBuf:=lBuf xor key[j+8];
       Mem_Dcr.WriteBuffer(fBuf,1);
       j:=j+1;
       if (j mod 8) = 0 then j:=j+16;
       if j >= keylen then
       begin
            keystart:=(keystart+8) Mod 24;
            j:=keystart;
       end;
  end;
  result:=Mem_Dcr;
end;

function GetTile(adr:integer;len:integer;cachepath:string):TMemoryStream;
var f:File;
    buf:Pointer;
begin
 AssignFile(f,cachepath);
 Reset(f,1);
 Seek(f,adr);
 GetMem(buf,len);
 BlockRead(f,buf^,len);
 CloseFile(f);
 result:=TMemoryStream.Create;
 result.WriteBuffer(buf^,len);
 result.Position:=0;
end;


function GEXYZtoTileName(x,y:integer;z:byte):string;
var os,prer:TPoint;
    i:byte;
begin
   if (x<0)or(x>=zoom[z])or(y<0)or(y>=zoom[z]) then
    begin
     result:='';
     exit;
    end;
   os.X:=zoom[z]shr 1;
   os.Y:=zoom[z]shr 1;
   prer:=os;
   result:='0';
   for i:=2 to z do
    begin
    prer.X:=prer.X shr 1;
    prer.Y:=prer.Y shr 1;
    if x<os.X
     then begin
           os.X:=os.X-prer.X;
           if y<os.y then begin
                            os.Y:=os.Y-prer.Y;
                            result:=result+'3';
                           end
                      else begin
                            os.Y:=os.Y+prer.Y;
                            result:=result+'0';
                           end;
          end
     else begin
           os.X:=os.X+prer.X;
           if y<os.y then begin
                           os.Y:=os.Y-prer.Y;
                           result:=result+'2';
                          end
                     else begin
                           os.Y:=os.Y+prer.Y;
                           result:=result+'1';
                          end;
         end;
    end;
end;

function Hex2GEName(HexName:string; Level:byte):string;
var
i:byte;
begin
 result:='0';
 for i:=1 to  Level div 2 do
  case HexName[i] of
   '0': result:=result+'00';
   '1': result:=result+'01';
   '2': result:=result+'02';
   '3': result:=result+'03';
   '4': result:=result+'10';
   '5': result:=result+'11';
   '6': result:=result+'12';
   '7': result:=result+'13';
   '8': result:=result+'20';
   '9': result:=result+'21';
   'A': result:=result+'22';
   'B': result:=result+'23';
   'C': result:=result+'30';
   'D': result:=result+'31';
   'E': result:=result+'32';
   'F': result:=result+'33';
  end;
 result:=copy(result,1,level);
end;


function GEFindTileAdr(indexpath:string;x,y:integer;z:byte; var size:integer):integer;
var ms:TMemoryStream;
    iblock:array [0..31] of byte;
    name1,name2:integer;
    Tname,FindName:string;
begin
 result:=0;
 ms:=TMemoryStream.Create;
 ms.LoadFromFile(indexpath);
 ms.Position:=0;
 FindName:=GEXYZtoTileName(x,y,z);
 Tname:='';
 While ((Tname<>FindName))and(ms.Position<ms.Size) do
  begin
   ms.Read(iblock,32);
   if (iblock[6]=130)and(z=iblock[8]+1)and(iblock[20]=0) then
    begin
     name1:=(iblock[12]or(iblock[13]shl 8)or(iblock[14]shl 16)or(iblock[15]shl 24));
     name2:=(iblock[16]or(iblock[17]shl 8)or(iblock[18]shl 16)or(iblock[19]shl 24));
     Tname:=Hex2GEName((inttohex((name1),8)+inttohex((name2),8)),z);
    end;
  end;
 if Tname=FindName then
  begin
   result:=(iblock[24]or(iblock[25]shl 8)or(iblock[26]shl 16)or(iblock[27]shl 24));
   size:=(iblock[28]or(iblock[29]shl 8)or(iblock[30]shl 16)or(iblock[31]shl 24));
  end;
 ms.Free;
end;

function GEGetTile(cachepath:string;x,y:integer;z:byte):TMemoryStream;
var adr,len:integer;
begin
 adr:=GEFindTileAdr(cachepath+'.index',x,y,z,len);
 if adr>0 then result:=GEDecrypt(GetTile(adr+36,len,cachepath))
          else result:=nil;
end;

end.
