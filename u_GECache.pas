unit u_GECache;

interface

uses
  Windows,
  Types,
  Classes,
  SysUtils,
  Graphics,
  Math,
  IJL,
  GR32,
  GR32_Resamplers,
  i_ICoordConverter,
  t_GeoTypes,
  u_GECrypt,
  u_GETexture;

type
TBMPbuf = record BMPTile: array [1..2] of TCustomBitmap32;
                 UpLatLon: array [1..2] of TDoublePoint;
                 DownLatLon: array [1..2] of TDoublePoint;
                 TileRez: array [1..2] of Extended;
                 count:byte; end;

function GetGETile(ResTile:TCustomBitmap32;cachepath:string;x,y:integer;z:byte;AConv:ICoordConverter):boolean;
function GETileExists(cachepath:string;x,y:integer;z:byte;AConv:ICoordConverter):boolean;
function GEGetTile(cachepath:string;x,y:integer;z:byte;var AisJpgStream:boolean):TMemoryStream;

var BMP_Bufer: TBMPbuf;
    indexfilename:string;
    indexfile:TMemoryStream;

implementation

uses
  u_CoordConverterSimpleLonLat;

const
  zoom: array [1..24] of longint = (256,512,1024,2048,4096,8192,16384,32768,65536,
                                   131072,262144,524288,1048576,2097152,4194304,
                                   8388608,16777216,33554432,67108864,134217728,
                                   268435456,536870912,1073741824,2147483647);

function GEXYZtoHexTileName(x,y:integer;z:byte):int64;
var os,prer:TPoint;
    i:byte;
    bytename:byte;
begin
   if (x<0)or(x>=zoom[z])or(y<0)or(y>=zoom[z]) then
    begin
     exit;
    end;
   os.X:=zoom[z]shr 1;
   os.Y:=zoom[z]shr 1;
   prer:=os;
   result:=0;
   bytename:=0;
   for i:=2 to z do begin
     prer.X:=prer.X shr 1;
     prer.Y:=prer.Y shr 1;
     if x<os.X then begin
       os.X:=os.X-prer.X;
       if y<os.y then begin
         os.Y:=os.Y-prer.Y;
         if (i mod 2)=0 then bytename:=12
                        else bytename:=bytename+3;
       end else begin
         os.Y:=os.Y+prer.Y;
         if (i mod 2)=0 then bytename:=0
                        else bytename:=bytename+0;
       end;
     end else begin
       os.X:=os.X+prer.X;
       if y<os.y then begin
         os.Y:=os.Y-prer.Y;
         if (i mod 2)=0 then bytename:=8
                        else bytename:=bytename+2;
       end else begin
         os.Y:=os.Y+prer.Y;
         if (i mod 2)=0 then bytename:=4
                        else bytename:=bytename+1;
       end;
     end;
     if (i mod 2)<>0 then begin
       result:=result or (int64(bytename) shl (64-4*(i div 2)));
     end;
   end;
   if (z mod 2)=0 then begin
     result:=result or (int64(bytename) shl (64-4*(i div 2)));
   end
end;

function GEFindTileAdr(indexpath:string;x,y:integer;z:byte; var size:integer):integer;
var iblock:array [0..31] of byte;
    name,FindName:int64;
    i:integer;
begin
 result:=0;
 size:=0;
 i:=0;
 if FileExists(indexpath) then
 try
   if (indexfilename<>indexpath)or(indexfile=nil) then begin
     indexfile:=TMemoryStream.Create;
     indexfile.LoadFromFile(indexpath);
     indexfilename:=indexpath;
   end;
   FindName:=GEXYZtoHexTileName(x,y,z);
   While ((FindName<>name))and(i<indexfile.Size) do begin
     if (Pbyte(longint(indexfile.Memory)+i+6)^=130)and
        (z=Pbyte(longint(indexfile.Memory)+i+8)^+1)and
        (Pbyte(longint(indexfile.Memory)+i+20)^=0) then begin
       copymemory(@iblock,Pointer(longint(indexfile.Memory)+i),32);
       name:=(int64(iblock[12])shl 32)or(int64(iblock[13])shl 40)or(int64(iblock[14])shl 48)or(int64(iblock[15])shl 56)or
             (iblock[16])or(iblock[17]shl 8)or(iblock[18]shl 16)or(iblock[19]shl 24);
     end;
     inc(i,32);
   end;
   if FindName=name then begin
     result:=(iblock[24]or(iblock[25]shl 8)or(iblock[26]shl 16)or(iblock[27]shl 24));
     size:=(iblock[28]or(iblock[29]shl 8)or(iblock[30]shl 16)or(iblock[31]shl 24));
   end;
 except
  result:=0;
  size:=0;
 end;
end;

function LoadJPG32(jpg_file: TMemoryStream; Btm: TCustomBitmap32): boolean;
  procedure RGBA2BGRA2(pData : Pointer; Width, Height : Integer);
  var W, H : Integer;
      p : PIntegerArray;
  begin
    p := PIntegerArray(pData);
    for H := 0 to Height-1 do begin
      for W := 0 to Width-1 do begin
        p^[W]:=(p^[W] and $FF000000)or((p^[W] and $00FF0000) shr 16)or(p^[W] and $0000FF00)or((p^[W] and $000000FF) shl 16);
      end;
      inc(p,width)
    end;
  end;
var
  iWidth, iHeight, iNChannels : Integer;
  iStatus : Integer;
  jcprops : TJPEG_CORE_PROPERTIES;
begin
 try
    result:=true;
    iStatus := ijlInit(@jcprops);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
    jcprops.JPGBytes:=PByte(jpg_file.Memory);
    jcprops.JPGSizeBytes:=jpg_file.Size;
    iStatus := ijlRead(@jcprops,IJL_JBUFF_READPARAMS);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
    iWidth := jcprops.JPGWidth;
    iHeight := jcprops.JPGHeight;
    iNChannels := 4;
    Btm.SetSize(iWidth,iHeight);
    jcprops.DIBWidth := iWidth;
    jcprops.DIBHeight := iHeight;
    jcprops.DIBChannels := iNChannels;
    jcprops.DIBColor := IJL_RGBA_FPX;
    jcprops.DIBPadBytes := ((((iWidth*iNChannels)+3) div 4)*4)-(iWidth*iNChannels);
    jcprops.DIBBytes := PByte(Btm.Bits);

    if (jcprops.JPGChannels = 3) then
      jcprops.JPGColor := IJL_YCBCR
    else if (jcprops.JPGChannels = 4) then
      jcprops.JPGColor := IJL_YCBCRA_FPX
    else if (jcprops.JPGChannels = 1) then
      jcprops.JPGColor := IJL_G
    else
    begin
      jcprops.DIBColor := TIJL_COLOR (IJL_OTHER);
      jcprops.JPGColor := TIJL_COLOR (IJL_OTHER);
    end;
    iStatus := ijlRead(@jcprops,IJL_JBUFF_READWHOLEIMAGE);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
    RGBA2BGRA2(jcprops.DIBBytes,iWidth,iHeight);
    ijlFree(@jcprops);
  except
    on E: Exception do
    begin
      result:=false;
      ijlFree(@jcprops);
    end;
  end;
end;

function FillBuff(cachepath:string;var x,y:integer;var z:byte;SourcePoint:TPoint;CoordConverter:ICoordConverter):byte;
 function Btm32loadfromstream(var btm:TCustomBitmap32; str:TMemoryStream):boolean;
 begin
   result:=true;
   try
     btm.LoadFromStream(str);
   except
     result:=false;
   end;
 end;

var TileStream:TmemoryStream;
    XY:TPoint;
    LatLon:TDoublePoint;
    i:byte;
    inBuf,isJpgStream:Boolean;
begin
     result:=0;
     inBuf:=false;
     try
         if not inBuf then
         begin
             TileStream:=GEGetTile(cachepath,x*256,y*256,z,isJpgStream);
             if (TileStream<>nil) then begin
              try
               i:=BMP_Bufer.Count+1;
               if i=3 then i:=1;

               if BMP_Bufer.BMPTile[i]<>nil then
               begin
                 FreeAndNil(BMP_Bufer.BMPTile[i]);
               end;

               BMP_Bufer.BMPTile[i]:=TCustomBitmap32.Create;
               if (isJpgStream and LoadJPG32(TileStream,BMP_Bufer.BMPTile[i]))or(not(isJpgStream) and Btm32loadfromstream(BMP_Bufer.BMPTile[i],TileStream)) then begin
                 XY.X:=X;
                 XY.Y:=Y;
                 LatLon:=CoordConverter.TilePos2LonLat(XY,Z-1);
                 BMP_Bufer.UpLatLon[i]:=LatLon;

                 inc(XY.Y);
                 LatLon:=CoordConverter.TilePos2LonLat(XY,Z-1);
                 BMP_Bufer.DownLatLon[i]:=LatLon;

                 BMP_Bufer.TileRez[i]:=(BMP_Bufer.UpLatLon[i].y-BMP_Bufer.DownLatLon[i].y)/256;
                 BMP_Bufer.Count:=i;

                 result:=i;
               end;
              finally
               TileStream.Free;
              end;
             end;
         end;
     finally
     end;
end;

function GetPixNum(var PixCoord:TDoublePoint;var ID:byte):TPoint;
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

procedure MakeInterlaseTile(ResTile:TCustomBitmap32;UpXY:TPoint;Level,id1,id2:byte;Aconv:ICoordConverter);
var LineCoord:TDoublePoint;
    J_GE:Tpoint;
    J_GM:byte;
begin
  if id1<>0 then begin
    ResTile.SetSize(256,256);
    UpXY.X:=UpXY.X*256;
    UpXY.Y:=UpXY.Y*256;
    for J_GM:=0 to 255 do begin
      LineCoord:=Aconv.PixelPos2LonLat(UpXY,(Level-1));
      J_GE:=GetPixNum(LineCoord,id1);
      if (J_GE.X=0)and(id2<>0) then begin
        J_GE:=GetPixNum(LineCoord,id2);
      end;
      if J_GE.X<>0 then begin
        CopyMemory(ResTile.ScanLine[J_GM],BMP_Bufer.BMPTile[J_GE.X].ScanLine[J_GE.Y],256*sizeof(TColor32));
      end;
      inc(UpXY.Y);
    end;
  end;
end;

function GetGETile(ResTile:TCustomBitmap32;cachepath:string;x,y:integer;z:byte;AConv:ICoordConverter):boolean;
var Up,Down:TDoublePoint;
    UpXY,DownXY,gXY1,gXY2:TPoint;
    id1,id2:byte;
    abort:boolean;
    CoordConverter:ICoordConverter;
begin
   CoordConverter:=TCoordConverterSimpleLonLat.Create(6378137, 6356752);
   abort:=false;
   id2:=0;
   UpXY.X:=X;
   UpXY.Y:=Y;
   Up:=AConv.TilePos2LonLat(UpXY,Z-1);     // долота/штрота верхней левой точки
   DownXY.X:=X;
   DownXY.Y:=Y+1;
   Down:=AConv.TilePos2LonLat(DownXY,Z-1);   // долота/штрота НИЖНЕЙ левой точки
   gXY1:=CoordConverter.LonLat2TilePos(Up,Z-1);
   gXY2:=CoordConverter.LonLat2TilePos(Down,Z-1);
   id1:=FillBuff(cachepath,gXY1.x,gXY1.y,z,UpXY,CoordConverter);
   if id1=0 then abort:=true;
   if (gXY1.X<>gXY2.X)or(gXY1.Y<>gXY2.Y) then
   begin
      id2:=FillBuff(cachepath,gXY2.x,gXY2.y,z,DownXY,CoordConverter);
      if id2=0 then abort:=true;
   end;
   CoordConverter:=nil;
   if (not abort) then begin
     MakeInterlaseTile(ResTile,UpXY,Z,id1,id2,AConv);
     Result:=true;
   end
   else begin
     result:=false;
   end;
end;

function GETileExists(cachepath:string;x,y:integer;z:byte;AConv:ICoordConverter):boolean;
var Up,Down:TDoublePoint;
    UpXY,DownXY,gXY1,gXY2:TPoint;
    bsize:integer;
    CoordConverter:ICoordConverter;
begin
   result:=false;
   CoordConverter:=TCoordConverterSimpleLonLat.Create(6378137, 6356752);
   UpXY.X:=X;
   UpXY.Y:=Y;
   Up:=AConv.TilePos2LonLat(UpXY,Z-1);     // долота/штрота верхней левой точки
   DownXY.X:=X;
   DownXY.Y:=Y+1;
   Down:=AConv.TilePos2LonLat(DownXY,Z-1);   // долота/штрота НИЖНЕЙ левой точки
   gXY1:=CoordConverter.LonLat2TilePos(Up,Z-1);
   gXY2:=CoordConverter.LonLat2TilePos(Down,Z-1);
   CoordConverter:=nil;
   if GEFindTileAdr(cachepath,gXY1.X*256,gXY1.Y*256,z,bsize)=0 then begin
     exit;
   end;
   if (gXY1.X<>gXY2.X)or(gXY1.Y<>gXY2.Y) then begin
     if GEFindTileAdr(cachepath,gXY2.X*256,gXY2.Y*256,z,bsize)=0 then begin
       exit;
     end;
   end;
   result:=true;
end;

//----------------------

function GetTile(var tile:TMemoryStream; adr:integer;len:integer;cachepath:string):boolean;
var f:File;
begin
 if FileExists(cachepath) then
 try
   try
     AssignFile(f,cachepath);
     Reset(f,1);
     Seek(f,adr);
     tile:=TMemoryStream.Create;
     tile.SetSize(len);
     BlockRead(f,tile.memory^,len);
     Result:=true;
   finally
     CloseFile(f);
   end;
 except
   result:=False;
 end;
end;

function GEGetTile(cachepath:string;x,y:integer;z:byte;var AisJpgStream:boolean):TMemoryStream;
var adr,len:integer;
begin
 adr:=GEFindTileAdr(cachepath+'.index',x,y,z,len);
 if adr>0 then begin
   if GetTile(result,adr+36,len,cachepath) then begin
    case LongWord(result.Memory^) of
          CRYPTED_JPEG: begin
                          GEcrypt(result.Memory,result.Size);
                          AisJpgStream:=true;
                        end;
        DECRYPTED_JPEG: AisJpgStream:=true;

          CRYPTED_DXT1: begin
                          if result.Size <> DXT_FILE_SIZE then begin
                            result:=nil;
                          end else begin
                            GEcrypt(result.Memory,result.Size);
                            Texture2BMP(result);
                            AisJpgStream:=false;
                          end;
                        end;

        DECRYPTED_DXT1: begin
                          if result.Size <> DXT_FILE_SIZE then begin
                            result:=nil;
                          end else begin
                            Texture2BMP(result);
                            AisJpgStream:=false;
                          end;
                        end

        else            begin
                          result:=nil;
                        end;
    end;
   end else begin
    result:=nil;
   end;
 end
 else result:=nil;
end;

end.
