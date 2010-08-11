unit u_GETexture;

interface

uses Classes;

const DXT_FILE_SIZE = 32792;
const BMP_FILE_SIZE = 196662;

//Переводит GE текстуру в BMP формат
procedure Texture2BMP (var Texture: TMemoryStream);

//Переводит GE текстуру в DDS формат
procedure Texture2DDS (var Texture: TMemoryStream; const PathToSave: string);

implementation

const BMP_HEAD: array[0..53] of byte = (
	$42, $4D, $36, $00, $03, $00, $00, $00, $00, $00, $36, $00, $00, $00, $28, $00,
	$00, $00, $00, $01, $00, $00, $00, $01, $00, $00, $01, $00, $18, $00, $00, $00,
	$00, $00, $00, $00, $03, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
	$00, $00, $00, $00, $00, $00 );

const DDS_HEAD: array[0..127] of byte = (
	$44, $44, $53, $20, $7C, $00, $00, $00, $07, $10, $00, $00, $00, $01, $00, $00, 
	$00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, 
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, 
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, 
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $20, $00, $00, $00, 
	$04, $00, $00, $00, $44, $58, $54, $31, $00, $00, $00, $00, $00, $00, $00, $00, 
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $10, $00, $00, 
	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);

type

  TGE_HEAD = packed record
    Magic : LongWord;
    Xrez  : LongWord;
    Yrez  : LongWord;
    Unk1  : LongWord;
    Unk2  : int64;
  end;

  TDXT1 = packed record
    Color0  : word;
    Color1  : word;
    BitMask : LongWord;
  end;

  TGETexture = packed record
    GE_HEAD : TGE_HEAD;
    DXT1    : array [0..63, 0..63] of TDXT1;
  end;

  TRGB = packed record
    B : byte;
    G : byte;
    R : byte;
  end;

//Определение опорных цветов
procedure GetColors (Color0,Color1:word; var Colors24bit: array of TRGB);

const b5 : array [0..31] of byte =(
 $00, $08, $10, $19, $21, $29, $31, $3A, $42, $4A, $52, $5A, $63, $6B, $73, $7B,
 132, 140, 148, 156, 165, 173, 181, 189, 197, 206, 214, 222, 230, 239, 247, 255);
const b6 : array [0..63] of byte =(
 0, 4, 8, 12, 16, 20, 24,  28, 32, 36, 40, 45, 49, 53, 57, 61, 65, 69, 73, 77,
 81, 85, 89, 93, 97, 101,  105, 109, 113, 117, 121, 125, 130, 134, 138, 142,
 146, 150, 154, 158, 162, 166,  170, 174, 178, 182, 186, 190, 194, 198, 202,
 206, 210, 215, 219, 223, 227, 231,  235, 239, 243, 247, 251, 255);

  function RGB565_To_RGB888 (Input:Word):TRGB;
  var x:word;
  begin
   result.R := b5[(Input shr 11)];
    x:= (Input shl 5);
   result.G := b6[(x shr 10)];
    x:= (Input shl 11);
   result.B := b5[(x shr 11)];
  end;

begin
   Colors24bit[0]:= RGB565_To_RGB888(Color0);
   Colors24bit[1]:= RGB565_To_RGB888(Color1);
   if Color0 > Color1 then begin
     Colors24bit[2].R:= (2*Colors24bit[0].R+Colors24bit[1].R) div 3;
     Colors24bit[2].G:= (2*Colors24bit[0].G+Colors24bit[1].G) div 3;
     Colors24bit[2].B:= (2*Colors24bit[0].B+Colors24bit[1].B) div 3;
     Colors24bit[3].R:= (Colors24bit[0].R+2*Colors24bit[1].R) div 3;
     Colors24bit[3].G:= (Colors24bit[0].G+2*Colors24bit[1].G) div 3;
     Colors24bit[3].B:= (Colors24bit[0].B+2*Colors24bit[1].B) div 3;
   end else begin
     Colors24bit[2].R:= (Colors24bit[0].R+Colors24bit[1].R) div 2;
     Colors24bit[2].G:= (Colors24bit[0].G+Colors24bit[1].G) div 2;
     Colors24bit[2].B:= (Colors24bit[0].B+Colors24bit[1].B) div 2;
     Colors24bit[3].R := 0;
     Colors24bit[3].G := 0;
     Colors24bit[3].B := 0;
   end;
end;

//По номеру пикселя определяется ID опорного цвета, для данного пикселя
function GetColorID (Mask: LongWord; Pixel: integer): integer;
begin
  if ((Mask shr (2*Pixel+1)) and 1) = 1  then begin
   if ((Mask shr (2*Pixel)) and 1) = 1  then result := 3
   else result:=2;
  end else begin
   if ((Mask shr (2*Pixel)) and 1) = 1  then result := 1
   else result:=0;
  end;
end;

//Распаковка DXT1 текстуры в BMP
procedure DecodeDXT (const Texture:TGETexture; var Buf:TMemoryStream);
var i,j,k,n : integer;
        pix : byte;
      Color : array [0..3] of TRGB;
begin
  Buf.SetSize(BMP_FILE_SIZE);
  Buf.Position:=0;
  Buf.Write(BMP_HEAD, Length(BMP_HEAD));                                        //пишем заголовок BMP
  for i := 0 to 63 do
  for j := 0 to 63 do begin
    GetColors(Texture.DXT1[i,j].Color0, Texture.DXT1[i,j].Color1, Color);       //определяем опорные цвета для блока
    pix:=0;
    for k := 0 to 3 do
    for n := 0 to 3 do begin
     Buf.Position:=54+((4*i+k)*256+4*j+n)*3;                                    //позиция пикселя на битмапе
     Buf.Write(Color[GetColorID(Texture.DXT1[i,j].BitMask, pix)], 3);           //для каждого пикселя из блока, записываем его реальный цвет
     inc(pix);
    end;
  end;
end;

procedure Texture2BMP (var Texture: TMemoryStream);
var Buf:TMemoryStream;
begin
  Buf:=TMemoryStream.Create;
  try
   DecodeDXT(TGETexture(Texture.Memory^),Buf);
   Texture.LoadFromStream(buf);
   Texture.Position:=0;
  finally
   Buf.Free;
  end;
end;

procedure Texture2DDS (var Texture: TMemoryStream; const PathToSave: string);
var Buf : TMemoryStream;
    Raw : TGETexture;
    i,j : integer;
begin
  Buf:=TMemoryStream.Create;
  try
   Buf.Write(DDS_HEAD, Length(DDS_HEAD));
   Raw:=TGETexture(Texture.Memory^);
   for i := 0 to 63 do begin
     for j := 0 to 63 do begin
       Buf.Write(Raw.DXT1[i,j], SizeOf(TDXT1));
     end;
   end;
   Buf.Position:=0;
   if PathToSave <> '' then Buf.SaveToFile(PathToSave);
  finally
   Buf.Free;
  end;
end;

end.
