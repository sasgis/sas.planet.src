unit u_BmpUtil;

interface

type
  TBGR= record
   b,g,r:byte;
  end;

  PlineRGBb = ^TlineRGBb;
  TlineRGBb = array[0..0] of TBGR;

  TBMPRead = procedure(Line:cardinal;InputArray:PLineRGBb) of object;
  TBmpCancel = function(): Boolean of object;

  procedure SaveBMP(W, H : integer; tPath : string; readcallback:TBMPRead; ACancelDelegate: TBmpCancel);

implementation

type
  bmFileHeader = record	{заголовок файла}
    //Typf : word;        {сигнатура }
    Size : longint;     {длина файла в байтах}
    Res1 : word;        {зарезервировано}
    Res2 : word;        {зарезервировано}
    OfBm : longint;     {смещение изображения в байтах (1078) = $36}
  end;
  bmInfoHeader = record   {информационный заголовок}
    Size : longint;       {длина заголовка в байтах (40) = $28}
    Widt : longint;       {ширина изображения (в точках)}
    Heig : longint;       {высота изображения (в точках)}
    Plan : word;          {число плоскостей (1)}
    BitC : word;          {глубина цвета (бит на точку) (8)}
    Comp : longint;       {тип компрессии (0 - нет)}
    SizI : longint;       {размер изображения в байтах}
    XppM : longint;       {горизонтальное разрешение}
 		          {(точек на метр - обычно 0)}
    YppM : longint;       {вертикальное разрешение}
		          {(точек на метр - обычно 0)}
    NCoL : longint;       {число цветов}
		          {(если максимально допустимое - 0)}
    NCoI : longint;       {число основных цветов}
  end;
  bmHeader = record       {полный заголовок файла}
    f : bmFileHeader;     {заголовок файла}
    i : bmInfoHeader;     {информационный заголовок}
    //p : array[0..255,0..3]of byte; {таблица палитры}
  end;

function SaveBMPHeader(filename:string;W : longint;H : longint): bmHeader;
begin
   Result.i.Size:=$28; //40;
   Result.i.Widt:=W;
   Result.i.Heig:=H;
   Result.i.Plan:=1;
   Result.i.BitC:=$18;    // количество цветов 24
   Result.i.Comp:=0;

   Result.i.SizI:=W * H * 3 + (W mod 4)*H;// размер витмапа
   Result.i.XppM:=0;
   Result.i.YppM:=0;
   Result.i.NCoL:=0;
   Result.i.NCoI:=0;

   Result.f.Res1:=0;
   Result.f.Res2:=0;
   Result.f.OfBm:=$36;        // $36 = 54  // смещение витмапа от начала файла
   Result.f.Size:=Result.i.SizI + Result.f.OfBm;   // полный размер файла
end;

procedure SaveBMP(W, H : integer; tPath : string; readcallback:TBMPRead; ACancelDelegate: TBmpCancel);  // Запись на диск файла
Var f : file;
    nNextLine: integer;
    InputArray:PlineRGBb;
    TypeBmp:Word;
    Header: bmHeader;
  BMPRead:TBMPRead;
begin
   Header:=SaveBMPHeader(tPath,W,H);
   AssignFile(f,tPath);
   ReWrite(f,1);
   TypeBmp  := $4D42;

   BlockWrite(f,TypeBmp,sizeof(TypeBmp));
   BlockWrite(f,Header,sizeof(Header));

   BMPRead:=readcallback;
   getmem(InputArray,W*3);

   for nNextLine:=0 to h-1 do begin
     if Assigned(ACancelDelegate) then begin
       if ACancelDelegate then begin
         break;
       end;
     end;
     BMPRead(nNextLine,InputArray);
     seek(f,(h-nNextLine-1)*(W*3+ (w mod 4) )+54);
     BlockWrite(f,InputArray^,(W*3+ (w mod 4) ));
    end;

   FreeMem(InputArray);
   CloseFile(F);
end;

end.

