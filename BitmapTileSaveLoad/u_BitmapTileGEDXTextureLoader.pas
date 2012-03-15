unit u_BitmapTileGEDXTextureLoader;

interface

(*
uses
  Classes,
  GR32,
  i_BinaryData,
  i_Bitmap32Static,
  i_InternalPerformanceCounter,
  i_BitmapTileSaveLoad;
*)

(*
type
  TBitmapTileGEDXTextureLoader = class(TInterfacedObject, IBitmapTileLoader)
  private
    FLoadStreamCounter: IInternalPerformanceCounter;
    procedure LoadFromMem(ABuffer: Pointer; ASize: Integer; ABtm: TCustomBitmap32);
  protected
    procedure LoadFromStream(AStream: TStream; ABtm: TCustomBitmap32);
    function Load(AData: IBinaryData): IBitmap32Static;
  public
    constructor Create(
      APerfCounterList: IInternalPerformanceCounterList
    );
  end;
*)

implementation

(*
uses
  SysUtils,
  u_Bitmap32Static;

const DXT_FILE_SIZE = 32792;

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

  PGETexture = ^TGETexture;
  TGETexture = packed record
    GE_HEAD : TGE_HEAD;
    DXT1    : array [0..63, 0..63] of TDXT1;
  end;
*)

{ TBitmapTileGEDXTextureLoader }

(*
constructor TBitmapTileGEDXTextureLoader.Create(
  APerfCounterList: IInternalPerformanceCounterList);
var
  VPerfCounterList: IInternalPerformanceCounterList;
begin
  VPerfCounterList := APerfCounterList.CreateAndAddNewSubList('GEDXTexture');
  FLoadStreamCounter := VPerfCounterList.CreateAndAddNewCounter('LoadStream');
end;

procedure TBitmapTileGEDXTextureLoader.LoadFromStream(AStream: TStream;
  ABtm: TCustomBitmap32);
var
  VMemStream: TMemoryStream;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FLoadStreamCounter.StartOperation;
  try
    if AStream is TMemoryStream then begin
      VMemStream := TMemoryStream(AStream);
      LoadFromMem(VMemStream.Memory, VMemStream.Size, ABtm);
    end else begin
      VMemStream := TMemoryStream.Create;
      try
        VMemStream.LoadFromStream(AStream);
        LoadFromMem(VMemStream.Memory, VMemStream.Size, ABtm);
      finally
        VMemStream.Free;
      end;
    end;
  finally
    FLoadStreamCounter.FinishOperation(VCounterContext);
  end;
end;
*)

(*
type
  TSampleColors = array [0..3] of TColor32Entry;
*)

(*
//Определение опорных цветов
procedure GetColors (Color0,Color1:word; var AMainColors: TSampleColors);

  function RGB565_To_RGB888 (Input:Word):TColor32Entry;
  const b5 : array [0..31] of byte =(
   $00, $08, $10, $19, $21, $29, $31, $3A, $42, $4A, $52, $5A, $63, $6B, $73, $7B,
   132, 140, 148, 156, 165, 173, 181, 189, 197, 206, 214, 222, 230, 239, 247, 255);
  const b6 : array [0..63] of byte =(
   0, 4, 8, 12, 16, 20, 24,  28, 32, 36, 40, 45, 49, 53, 57, 61, 65, 69, 73, 77,
   81, 85, 89, 93, 97, 101,  105, 109, 113, 117, 121, 125, 130, 134, 138, 142,
   146, 150, 154, 158, 162, 166,  170, 174, 178, 182, 186, 190, 194, 198, 202,
   206, 210, 215, 219, 223, 227, 231,  235, 239, 243, 247, 251, 255);
  begin
   result.R := b5[(Input shr 11) and $1F];
   result.G := b6[(Input shr 5) and $3F];
   result.B := b5[Input and $1F];
   Result.A := $FF;
  end;

begin
   AMainColors[0]:= RGB565_To_RGB888(Color0);
   AMainColors[1]:= RGB565_To_RGB888(Color1);
   if Color0 > Color1 then begin
     AMainColors[2].R:= (2*AMainColors[0].R+AMainColors[1].R) div 3;
     AMainColors[2].G:= (2*AMainColors[0].G+AMainColors[1].G) div 3;
     AMainColors[2].B:= (2*AMainColors[0].B+AMainColors[1].B) div 3;
     AMainColors[2].A := $FF;
     AMainColors[3].R:= (AMainColors[0].R+2*AMainColors[1].R) div 3;
     AMainColors[3].G:= (AMainColors[0].G+2*AMainColors[1].G) div 3;
     AMainColors[3].B:= (AMainColors[0].B+2*AMainColors[1].B) div 3;
     AMainColors[3].A := $FF;
   end else begin
     AMainColors[2].R:= (AMainColors[0].R+AMainColors[1].R) div 2;
     AMainColors[2].G:= (AMainColors[0].G+AMainColors[1].G) div 2;
     AMainColors[2].B:= (AMainColors[0].B+AMainColors[1].B) div 2;
     AMainColors[2].A := $FF;
     AMainColors[3].R := 0;
     AMainColors[3].G := 0;
     AMainColors[3].B := 0;
     AMainColors[3].A := $FF;
   end;
end;

function GetColorIndex (Mask: LongWord; Pixel: integer): integer;
begin
  Result := (Mask shr (2*Pixel)) and $03;
end;

function TBitmapTileGEDXTextureLoader.Load(AData: IBinaryData): IBitmap32Static;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VBtm: TCustomBitmap32;
begin
  VCounterContext := FLoadStreamCounter.StartOperation;
  try
    VBtm := TCustomBitmap32.Create;
    try
      LoadFromMem(AData.Buffer, AData.Size, VBtm);
    except
      FreeAndNil(VBtm);
      raise;
    end;
    Result := TBitmap32Static.CreateWithOwn(VBtm);
  finally
    FLoadStreamCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TBitmapTileGEDXTextureLoader.LoadFromMem(
  ABuffer: Pointer;
  ASize: Integer;
  ABtm: TCustomBitmap32);
var
  VSize: TPoint;
  i, j: Integer;
  k, n: Integer;
  VDXT1: TDXT1;
  VColors: TSampleColors;
  pix : byte;
  VPosition: Integer;
begin
  if ASize <> DXT_FILE_SIZE then begin
    raise Exception.Create('Ошибочный размер DXT тайла.');
  end;
  VSize.X := PGETexture(ABuffer).GE_HEAD.Xrez div 4;
  VSize.Y := PGETexture(ABuffer).GE_HEAD.Yrez div 4;
  ABtm.SetSize(VSize.X * 4, VSize.Y * 4);
  for i := 0 to VSize.Y - 1 do begin
    for j := 0 to VSize.X - 1 do begin
      VDXT1 := PGETexture(ABuffer).DXT1[i, j];
      GetColors(VDXT1.Color0, VDXT1.Color1, VColors);
      pix := 0;
      for k := 0 to 3 do begin
        for n := 0 to 3 do begin
          VPosition := (255 - (i * 4 + k)) * VSize.X * 4 + 4 * j + n;
          ABtm.Bits[VPosition] := VColors[GetColorIndex(VDXT1.BitMask, pix)].ARGB;
          inc(pix);
        end;
      end;
    end;
  end;
end;

*)

end.
