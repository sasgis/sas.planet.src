{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ARGBToPaletteConverter;

interface

uses
  SysUtils,
  ImagingTypes,
  i_ARGBToPaletteConverter;

const
  MaxPossibleColors = 4096;
  HashSize = 32768;

type
  PColorBin = ^TColorBin;

  TColorBin = record
    Color: TColor32Rec;
    Number: LongInt;
    Next: PColorBin;
  end;

  THashTable = array[0..HashSize - 1] of PColorBin;

  TColorBox = record
    AMin, AMax,
    RMin, RMax,
    GMin, GMax,
    BMin, BMax: LongInt;
    Total: LongInt;
    Represented: TColor32Rec;
    List: PColorBin;
  end;

type
  TARGBToPaletteConverter = class(TInterfacedObject, IARGBToPaletteConverter)
  private
    FCS: IReadWriteSync;
    Table: THashTable;
    Box: array[0..MaxPossibleColors - 1] of TColorBox;
    Boxes: LongInt;
    procedure CreateHistogram(
      NumPixels: Integer;
      Src: PByte;
      SrcInfo: PImageFormatInfo;
      ChannelMask: Byte
    );
    procedure InitBox(var Box: TColorBox);
    procedure ChangeBox(
      var Box: TColorBox;
      const C: TColorBin
    );
    procedure MakeColormap(
      MaxColors: LongInt;
      ChannelMask: Byte
    );
    procedure FillOutputPalette(
      MaxColors: Integer;
      DstPal: PPalette32
    );
    function MapColor(const Col: TColor32Rec): LongInt;
    procedure MapImage(
      NumPixels: Integer;
      Src, Dst: PByte;
      SrcInfo, DstInfo: PImageFormatInfo
    );
    procedure ReduceColorsMedianCut(
      NumPixels: LongInt;
      Src, Dst: PByte;
      SrcInfo, DstInfo: PImageFormatInfo;
      MaxColors: LongInt;
      ChannelMask: Byte;
      DstPal: PPalette32
    );
  protected
    procedure Convert(var AImage: TImageData);
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  Imaging,
  ImagingFormats,
  ImagingUtility;

const
  AlphaWeight = 1024;
  RedWeight = 612;
  GreenWeight = 1202;
  BlueWeight = 234;

{ TARGBToPaletteConverter }

procedure TARGBToPaletteConverter.Convert(var AImage: TImageData);
var
  NewData: Pointer;
  NewPal: PPalette32;
  NewSize, NumPixels: LongInt;
  SrcInfo: TImageFormatInfo;
  DstInfo: TImageFormatInfo;
  DestFormat: TImageFormat;
begin
  DestFormat := ifIndex8;
  GetImageFormatInfo(DestFormat, DstInfo);
  GetImageFormatInfo(AImage.Format, SrcInfo);
  NumPixels := AImage.Width * AImage.Height;
  NewSize := NumPixels * DstInfo.BytesPerPixel;
  GetMem(NewData, NewSize);
  FillChar(NewData^, NewSize, 0);
  GetMem(NewPal, DstInfo.PaletteEntries * SizeOf(TColor32Rec));
  FillChar(NewPal^, DstInfo.PaletteEntries * SizeOf(TColor32Rec), 0);

  FCS.BeginWrite;
  try
    ReduceColorsMedianCut(NumPixels, AImage.Bits, NewData, @SrcInfo, @DstInfo, DstInfo.PaletteEntries,
      GetOption(ImagingColorReductionMask), NewPal);
  finally
    FCS.EndWrite;
  end;

  FreeMemNil(AImage.Bits);
  FreeMemNil(AImage.Palette);
  AImage.Format := DestFormat;
  AImage.Bits := NewData;
  AImage.Size := NewSize;
  AImage.Palette := NewPal;
end;

constructor TARGBToPaletteConverter.Create;
begin
  inherited;
  FCS := MakeSyncObj(Self, TRUE);
end;

destructor TARGBToPaletteConverter.Destroy;
begin
  FCS := nil;
  inherited;
end;

procedure TARGBToPaletteConverter.InitBox(var Box: TColorBox);
begin
  Box.AMin := 256;
  Box.RMin := 256;
  Box.GMin := 256;
  Box.BMin := 256;
  Box.AMax := -1;
  Box.RMax := -1;
  Box.GMax := -1;
  Box.BMax := -1;
  Box.Total := 0;
  Box.List := nil;
end;

procedure TARGBToPaletteConverter.ChangeBox(
  var Box: TColorBox;
  const C: TColorBin
);
begin
  with C.Color do begin
    if A < Box.AMin then begin
      Box.AMin := A;
    end;
    if A > Box.AMax then begin
      Box.AMax := A;
    end;
    if B < Box.BMin then begin
      Box.BMin := B;
    end;
    if B > Box.BMax then begin
      Box.BMax := B;
    end;
    if G < Box.GMin then begin
      Box.GMin := G;
    end;
    if G > Box.GMax then begin
      Box.GMax := G;
    end;
    if R < Box.RMin then begin
      Box.RMin := R;
    end;
    if R > Box.RMax then begin
      Box.RMax := R;
    end;
  end;
  Inc(Box.Total, C.Number);
end;

procedure TARGBToPaletteConverter.CreateHistogram(
  NumPixels: Integer;
  Src: PByte;
  SrcInfo: PImageFormatInfo;
  ChannelMask: Byte
);
var
  A, R, G, B: Byte;
  I, VAddr: LongInt;
  PC: PColorBin;
  Col: TColor32Rec;
begin
  for I := 0 to NumPixels - 1 do begin
    Col := GetPixel32Generic(Src, SrcInfo, nil);
    A := Col.A and ChannelMask;
    R := Col.R and ChannelMask;
    G := Col.G and ChannelMask;
    B := Col.B and ChannelMask;

    VAddr := (A + 11 * B + 59 * R + 119 * G) mod HashSize;
    PC := Table[VAddr];

    while (PC <> nil) and ((PC.Color.R <> R) or (PC.Color.G <> G) or
        (PC.Color.B <> B) or (PC.Color.A <> A)) do begin
      PC := PC.Next;
    end;

    if PC = nil then begin
      New(PC);
      PC.Color.R := R;
      PC.Color.G := G;
      PC.Color.B := B;
      PC.Color.A := A;
      PC.Number := 1;
      PC.Next := Table[VAddr];
      Table[VAddr] := PC;
    end else begin
      Inc(PC^.Number);
    end;
    Inc(Src, SrcInfo.BytesPerPixel);
  end;
end;

procedure TARGBToPaletteConverter.MakeColormap(
  MaxColors: LongInt;
  ChannelMask: Byte
);
var
  I, J: LongInt;
  CP, Pom: PColorBin;
  Cut, LargestIdx, Largest, Size, S: LongInt;
  CutA, CutR, CutG, CutB: Boolean;
  SumA, SumR, SumG, SumB: LongInt;
  Temp: TColorBox;
begin
  I := 0;
  Boxes := 1;
  LargestIdx := 0;
  while (I < HashSize) and (Table[I] = nil) do begin
    Inc(i);
  end;
  if I < HashSize then begin
    // put all colors into Box[0]
    InitBox(Box[0]);
    repeat
      CP := Table[I];
      while CP.Next <> nil do begin
        ChangeBox(Box[0], CP^);
        CP := CP.Next;
      end;
      ChangeBox(Box[0], CP^);
      CP.Next := Box[0].List;
      Box[0].List := Table[I];
      Table[I] := nil;
      repeat
        Inc(I)
      until (I = HashSize) or (Table[I] <> nil);
    until I = HashSize;
    // now all colors are in Box[0]
    repeat
      // cut one color box
      Largest := 0;
      for I := 0 to Boxes - 1 do begin
        with Box[I] do begin
          Size := (AMax - AMin) * AlphaWeight;
          S := (RMax - RMin) * RedWeight;
          if S > Size then begin
            Size := S;
          end;
          S := (GMax - GMin) * GreenWeight;
          if S > Size then begin
            Size := S;
          end;
          S := (BMax - BMin) * BlueWeight;
          if S > Size then begin
            Size := S;
          end;
          if Size > Largest then begin
            Largest := Size;
            LargestIdx := I;
          end;
        end;
      end;
      if Largest > 0 then begin
        // cutting Box[LargestIdx] into Box[LargestIdx] and Box[Boxes]
        CutR := False;
        CutG := False;
        CutB := False;
        CutA := False;
        with Box[LargestIdx] do begin
          if (AMax - AMin) * AlphaWeight = Largest then begin
            Cut := (AMax + AMin) shr 1;
            CutA := True;
          end else if (RMax - RMin) * RedWeight = Largest then begin
            Cut := (RMax + RMin) shr 1;
            CutR := True;
          end else if (GMax - GMin) * GreenWeight = Largest then begin
            Cut := (GMax + GMin) shr 1;
            CutG := True;
          end else begin
            Cut := (BMax + BMin) shr 1;
            CutB := True;
          end;
          CP := List;
        end;
        InitBox(Box[LargestIdx]);
        InitBox(Box[Boxes]);
        repeat
          // distribute one color
          Pom := CP.Next;
          with CP.Color do begin
            if (CutA and (A <= Cut)) or (CutR and (R <= Cut)) or
              (CutG and (G <= Cut)) or (CutB and (B <= Cut)) then begin
              I := LargestIdx;
            end else begin
              I := Boxes;
            end;
          end;
          CP.Next := Box[i].List;
          Box[i].List := CP;
          ChangeBox(Box[i], CP^);
          CP := Pom;
        until CP = nil;
        Inc(Boxes);
      end;
    until (Boxes = MaxColors) or (Largest = 0);
    // compute box representation
    for I := 0 to Boxes - 1 do begin
      SumR := 0;
      SumG := 0;
      SumB := 0;
      SumA := 0;
      repeat
        CP := Box[I].List;
        Inc(SumR, CP.Color.R * CP.Number);
        Inc(SumG, CP.Color.G * CP.Number);
        Inc(SumB, CP.Color.B * CP.Number);
        Inc(SumA, CP.Color.A * CP.Number);
        Box[I].List := CP.Next;
        Dispose(CP);
      until Box[I].List = nil;
      with Box[I] do begin
        Represented.A := SumA div Total;
        Represented.R := SumR div Total;
        Represented.G := SumG div Total;
        Represented.B := SumB div Total;
        AMin := AMin and ChannelMask;
        RMin := RMin and ChannelMask;
        GMin := GMin and ChannelMask;
        BMin := BMin and ChannelMask;
        AMax := (AMax and ChannelMask) + (not ChannelMask);
        RMax := (RMax and ChannelMask) + (not ChannelMask);
        GMax := (GMax and ChannelMask) + (not ChannelMask);
        BMax := (BMax and ChannelMask) + (not ChannelMask);
      end;
    end;
    // sort color boxes
    for I := 0 to Boxes - 2 do begin
      Largest := 0;
      for J := I to Boxes - 1 do begin
        if Box[J].Total > Largest then begin
          Largest := Box[J].Total;
          LargestIdx := J;
        end;
      end;
      if LargestIdx <> I then begin
        Temp := Box[I];
        Box[I] := Box[LargestIdx];
        Box[LargestIdx] := Temp;
      end;
    end;
  end;
end;

procedure TARGBToPaletteConverter.FillOutputPalette(
  MaxColors: Integer;
  DstPal: PPalette32
);
var
  I: LongInt;
begin
  FillChar(DstPal^, SizeOf(TColor32Rec) * MaxColors, $FF);
  for I := 0 to MaxColors - 1 do begin
    if I < Boxes then begin
      with Box[I].Represented do begin
        DstPal[I].A := A;
        DstPal[I].R := R;
        DstPal[I].G := G;
        DstPal[I].B := B;
      end;
    end else begin
      DstPal[I].Color := $FF000000;
    end;
  end;
end;

function TARGBToPaletteConverter.MapColor(const Col: TColor32Rec): LongInt;
var
  I: LongInt;
begin
  I := 0;
  with Col do begin
    while (I < Boxes) and ((Box[I].AMin > A) or (Box[I].AMax < A) or
        (Box[I].RMin > R) or (Box[I].RMax < R) or (Box[I].GMin > G) or
        (Box[I].GMax < G) or (Box[I].BMin > B) or (Box[I].BMax < B)) do begin
      Inc(I);
    end;
  end;
  if I = Boxes then begin
    MapColor := 0;
  end else begin
    MapColor := I;
  end;
end;

procedure TARGBToPaletteConverter.MapImage(
  NumPixels: Integer;
  Src, Dst: PByte;
  SrcInfo, DstInfo: PImageFormatInfo
);
var
  I: LongInt;
  Col: TColor32Rec;
begin
  for I := 0 to NumPixels - 1 do begin
    Col := GetPixel32Generic(Src, SrcInfo, nil);
    IndexSetDstPixel(Dst, DstInfo, MapColor(Col));
    Inc(Src, SrcInfo.BytesPerPixel);
    Inc(Dst, DstInfo.BytesPerPixel);
  end;
end;

procedure TARGBToPaletteConverter.ReduceColorsMedianCut(
  NumPixels: Integer;
  Src,
  Dst: PByte;
  SrcInfo, DstInfo: PImageFormatInfo;
  MaxColors: Integer;
  ChannelMask: Byte;
  DstPal: PPalette32
);
begin
  MaxColors := ClampInt(MaxColors, 2, MaxPossibleColors);

  FillChar(Table, SizeOf(Table), 0);

  CreateHistogram(NumPixels, Src, SrcInfo, ChannelMask);

  MakeColorMap(MaxColors, ChannelMask);
  FillOutputPalette(MaxColors, DstPal);

  MapImage(NumPixels, Src, Dst, SrcInfo, DstInfo);
end;

end.
