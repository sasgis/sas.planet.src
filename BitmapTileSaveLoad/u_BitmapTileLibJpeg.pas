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

unit u_BitmapTileLibJpeg;

interface

uses
  Classes,
  SysUtils,
  GR32,
  i_InternalPerformanceCounter,
  i_BinaryData,
  i_Bitmap32Static,
  i_BitmapTileSaveLoad;

type
  TLibJpegTileLoader = class(TInterfacedObject, IBitmapTileLoader)
  protected
    FLoadStreamCounter: IInternalPerformanceCounter;
    function ReadLine(Sender: TObject; ALine: PByte; ALineSize: Cardinal;
      ALineNumber: Integer): Boolean;
  public
    constructor Create(APerfCounterList: IInternalPerformanceCounterList);
    destructor Destroy; override;
    procedure LoadFromStream(AStream: TStream; ABtm: TCustomBitmap32);
    function Load(const AData: IBinaryData): IBitmap32Static;
  end;

  TLibJpegTileSaver = class(TInterfacedObject, IBitmapTileSaver)
  protected
    FCompressionQuality: Byte;
    function WriteLine(Sender: TObject; ALineNumber: Integer; out Abort: Boolean): PByte;
  public
    constructor Create(ACompressionQuality: Byte);
    destructor Destroy; override;
    procedure SaveToStream(ABtm: TCustomBitmap32; AStream: TStream);
    function Save(const ABitmap: IBitmap32Static): IBinaryData;
  end;

implementation

uses
  LibJpegRead,
  LibJpegWrite,
  u_Bitmap32Static,
  u_BinaryDataByMemStream;

type
  TColor32Rec = packed record
    B, G, R, A: Byte;
  end;

  TWriterAppData = record
    Bitmap: TCustomBitmap32;
    Line: PByte;
  end;

{ TLibJpegTileLoader }

constructor TLibJpegTileLoader.Create(APerfCounterList: IInternalPerformanceCounterList);
begin
  FLoadStreamCounter := APerfCounterList.CreateAndAddNewCounter('LibJPEG/LoadStream');
end;

destructor TLibJpegTileLoader.Destroy;
begin
  inherited;
end;

function TLibJpegTileLoader.Load(const AData: IBinaryData): IBitmap32Static;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VJpeg: TJpegReader;
  VStream: TMemoryStream;
  VBitmap: TCustomBitmap32;
begin
  VCounterContext := FLoadStreamCounter.StartOperation;
  try
    VStream := TMemoryStream.Create;
    try
      VStream.WriteBuffer(AData.Buffer^, AData.Size);
      VStream.Position := 0;
      VJpeg := TJpegReader.Create(VStream);
      try
        if VJpeg.ReadHeader() then begin
          VBitmap := TCustomBitmap32.Create;
          try
            VBitmap.Width := VJpeg.Width;
            VBitmap.Height := VJpeg.Height;
            VJpeg.AppData := @VBitmap;
            if not VJpeg.Decompress(Self.ReadLine) then begin
              raise Exception.Create('Jpeg decompress error!');
            end;
            Result := TBitmap32Static.CreateWithOwn(VBitmap);
            VBitmap := nil;
          finally
            VBitmap.Free;
          end;
        end else begin
          raise Exception.Create('Jpeg open error!');
        end;
      finally
        VJpeg.Free;
      end;
    finally
      VStream.Free;
    end;
  finally
    FLoadStreamCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TLibJpegTileLoader.LoadFromStream(AStream: TStream; ABtm: TCustomBitmap32);
var
  VCounterContext: TInternalPerformanceCounterContext;
  VJpeg: TJpegReader;
begin
  VCounterContext := FLoadStreamCounter.StartOperation;
  try
    VJpeg := TJpegReader.Create(AStream);
    try
      if VJpeg.ReadHeader() then begin
        ABtm.Width := VJpeg.Width;
        ABtm.Height := VJpeg.Height;
        VJpeg.AppData := @ABtm;
        if not VJpeg.Decompress(Self.ReadLine) then begin
          ABtm.Clear;
          raise Exception.Create('Jpeg decompress error!');
        end;
      end else begin
        raise Exception.Create('Jpeg open error!');
      end;
    finally
      VJpeg.Free;
    end;
  finally
    FLoadStreamCounter.FinishOperation(VCounterContext);
  end;
end;

function TLibJpegTileLoader.ReadLine(Sender: TObject; ALine: PByte;
  ALineSize: Cardinal; ALineNumber: Integer): Boolean;
var
  VJpeg: TJpegReader;
  VBtm: TCustomBitmap32;
  VColor: TColor32Rec;
  I: Integer;
begin
  VJpeg := Sender as TJpegReader;
  VBtm := TCustomBitmap32(VJpeg.AppData^);
  for I := 0 to VBtm.Width - 1 do begin
    VColor.R := ALine^; Inc(ALine, 1);
    VColor.G := ALine^; Inc(ALine, 1);
    VColor.B := ALine^; Inc(ALine, 1);
    VColor.A := $FF;
    VBtm.Pixel[I, ALineNumber] := TColor32(VColor);
  end;
  Result := True;
end;

{ TLibJpegTileSaver }

constructor TLibJpegTileSaver.Create(ACompressionQuality: Byte);
begin
  inherited Create;
  FCompressionQuality := ACompressionQuality;
end;

destructor TLibJpegTileSaver.Destroy;
begin
  inherited Destroy;
end;

function TLibJpegTileSaver.Save(const ABitmap: IBitmap32Static): IBinaryData;
var
  VJpeg: TJpegWriter;
  VAppData: TWriterAppData;
  VMemStream: TMemoryStream;
begin
  Result := nil;
  VMemStream := TMemoryStream.Create;
  try
    VJpeg := TJpegWriter.Create(VMemStream);
    try
      VAppData.Bitmap := ABitmap.Bitmap;
      GetMem(VAppData.Line, VAppData.Bitmap.Width * 3);
      try
        VJpeg.Width := VAppData.Bitmap.Width;
        VJpeg.Height := VAppData.Bitmap.Height;
        VJpeg.Quality := FCompressionQuality;
        VJpeg.AppData := @VAppData;
        if not VJpeg.Compress(Self.WriteLine) then begin
          raise Exception.Create('Jpeg compression error!');
        end;
        VMemStream.Position := 0;
      finally
        FreeMem(VAppData.Line);
      end;
    finally
      VJpeg.Free;
    end;
    Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
    VMemStream := nil;
  finally
    VMemStream.Free;
  end;
end;

procedure TLibJpegTileSaver.SaveToStream(ABtm: TCustomBitmap32; AStream: TStream);
var
  VJpeg: TJpegWriter;
  VAppData: TWriterAppData;
begin
  VJpeg := TJpegWriter.Create(AStream);
  try
    VAppData.Bitmap := ABtm;
    GetMem(VAppData.Line, VAppData.Bitmap.Width * 3);
    try
      VJpeg.Width := VAppData.Bitmap.Width;
      VJpeg.Height := VAppData.Bitmap.Height;
      VJpeg.Quality := FCompressionQuality;
      VJpeg.AppData := @VAppData;
      if not VJpeg.Compress(Self.WriteLine) then begin
        raise Exception.Create('Jpeg compression error!');
      end;
      AStream.Position := 0;
    finally
      FreeMem(VAppData.Line);
    end;
  finally
    VJpeg.Free;
  end;
end;

function TLibJpegTileSaver.WriteLine(Sender: TObject; ALineNumber: Integer;
  out Abort: Boolean): PByte;
var
  VJpeg: TJpegWriter;
  VAppData: TWriterAppData;
  VPixColor: TColor32Rec;
  VLine: PByte;
  I: Integer;
begin
  VJpeg := Sender as TJpegWriter;
  VAppData := TWriterAppData(VJpeg.AppData^);
  VLine := VAppData.Line;
  for I := 0 to VAppData.Bitmap.Width - 1 do begin
    VPixColor := TColor32Rec(VAppData.Bitmap.Pixel[I, ALineNumber]);
    VLine^ := VPixColor.R; Inc(VLine);
    VLine^ := VPixColor.G; Inc(VLine);
    VLine^ := VPixColor.B; Inc(VLine);
  end;
  Result := VAppData.Line;
  Abort := False;
end;

end.
