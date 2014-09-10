{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_BitmapTileLibJpeg;

interface

uses
  Types,
  Classes,
  SysUtils,
  t_Bitmap32,
  i_InternalPerformanceCounter,
  i_BinaryData,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoad,
  u_BaseInterfacedObject;

type
  TLibJpegTileLoader = class(TBaseInterfacedObject, IBitmapTileLoader)
  private
    FLoadStreamCounter: IInternalPerformanceCounter;
    FBitmap32StaticFactory: IBitmap32StaticFactory;

    function ReadLine(
      Sender: TObject;
      ALine: PByte;
      ALineSize: Cardinal;
      ALineNumber: Integer;
      ABGRAColorSpace: Boolean
    ): Boolean;
  private
    function Load(const AData: IBinaryData): IBitmap32Static;
  public
    constructor Create(
      const APerfCounterList: IInternalPerformanceCounterList;
      const ABitmap32StaticFactory: IBitmap32StaticFactory
    );
  end;

  TLibJpegTileSaver = class(TBaseInterfacedObject, IBitmapTileSaver)
  private
    FSaveCounter: IInternalPerformanceCounter;
    FCompressionQuality: Byte;
    function WriteLine(
      Sender: TObject;
      ALineNumber: Integer;
      ALineSize: Cardinal;
      out Abort: Boolean
    ): PByte;
  private
    function Save(const ABitmap: IBitmap32Static): IBinaryData;
  public
    constructor Create(
      ACompressionQuality: Byte;
      const APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  LibJpegRead,
  LibJpegWrite,
  u_StreamReadOnlyByBinaryData,
  u_BinaryDataByMemStream;

type
  TColor32Rec = packed record
    B, G, R, A: Byte;
  end;

  TWriterAppData = record
    Size: TPoint;
    Data: PColor32Array;
    Line: PByte;
    LineSize: Cardinal;
    BGRAColorSpace: Boolean;
  end;

const
  cUseLibJpeg8 = False;
  cUseBGRAColorSpace = True; // Available for libjpeg-turbo only

{ TLibJpegTileLoader }

constructor TLibJpegTileLoader.Create(
  const APerfCounterList: IInternalPerformanceCounterList;
  const ABitmap32StaticFactory: IBitmap32StaticFactory
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FLoadStreamCounter := APerfCounterList.CreateAndAddNewCounter('LibJPEG/LoadStream');
  FBitmap32StaticFactory := ABitmap32StaticFactory;
end;

function TLibJpegTileLoader.Load(const AData: IBinaryData): IBitmap32Static;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VJpeg: TJpegReader;
  VStream: TStream;
  VBuffer: IBitmap32Buffer;
begin
  Result := nil;
  VCounterContext := FLoadStreamCounter.StartOperation;
  try
    VStream := TStreamReadOnlyByBinaryData.Create(AData);
    try
      VStream.Position := 0;
      VJpeg := TJpegReader.Create(VStream, cUseBGRAColorSpace, cUseLibJpeg8);
      try
        if VJpeg.ReadHeader() then begin
          VBuffer :=
            FBitmap32StaticFactory.BufferFactory.BuildEmpty(
              Point(VJpeg.Width, VJpeg.Height)
            );
          Assert(Assigned(VBuffer));
          if VBuffer <> nil then begin
            VJpeg.AppData := VBuffer.Data;
            if not VJpeg.Decompress(Self.ReadLine) then begin
              raise Exception.Create('Jpeg decompress error!');
            end;
            Result := FBitmap32StaticFactory.BuildWithOwnBuffer(VBuffer);
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

function TLibJpegTileLoader.ReadLine(
  Sender: TObject;
  ALine: PByte;
  ALineSize: Cardinal;
  ALineNumber: Integer;
  ABGRAColorSpace: Boolean
): Boolean;
var
  VJpeg: TJpegReader;
  VData: PColor32Array;
  VColor: TColor32Rec;
  I: Integer;
begin
  VJpeg := Sender as TJpegReader;
  VData := PColor32Array(VJpeg.AppData);
  if ABGRAColorSpace then begin
    Move(
      ALine^,
      VData[ALineNumber * VJpeg.Width],
      ALineSize
    );
  end else begin
    for I := 0 to VJpeg.Width - 1 do begin
      VColor.R := ALine^;
      Inc(ALine, 1);
      VColor.G := ALine^;
      Inc(ALine, 1);
      VColor.B := ALine^;
      Inc(ALine, 1);
      VColor.A := $FF;
      VData[ALineNumber * VJpeg.Width + I] := TColor32(VColor);
    end;
  end;
  Result := True;
end;

{ TLibJpegTileSaver }

constructor TLibJpegTileSaver.Create(
  ACompressionQuality: Byte;
  const APerfCounterList: IInternalPerformanceCounterList
);
begin
  inherited Create;
  FSaveCounter := APerfCounterList.CreateAndAddNewCounter('LibJPEG/SaveStream');
  FCompressionQuality := ACompressionQuality;
end;

function TLibJpegTileSaver.Save(const ABitmap: IBitmap32Static): IBinaryData;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VJpeg: TJpegWriter;
  VAppData: TWriterAppData;
  VMemStream: TMemoryStream;
begin
  Result := nil;
  VCounterContext := FSaveCounter.StartOperation;
  try
    VMemStream := TMemoryStream.Create;
    try
      VAppData.Size := ABitmap.Size;
      VAppData.Data := ABitmap.Data;
      VAppData.BGRAColorSpace := cUseBGRAColorSpace;
      VJpeg := TJpegWriter.Create(VMemStream, VAppData.BGRAColorSpace, cUseLibJpeg8);
      try
        if VAppData.BGRAColorSpace then begin
          VAppData.LineSize := VAppData.Size.X * 4;
        end else begin
          VAppData.LineSize := VAppData.Size.X * 3;
        end;
        GetMem(VAppData.Line, VAppData.LineSize);
        try
          VJpeg.Width := VAppData.Size.X;
          VJpeg.Height := VAppData.Size.Y;
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
  finally
    FSaveCounter.FinishOperation(VCounterContext);
  end;
end;

function TLibJpegTileSaver.WriteLine(
  Sender: TObject;
  ALineNumber: Integer;
  ALineSize: Cardinal;
  out Abort: Boolean
): PByte;
var
  VJpeg: TJpegWriter;
  VAppData: TWriterAppData;
  VPixColor: TColor32Rec;
  VLine: PByte;
  I: Integer;
begin
  VJpeg := Sender as TJpegWriter;
  VAppData := TWriterAppData(VJpeg.AppData^);
  Assert(ALineSize = VAppData.LineSize);
  VLine := VAppData.Line;
  if VAppData.BGRAColorSpace then begin
    Move(
      VAppData.Data[ALineNumber * VAppData.Size.X],
      VLine^,
      VAppData.LineSize
    );
  end else begin
    for I := 0 to VAppData.Size.X - 1 do begin
      VPixColor := TColor32Rec(VAppData.Data[I + ALineNumber * VAppData.Size.X]);
      VLine^ := VPixColor.R;
      Inc(VLine);
      VLine^ := VPixColor.G;
      Inc(VLine);
      VLine^ := VPixColor.B;
      Inc(VLine);
    end;
  end;
  Result := VAppData.Line;
  Abort := False;
end;

end.
