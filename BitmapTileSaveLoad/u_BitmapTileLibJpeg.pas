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
    function Load(AData: IBinaryData): IBitmap32Static;
  end;

implementation

uses
  LibJpegRead,
  u_Bitmap32Static;

{ TLibJpegTileLoader }

constructor TLibJpegTileLoader.Create(
  APerfCounterList: IInternalPerformanceCounterList
);
begin
  FLoadStreamCounter := APerfCounterList.CreateAndAddNewCounter('LibJPEG/LoadStream');
end;

destructor TLibJpegTileLoader.Destroy;
begin
  inherited;
end;

function TLibJpegTileLoader.Load(AData: IBinaryData): IBitmap32Static;
var
  VCounterContext: TInternalPerformanceCounterContext;
  VJpeg: TJpegReader;
  VStream: TMemoryStream;
  VBtm: TCustomBitmap32;
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
          VBtm := TCustomBitmap32.Create;
          VBtm.Width := VJpeg.Width;
          VBtm.Height := VJpeg.Height;
          VJpeg.AppData := @VBtm;
          if not VJpeg.Decompress(Self.ReadLine) then begin
            VBtm.Clear;
            raise Exception.Create('Jpeg decompress error!');
          end;
          Result := TBitmap32Static.CreateWithOwn(VBtm);
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
type
  TColor32Rec = packed record
    B, G, R, A: Byte;
  end;
var
  VJpeg: TJpegReader;
  VBtm: TCustomBitmap32;
  VColor: TColor32Rec;
  I: Integer;
begin
  VJpeg := Sender as TJpegReader;
  VBtm := TCustomBitmap32(VJpeg.AppData^);
  for I := 0 to VBtm.Height - 1 do begin
    VColor.R := ALine^; Inc(ALine, 1);
    VColor.G := ALine^; Inc(ALine, 1);
    VColor.B := ALine^; Inc(ALine, 1);
    VColor.A := $FF;
    VBtm.Pixel[I, ALineNumber] := TColor32(VColor);
  end;
  Result := True;
end;

end.
