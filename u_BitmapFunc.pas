unit u_BitmapFunc;

interface

uses
  GR32,
  i_Bitmap32Static;

procedure AssignStaticToBitmap32(
  ADst: TCustomBitmap32;
  const ASource: IBitmap32Static
);

procedure StretchTransferFull(
  ADst: TCustomBitmap32;
  const ADstRect: TRect;
  const ASource: IBitmap32Static;
  AResampler: TCustomResampler;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
);

procedure StretchTransfer(
  ADst: TCustomBitmap32;
  const ADstRect: TRect;
  const ASource: IBitmap32Static;
  const ASrcRect: TRect;
  AResampler: TCustomResampler;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
);

procedure BlockTransferFull(
  ADst: TCustomBitmap32;
  ADstX: Integer;
  ADstY: Integer;
  const ASource: IBitmap32Static;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
);

procedure BlockTransfer(
  ADst: TCustomBitmap32;
  ADstX: Integer;
  ADstY: Integer;
  const ASource: IBitmap32Static;
  const ASrcRect: TRect;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
);

implementation

uses
  Types,
  GR32_LowLevel,
  GR32_Resamplers;

procedure AssignStaticToBitmap32(
  ADst: TCustomBitmap32;
  const ASource: IBitmap32Static
);
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  ADst.SetSize(VSize.X, VSize.Y);
  MoveLongword(ASource.Data[0], ADst.Bits[0], VSize.X * VSize.Y);
end;

procedure StretchTransferFull(
  ADst: TCustomBitmap32;
  const ADstRect: TRect;
  const ASource: IBitmap32Static;
  AResampler: TCustomResampler;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
);
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  GR32_Resamplers.StretchTransfer(
    ADst,
    ADstRect,
    ADst.ClipRect,
    ASource.Data,
    VSize.X,
    VSize.Y,
    Bounds(0, 0, VSize.X, VSize.Y),
    AResampler,
    ACombineOp,
    ACombineMode,
    AMasterAlpha,
    AOuterColor
  );
end;

procedure StretchTransfer(
  ADst: TCustomBitmap32;
  const ADstRect: TRect;
  const ASource: IBitmap32Static;
  const ASrcRect: TRect;
  AResampler: TCustomResampler;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode;
  AMasterAlpha: Cardinal;
  AOuterColor: TColor32
);
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  GR32_Resamplers.StretchTransfer(
    ADst,
    ADstRect,
    ADst.ClipRect,
    ASource.Data,
    VSize.X,
    VSize.Y,
    ASrcRect,
    AResampler,
    ACombineOp,
    ACombineMode,
    AMasterAlpha,
    AOuterColor
  );
end;

procedure BlockTransferFull(
  ADst: TCustomBitmap32;
  ADstX: Integer;
  ADstY: Integer;
  const ASource: IBitmap32Static;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode;
  AMasterAlpha: Cardinal;
  AOuterColor: TColor32
);
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  GR32_Resamplers.BlockTransfer(
    ADst,
    ADstX,
    ADstY,
    ADst.ClipRect,
    ASource.Data,
    VSize.X,
    VSize.Y,
    Bounds(0, 0, VSize.X, VSize.Y),
    ACombineOp,
    ACombineMode,
    AMasterAlpha,
    AOuterColor
  );
end;

procedure BlockTransfer(
  ADst: TCustomBitmap32;
  ADstX: Integer;
  ADstY: Integer;
  const ASource: IBitmap32Static;
  const ASrcRect: TRect;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode;
  AMasterAlpha: Cardinal;
  AOuterColor: TColor32
);
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  GR32_Resamplers.BlockTransfer(
    ADst,
    ADstX,
    ADstY,
    ADst.ClipRect,
    ASource.Data,
    VSize.X,
    VSize.Y,
    ASrcRect,
    ACombineOp,
    ACombineMode,
    AMasterAlpha,
    AOuterColor
  );
end;

end.
