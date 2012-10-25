unit u_BitmapFunc;

interface

uses
  GR32,
  i_Bitmap32Static;

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
  GR32_Resamplers;

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
begin
  GR32_Resamplers.StretchTransfer(
    ADst,
    ADstRect,
    ADst.ClipRect,
    ASource.Bitmap,
    ASrcRect,
    AResampler,
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
begin
  GR32_Resamplers.BlockTransfer(
    ADst,
    ADstX,
    ADstY,
    ADst.ClipRect,
    ASource.Bitmap,
    ASrcRect,
    ACombineOp,
    ACombineMode,
    AMasterAlpha,
    AOuterColor
  );
end;


end.
