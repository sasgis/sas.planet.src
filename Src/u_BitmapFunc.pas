{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_BitmapFunc;

interface

{.$DEFINE USE_GR32_UPSTREAM}

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
); overload;

procedure StretchTransferFull(
  ADst: TCustomBitmap32;
  const ADstRect: TRect;
  const ASourceSize: TPoint;
  const ASourceData: PColor32Array;
  AResampler: TCustomResampler;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
); overload;

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
); inline; overload;

procedure BlockTransferFull(
  ADst: TCustomBitmap32;
  ADstX: Integer;
  ADstY: Integer;
  const ASourceSize: TPoint;
  const ASourceData: PColor32Array;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode = cmMerge;
  AMasterAlpha: Cardinal = 255;
  AOuterColor: TColor32 = 0
); overload;

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

procedure CopyBitmap32StaticToBitmap32(
  const ASourceBitmap: IBitmap32Static;
  ATarget: TCustomBitmap32
);

implementation

uses
  Types,
  Math,
  {$IFDEF USE_GR32_UPSTREAM}
  Classes,
  SysUtils,
  u_Synchronizer,
  {$ENDIF}
  GR32_LowLevel,
  GR32_Resamplers;

{$IFDEF USE_GR32_UPSTREAM}
type
  TBitmap32Pool = class
  private
    FLock: IReadWriteSync;
    FCount: Integer;
    FCapacity: Integer;
    FItems: array [0..255] of TCustomBitmap32;
    FMainThreadItem: TCustomBitmap32;
    class function IsMainThread: Boolean; static; inline;
    procedure Clear;
  public
    function Pop(
      const ASourceData: PColor32Array;
      const ASourceSize: TPoint;
      const ACombineMode: TCombineMode;
      const AMasterAlpha: Cardinal;
      const AOuterColor: TColor32
    ): TCustomBitmap32;

    procedure Push(
      const ABitmap: TCustomBitmap32
    );
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  GBitmap32Pool: TBitmap32Pool;
{$ENDIF}

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
  ACombineMode: TCombineMode;
  AMasterAlpha: Cardinal;
  AOuterColor: TColor32
);
{$IFDEF USE_GR32_UPSTREAM}
var
  VSize: TPoint;
  VSrc: TCustomBitmap32;
begin
  VSize := ASource.Size;
  VSrc := GBitmap32Pool.Pop(ASource.Data, VSize, ACombineMode, AMasterAlpha, AOuterColor);
  try
    GR32_Resamplers.StretchTransfer(
      ADst,
      ADstRect,
      ADst.ClipRect,
      VSrc,
      Bounds(0, 0, VSize.X, VSize.Y),
      AResampler,
      ACombineOp
    );
  finally
    GBitmap32Pool.Push(VSrc);
  end;
end;
{$ELSE}
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  GR32_Resamplers.StretchTransferZ(
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
{$ENDIF}

procedure StretchTransferFull(
  ADst: TCustomBitmap32;
  const ADstRect: TRect;
  const ASourceSize: TPoint;
  const ASourceData: PColor32Array;
  AResampler: TCustomResampler;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode;
  AMasterAlpha: Cardinal;
  AOuterColor: TColor32
);
{$IFDEF USE_GR32_UPSTREAM}
var
  VSrc: TCustomBitmap32;
begin
  VSrc := GBitmap32Pool.Pop(ASourceData, ASourceSize, ACombineMode, AMasterAlpha, AOuterColor);
  try
    GR32_Resamplers.StretchTransfer(
      ADst,
      ADstRect,
      ADst.ClipRect,
      VSrc,
      Bounds(0, 0, ASourceSize.X, ASourceSize.Y),
      AResampler,
      ACombineOp
    );
  finally
    GBitmap32Pool.Push(VSrc);
  end;
end;
{$ELSE}
begin
  GR32_Resamplers.StretchTransferZ(
    ADst,
    ADstRect,
    ADst.ClipRect,
    ASourceData,
    ASourceSize.X,
    ASourceSize.Y,
    Bounds(0, 0, ASourceSize.X, ASourceSize.Y),
    AResampler,
    ACombineOp,
    ACombineMode,
    AMasterAlpha,
    AOuterColor
  );
end;
{$ENDIF}

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
{$IFDEF USE_GR32_UPSTREAM}
var
  VSrc: TCustomBitmap32;
begin
  VSrc := GBitmap32Pool.Pop(ASource.Data, ASource.Size, ACombineMode, AMasterAlpha, AOuterColor);
  try
    GR32_Resamplers.StretchTransfer(
      ADst,
      ADstRect,
      ADst.ClipRect,
      VSrc,
      ASrcRect,
      AResampler,
      ACombineOp
    );
  finally
    GBitmap32Pool.Push(VSrc);
  end;
end;
{$ELSE}
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  GR32_Resamplers.StretchTransferZ(
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
{$ENDIF}

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
begin
  BlockTransferFull(
    ADst,
    ADstX,
    ADstY,
    ASource.Size,
    ASource.Data,
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
  const ASourceSize: TPoint;
  const ASourceData: PColor32Array;
  ACombineOp: TDrawMode;
  ACombineMode: TCombineMode;
  AMasterAlpha: Cardinal;
  AOuterColor: TColor32
); overload;
{$IFDEF USE_GR32_UPSTREAM}
var
  VSrc: TCustomBitmap32;
begin
  VSrc := GBitmap32Pool.Pop(ASourceData, ASourceSize, ACombineMode, AMasterAlpha, AOuterColor);
  try
    GR32_Resamplers.BlockTransfer(
      ADst,
      ADstX,
      ADstY,
      ADst.ClipRect,
      VSrc,
      Bounds(0, 0, ASourceSize.X, ASourceSize.Y),
      ACombineOp
    );
  finally
    GBitmap32Pool.Push(VSrc);
  end;
end;
{$ELSE}
begin
  GR32_Resamplers.BlockTransferZ(
    ADst,
    ADstX,
    ADstY,
    ADst.ClipRect,
    ASourceData,
    ASourceSize.X,
    ASourceSize.Y,
    Bounds(0, 0, ASourceSize.X, ASourceSize.Y),
    ACombineOp,
    ACombineMode,
    AMasterAlpha,
    AOuterColor
  );
end;
{$ENDIF}

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
{$IFDEF USE_GR32_UPSTREAM}
var
  VSrc: TCustomBitmap32;
begin
  VSrc := GBitmap32Pool.Pop(ASource.Data, ASource.Size, ACombineMode, AMasterAlpha, AOuterColor);
  try
    GR32_Resamplers.BlockTransfer(
      ADst,
      ADstX,
      ADstY,
      ADst.ClipRect,
      VSrc,
      ASrcRect,
      ACombineOp
    );
  finally
    GBitmap32Pool.Push(VSrc);
  end;
end;
{$ELSE}
var
  VSize: TPoint;
begin
  VSize := ASource.Size;
  GR32_Resamplers.BlockTransferZ(
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
{$ENDIF}

procedure CopyBitmap32StaticToBitmap32(
  const ASourceBitmap: IBitmap32Static;
  ATarget: TCustomBitmap32
);
var
  VSourceSize: TPoint;
  VScale: Double;
  VSourceRect: TRect;
  VDstRect: TRect;
  VResampler: TCustomResampler;
begin
  VSourceSize := ASourceBitmap.Size;
  if (VSourceSize.X > 0) and (VSourceSize.Y > 0) then begin
    ATarget.Clear(clWhite32);
    VScale := Min(ATarget.Width / VSourceSize.X, ATarget.Height / VSourceSize.Y);
    VSourceRect := Rect(0, 0, VSourceSize.X, VSourceSize.Y);
    VDstRect :=
      Rect(
        Trunc((ATarget.Width - VSourceSize.X * VScale) / 2),
        Trunc((ATarget.Height - VSourceSize.Y * VScale) / 2),
        Trunc((ATarget.Width + VSourceSize.X * VScale) / 2),
        Trunc((ATarget.Height + VSourceSize.Y * VScale) / 2)
      );
    VResampler := TLinearResampler.Create;
    try
      StretchTransfer(
        ATarget,
        VDstRect,
        ASourceBitmap,
        VSourceRect,
        VResampler,
        dmBlend,
        cmBlend
      );
    finally
      VResampler.Free;
    end;
  end;
end;

{$IFDEF USE_GR32_UPSTREAM}
type
  TStaticBackend = class(TCustomBackend)
  private
    FData: PColor32Array;
  protected
    procedure InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean); override;
    procedure FinalizeSurface; override;
  public
    property Data: PColor32Array read FData write FData;
  end;

{ TStaticBackend }

procedure TStaticBackend.InitializeSurface(NewWidth, NewHeight: Integer; ClearBuffer: Boolean);
begin
  FBits := FData;
end;

procedure TStaticBackend.FinalizeSurface;
begin
  FBits := nil;
end;

{ TBitmap32ObjectPool }

constructor TBitmap32Pool.Create;
begin
  inherited Create;
  FCount := 0;
  FCapacity := Length(FItems);
  FMainThreadItem := TCustomBitmap32.Create(TStaticBackend);
  FLock := GSync.SyncVariable.Make(Self.ClassName);
end;

destructor TBitmap32Pool.Destroy;
begin
  Clear;
  FreeAndNil(FMainThreadItem);
  inherited Destroy;
end;

procedure TBitmap32Pool.Clear;
var
  I: Integer;
begin
  FLock.BeginWrite;
  try
    for I := 0 to FCount - 1 do begin
      FItems[I].Free;
    end;
    FCount := 0;
  finally
    FLock.EndWrite;
  end;
end;

function TBitmap32Pool.Pop(
  const ASourceData: PColor32Array;
  const ASourceSize: TPoint;
  const ACombineMode: TCombineMode;
  const AMasterAlpha: Cardinal;
  const AOuterColor: TColor32
): TCustomBitmap32;
begin
  if IsMainThread then begin
    Result := FMainThreadItem;
  end else begin
    FLock.BeginWrite;
    try
      if FCount > 0 then begin
        Dec(FCount);
        Result := FItems[FCount];
      end else begin
        Result := nil;
      end;
    finally
      FLock.EndWrite;
    end;
    if Result = nil then begin
      Result := TCustomBitmap32.Create(TStaticBackend);
    end;
  end;

  TStaticBackend(Result.Backend).Data := ASourceData;
  Result.SetSize(ASourceSize.X, ASourceSize.Y);
  Result.CombineMode := ACombineMode;
  Result.MasterAlpha := AMasterAlpha;
  Result.OuterColor := AOuterColor;
end;

procedure TBitmap32Pool.Push(
  const ABitmap: TCustomBitmap32
);
begin
  ABitmap.SetSize(0, 0);
  TStaticBackend(ABitmap.Backend).Data := nil;

  if ABitmap = FMainThreadItem then begin
    Exit;
  end;

  FLock.BeginWrite;
  try
    if FCount < FCapacity then begin
      FItems[FCount] := ABitmap;
      Inc(FCount);
    end else begin
      ABitmap.Free;
    end;
  finally
    FLock.EndWrite;
  end;
end;

class function TBitmap32Pool.IsMainThread: Boolean;
begin
  Result := TThread.Current.ThreadID = MainThreadID;
end;

initialization
  GBitmap32Pool := TBitmap32Pool.Create;

finalization
  FreeAndNil(GBitmap32Pool);
{$ENDIF}

end.
