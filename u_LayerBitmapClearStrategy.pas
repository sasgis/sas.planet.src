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

unit u_LayerBitmapClearStrategy;

interface

uses
  GR32,
  i_InternalPerformanceCounter,
  i_LayerBitmapClearStrategy;

type
  TLayerBitmapClearStrategyBase = class(TInterfacedObject, ILayerBitmapClearStrategy)
  private
    FCounter: IInternalPerformanceCounter;
  protected
    procedure DoClear(ABitmap: TCustomBitmap32); virtual; abstract;
  protected
    procedure Clear(ABitmap: TCustomBitmap32);
  public
    constructor Create(const ACounter: IInternalPerformanceCounter);
  end;

  TLayerBitmapClearStrategySimpleClear = class(TLayerBitmapClearStrategyBase)
  protected
    procedure DoClear(ABitmap: TCustomBitmap32); override;
  end;

  TLayerBitmapClearStrategyMoveImage = class(TLayerBitmapClearStrategyBase)
  private
    FDelta: TPoint;
  protected
    procedure DoClear(ABitmap: TCustomBitmap32); override;
  public
    constructor Create(
      const ACounter: IInternalPerformanceCounter;
      const ADelta: TPoint
    );
  end;

  TLayerBitmapClearStrategyImageResize = class(TLayerBitmapClearStrategyBase)
  private
    FSourceBitmap: TCustomBitmap32;
    FInTargetTopLeft: TPoint;
  protected
    procedure DoClear(ABitmap: TCustomBitmap32); override;
  public
    constructor Create(
      const ACounter: IInternalPerformanceCounter;
      ASourceBitmap: TCustomBitmap32;
      const ATargetRectInSource: TRect
    );
    destructor Destroy; override;
  end;


  TLayerBitmapClearStrategyZoomChange = class(TLayerBitmapClearStrategyBase)
  private
    FSourceBitmap: TCustomBitmap32;
    FTargetRect: TRect;
  protected
    procedure DoClear(ABitmap: TCustomBitmap32); override;
  public
    constructor Create(
      const ACounter: IInternalPerformanceCounter;
      AResumpler: TCustomResampler;
      ASourceBitmap: TCustomBitmap32;
      const ASourceRect: TRect;
      const ATargetRect: TRect
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Resamplers;

{ TLayerBitmapClearStrategyBase }

constructor TLayerBitmapClearStrategyBase.Create(
  const ACounter: IInternalPerformanceCounter
);
begin
  inherited Create;
  FCounter := ACounter;
end;

procedure TLayerBitmapClearStrategyBase.Clear(ABitmap: TCustomBitmap32);
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FCounter.StartOperation;
  try
    DoClear(ABitmap);
  finally
    FCounter.FinishOperation(VCounterContext);
  end;
end;

{ TLayerBitmapClearStrategySimpleClear }

procedure TLayerBitmapClearStrategySimpleClear.DoClear(ABitmap: TCustomBitmap32);
begin
  ABitmap.Clear(0);
end;

{ TLayerBitmapClearStrategyMoveImage }

constructor TLayerBitmapClearStrategyMoveImage.Create(
  const ACounter: IInternalPerformanceCounter;
  const ADelta: TPoint
);
begin
  inherited Create(ACounter);
  FDelta := ADelta;
end;

procedure TLayerBitmapClearStrategyMoveImage.DoClear(ABitmap: TCustomBitmap32);
begin
  ABitmap.Roll(FDelta.X, FDelta.Y, True, 0);
end;

{ TLayerBitmapClearStrategyImageResize }

constructor TLayerBitmapClearStrategyImageResize.Create(
  const ACounter: IInternalPerformanceCounter;
  ASourceBitmap: TCustomBitmap32;
  const ATargetRectInSource: TRect
);
var
  VCopyRect: TRect;
begin
  inherited Create(ACounter);
  FSourceBitmap := TCustomBitmap32.Create;

  if ATargetRectInSource.Left <= 0 then begin
    VCopyRect.Left := 0;
    FInTargetTopLeft.X := -ATargetRectInSource.Left;
  end else begin
    VCopyRect.Left := ATargetRectInSource.Left;
    FInTargetTopLeft.X := 0;
  end;

  if ATargetRectInSource.Top <= 0 then begin
    VCopyRect.Top := 0;
    FInTargetTopLeft.Y := -ATargetRectInSource.Top;
  end else begin
    VCopyRect.Top := ATargetRectInSource.Top;
    FInTargetTopLeft.Y := 0;
  end;

  if ATargetRectInSource.Right >= ASourceBitmap.Width then begin
    VCopyRect.Right := ASourceBitmap.Width;
  end else begin
    VCopyRect.Right := ATargetRectInSource.Right;
  end;

  if ATargetRectInSource.Bottom >= ASourceBitmap.Height then begin
    VCopyRect.Bottom := ASourceBitmap.Height;
  end else begin
    VCopyRect.Bottom := ATargetRectInSource.Bottom;
  end;

  FSourceBitmap.SetSize(VCopyRect.Right - VCopyRect.Left, VCopyRect.Bottom - VCopyRect.Top);
  BlockTransfer(
    FSourceBitmap,
    0, 0,
    FSourceBitmap.ClipRect,
    ASourceBitmap,
    VCopyRect,
    dmOpaque
  );
end;

destructor TLayerBitmapClearStrategyImageResize.Destroy;
begin
  FreeAndNil(FSourceBitmap);
  inherited;
end;

procedure TLayerBitmapClearStrategyImageResize.DoClear(ABitmap: TCustomBitmap32);
begin
  ABitmap.Clear(0);
  BlockTransfer(
    ABitmap,
    FInTargetTopLeft.X, FInTargetTopLeft.Y,
    ABitmap.ClipRect,
    FSourceBitmap,
    FSourceBitmap.BoundsRect,
    dmOpaque
  );
end;

{ TLayerBitmapClearStrategyZoomChange }

constructor TLayerBitmapClearStrategyZoomChange.Create(
  const ACounter: IInternalPerformanceCounter;
  AResumpler: TCustomResampler;
  ASourceBitmap: TCustomBitmap32;
  const ASourceRect: TRect;
  const ATargetRect: TRect
);
begin
  inherited Create(ACounter);
  FSourceBitmap := TCustomBitmap32.Create;
  FSourceBitmap.SetSize(ASourceRect.Right - ASourceRect.Left, ASourceRect.Bottom - ASourceRect.Top);
  BlockTransfer(FSourceBitmap, 0, 0, FSourceBitmap.ClipRect, ASourceBitmap, ASourceRect, dmOpaque);
  if AResumpler <> nil then begin
    FSourceBitmap.Resampler := AResumpler;
  end;
  FTargetRect := ATargetRect;
end;

destructor TLayerBitmapClearStrategyZoomChange.Destroy;
begin
  FreeAndNil(FSourceBitmap);
  inherited;
end;

procedure TLayerBitmapClearStrategyZoomChange.DoClear(ABitmap: TCustomBitmap32);
begin
  ABitmap.Clear(0);
  FSourceBitmap.DrawTo(ABitmap, FTargetRect, FSourceBitmap.ClipRect);
end;

end.
