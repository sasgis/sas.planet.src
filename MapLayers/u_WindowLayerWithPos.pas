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

unit u_WindowLayerWithPos;

interface

uses
  Types,
  SysUtils,
  GR32,
  GR32_Layers,
  i_NotifierOperation,
  i_LocalCoordConverterChangeable,
  i_SimpleFlag,
  i_InternalPerformanceCounter,
  u_WindowLayerBasic;

type
  TWindowLayerBasicBase = class(TWindowLayerAbstract)
  private
    FLayer: TCustomLayer;
    FVisible: Boolean;
    FNeedUpdateLayerVisibilityFlag: ISimpleFlag;
    FNeedFullRepaintLayerFlag: ISimpleFlag;

    FOnPaintCounter: IInternalPerformanceCounter;
    FOnMeasuringPaintCounter: IInternalPerformanceCounter;
    procedure SetVisible(const Value: Boolean);
    procedure OnPaintLayer(
      Sender: TObject;
      Buffer: TBitmap32
    );
  protected
    procedure StartThreads; override;
    procedure DoViewUpdate; override;
  protected
    procedure SetNeedUpdateLayerVisibility;
    procedure DoUpdateLayerVisibility; virtual;
    procedure SetNeedFullRepaintLayer;
    procedure DoFullRepaintLayer; virtual;

    procedure PaintLayer(ABuffer: TBitmap32); virtual; abstract;

    property Layer: TCustomLayer read FLayer;
    property Visible: Boolean read FVisible write SetVisible;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      ALayer: TCustomLayer
    );
  end;

  TWindowLayerWithLocationBase = class(TWindowLayerAbstract)
  private
    FLayer: TPositionedLayer;
    FVisible: Boolean;
    FNeedUpdateLayerVisibilityFlag: ISimpleFlag;
    FNeedUpdateLayerLocationFlag: ISimpleFlag;
    FNeedFullRepaintLayerFlag: ISimpleFlag;

    FOnPaintCounter: IInternalPerformanceCounter;
    FOnMeasuringPaintCounter: IInternalPerformanceCounter;
    procedure SetVisible(const Value: Boolean);

    procedure OnPaintLayer(
      Sender: TObject;
      Buffer: TBitmap32
    );
  protected
    procedure StartThreads; override;
    procedure DoViewUpdate; override;
  protected
    procedure SetNeedUpdateLayerVisibility;
    procedure DoUpdateLayerVisibility; virtual;

    procedure SetNeedUpdateLayerLocation;
    function GetNewLayerLocation: TFloatRect; virtual; abstract;
    procedure DoUpdateLayerLocation(ALocation: TFloatRect); virtual;

    procedure SetNeedFullRepaintLayer;
    procedure DoFullRepaintLayer; virtual;

    procedure PaintLayer(ABuffer: TBitmap32); virtual; abstract;

    property Layer: TPositionedLayer read FLayer;
    property Visible: Boolean read FVisible write SetVisible;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      ALayer: TPositionedLayer
    );
  end;

  TWindowLayerWithBitmapBase = class(TWindowLayerAbstract)
  private
    FLayer: TBitmapLayer;
    FVisible: Boolean;
    FNeedUpdateLayerVisibilityFlag: ISimpleFlag;
    FNeedUpdateBitmapDrawFlag: ISimpleFlag;
    FNeedUpdateBitmapSizeFlag: ISimpleFlag;
    FNeedUpdateLayerLocationFlag: ISimpleFlag;

    FResizeCounter: IInternalPerformanceCounter;
    FRedrawCounter: IInternalPerformanceCounter;

    procedure SetVisible(const Value: Boolean);
  protected
    procedure DoViewUpdate; override;
  protected
    procedure SetNeedUpdateLayerVisibility;
    procedure DoUpdateLayerVisibility; virtual;

    procedure SetNeedUpdateBitmapDraw;
    procedure DoUpdateBitmapDraw; virtual; abstract;

    procedure SetNeedUpdateBitmapSize;
    function GetNewBitmapSize: TPoint; virtual; abstract;
    procedure DoUpdateBitmapSize(const ASize: TPoint); virtual;

    procedure SetNeedUpdateLayerLocation;
    function GetNewLayerLocation: TFloatRect; virtual; abstract;
    procedure DoUpdateLayerLocation(ALocation: TFloatRect); virtual;

    property Layer: TBitmapLayer read FLayer;
    property Visible: Boolean read FVisible write SetVisible;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      ALayer: TBitmapLayer
    );
  end;

implementation

uses
  u_SimpleFlagWithInterlock;

{ TWindowLayerWithBitmapBase }

constructor TWindowLayerWithBitmapBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier,
  AAppClosingNotifier: INotifierOneOperation;
  ALayer: TBitmapLayer
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier
  );
  FLayer := ALayer;
  FLayer.Visible := False;
  FLayer.MouseEvents := False;
  FLayer.Bitmap.DrawMode := dmBlend;

  FNeedUpdateLayerVisibilityFlag := TSimpleFlagWithInterlock.Create;
  FNeedUpdateBitmapDrawFlag := TSimpleFlagWithInterlock.Create;
  FNeedUpdateBitmapSizeFlag := TSimpleFlagWithInterlock.Create;
  FNeedUpdateLayerLocationFlag := TSimpleFlagWithInterlock.Create;

  FResizeCounter := PerfList.CreateAndAddNewCounter('Resize');
  FRedrawCounter := PerfList.CreateAndAddNewCounter('Redraw');
end;

procedure TWindowLayerWithBitmapBase.DoUpdateBitmapSize(const ASize: TPoint);
begin
  FLayer.Bitmap.SetSize(ASize.X, ASize.Y);
end;

procedure TWindowLayerWithBitmapBase.DoUpdateLayerLocation(ALocation: TFloatRect);
begin
  FLayer.Location := ALocation;
end;

procedure TWindowLayerWithBitmapBase.DoUpdateLayerVisibility;
begin
  FLayer.Visible := FVisible;
end;

procedure TWindowLayerWithBitmapBase.DoViewUpdate;
var
  VSize: TPoint;
  VLocation: TFloatRect;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  inherited;
  if FNeedUpdateLayerVisibilityFlag.CheckFlagAndReset then begin
    if FVisible <> FLayer.Visible then begin
      SetNeedUpdateLayerVisibility;
      SetNeedUpdateBitmapSize;
    end;
  end;
  if FNeedUpdateBitmapSizeFlag.CheckFlagAndReset then begin
    if FVisible then begin
      VSize := GetNewBitmapSize;
    end else begin
      VSize := Types.Point(0, 0);
    end;
    if (VSize.X <> FLayer.Bitmap.Width) or (VSize.Y <> FLayer.Bitmap.Height) then begin
      VCounterContext := FResizeCounter.StartOperation;
      try
        DoUpdateBitmapSize(VSize);
        if FVisible then begin
          SetNeedUpdateBitmapDraw;
          SetNeedUpdateLayerLocation;
        end;
      finally
        FResizeCounter.FinishOperation(VCounterContext);
      end;
    end;
  end;
  if FNeedUpdateBitmapDrawFlag.CheckFlagAndReset then begin
    if FVisible then begin
      VCounterContext := FRedrawCounter.StartOperation;
      try
        DoUpdateBitmapDraw;
      finally
        FRedrawCounter.FinishOperation(VCounterContext);
      end;
    end;
  end;
  if FNeedUpdateLayerLocationFlag.CheckFlagAndReset then begin
    VLocation := GetNewLayerLocation;
    if not EqualRect(VLocation, FLayer.Location) then begin
      DoUpdateLayerLocation(VLocation);
    end;
  end;
  if FNeedUpdateLayerVisibilityFlag.CheckFlagAndReset then begin
    if FLayer.Visible <> FVisible then begin
      DoUpdateLayerVisibility;
    end;
  end;
end;

procedure TWindowLayerWithBitmapBase.SetNeedUpdateBitmapDraw;
begin
  FNeedUpdateBitmapDrawFlag.SetFlag;
end;

procedure TWindowLayerWithBitmapBase.SetNeedUpdateBitmapSize;
begin
  FNeedUpdateBitmapSizeFlag.SetFlag;
end;

procedure TWindowLayerWithBitmapBase.SetNeedUpdateLayerLocation;
begin
  FNeedUpdateLayerLocationFlag.SetFlag;
end;

procedure TWindowLayerWithBitmapBase.SetNeedUpdateLayerVisibility;
begin
  FNeedUpdateLayerVisibilityFlag.SetFlag;
end;

procedure TWindowLayerWithBitmapBase.SetVisible(const Value: Boolean);
begin
  ViewUpdateLock;
  try
    FVisible := Value;
    SetNeedUpdateLayerVisibility;
  finally
    ViewUpdateUnlock;
  end;
end;

{ TWindowLayerWithLocationBase }

constructor TWindowLayerWithLocationBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier,
  AAppClosingNotifier: INotifierOneOperation;
  ALayer: TPositionedLayer
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier
  );
  FLayer := ALayer;
  FLayer.Visible := False;
  FLayer.MouseEvents := False;

  FNeedUpdateLayerVisibilityFlag := TSimpleFlagWithInterlock.Create;
  FNeedUpdateLayerLocationFlag := TSimpleFlagWithInterlock.Create;
  FNeedFullRepaintLayerFlag := TSimpleFlagWithInterlock.Create;

  FOnPaintCounter := PerfList.CreateAndAddNewCounter('OnPaint');
  FOnMeasuringPaintCounter := PerfList.CreateAndAddNewCounter('OnMeasuringPaint');
end;

procedure TWindowLayerWithLocationBase.DoFullRepaintLayer;
begin
  FLayer.Changed;
end;

procedure TWindowLayerWithLocationBase.DoUpdateLayerLocation(
  ALocation: TFloatRect);
begin
  FLayer.Location := ALocation;
end;

procedure TWindowLayerWithLocationBase.DoUpdateLayerVisibility;
begin
  FLayer.Visible := FVisible;
end;

procedure TWindowLayerWithLocationBase.DoViewUpdate;
var
  VLocation: TFloatRect;
begin
  inherited;
  if FNeedUpdateLayerVisibilityFlag.CheckFlagAndReset then begin
    if FLayer.Visible <> FVisible then begin
      SetNeedUpdateLayerVisibility;
      SetNeedUpdateLayerLocation;
    end;
  end;
  if FNeedUpdateLayerLocationFlag.CheckFlagAndReset then begin
    VLocation := GetNewLayerLocation;
    if not EqualRect(VLocation, FLayer.Location) then begin
      DoUpdateLayerLocation(VLocation);
    end;
  end;
  if FNeedUpdateLayerVisibilityFlag.CheckFlagAndReset then begin
    if FLayer.Visible <> FVisible then begin
      DoUpdateLayerVisibility;
      SetNeedUpdateLayerLocation;
    end;
  end;
  if FNeedFullRepaintLayerFlag.CheckFlagAndReset then begin
    DoFullRepaintLayer;
  end;
end;

procedure TWindowLayerWithLocationBase.OnPaintLayer(
  Sender: TObject;
  Buffer: TBitmap32
);
var
  VCounter: IInternalPerformanceCounter;
  VCounterContext: TInternalPerformanceCounterContext;
  VOldClipRect: TRect;
  VNewClipRect: TRect;
begin
  if Buffer.MeasuringMode then begin
    VCounter := FOnMeasuringPaintCounter;
  end else begin
    VCounter := FOnPaintCounter;
  end;
  VCounterContext := VCounter.StartOperation;
  try
    VOldClipRect := Buffer.ClipRect;
    if Types.IntersectRect(VNewClipRect, VOldClipRect, MakeRect(FLayer.Location, rrClosest)) then begin
      Buffer.ClipRect := VNewClipRect;
      try
        PaintLayer(Buffer);
      finally
        Buffer.ClipRect := VOldClipRect;
      end;
    end;
  finally
    VCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TWindowLayerWithLocationBase.SetNeedFullRepaintLayer;
begin
  FNeedFullRepaintLayerFlag.SetFlag;
end;

procedure TWindowLayerWithLocationBase.SetNeedUpdateLayerLocation;
begin
  FNeedUpdateLayerLocationFlag.SetFlag;
end;

procedure TWindowLayerWithLocationBase.SetNeedUpdateLayerVisibility;
begin
  FNeedUpdateLayerVisibilityFlag.SetFlag;
end;

procedure TWindowLayerWithLocationBase.SetVisible(const Value: Boolean);
begin
  ViewUpdateLock;
  try
    FVisible := Value;
    SetNeedUpdateLayerVisibility;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerWithLocationBase.StartThreads;
begin
  inherited;
  FLayer.OnPaint := OnPaintLayer;
end;

{ TWindowLayerBasicBase }

constructor TWindowLayerBasicBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier,
  AAppClosingNotifier: INotifierOneOperation;
  ALayer: TCustomLayer
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier
  );
  FLayer := ALayer;
  FLayer.Visible := False;
  FLayer.MouseEvents := False;

  FNeedUpdateLayerVisibilityFlag := TSimpleFlagWithInterlock.Create;
  FNeedFullRepaintLayerFlag := TSimpleFlagWithInterlock.Create;

  FOnPaintCounter := PerfList.CreateAndAddNewCounter('OnPaint');
  FOnMeasuringPaintCounter := PerfList.CreateAndAddNewCounter('OnMeasuringPaint');
end;

procedure TWindowLayerBasicBase.DoFullRepaintLayer;
begin
  FLayer.Changed;
end;

procedure TWindowLayerBasicBase.DoUpdateLayerVisibility;
begin
  FLayer.Visible := FVisible;
end;

procedure TWindowLayerBasicBase.DoViewUpdate;
begin
  inherited;
  if FNeedUpdateLayerVisibilityFlag.CheckFlagAndReset then begin
    if FLayer.Visible <> FVisible then begin
      DoUpdateLayerVisibility;
      if Visible then begin
        SetNeedFullRepaintLayer;
      end;
    end;
  end;
  if FNeedFullRepaintLayerFlag.CheckFlagAndReset then begin
    if FVisible then begin
      DoFullRepaintLayer;
    end;
  end;
end;

procedure TWindowLayerBasicBase.OnPaintLayer(
  Sender: TObject;
  Buffer: TBitmap32
);
var
  VCounter: IInternalPerformanceCounter;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if Buffer.MeasuringMode then begin
    VCounter := FOnMeasuringPaintCounter;
  end else begin
    VCounter := FOnPaintCounter;
  end;
  VCounterContext := VCounter.StartOperation;
  try
    PaintLayer(Buffer);
  finally
    VCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TWindowLayerBasicBase.SetNeedFullRepaintLayer;
begin
  FNeedFullRepaintLayerFlag.SetFlag;
end;

procedure TWindowLayerBasicBase.SetNeedUpdateLayerVisibility;
begin
  FNeedUpdateLayerVisibilityFlag.SetFlag;
end;

procedure TWindowLayerBasicBase.SetVisible(const Value: Boolean);
begin
  ViewUpdateLock;
  try
    FVisible := Value;
    SetNeedUpdateLayerVisibility;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerBasicBase.StartThreads;
begin
  inherited;
  FLayer.OnPaint := OnPaintLayer;
end;

end.
