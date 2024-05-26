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

unit u_WindowLayerBasicBase;

interface

uses
  Types,
  SysUtils,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_NotifierOperation,
  i_SimpleFlag,
  i_InternalPerformanceCounter,
  u_WindowLayerAbstract;

type
  TWindowLayerBasicBase = class(TWindowLayerAbstract)
  private
    FParentMap: TImage32;
    FLayer: TCustomLayer;
    FVisible: Boolean;
    FNeedUpdateLayerVisibilityFlag: ISimpleFlag;
    FNeedFullRepaintLayerFlag: ISimpleFlag;

    FOnInvalidateCounter: IInternalPerformanceCounter;
    FOnPaintCounter: IInternalPerformanceCounter;
    FOnMeasuringPaintCounter: IInternalPerformanceCounter;

    procedure SetVisible(const AValue: Boolean);
    procedure OnPaintLayer(Sender: TObject; ABuffer: TBitmap32);
  protected
    procedure StartThreads; override;
    procedure DoViewUpdate; override;
  protected
    procedure DoInvalidateFull;
    procedure DoInvalidateRect(const ARect: TRect);

    procedure SetNeedUpdateLayerVisibility;
    procedure DoUpdateLayerVisibility; virtual;

    procedure SetNeedFullRepaintLayer;
    procedure DoFullRepaintLayer; virtual;

    procedure InvalidateLayer; virtual; abstract;
    procedure PaintLayer(ABuffer: TBitmap32); virtual; abstract;

    property Layer: TCustomLayer read FLayer;
    property Visible: Boolean read FVisible write SetVisible;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AParentMap: TImage32;
      const ALayer: TCustomLayer
    );
  end;

implementation

uses
  u_SimpleFlagWithInterlock;

{ TWindowLayerBasicBase }

constructor TWindowLayerBasicBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AParentMap: TImage32;
  const ALayer: TCustomLayer
);
begin
  inherited Create(
    AAppStartedNotifier,
    AAppClosingNotifier
  );

  FParentMap := AParentMap;

  FLayer := ALayer;
  FLayer.Visible := False;
  FLayer.MouseEvents := False;

  FNeedUpdateLayerVisibilityFlag := TSimpleFlagWithInterlock.Create;
  FNeedFullRepaintLayerFlag := TSimpleFlagWithInterlock.Create;

  FOnInvalidateCounter := APerfList.CreateAndAddNewCounter('OnInvalidate');
  FOnPaintCounter := APerfList.CreateAndAddNewCounter('OnPaint');
  FOnMeasuringPaintCounter := APerfList.CreateAndAddNewCounter('OnMeasuringPaint');
end;

procedure TWindowLayerBasicBase.DoInvalidateFull;
begin
  FParentMap.ForceFullInvalidate;
end;

procedure TWindowLayerBasicBase.DoInvalidateRect(const ARect: TRect);
begin
  FParentMap.Invalidate(ARect);
end;

procedure TWindowLayerBasicBase.DoFullRepaintLayer;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FOnInvalidateCounter.StartOperation;
  try
    InvalidateLayer;
  finally
    FOnInvalidateCounter.FinishOperation(VCounterContext);
  end;
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
      SetNeedFullRepaintLayer;
    end;
  end;
  if FNeedFullRepaintLayerFlag.CheckFlagAndReset then begin
    DoFullRepaintLayer;
  end;
end;

procedure TWindowLayerBasicBase.OnPaintLayer(Sender: TObject; ABuffer: TBitmap32);
var
  VCounter: IInternalPerformanceCounter;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if ABuffer.MeasuringMode then begin
    VCounter := FOnMeasuringPaintCounter;
  end else begin
    VCounter := FOnPaintCounter;
  end;
  VCounterContext := VCounter.StartOperation;
  try
    if FVisible then begin
      PaintLayer(ABuffer);
    end;
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

procedure TWindowLayerBasicBase.SetVisible(const AValue: Boolean);
begin
  ViewUpdateLock;
  try
    FVisible := AValue;
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
