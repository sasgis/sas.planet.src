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

unit u_WindowLayerBasicBase;

interface

uses
  Types,
  SysUtils,
  GR32,
  GR32_Layers,
  i_NotifierOperation,
  i_SimpleFlag,
  i_InternalPerformanceCounter,
  u_WindowLayerAbstract;

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

implementation

uses
  u_SimpleFlagWithInterlock;

{ TWindowLayerBasicBase }

constructor TWindowLayerBasicBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier,
  AAppClosingNotifier: INotifierOneOperation;
  ALayer: TCustomLayer
);
begin
  inherited Create(
    AAppStartedNotifier,
    AAppClosingNotifier
  );
  FLayer := ALayer;
  FLayer.Visible := False;
  FLayer.MouseEvents := False;

  FNeedUpdateLayerVisibilityFlag := TSimpleFlagWithInterlock.Create;
  FNeedFullRepaintLayerFlag := TSimpleFlagWithInterlock.Create;

  FOnPaintCounter := APerfList.CreateAndAddNewCounter('OnPaint');
  FOnMeasuringPaintCounter := APerfList.CreateAndAddNewCounter('OnMeasuringPaint');
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
