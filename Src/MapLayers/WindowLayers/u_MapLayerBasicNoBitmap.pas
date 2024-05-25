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

unit u_MapLayerBasicNoBitmap;

interface

uses
  GR32,
  GR32_Layers,
  GR32_Image,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_SimpleFlag,
  i_InternalPerformanceCounter,
  u_WindowLayerAbstract;

type
  TMapLayerBasicNoBitmap = class(TWindowLayerAbstract)
  private
    FParentMap: TImage32;
    FView: ILocalCoordConverterChangeable;
    FVisible: Boolean;
    FLayer: TCustomLayer;

    FNeedRedrawFlag: ISimpleFlag;

    FOnInvalidateCounter: IInternalPerformanceCounter;
    FOnPaintCounter: IInternalPerformanceCounter;
    FOnMeasuringPaintCounter: IInternalPerformanceCounter;

    procedure OnPaintLayer(Sender: TObject; ABuffer: TBitmap32);
    procedure OnViewChange;

    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
  protected
    procedure DoInvalidateFull;
    procedure DoInvalidateRect(const ARect: TRect);

    procedure SetNeedRedraw;

    procedure Show;
    procedure DoShow; virtual;
    procedure Hide;
    procedure DoHide; virtual;

    procedure InvalidateLayer(const ALocalConverter: ILocalCoordConverter); virtual; abstract;
    procedure PaintLayer(ABuffer: TBitmap32); virtual; abstract;

    property Visible: Boolean read GetVisible write SetVisible;
    property View: ILocalCoordConverterChangeable read FView;
  protected
    procedure StartThreads; override;
    procedure DoViewUpdate; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      const AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_SimpleFlagWithInterlock,
  u_ListenerByEvent;

{ TMapLayerBasicNoBitmap }

constructor TMapLayerBasicNoBitmap.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  const AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable
);
begin
  inherited Create(
    AAppStartedNotifier,
    AAppClosingNotifier
  );
  FParentMap := AParentMap;
  FView := AView;
  FLayer := TCustomLayer.Create(AParentMap.Layers);

  FLayer.MouseEvents := False;
  FLayer.Visible := False;
  FVisible := False;
  FNeedRedrawFlag := TSimpleFlagWithInterlock.Create;

  FOnInvalidateCounter := APerfList.CreateAndAddNewCounter('OnInvalidate');
  FOnPaintCounter := APerfList.CreateAndAddNewCounter('OnPaint');
  FOnMeasuringPaintCounter := APerfList.CreateAndAddNewCounter('OnMeasuringPaint');

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnViewChange),
    View.ChangeNotifier
  );
end;

destructor TMapLayerBasicNoBitmap.Destroy;
begin
  FLayer := nil;
  inherited;
end;

procedure TMapLayerBasicNoBitmap.DoInvalidateFull;
begin
  FParentMap.ForceFullInvalidate;
end;

procedure TMapLayerBasicNoBitmap.DoInvalidateRect(const ARect: TRect);
begin
  FParentMap.Invalidate(ARect);
end;

procedure TMapLayerBasicNoBitmap.DoViewUpdate;
var
  VLocalConverter: ILocalCoordConverter;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if FNeedRedrawFlag.CheckFlagAndReset then begin
    VLocalConverter := View.GetStatic;
    if VLocalConverter <> nil then begin
      VCounterContext := FOnInvalidateCounter.StartOperation;
      try
        InvalidateLayer(VLocalConverter);
      finally
        FOnInvalidateCounter.FinishOperation(VCounterContext);
      end;
    end;
  end;
end;

procedure TMapLayerBasicNoBitmap.OnPaintLayer(Sender: TObject; ABuffer: TBitmap32);
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

procedure TMapLayerBasicNoBitmap.OnViewChange;
begin
  ViewUpdateLock;
  try
    if FVisible then begin
      SetNeedRedraw;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerBasicNoBitmap.Hide;
begin
  ViewUpdateLock;
  try
    if FVisible then begin
      DoHide;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerBasicNoBitmap.DoHide;
begin
  FVisible := False;
  FLayer.Visible := False;
  SetNeedRedraw;
end;

procedure TMapLayerBasicNoBitmap.Show;
begin
  ViewUpdateLock;
  try
    if not Visible then begin
      DoShow;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerBasicNoBitmap.DoShow;
begin
  FVisible := True;
  FLayer.Visible := True;
  SetNeedRedraw;
end;

function TMapLayerBasicNoBitmap.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TMapLayerBasicNoBitmap.SetVisible(const Value: Boolean);
begin
  if Value then begin
    Show;
  end else begin
    Hide;
  end;
end;

procedure TMapLayerBasicNoBitmap.SetNeedRedraw;
begin
  FNeedRedrawFlag.SetFlag;
end;

procedure TMapLayerBasicNoBitmap.StartThreads;
begin
  inherited;
  FLayer.OnPaint := OnPaintLayer;
end;

end.
