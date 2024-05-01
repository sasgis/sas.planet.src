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

    procedure OnPaintLayer(Sender: TObject; Buffer: TBitmap32);
    procedure OnViewChange;
    procedure RedrawIfNeed;

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

    procedure InvalidateLayer; virtual; //abstract; // todo
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); virtual; abstract;

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

procedure TMapLayerBasicNoBitmap.DoViewUpdate;
begin
  inherited;
  RedrawIfNeed;
end;

procedure TMapLayerBasicNoBitmap.DoInvalidateFull;
begin
  FParentMap.ForceFullInvalidate;
end;

procedure TMapLayerBasicNoBitmap.DoInvalidateRect(const ARect: TRect);
begin
  FParentMap.Invalidate(ARect);
end;

procedure TMapLayerBasicNoBitmap.InvalidateLayer;
begin
  DoInvalidateFull; // todo
end;

procedure TMapLayerBasicNoBitmap.OnPaintLayer(
  Sender: TObject;
  Buffer: TBitmap32
);
var
  VLocalConverter: ILocalCoordConverter;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VLocalConverter := View.GetStatic;
  if VLocalConverter <> nil then begin
    VCounterContext := FOnPaintCounter.StartOperation;
    try
      if FVisible then begin
        PaintLayer(Buffer, VLocalConverter);
      end;
    finally
      FOnPaintCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

procedure TMapLayerBasicNoBitmap.OnViewChange;
begin
  SetNeedRedraw;
end;

procedure TMapLayerBasicNoBitmap.StartThreads;
begin
  inherited;
  FLayer.OnPaint := OnPaintLayer;
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

procedure TMapLayerBasicNoBitmap.RedrawIfNeed;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if FNeedRedrawFlag.CheckFlagAndReset then begin
    VCounterContext := FOnInvalidateCounter.StartOperation;
    try
      InvalidateLayer;
    finally
      FOnInvalidateCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

procedure TMapLayerBasicNoBitmap.SetNeedRedraw;
begin
  FNeedRedrawFlag.SetFlag;
end;

procedure TMapLayerBasicNoBitmap.SetVisible(const Value: Boolean);
begin
  if Value then begin
    Show;
  end else begin
    Hide;
  end;
end;

end.
