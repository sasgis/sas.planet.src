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
  Windows,
  SysUtils,
  GR32,
  GR32_Layers,
  GR32_Image,
  i_LocalCoordConverter,
  i_ViewPortState,
  i_InternalPerformanceCounter,
  u_WindowLayerBasic;

type
  TWindowLayerWithPosBase = class(TWindowLayerAbstract)
  private
    FViewUpdateLock: Integer;
    FViewPortState: IViewPortState;
    FViewCoordConverter: ILocalCoordConverter;
    FViewCoordConverterCS: IReadWriteSync;
    FLayerCoordConverter: ILocalCoordConverter;
    FLayerCoordConverterCS: IReadWriteSync;
    procedure OnViewPortPosChange;
    procedure OnViewPortScaleChange;
    function GetLayerCoordConverter: ILocalCoordConverter;
    function GetViewCoordConverter: ILocalCoordConverter;
  protected
    procedure SetViewCoordConverter(AValue: ILocalCoordConverter); virtual;
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); virtual;

    function GetLayerCoordConverterByViewConverter(ANewViewCoordConverter: ILocalCoordConverter): ILocalCoordConverter; virtual;

    procedure PosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;

    procedure ScaleChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;
    procedure DoScaleChange(ANewVisualCoordConverter: ILocalCoordConverter); virtual;

    procedure ViewUpdateLock;
    procedure ViewUpdateUnlock;
    procedure ViewUpdate;
    procedure DoViewUpdate; virtual;

    property ViewPortState: IViewPortState read FViewPortState;
    property ViewCoordConverter: ILocalCoordConverter read GetViewCoordConverter;
    property LayerCoordConverter: ILocalCoordConverter read GetLayerCoordConverter;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AViewPortState: IViewPortState;
      AListenScaleChange: Boolean
    );
    destructor Destroy; override;

    procedure StartThreads; override;
  end;

  TWindowLayerBasic = class(TWindowLayerWithPosBase)
  private
    FVisible: Boolean;
    FLayer: TPositionedLayer;

    FNeedRedraw: Boolean;
    FNeedUpdateLocation: Boolean;
    FNeedRedrawCS: IReadWriteSync;
    FRedrawCounter: IInternalPerformanceCounter;
  protected
    function GetVisible: Boolean; virtual;
    procedure SetVisible(const Value: Boolean); virtual;

    procedure SetNeedRedraw; virtual;
    procedure SetNeedUpdateLocation; virtual;

    function GetVisibleForNewPos(ANewVisualCoordConverter: ILocalCoordConverter): Boolean; virtual;

    function GetMapLayerLocationRect: TFloatRect; virtual; abstract;
    procedure UpdateLayerLocationIfNeed; virtual;
    procedure UpdateLayerLocation; virtual;
    procedure DoUpdateLayerLocation(ANewLocation: TFloatRect); virtual;

    procedure Show; virtual;
    procedure DoShow; virtual;
    procedure Hide; virtual;
    procedure DoHide; virtual;
    procedure DoRedraw; virtual; abstract;
    procedure RedrawIfNeed; virtual;

    property LayerPositioned: TPositionedLayer read FLayer;
    property Visible: Boolean read GetVisible write SetVisible;
  protected
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
    procedure DoViewUpdate; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      ALayer: TPositionedLayer;
      AViewPortState: IViewPortState;
      AListenScaleChange: Boolean
    );
    destructor Destroy; override;
    procedure Redraw; virtual;
  end;

  TWindowLayerWithBitmap = class(TWindowLayerBasic)
  private
    FNeedUpdateLayerSize: Boolean;
  protected
    FLayer: TBitmapLayer;
    procedure SetNeedUpdateLayerSize; virtual;

    procedure UpdateLayerSize; virtual;
    procedure UpdateLayerSizeIfNeed; virtual;
    procedure DoUpdateLayerSize(ANewSize: TPoint); virtual;
    function GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; virtual; abstract;
  protected
    procedure DoHide; override;
    procedure DoShow; override;
    procedure DoViewUpdate; override;
  public
    constructor Create(
      APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      AViewPortState: IViewPortState
    );
  end;

  TWindowLayerFixedSizeWithBitmap = class(TWindowLayerWithBitmap)
  protected
    function GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
    procedure DoRedraw; override;
  end;

implementation

uses
  Types,
  u_Synchronizer,
  u_NotifyEventListener;

{ TWindowLayerWithPosBase }

constructor TWindowLayerWithPosBase.Create(
  APerfList: IInternalPerformanceCounterList;
  AViewPortState: IViewPortState;
  AListenScaleChange: Boolean
);
begin
  inherited Create(APerfList);
  FViewUpdateLock := 0;
  FViewPortState := AViewPortState;

  FViewCoordConverterCS := MakeSyncMulti(Self);
  FLayerCoordConverterCS := MakeSyncMulti(Self);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnViewPortPosChange),
    FViewPortState.GetChangeNotifier
  );
  if AListenScaleChange then begin
    LinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnViewPortScaleChange),
      ViewPortState.ScaleChangeNotifier
    );
  end;
end;

destructor TWindowLayerWithPosBase.Destroy;
begin
  FViewCoordConverterCS := nil;
  FLayerCoordConverterCS := nil;
  FViewCoordConverter := nil;
  FLayerCoordConverter := nil;
  FViewPortState := nil;
  inherited;
end;

procedure TWindowLayerWithPosBase.OnViewPortPosChange;
begin
  PosChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TWindowLayerWithPosBase.PosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  ViewUpdateLock;
  try
    DoPosChange(ANewVisualCoordConverter);
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TWindowLayerWithPosBase.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  SetViewCoordConverter(ANewVisualCoordConverter);
  SetLayerCoordConverter(GetLayerCoordConverterByViewConverter(ANewVisualCoordConverter));
end;

procedure TWindowLayerWithPosBase.OnViewPortScaleChange;
begin
  ScaleChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TWindowLayerWithPosBase.ScaleChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  ViewUpdateLock;
  try
    DoScaleChange(ANewVisualCoordConverter);
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TWindowLayerWithPosBase.DoScaleChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  SetViewCoordConverter(ANewVisualCoordConverter);
end;

function TWindowLayerWithPosBase.GetLayerCoordConverter: ILocalCoordConverter;
begin
  FLayerCoordConverterCS.BeginRead;
  try
    Result := FLayerCoordConverter;
  finally
    FLayerCoordConverterCS.EndRead;
  end;
end;

function TWindowLayerWithPosBase.GetLayerCoordConverterByViewConverter(
  ANewViewCoordConverter: ILocalCoordConverter): ILocalCoordConverter;
begin
  Result := ANewViewCoordConverter;
end;

function TWindowLayerWithPosBase.GetViewCoordConverter: ILocalCoordConverter;
begin
  FViewCoordConverterCS.BeginRead;
  try
    Result := FViewCoordConverter;
  finally
    FViewCoordConverterCS.EndRead;
  end;
end;

procedure TWindowLayerWithPosBase.DoViewUpdate;
begin
end;

procedure TWindowLayerWithPosBase.SetLayerCoordConverter(
  AValue: ILocalCoordConverter);
begin
  FLayerCoordConverterCS.BeginWrite;
  try
    FLayerCoordConverter := AValue;
  finally
    FLayerCoordConverterCS.EndWrite;
  end;
end;

procedure TWindowLayerWithPosBase.SetViewCoordConverter(
  AValue: ILocalCoordConverter);
begin
  FViewCoordConverterCS.BeginWrite;
  try
    FViewCoordConverter := AValue;
  finally
    FViewCoordConverterCS.EndWrite;
  end;
end;

procedure TWindowLayerWithPosBase.StartThreads;
begin
  inherited;
  OnViewPortPosChange;
end;

procedure TWindowLayerWithPosBase.ViewUpdate;
begin
  if FViewUpdateLock = 0 then begin
    DoViewUpdate;
  end;
end;

procedure TWindowLayerWithPosBase.ViewUpdateLock;
begin
  InterlockedIncrement(FViewUpdateLock);
end;

procedure TWindowLayerWithPosBase.ViewUpdateUnlock;
{$IFDEF DEBUG}
var
  VLockCount: Integer;
begin
  VLockCount := InterlockedDecrement(FViewUpdateLock);
  Assert(VLockCount >= 0);
end;
{$ELSE}
begin
  InterlockedDecrement(FViewUpdateLock);
end;
{$ENDIF}

{ TWindowLayerBasic }

constructor TWindowLayerBasic.Create(
  APerfList: IInternalPerformanceCounterList;
  ALayer: TPositionedLayer;
  AViewPortState: IViewPortState;
  AListenScaleChange: Boolean
);
begin
  inherited Create(APerfList, AViewPortState, AListenScaleChange);
  FLayer := ALayer;
  FRedrawCounter := PerfList.CreateAndAddNewCounter('Redraw');

  FLayer.MouseEvents := false;
  FLayer.Visible := false;
  FVisible := False;
  FNeedRedraw := True;
  FNeedRedrawCS := MakeSyncFake(Self);
end;

destructor TWindowLayerBasic.Destroy;
begin
  FNeedRedrawCS := nil;
  FLayer := nil;
  inherited;
end;

procedure TWindowLayerBasic.DoViewUpdate;
begin
  inherited;
  RedrawIfNeed;
  UpdateLayerLocationIfNeed;
end;

procedure TWindowLayerBasic.Hide;
begin
  ViewUpdateLock;
  try
    if FVisible then begin
      DoHide;
    end;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TWindowLayerBasic.DoHide;
begin
  FVisible := False;
  FLayer.Visible := False;
  SetNeedUpdateLocation;
  SetNeedRedraw;
end;

procedure TWindowLayerBasic.Show;
begin
  ViewUpdateLock;
  try
    if not Visible then begin
      DoShow;
    end;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TWindowLayerBasic.DoShow;
begin
  FVisible := True;
  FLayer.Visible := True;
  SetNeedUpdateLocation;
  SetNeedRedraw;
end;

procedure TWindowLayerBasic.DoUpdateLayerLocation(ANewLocation: TFloatRect);
begin
  FLayer.Location := ANewLocation;
end;

function TWindowLayerBasic.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TWindowLayerBasic.GetVisibleForNewPos(
  ANewVisualCoordConverter: ILocalCoordConverter): Boolean;
begin
  Result := FVisible;
end;

procedure TWindowLayerBasic.UpdateLayerLocation;
begin
  if FVisible then begin
    FNeedRedrawCS.BeginWrite;
    try
      FNeedUpdateLocation := False;
    finally
      FNeedRedrawCS.EndWrite;
    end;
    DoUpdateLayerLocation(GetMapLayerLocationRect);
  end;
end;

procedure TWindowLayerBasic.UpdateLayerLocationIfNeed;
var
  VNeed: Boolean;
begin
  VNeed := False;
  FNeedRedrawCS.BeginWrite;
  try
    if FNeedUpdateLocation then begin
      FNeedUpdateLocation := False;
      VNeed := True;
    end;
  finally
    FNeedRedrawCS.EndWrite;
  end;
  if VNeed then begin
    UpdateLayerLocation;
  end;
end;

procedure TWindowLayerBasic.Redraw;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if FVisible then begin
    FNeedRedrawCS.BeginWrite;
    try
      FNeedRedraw := False;
    finally
      FNeedRedrawCS.EndWrite;
    end;
    VCounterContext := FRedrawCounter.StartOperation;
    try
      DoRedraw;
    finally
      FRedrawCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

procedure TWindowLayerBasic.RedrawIfNeed;
var
  VNeed: Boolean;
begin
  VNeed := False;
  FNeedRedrawCS.BeginWrite;
  try
    if FNeedRedraw then begin
      FNeedRedraw := False;
      VNeed := True;
    end;
  finally
    FNeedRedrawCS.EndWrite;
  end;
  if VNeed then begin
    Redraw;
  end;
end;

procedure TWindowLayerBasic.SetLayerCoordConverter(
  AValue: ILocalCoordConverter);
begin
  SetVisible(GetVisibleForNewPos(AValue));
  inherited;
end;

procedure TWindowLayerBasic.SetNeedRedraw;
begin
  FNeedRedrawCS.BeginWrite;
  try
    FNeedRedraw := True;
  finally
    FNeedRedrawCS.EndWrite;
  end;
  SetNeedUpdateLocation;
end;

procedure TWindowLayerBasic.SetNeedUpdateLocation;
begin
  FNeedRedrawCS.BeginWrite;
  try
    FNeedUpdateLocation := True;
  finally
    FNeedRedrawCS.EndWrite;
  end;
end;

procedure TWindowLayerBasic.SetVisible(const Value: Boolean);
begin
  if Value then begin
    Show;
  end else begin
    Hide;
  end;
end;

{ TWindowLayerWithBitmap }

constructor TWindowLayerWithBitmap.Create(
  APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  AViewPortState: IViewPortState
);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(APerfList, FLayer, AViewPortState, True);

  FLayer.Bitmap.DrawMode := dmBlend;
end;

procedure TWindowLayerWithBitmap.DoViewUpdate;
begin
  UpdateLayerSizeIfNeed;
  inherited;
end;

procedure TWindowLayerWithBitmap.DoHide;
begin
  inherited;
  SetNeedUpdateLayerSize;
end;

procedure TWindowLayerWithBitmap.DoShow;
begin
  inherited;
  SetNeedUpdateLayerSize;
end;

procedure TWindowLayerWithBitmap.DoUpdateLayerSize(ANewSize: TPoint);
var
  VNedRedraw: Boolean;
begin
  FLayer.Bitmap.Lock;
  try
    VNedRedraw := FLayer.Bitmap.SetSize(ANewSize.X, ANewSize.Y);
  finally
    FLayer.Bitmap.Unlock;
  end;
  if VNedRedraw then begin
    SetNeedRedraw;
  end;
end;

procedure TWindowLayerWithBitmap.SetNeedUpdateLayerSize;
begin
  FNeedRedrawCS.BeginWrite;
  try
    FNeedUpdateLayerSize := True;
  finally
    FNeedRedrawCS.EndWrite;
  end;
end;

procedure TWindowLayerWithBitmap.UpdateLayerSize;
begin
  if FVisible then begin
    FNeedRedrawCS.BeginWrite;
    try
      FNeedUpdateLayerSize := False;
    finally
      FNeedRedrawCS.EndWrite;
    end;
    DoUpdateLayerSize(GetLayerSizeForView(LayerCoordConverter));
  end;
end;

procedure TWindowLayerWithBitmap.UpdateLayerSizeIfNeed;
var
  VNeed: Boolean;
begin
  VNeed := False;
  FNeedRedrawCS.BeginWrite;
  try
    if FNeedUpdateLayerSize then begin
      FNeedUpdateLayerSize := False;
      VNeed := True;
    end;
  finally
    FNeedRedrawCS.EndWrite;
  end;
  if VNeed then begin
    UpdateLayerSize;
  end;
end;

{ TWindowLayerFixedSizeWithBitmap }

procedure TWindowLayerFixedSizeWithBitmap.DoRedraw;
begin
  // По-умолчанию ничего не делаем.
end;

function TWindowLayerFixedSizeWithBitmap.GetLayerSizeForView(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
begin
  FLayer.Bitmap.Lock;
  try
    Result := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

end.
