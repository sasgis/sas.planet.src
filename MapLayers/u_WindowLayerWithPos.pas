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
  i_SimpleFlag,
  i_InternalPerformanceCounter,
  u_WindowLayerBasic;

type
  TWindowLayerWithPosBase = class(TWindowLayerAbstract)
  private
    FViewPortState: IViewPortState;

    FViewCoordConverter: ILocalCoordConverter;
    FViewCoordConverterCS: IReadWriteSync;

    FLayerCoordConverter: ILocalCoordConverter;
    FLayerCoordConverterCS: IReadWriteSync;

    FViewUpdateLock: Integer;
    procedure OnViewPortPosChange;
    procedure OnViewPortScaleChange;
    function GetLayerCoordConverter: ILocalCoordConverter;
    function GetViewCoordConverter: ILocalCoordConverter;

    procedure ScaleChange(const ANewVisualCoordConverter: ILocalCoordConverter);
  protected
    procedure SetViewCoordConverter(const AValue: ILocalCoordConverter); virtual;
    procedure SetLayerCoordConverter(const AValue: ILocalCoordConverter); virtual;

    function GetLayerCoordConverterByViewConverter(const ANewViewCoordConverter: ILocalCoordConverter): ILocalCoordConverter; virtual;

    procedure PosChange(const ANewVisualCoordConverter: ILocalCoordConverter);

    procedure ViewUpdateLock;
    procedure ViewUpdateUnlock;
    procedure DoViewUpdate; virtual;

    property ViewCoordConverter: ILocalCoordConverter read GetViewCoordConverter;
    property LayerCoordConverter: ILocalCoordConverter read GetLayerCoordConverter;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AViewPortState: IViewPortState;
      AListenScaleChange: Boolean
    );
    destructor Destroy; override;

    procedure StartThreads; override;
  end;

  TWindowLayerBasic = class(TWindowLayerWithPosBase)
  private
    FVisible: Boolean;
    FLayer: TCustomLayer;

    FNeedRedrawFlag: ISimpleFlag;
    FRedrawCounter: IInternalPerformanceCounter;
  protected
    function GetVisible: Boolean; virtual;
    procedure SetVisible(const Value: Boolean);

    procedure SetNeedRedraw; virtual;

    function GetVisibleForNewPos(const ANewVisualCoordConverter: ILocalCoordConverter): Boolean; virtual;

    procedure Show; virtual;
    procedure DoShow; virtual;
    procedure Hide; virtual;
    procedure DoHide; virtual;
    procedure DoRedraw; virtual; abstract;
    procedure RedrawIfNeed; virtual;

    property Layer: TCustomLayer read FLayer;
    property Visible: Boolean read GetVisible write SetVisible;
  protected
    procedure SetLayerCoordConverter(const AValue: ILocalCoordConverter); override;
    procedure DoViewUpdate; override;
    procedure Redraw; virtual;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      ALayer: TCustomLayer;
      const AViewPortState: IViewPortState;
      AListenScaleChange: Boolean
    );
    destructor Destroy; override;
  end;

  TWindowLayerWithBitmap = class(TWindowLayerBasic)
  private
    FNeedUpdateLayerSizeFlag: ISimpleFlag;
    FNeedUpdateLocationFlag: ISimpleFlag;
    FLayer: TBitmapLayer;
  protected
    property Layer: TBitmapLayer read FLayer;

    procedure SetNeedUpdateLayerSize; virtual;

    procedure UpdateLayerSize; virtual;
    procedure UpdateLayerSizeIfNeed; virtual;
    procedure DoUpdateLayerSize(const ANewSize: TPoint); virtual;
    function GetLayerSizeForView(const ANewVisualCoordConverter: ILocalCoordConverter): TPoint; virtual; abstract;
  protected
    function GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect; virtual; abstract;
    procedure UpdateLayerLocationIfNeed; virtual;
    procedure UpdateLayerLocation; virtual;
    procedure DoUpdateLayerLocation(const ANewLocation: TFloatRect); virtual;
  protected
    procedure SetNeedUpdateLocation;
    procedure SetNeedRedraw; override;
    procedure SetViewCoordConverter(const AValue: ILocalCoordConverter); override;
    procedure DoHide; override;
    procedure DoShow; override;
    procedure DoViewUpdate; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState
    );
  end;

  TWindowLayerFixedSizeWithBitmap = class(TWindowLayerWithBitmap)
  protected
    function GetLayerSizeForView(const ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
    procedure DoRedraw; override;
  end;

implementation

uses
  Types,
  u_Synchronizer,
  u_SimpleFlagWithInterlock,
  u_NotifyEventListener;

{ TWindowLayerWithPosBase }

constructor TWindowLayerWithPosBase.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AViewPortState: IViewPortState;
  AListenScaleChange: Boolean
);
begin
  inherited Create(APerfList);
  FViewUpdateLock := 0;
  FViewPortState := AViewPortState;

  FViewCoordConverterCS := MakeSyncRW_Var(Self);
  FLayerCoordConverterCS := MakeSyncRW_Var(Self);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnViewPortPosChange),
    FViewPortState.GetChangeNotifier
  );
  if AListenScaleChange then begin
    LinksList.Add(
      TNotifyNoMmgEventListener.Create(Self.OnViewPortScaleChange),
      FViewPortState.ScaleChangeNotifier
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
  const ANewVisualCoordConverter: ILocalCoordConverter
);
begin
  ViewUpdateLock;
  try
    SetViewCoordConverter(ANewVisualCoordConverter);
    SetLayerCoordConverter(GetLayerCoordConverterByViewConverter(ANewVisualCoordConverter));
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerWithPosBase.OnViewPortScaleChange;
begin
  ScaleChange(FViewPortState.GetVisualCoordConverter);
end;

procedure TWindowLayerWithPosBase.ScaleChange(
  const ANewVisualCoordConverter: ILocalCoordConverter
);
begin
  ViewUpdateLock;
  try
    SetViewCoordConverter(ANewVisualCoordConverter);
  finally
    ViewUpdateUnlock;
  end;
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
  const ANewViewCoordConverter: ILocalCoordConverter
): ILocalCoordConverter;
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
  const AValue: ILocalCoordConverter
);
begin
  FLayerCoordConverterCS.BeginWrite;
  try
    FLayerCoordConverter := AValue;
  finally
    FLayerCoordConverterCS.EndWrite;
  end;
end;

procedure TWindowLayerWithPosBase.SetViewCoordConverter(
  const AValue: ILocalCoordConverter
);
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

procedure TWindowLayerWithPosBase.ViewUpdateLock;
begin
  InterlockedIncrement(FViewUpdateLock);
end;

procedure TWindowLayerWithPosBase.ViewUpdateUnlock;
var
  VLockCount: Integer;
begin
  VLockCount := InterlockedDecrement(FViewUpdateLock);
  Assert(VLockCount >= 0);
  if VLockCount = 0 then begin
    DoViewUpdate;
  end;
end;

{ TWindowLayerBasic }

constructor TWindowLayerBasic.Create(
  const APerfList: IInternalPerformanceCounterList;
  ALayer: TCustomLayer;
  const AViewPortState: IViewPortState;
  AListenScaleChange: Boolean
);
begin
  inherited Create(APerfList, AViewPortState, AListenScaleChange);
  FLayer := ALayer;
  FRedrawCounter := PerfList.CreateAndAddNewCounter('Redraw');

  FLayer.MouseEvents := false;
  FLayer.Visible := false;
  FVisible := False;
  FNeedRedrawFlag := TSimpleFlagWithInterlock.Create;
end;

destructor TWindowLayerBasic.Destroy;
begin
  FLayer := nil;
  inherited;
end;

procedure TWindowLayerBasic.DoViewUpdate;
begin
  inherited;
  RedrawIfNeed;
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
end;

procedure TWindowLayerBasic.DoHide;
begin
  FVisible := False;
  FLayer.Visible := False;
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
end;

procedure TWindowLayerBasic.DoShow;
begin
  FVisible := True;
  FLayer.Visible := True;
  SetNeedRedraw;
end;

function TWindowLayerBasic.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TWindowLayerBasic.GetVisibleForNewPos(
  const ANewVisualCoordConverter: ILocalCoordConverter
): Boolean;
begin
  Result := FVisible;
end;

procedure TWindowLayerBasic.Redraw;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if FVisible then begin
    FNeedRedrawFlag.CheckFlagAndReset;
    VCounterContext := FRedrawCounter.StartOperation;
    try
      DoRedraw;
    finally
      FRedrawCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

procedure TWindowLayerBasic.RedrawIfNeed;
begin
  if FNeedRedrawFlag.CheckFlagAndReset then begin
    Redraw;
  end;
end;

procedure TWindowLayerBasic.SetLayerCoordConverter(
  const AValue: ILocalCoordConverter
);
begin
  SetVisible(GetVisibleForNewPos(AValue));
  inherited;
end;

procedure TWindowLayerBasic.SetNeedRedraw;
begin
  FNeedRedrawFlag.SetFlag;
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
  const APerfList: IInternalPerformanceCounterList;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState
);
begin
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  inherited Create(APerfList, FLayer, AViewPortState, True);

  FNeedUpdateLayerSizeFlag := TSimpleFlagWithInterlock.Create;
  FNeedUpdateLocationFlag := TSimpleFlagWithInterlock.Create;
  FLayer.Bitmap.DrawMode := dmBlend;
end;

procedure TWindowLayerWithBitmap.DoViewUpdate;
begin
  UpdateLayerSizeIfNeed;
  inherited;
  UpdateLayerLocationIfNeed;
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

procedure TWindowLayerWithBitmap.DoUpdateLayerLocation(
  const ANewLocation: TFloatRect
);
begin
  FLayer.Location := ANewLocation;
end;

procedure TWindowLayerWithBitmap.DoUpdateLayerSize(const ANewSize: TPoint);
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

procedure TWindowLayerWithBitmap.SetNeedRedraw;
begin
  inherited;
  SetNeedUpdateLocation;
end;

procedure TWindowLayerWithBitmap.SetNeedUpdateLayerSize;
begin
  FNeedUpdateLayerSizeFlag.SetFlag;
end;

procedure TWindowLayerWithBitmap.SetNeedUpdateLocation;
begin
  FNeedUpdateLocationFlag.SetFlag;
end;

procedure TWindowLayerWithBitmap.SetViewCoordConverter(
  const AValue: ILocalCoordConverter
);
var
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := ViewCoordConverter;
  if (VLocalConverter = nil) or (not VLocalConverter.GetIsSameConverter(AValue)) then begin
    SetNeedUpdateLocation;
  end;
  inherited;
end;

procedure TWindowLayerWithBitmap.UpdateLayerLocation;
begin
  if Visible then begin
    FNeedUpdateLocationFlag.CheckFlagAndReset;
    DoUpdateLayerLocation(GetMapLayerLocationRect(ViewCoordConverter));
  end;
end;

procedure TWindowLayerWithBitmap.UpdateLayerLocationIfNeed;
begin
  if FNeedUpdateLocationFlag.CheckFlagAndReset then begin
    UpdateLayerLocation;
  end;
end;

procedure TWindowLayerWithBitmap.UpdateLayerSize;
begin
  if FVisible then begin
    FNeedUpdateLayerSizeFlag.CheckFlagAndReset;
    DoUpdateLayerSize(GetLayerSizeForView(LayerCoordConverter));
  end;
end;

procedure TWindowLayerWithBitmap.UpdateLayerSizeIfNeed;
begin
  if FNeedUpdateLayerSizeFlag.CheckFlagAndReset then begin
    UpdateLayerSize;
  end;
end;

{ TWindowLayerFixedSizeWithBitmap }

procedure TWindowLayerFixedSizeWithBitmap.DoRedraw;
begin
  // По-умолчанию ничего не делаем.
end;

function TWindowLayerFixedSizeWithBitmap.GetLayerSizeForView(
  const ANewVisualCoordConverter: ILocalCoordConverter
): TPoint;
begin
  FLayer.Bitmap.Lock;
  try
    Result := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  finally
    FLayer.Bitmap.Unlock;
  end;
end;

end.
