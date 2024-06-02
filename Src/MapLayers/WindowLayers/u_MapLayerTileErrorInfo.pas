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

unit u_MapLayerTileErrorInfo;

interface

uses
  Windows,
  SysUtils,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierTime,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_Bitmap32BufferFactory,
  i_TileError,
  i_SimpleFlag,
  i_MarkerDrawable,
  i_TileErrorLogProviedrStuped,
  i_MapType,
  i_MapTypeSet,
  i_MainFormState,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerTileErrorInfo = class(TMapLayerBasicNoBitmap)
  private
    FMainFormState: IMainFormState;
    FLogProvider: ITileErrorLogProviedrStuped;
    FBitmapFactory: IBitmap32StaticFactory;
    FMapsSet: IMapTypeSet;
    FNeedUpdateFlag: ISimpleFlag;

    FErrorInfo: ITileErrorInfo;
    FHideAfterTime: Cardinal;
    FMarker: IMarkerDrawable;

    FIsValid: Boolean;
    FRect: TRect;
    FPos: TDoublePoint;

    function CreateMarkerByError(
      const AMapType: IMapType;
      const AErrorInfo: ITileErrorInfo
    ): IMarkerDrawable;

    procedure OnTimer;
    procedure OnErrorRecive;
  protected
    procedure InvalidateLayer(const ALocalConverter: ILocalCoordConverter); override;
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure DoHide; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AMainFormState: IMainFormState;
      const AMapsSet: IMapTypeSet;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ALogProvider: ITileErrorLogProviedrStuped;
      const AGuiSyncronizedTimerNotifier: INotifierTime
    );
  end;

implementation

uses
  Types,
  c_ZeroGUID,
  i_Projection,
  i_Bitmap32Static,
  u_ListenerByEvent,
  u_ListenerTime,
  u_SimpleFlagWithInterlock,
  u_MarkerDrawableByBitmap32Static,
  u_Synchronizer,
  u_GeoFunc;

{ TMapLayerTileErrorInfo }

constructor TMapLayerTileErrorInfo.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AMainFormState: IMainFormState;
  const AMapsSet: IMapTypeSet;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ALogProvider: ITileErrorLogProviedrStuped;
  const AGuiSyncronizedTimerNotifier: INotifierTime
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );

  FMainFormState := AMainFormState;
  FLogProvider := ALogProvider;
  FMapsSet := AMapsSet;
  FBitmapFactory := ABitmapFactory;

  FErrorInfo := nil;
  FNeedUpdateFlag := TSimpleFlagWithInterlock.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnErrorRecive),
    FLogProvider.GetNotifier
  );
  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 1000),
    AGuiSyncronizedTimerNotifier
  );
end;

function TMapLayerTileErrorInfo.CreateMarkerByError(
  const AMapType: IMapType;
  const AErrorInfo: ITileErrorInfo
): IMarkerDrawable;
var
  VText: string;
  VSize: TPoint;
  VMapNameSize: TSize;
  VMessageSize: TSize;
  VBitmap: TBitmap32;
  VBitmapStatic: IBitmap32Static;
begin
  Result := nil;
  if AErrorInfo <> nil then begin
    VBitmap := TBitmap32.Create;
    try
      VBitmap.CombineMode := cmMerge;
      if AMapType <> nil then begin
        VText := AMapType.GUIConfig.Name.Value;
        VMapNameSize := VBitmap.TextExtent(VText);
        VSize.X := VMapNameSize.cx;
        VSize.Y := VMapNameSize.cy + 20;
        VMessageSize := VBitmap.TextExtent(AErrorInfo.ErrorText);
        if VSize.X < VMessageSize.cx then begin
          VSize.X := VMessageSize.cx;
        end;
        Inc(VSize.Y, VMessageSize.cy + 20);
        Inc(VSize.X, 20);
        VBitmap.SetSize(VSize.X, VSize.Y);
        VBitmap.Clear(0);

        VBitmap.RenderText((VSize.X - VMapNameSize.cx) div 2, 10, VText, clBlack32, False);
        VBitmap.RenderText((VSize.X - VMessageSize.cx) div 2, 30 + VMapNameSize.cy, AErrorInfo.ErrorText, clBlack32, False);
      end else begin
        VMessageSize := VBitmap.TextExtent(AErrorInfo.ErrorText);
        VSize.X := VMessageSize.cx + 20;
        VSize.Y := VMessageSize.cy + 20;

        VBitmap.SetSize(VSize.X, VSize.Y);
        VBitmap.Clear(0);

        VBitmap.RenderText((VSize.X - VMessageSize.cx) div 2, 10, AErrorInfo.ErrorText, clBlack32, False);
      end;
      VBitmapStatic := FBitmapFactory.Build(VSize, VBitmap.Bits);
    finally
      VBitmap.Free;
    end;
    Result := TMarkerDrawableByBitmap32Static.Create(VBitmapStatic, DoublePoint(VSize.X / 2, VSize.Y / 2));
  end;
end;

procedure TMapLayerTileErrorInfo.DoHide;
begin
  inherited DoHide;
  FHideAfterTime := 0;
  FErrorInfo := nil;
  FMarker := nil;
end;

procedure TMapLayerTileErrorInfo.OnErrorRecive;
begin
  FNeedUpdateFlag.SetFlag;
end;

procedure TMapLayerTileErrorInfo.OnTimer;
var
  VCurrTime: Cardinal;
  VNeedHide: Boolean;
  VErrorInfo: ITileErrorInfo;
begin
  VErrorInfo := nil;
  if FNeedUpdateFlag.CheckFlagAndReset then begin
    VErrorInfo := FLogProvider.GetLastErrorInfo;
  end;
  if VErrorInfo <> nil then begin
    ViewUpdateLock;
    try
      FErrorInfo := VErrorInfo;
      FHideAfterTime := GetTickCount + 10000;
      SetNeedRedraw;
      Show;
    finally
      ViewUpdateUnlock;
    end;
  end else
  if Visible then begin
    ViewUpdateLock;
    try
      VCurrTime := GetTickCount;
      VNeedHide := False;
      if (FHideAfterTime = 0) or (FErrorInfo = nil) or (VCurrTime >= FHideAfterTime) then begin
        VNeedHide := True;
        FHideAfterTime := 0;
        FErrorInfo := nil;
      end;
      if VNeedHide then begin
        FMarker := nil;
        Hide;
      end;
    finally
      ViewUpdateUnlock;
    end;
  end;
end;

procedure TMapLayerTileErrorInfo.InvalidateLayer(const ALocalConverter: ILocalCoordConverter);
var
  VFixedOnView: TDoublePoint;
  VProjection: IProjection;
  VGUID: TGUID;
  VMapType: IMapType;
  VTile: TPoint;
  VFixedLonLat: TDoublePoint;
begin
  if FIsValid then begin
    FIsValid := False;
    DoInvalidateRect(FRect); // erase
  end;

  if not Visible then begin
    Exit;
  end;

  if FErrorInfo <> nil then begin
    VGUID := FErrorInfo.MapTypeGUID;
    VMapType := nil;
    if not IsEqualGUID(VGUID, CGUID_Zero) then begin
      VMapType := FMapsSet.GetMapTypeByGUID(VGUID);
    end;
    VProjection := VMapType.ProjectionSet.Zooms[FErrorInfo.Zoom];
    VTile := FErrorInfo.Tile;
    VProjection.ValidateTilePosStrict(VTile, True);
    VFixedLonLat := VProjection.PixelPosFloat2LonLat(RectCenter(VProjection.TilePos2PixelRect(VTile)));
    ALocalConverter.Projection.ProjectionType.ValidateLonLatPos(VFixedLonLat);
    VFixedOnView := ALocalConverter.LonLat2LocalPixelFloat(VFixedLonLat);
    if PixelPointInRect(VFixedOnView, DoubleRect(ALocalConverter.GetLocalRect)) then begin
      if FMarker = nil then begin
        FMarker := CreateMarkerByError(VMapType, FErrorInfo);
      end;
      if FMarker <> nil then begin
        FPos := ALocalConverter.LonLat2LocalPixelFloat(VFixedLonLat);
        FRect := FMarker.GetBoundsForPosition(FPos);
        FIsValid := not GR32.IsRectEmpty(FRect);
        if not FIsValid then begin
          Exit;
        end;

        // draw
        if FMainFormState.IsMapMoving then begin
          DoInvalidateFull;
        end else begin
          DoInvalidateRect(FRect);
        end;
      end;
    end;
  end;
end;

procedure TMapLayerTileErrorInfo.PaintLayer(ABuffer: TBitmap32);
begin
  if FIsValid then begin
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(FRect);
    end else begin
      FMarker.DrawToBitmap(ABuffer, FPos);
    end;
  end;
end;

end.
