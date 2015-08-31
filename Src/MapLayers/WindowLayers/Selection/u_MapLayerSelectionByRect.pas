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

unit u_MapLayerSelectionByRect;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_SelectionRect,
  i_SelectionRectLayerConfig,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerSelectionByRect = class(TMapLayerBasicNoBitmap)
  private
    FConfig: ISelectionRectLayerConfig;
    FSelection: ISelectionRect;
    FSelectedLonLat: TDoubleRect;

    FFillColor: TColor32;
    FBorderColor: TColor32;
    FFontSize: Integer;
    FZoomDeltaColors: TArrayOfColor32;

    procedure OnSelectionChange;
    procedure OnConfigChange;
  protected
    procedure PaintLayer(
      ABuffer: TBitmap32;
      const ALocalConverter: ILocalCoordConverter
    ); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const ASelection: ISelectionRect;
      const AConfig: ISelectionRectLayerConfig
    );
  end;


implementation

uses
  SysUtils,
  Types,
  Math,
  i_CoordConverter,
  u_ListenerByEvent,
  u_GeoFunc;

{ TSelectionRectLayer }

constructor TMapLayerSelectionByRect.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const ASelection: ISelectionRect;
  const AConfig: ISelectionRectLayerConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AView
  );
  FConfig := AConfig;
  FSelection := ASelection;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSelectionChange),
    FSelection.GetChangeNotifier
  );
end;

procedure TMapLayerSelectionByRect.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      FFillColor := FConfig.FillColor;
      FBorderColor := FConfig.BorderColor;
      FFontSize := FConfig.FontSize;
      FZoomDeltaColors := FConfig.ZoomDeltaColors;
    finally
      FConfig.UnlockRead;
    end;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerSelectionByRect.OnSelectionChange;
begin
  ViewUpdateLock;
  try
    if FSelection.IsEmpty then begin
      Hide;
    end else begin
      FSelectedLonLat := FSelection.GetRect;
      SetNeedRedraw;
      Show;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerSelectionByRect.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  jj: integer;
  VDrawRect: TRect;
  VSelectedLonLat: TDoubleRect;
  VSelectedPixels: TDoubleRect;
  VZoomDelta: Byte;
  VColor: TColor32;
  VSelectedRelative: TDoubleRect;
  VSelectedTilesFloat: TDoubleRect;
  VSelectedTiles: TRect;
  VMaxZoomDelta: Integer;
  VGeoConvert: ICoordConverter;
  VZoom: Byte;
begin
  VGeoConvert := ALocalConverter.GeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VSelectedLonLat := FSelectedLonLat;
  VGeoConvert.ValidateLonLatRect(VSelectedLonLat);
  VSelectedRelative := VGeoConvert.LonLatRect2RelativeRect(VSelectedLonLat);
  VSelectedPixels := VGeoConvert.RelativeRect2PixelRectFloat(VSelectedRelative, VZoom);
  VDrawRect :=
    RectFromDoubleRect(
      ALocalConverter.MapRectFloat2LocalRectFloat(VSelectedPixels),
      rrToTopLeft
    );
  ABuffer.FillRectTS(
    VDrawRect.Left,
    VDrawRect.Top,
    VDrawRect.Right,
    VDrawRect.Bottom,
    FFillColor
  );
  ABuffer.FrameRectTS(
    VDrawRect.Left,
    VDrawRect.Top,
    VDrawRect.Right,
    VDrawRect.Bottom,
    FBorderColor
  );
  ABuffer.FrameRectTS(
    VDrawRect.Left - 1,
    VDrawRect.Top - 1,
    VDrawRect.Right + 1,
    VDrawRect.Bottom + 1,
    FBorderColor
  );

  jj := VZoom;
  VZoomDelta := 0;
  VMaxZoomDelta := Length(FZoomDeltaColors) - 1;
  while (VZoomDelta <= VMaxZoomDelta) and (jj < 24) do begin
    VSelectedTilesFloat := VGeoConvert.RelativeRect2TileRectFloat(VSelectedRelative, jj);
    VSelectedTiles := RectFromDoubleRect(VSelectedTilesFloat, rrOutside);
    VSelectedPixels :=
      VGeoConvert.RelativeRect2PixelRectFloat(
        VGeoConvert.TileRect2RelativeRect(VSelectedTiles, jj),
        VZoom
      );
    VDrawRect :=
      RectFromDoubleRect(
        ALocalConverter.MapRectFloat2LocalRectFloat(VSelectedPixels),
        rrToTopLeft
      );
    VColor := FZoomDeltaColors[VZoomDelta];

    ABuffer.FrameRectTS(
      VDrawRect.Left - (VZoomDelta + 1),
      VDrawRect.Top - (VZoomDelta + 1),
      VDrawRect.Right + (VZoomDelta + 1),
      VDrawRect.Bottom + (VZoomDelta + 1),
      VColor
    );

    ABuffer.Font.Size := FFontSize;
    ABuffer.RenderText(
      VDrawRect.Right - ((VDrawRect.Right - VDrawRect.Left) div 2) - 42 + VZoomDelta * 26,
      VDrawRect.Bottom - ((VDrawRect.Bottom - VDrawRect.Top) div 2) - 6,
      'z' + inttostr(jj + 1), 3, VColor
    );
    Inc(jj);
    Inc(VZoomDelta);
  end;
end;

procedure TMapLayerSelectionByRect.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
