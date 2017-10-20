{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_MapLayerCalcCircleCaptions;

interface

uses
  Types,
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_LineOnMapEdit,
  i_GeometryLonLat,
  i_Projection,
  i_ValueToStringConverter,
  i_PointCaptionsLayerConfig,
  u_MapLayerBasicNoBitmap;

type
  TMapLayerCalcCircleCaptions = class(TMapLayerBasicNoBitmap)
  private
    FConfig: IPointCaptionsLayerConfig;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FCircleOnMapEdit: ICircleOnMapEdit;
    FTempBitmap: TBitmap32;
    FLine: ILonLatPathWithSelected;
    procedure DrawPointText(
      ABuffer: TBitmap32;
      const ABitmapSize: TPoint;
      const AText: string;
      const ATextSize: TSize;
      const APosOnBitmap: TDoublePoint;
      const AFontSize: Integer;
      const ATextBGColor: TColor32;
      const ATextColor: TColor32
    );
    procedure OnConfigChange;
    procedure OnLineChange;
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
      const ACircleOnMapEdit: ICircleOnMapEdit;
      const AConfig: IPointCaptionsLayerConfig;
      const AValueToStringConverter: IValueToStringConverterChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_ProjectionType,
  u_ListenerByEvent,  
  u_ResStrings;

{ TMapLayerCalcLineCaptions }

constructor TMapLayerCalcCircleCaptions.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const ACircleOnMapEdit: ICircleOnMapEdit;
  const AConfig: IPointCaptionsLayerConfig;
  const AValueToStringConverter: IValueToStringConverterChangeable
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
  FValueToStringConverter := AValueToStringConverter;
  FCircleOnMapEdit := ACircleOnMapEdit;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FValueToStringConverter.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLineChange),
    FCircleOnMapEdit.GetChangeNotifier
  );
  FTempBitmap := TBitmap32.Create;
  FTempBitmap.Font.Size := 9;
end;

destructor TMapLayerCalcCircleCaptions.Destroy;
begin
  FreeAndNil(FTempBitmap);
  inherited;
end;

procedure TMapLayerCalcCircleCaptions.DrawPointText(
  ABuffer: TBitmap32;
  const ABitmapSize: TPoint;
  const AText: string;
  const ATextSize: TSize;
  const APosOnBitmap: TDoublePoint;
  const AFontSize: Integer;
  const ATextBGColor: TColor32;
  const ATextColor: TColor32
);
var
  VRect: TRect;
begin
  if (APosOnBitmap.X > 0) and (APosOnBitmap.X < ABitmapSize.X) and
     (APosOnBitmap.Y > 0) and (APosOnBitmap.Y < ABitmapSize.Y)
  then begin
    ABuffer.Font.Size := AFontSize;
    VRect.Left := Trunc(APosOnBitmap.X + 12);
    VRect.Top := Trunc(APosOnBitmap.Y);
    VRect.Right := VRect.Left + ATextSize.cx + 4;
    VRect.Bottom := VRect.Top + ATextSize.cy + 4;
    if ABuffer.MeasuringMode then begin
      ABuffer.Changed(VRect);
    end else begin
      ABuffer.FillRectTS(VRect, ATextBGColor);
      ABuffer.RenderText(VRect.Left + 2, VRect.Top + 2, AText, 3, ATextColor);
    end;
  end;
end;

procedure TMapLayerCalcCircleCaptions.OnConfigChange;
var
  VConfig: IPointCaptionsLayerConfigStatic;
begin
  ViewUpdateLock;
  try
    VConfig := FConfig.GetStatic;
    FTempBitmap.Font.Size := VConfig.LastPointFontSize;
    Visible := VConfig.Visible and Assigned(FLine);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerCalcCircleCaptions.OnLineChange;
begin
  ViewUpdateLock;
  try
    FLine := FCircleOnMapEdit.Path;
    if Assigned(FLine) then begin
      SetNeedRedraw;
      Visible := FConfig.Visible;
    end else begin
      Hide;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapLayerCalcCircleCaptions.PaintLayer(
  ABuffer: TBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VConfig: IPointCaptionsLayerConfigStatic;
  VProjection: IProjection;
  VPoints: PDoublePointArray;
  VLocalRect: TRect;
  VBitmapSize: TPoint;
  VPosOnMap: TDoublePoint;
  VPosOnBitmap: TDoublePoint;
  VText: string;
  VTextSize: TSize;
  VLine: IGeometryLonLatSingleLine;
  VLonLat: TDoublePoint;
  VValueConverter: IValueToStringConverter;
  VLonLatPath: ILonLatPathWithSelected;
begin
  inherited;

  VLonLatPath := FLine;
  if (VLonLatPath = nil) or (VLonLatPath.Count <= 1) then begin
    Exit;
  end;

  if Supports(VLonLatPath.Geometry, IGeometryLonLatSingleLine, VLine) then begin

    VPoints := VLine.Points;
    Inc(VPoints);
    VLonLat := VPoints[0];

    VProjection := ALocalConverter.Projection;
    VProjection.ProjectionType.ValidateLonLatPos(VLonLat);
    VPosOnMap := VProjection.LonLat2PixelPosFloat(VLonLat);
    VPosOnBitmap := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPosOnMap);

    VValueConverter := FValueToStringConverter.GetStatic;
    VText := SAS_STR_Radius + ': ' + VValueConverter.DistConvert(FCircleOnMapEdit.Radius);

    VTextSize := FTempBitmap.TextExtent(VText);

    VLocalRect := ALocalConverter.GetLocalRect;
    VBitmapSize.X := VLocalRect.Right - VLocalRect.Left;
    VBitmapSize.Y := VLocalRect.Bottom - VLocalRect.Top;

    VConfig := FConfig.GetStatic;

    DrawPointText(
      ABuffer,
      VBitmapSize,
      VText,
      VTextSize,
      VPosOnBitmap,
      VConfig.LastPointFontSize,
      VConfig.TextBGColor,
      VConfig.TextColor
    );
  end;
end;

procedure TMapLayerCalcCircleCaptions.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
