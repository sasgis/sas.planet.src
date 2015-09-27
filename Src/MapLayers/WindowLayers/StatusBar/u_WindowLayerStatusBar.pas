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

unit u_WindowLayerStatusBar;

interface

uses
  Windows,
  Types,
  Controls,
  Classes,
  GR32,
  GR32_Image,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_InternalPerformanceCounter,
  i_StatBarConfig,
  i_MouseState,
  i_MapType,
  i_ValueToStringConverter,
  i_DownloadInfoSimple,
  i_GlobalInternetState,
  i_LanguageManager,
  i_TerrainInfo,
  i_TerrainConfig,
  i_TerrainProviderList,
  i_PopUp,
  u_TimeZoneInfo,
  u_WindowLayerWithBitmapBase;

type
  TWindowLayerStatusBar = class(TWindowLayerWithBitmapBase)
  private
    FConfig: IStatBarConfig;
    FMainMap: IMapTypeChangeable;
    FDownloadInfo: IDownloadInfoSimple;
    FGlobalInternetState: IGlobalInternetState;
    FMouseState: IMouseState;
    FTimeZoneInfo: TTimeZoneInfo;
    FTerrainInfo: ITerrainInfo;
    FTerrainConfig: ITerrainConfig;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FView: ILocalCoordConverterChangeable;
    FPopupMenu: IPopUp;
    FLastUpdateTick: DWORD;
    FMinUpdate: Cardinal;
    FBgColor: TColor32;
    FTextColor: TColor32;
    FAALevel: Integer;
    procedure OnConfigChange;
    procedure OnTimerEvent;
    procedure OnPosChange;
    procedure OnMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
  protected
    function GetNewBitmapSize: TPoint; override;
    function GetNewLayerLocation: TFloatRect; override;
    procedure DoUpdateBitmapDraw; override;
    procedure DoUpdateLayerVisibility; override;
    procedure StartThreads; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AView: ILocalCoordConverterChangeable;
      const AConfig: IStatBarConfig;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AMouseState: IMouseState;
      const ATimerNoifier: INotifierTime;
      const ATerrainProviderList: ITerrainProviderList;
      const ATerrainConfig: ITerrainConfig;
      const ADownloadInfo: IDownloadInfoSimple;
      const AGlobalInternetState: IGlobalInternetState;
      const APopupMenu: IPopUp;
      const AMainMap: IMapTypeChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Math,
  StrUtils,
  Graphics,
  GR32_Layers,
  i_Projection,
  u_ListenerTime,
  u_ListenerByEvent,
  u_ResStrings,
  u_GeoFunc,
  u_TerrainInfo;

const
  // Константа для преобразования градусов в радианы
  D2R: Double = 0.017453292519943295769236907684886;

{ TWindowLayerStatusBar }

constructor TWindowLayerStatusBar.Create(
  const ALanguageManager: ILanguageManager;
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AConfig: IStatBarConfig;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AMouseState: IMouseState;
  const ATimerNoifier: INotifierTime;
  const ATerrainProviderList: ITerrainProviderList;
  const ATerrainConfig: ITerrainConfig;
  const ADownloadInfo: IDownloadInfoSimple;
  const AGlobalInternetState: IGlobalInternetState;
  const APopupMenu: IPopUp;
  const AMainMap: IMapTypeChangeable
);
begin
  Assert(Assigned(APopupMenu));
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TBitmapLayer.Create(AParentMap.Layers)
  );
  FConfig := AConfig;
  FTerrainConfig := ATerrainConfig;
  FGlobalInternetState := AGlobalInternetState;
  FValueToStringConverter := AValueToStringConverter;
  FPopupMenu := APopupMenu;

  FTimeZoneInfo := TTimeZoneInfo.Create;
  FConfig.TimeZoneInfoAvailable := FTimeZoneInfo.Available;

  FTerrainInfo := TTerrainInfo.Create(FTerrainConfig, ATerrainProviderList);

  FDownloadInfo := ADownloadInfo;
  FMouseState := AMouseState;
  FView := AView;

  Layer.OnMouseDown := OnMouseDown;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimerEvent, FConfig.MinUpdateTickCount),
    ATimerNoifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FView.ChangeNotifier
  );
  FMainMap := AMainMap;
  FLastUpdateTick := 0;
end;

destructor TWindowLayerStatusBar.Destroy;
begin
  FreeAndNil(FTimeZoneInfo);
  inherited;
end;

function TWindowLayerStatusBar.GetNewBitmapSize: TPoint;
begin
  Result.X := FView.GetStatic.GetLocalRectSize.X;
  Result.Y := FConfig.Height;
end;

function TWindowLayerStatusBar.GetNewLayerLocation: TFloatRect;
var
  VLocalCoordConverter: ILocalCoordConverter;
begin
  VLocalCoordConverter := FView.GetStatic;
  if VLocalCoordConverter <> nil then begin
    Result.Left := 0;
    Result.Bottom := VLocalCoordConverter.GetLocalRectSize.Y;
    Result.Right := Result.Left + Layer.Bitmap.Width;
    Result.Top := Result.Bottom - Layer.Bitmap.Height;
  end else begin
    Result.Left := 0;
    Result.Bottom := 0;
    Result.Right := 0;
    Result.Top := 0;
  end;
end;

procedure TWindowLayerStatusBar.OnConfigChange;

  procedure SetValidFontSize(
    AFont: TFont;
    ASize: Integer;
    AMaxHeight: Integer
  );
  begin
    if abs(AFont.Height) < AMaxHeight then begin
      AFont.Size := ASize;
    end;
    while abs(AFont.Height) >= AMaxHeight do begin
      AFont.Size := AFont.Size - 1;
    end;
  end;

var
  VVisible: Boolean;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      Layer.Bitmap.Font.Name := FConfig.FontName;
      SetValidFontSize(Layer.Bitmap.Font, FConfig.FontSize, (FConfig.Height - 2));

      FMinUpdate := FConfig.MinUpdateTickCount;
      FBgColor := FConfig.BgColor;
      FTextColor := FConfig.TextColor;
      VVisible := FConfig.Visible;
      FAALevel := 0;
    finally
      FConfig.UnlockRead;
    end;
    Visible := VVisible;
    SetNeedUpdateBitmapSize;
    SetNeedUpdateBitmapDraw;
    SetNeedUpdateLayerVisibility;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerStatusBar.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateBitmapSize;
    SetNeedUpdateLayerLocation;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerStatusBar.OnTimerEvent;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateBitmapDraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TWindowLayerStatusBar.OnMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  if Button = mbRight then begin
    FPopupMenu.PopUp;
  end;
end;

procedure TWindowLayerStatusBar.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

procedure TWindowLayerStatusBar.DoUpdateLayerVisibility;
begin
  inherited;
  Layer.MouseEvents := Visible;
end;

procedure TWindowLayerStatusBar.DoUpdateBitmapDraw;

  procedure RenderText(
  const AOffset: TPoint;
  const AText: string;
    ADrawLine: Boolean = True
  );
  begin
    Layer.Bitmap.RenderText(AOffset.X, AOffset.Y, AText, FAALevel, FTextColor);
    if ADrawLine then begin
      Layer.Bitmap.LineS(AOffset.X - 10, 0, AOffset.X - 10, Layer.Bitmap.Height, SetAlpha(clBlack32, 125));
    end;
  end;

var
  VLonLat: TDoublePoint;
  VString: string;
  VMapPoint: TDoublePoint;
  VSize: TPoint;
  VRad: Extended;
  VTile: TPoint;
  VMapType: IMapType;
  VProjection: IProjection;
  VMapProjection: IProjection;
  VPixelsAtZoom: Double;
  VCurrentTick: DWORD;
  VMousePos: TPoint;
  VVisualCoordConverter: ILocalCoordConverter;
  VValueConverter: IValueToStringConverter;
  VOffset: TPoint;
  VTileName: string;
  VShortTileName: string;
  VTileNameWidth: Integer;
  VTileNameWidthAviable: Integer;
  VNeedSeparator: Boolean;
begin
  inherited;
  VCurrentTick := GetTickCount;
  if (VCurrentTick < FLastUpdateTick) or (VCurrentTick > FLastUpdateTick + FMinUpdate) then begin
    VValueConverter := FValueToStringConverter.GetStatic;
    VVisualCoordConverter := FView.GetStatic;
    VMousePos := FMouseState.CurentPos;
    VProjection := VVisualCoordConverter.Projection;
    VSize := Types.Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
    VMapType := FMainMap.GetStatic;

    VMapPoint := VVisualCoordConverter.LocalPixel2MapPixelFloat(VMousePos);
    VProjection.ValidatePixelPosFloatStrict(VMapPoint, True);
    VLonLat := VProjection.PixelPosFloat2LonLat(VMapPoint);

    Layer.Bitmap.Clear(FBgColor);
    Layer.Bitmap.LineS(0, 0, VSize.X, 0, SetAlpha(clBlack32, 255));

    VOffset.Y := 1;
    VOffset.X := -10;
    VString := '';
    VNeedSeparator := False;

    if FConfig.ViewZoomInfo then begin
      VOffset.X := VOffset.X + 20;
      VString := 'z' + inttostr(VProjection.Zoom + 1);
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.ViewLonLatInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VString := VValueConverter.LonLatConvert(VLonLat);
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.ViewMetrPerPixInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VRad := VProjection.ProjectionType.Datum.GetSpheroidRadiusA;
      VPixelsAtZoom := VProjection.GetPixelsFloat;
      VString := VValueConverter.DistPerPixelConvert(1 / ((VPixelsAtZoom / (2 * PI)) / (VRad * cos(VLonLat.y * D2R))));
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FTerrainConfig.ShowInStatusBar and FTerrainConfig.ElevationInfoAvailable then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VString := FTerrainInfo.GetElevationInfoStr(VLonLat, VProjection.Zoom);
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.TimeZoneInfoAvailable and FConfig.ViewTimeZoneTimeInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VString := FTimeZoneInfo.GetStatusBarTzInfo(VLonLat);
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.ViewDownloadedInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VString := SAS_STR_load + ' ' +
        inttostr(FDownloadInfo.TileCount) +
        ' (' + VValueConverter.DataSizeConvert(FDownloadInfo.Size / 1024) + ')';
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.ViewHttpQueueInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VString := SAS_STR_queue + ' ' + IntToStr(FGlobalInternetState.QueueCount);
      RenderText(VOffset, VString, VNeedSeparator);
      VNeedSeparator := True;
    end;

    if FConfig.ViewTilePathInfo then begin
      VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
      VTileNameWidthAviable := Layer.Bitmap.Width - VOffset.X;

      VMapProjection := VMapType.ProjectionSet.GetSuitableProjection(VProjection);
      if VMapProjection.ProjectionType.CheckLonLatPos(VLonLat) then begin
        VTile :=
          PointFromDoublePoint(
            VMapProjection.LonLat2TilePosFloat(VLonLat),
            prToTopLeft
          );

        VTileName := VMapType.GetTileShowName(VTile, VProjection.Zoom, VMapType.VersionRequest.GetStatic.BaseVersion);
        if Length(VTileName) > 0 then begin
          if VTileNameWidthAviable > 30 then begin
            VTileNameWidth := Layer.Bitmap.TextWidth(VTileName);
            if VTileNameWidthAviable < VTileNameWidth + 40 then begin
              SetLength(VShortTileName, 6);
              StrLCopy(PChar(VShortTileName), PChar(VTileName), 6);
              VShortTileName :=
                VShortTileName + '...' +
                RightStr(
                VTileName,
                Trunc(
                  (Length(VTileName) / VTileNameWidth) * (VTileNameWidthAviable - Layer.Bitmap.TextWidth(VShortTileName) - 40)
                )
              );
              VTileName := VShortTileName;
            end;
          end;
        end;
        VString := SAS_STR_File + ' ' + VTileName;
        RenderText(VOffset, VString, VNeedSeparator);
      end;
    end;

    FLastUpdateTick := GetTickCount;
  end else begin
    SetNeedUpdateBitmapDraw;
  end;
end;

end.
