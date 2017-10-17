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
  i_CoordToStringConverter,
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
  TStatusBarItemID = (
    sbiZoom,
    sbiLonLat,
    sbiMetrPerPix,
    sbiElevation,
    sbiTimeZone,
    sbiDownloadedInfo,
    sbiHttpQueueInfo,
    sbiTilePath
  );

  TStatusBarItemInfo = record
    Text: string;
    Visible: Boolean;
  end;

  TStatusBarItems = array[Low(TStatusBarItemID)..High(TStatusBarItemID)] of TStatusBarItemInfo;

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
    FCoordToStringConverter: ICoordToStringConverterChangeable;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FView: ILocalCoordConverterChangeable;
    FPopupMenu: IPopUp;
    FLastUpdateTick: DWORD;
    FMinUpdate: Cardinal;
    FBgColor: TColor32;
    FTextColor: TColor32;
    FAALevel: Integer;
    FItemsInfo: TStatusBarItems;
    FPrevItemsInfo: TStatusBarItems;
    FIsNeedForceRedraw: Boolean;
    procedure OnConfigChange;
    procedure OnTimerEvent;
    procedure OnPosChange;
    procedure OnMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure ResetItems(out AItems: TStatusBarItems);
    procedure GetItemsInfo(out AItems: TStatusBarItems);
    function IsEqualItems(const A, B: TStatusBarItems): Boolean;
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
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
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

{ TWindowLayerStatusBar }

constructor TWindowLayerStatusBar.Create(
  const ALanguageManager: ILanguageManager;
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AView: ILocalCoordConverterChangeable;
  const AConfig: IStatBarConfig;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
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
  FCoordToStringConverter := ACoordToStringConverter;
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
  FIsNeedForceRedraw := True;
  ResetItems(FPrevItemsInfo);
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
    FIsNeedForceRedraw := True;
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
    FIsNeedForceRedraw := True;
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
    FIsNeedForceRedraw := False;
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
    const ADrawLine: Boolean
  );
  begin
    Layer.Bitmap.RenderText(AOffset.X, AOffset.Y, AText, FAALevel, FTextColor);
    if ADrawLine then begin
      Layer.Bitmap.VertLineS(AOffset.X - 10, 0, Layer.Bitmap.Height, SetAlpha(clBlack32, 125));
    end;
  end;

  function GetTilePathStr(
    const ATilePath: string;
    const AOffsetX: Integer
  ): string;
  var
    VCount: Integer;
    VShortTileName: string;
    VTileNameWidth: Integer;
    VTileNameWidthAviable: Integer;
  begin
    Result := ATilePath;
    VTileNameWidthAviable := Layer.Bitmap.Width - AOffsetX;
    if Length(Result) > 0 then begin
      if VTileNameWidthAviable > 30 then begin
        VTileNameWidth := Layer.Bitmap.TextWidth(Result);
        if VTileNameWidthAviable < VTileNameWidth + 40 then begin
          SetLength(VShortTileName, 6);
          StrLCopy(PChar(VShortTileName), PChar(Result), 6);
          VCount := Trunc((Length(Result) / VTileNameWidth) * (VTileNameWidthAviable - Layer.Bitmap.TextWidth(VShortTileName) - 40));
          Result := VShortTileName + '...' + RightStr(Result, VCount);
        end;
      end;
    end;
    Result := SAS_STR_File + ' ' + Result;
  end;

var
  I: TStatusBarItemID;
  VString: string;
  VCurrentTick: DWORD;
  VOffset: TPoint;
  VNeedSeparator: Boolean;
begin
  inherited;

  if FIsNeedForceRedraw then begin
    GetItemsInfo(FItemsInfo);
  end else begin
    VCurrentTick := GetTickCount;
    if (VCurrentTick > FLastUpdateTick) and (VCurrentTick < FLastUpdateTick + FMinUpdate) then begin
      Exit;
    end;
    GetItemsInfo(FItemsInfo);
    if IsEqualItems(FPrevItemsInfo, FItemsInfo) then begin
      Exit;
    end;
  end;

  Layer.Bitmap.BeginUpdate;
  try
    Layer.Bitmap.Clear(FBgColor);
    Layer.Bitmap.HorzLineS(0, 0, Layer.Bitmap.Width, SetAlpha(clBlack32, 255));

    VOffset.Y := 1;
    VOffset.X := -10;
    VString := '';
    VNeedSeparator := False;

    for I := Low(TStatusBarItemID) to High(TStatusBarItemID) do begin
      if FItemsInfo[I].Visible then begin
        if VString <> '' then begin
          VOffset.X := VOffset.X + Layer.Bitmap.TextWidth(VString) + 20;
        end else begin
          VOffset.X := VOffset.X + 20;
        end;

        if I = sbiTilePath then begin
          VString := GetTilePathStr(FItemsInfo[I].Text, VOffset.X);
        end else begin
          VString := FItemsInfo[I].Text;
        end;

        RenderText(VOffset, VString, VNeedSeparator);
        VNeedSeparator := True;
      end;
    end;
    FPrevItemsInfo := FItemsInfo;
    FLastUpdateTick := GetTickCount;
  finally
    Layer.Bitmap.EndUpdate;
    Layer.Bitmap.Changed;
  end;
end;

procedure TWindowLayerStatusBar.ResetItems(out AItems: TStatusBarItems);
var
  I: TStatusBarItemID;
begin
  for I := Low(TStatusBarItemID) to High(TStatusBarItemID) do begin
    AItems[I].Text := '';
    AItems[I].Visible := False;
  end;
end;

function TWindowLayerStatusBar.IsEqualItems(const A, B: TStatusBarItems): Boolean;
var
  I: TStatusBarItemID;
begin
  Result := True;
  for I := Low(TStatusBarItemID) to High(TStatusBarItemID) do begin
    if not A[I].Visible and not B[I].Visible then begin
      // don't compare invisible items
      Continue;
    end;
    if (A[I].Visible <> B[I].Visible) or (A[I].Text <> B[I].Text) then begin
      Result := False;
      Break;
    end;
  end;
end;

procedure TWindowLayerStatusBar.GetItemsInfo(out AItems: TStatusBarItems);
const
  D2R: Double = 0.017453292519943295769236907684886;
var
  I: TStatusBarItemID;
  VLonLat: TDoublePoint;
  VMapPoint: TDoublePoint;
  VRad: Extended;
  VTile: TPoint;
  VMapType: IMapType;
  VProjection: IProjection;
  VMapProjection: IProjection;
  VPixelsAtZoom: Double;
  VMousePos: TPoint;
  VVisualCoordConverter: ILocalCoordConverter;
  VValueConverter: IValueToStringConverter;
  VCoordToStringConverter: ICoordToStringConverter;
begin
  ResetItems(AItems);

  VValueConverter := FValueToStringConverter.GetStatic;
  VVisualCoordConverter := FView.GetStatic;
  VProjection := VVisualCoordConverter.Projection;

  VMousePos := FMouseState.CurentPos;
  VMapPoint := VVisualCoordConverter.LocalPixel2MapPixelFloat(VMousePos);
  VProjection.ValidatePixelPosFloatStrict(VMapPoint, True);
  VLonLat := VProjection.PixelPosFloat2LonLat(VMapPoint);

  I := Low(TStatusBarItemID);
  AItems[I].Visible := FConfig.ViewZoomInfo;
  if AItems[I].Visible then begin
    AItems[I].Text := 'z' + IntToStr(VProjection.Zoom + 1);
  end;

  Inc(I);
  AItems[I].Visible := FConfig.ViewLonLatInfo;
  if AItems[I].Visible then begin
    VCoordToStringConverter := FCoordToStringConverter.GetStatic;
    AItems[I].Text := VCoordToStringConverter.GetCoordSysInfo(VLonLat);
    if AItems[I].Text <> '' then begin
      AItems[I].Text := AItems[I].Text + ' ';
    end;
    AItems[I].Text := AItems[I].Text + VCoordToStringConverter.LonLatConvert(VLonLat);
  end;

  Inc(I);
  AItems[I].Visible := FConfig.ViewMetrPerPixInfo;
  if AItems[I].Visible then begin
    VRad := VProjection.ProjectionType.Datum.GetSpheroidRadiusA;
    VPixelsAtZoom := VProjection.GetPixelsFloat;
    AItems[I].Text := VValueConverter.DistPerPixelConvert(
      1 / ((VPixelsAtZoom / (2 * PI)) / (VRad * Cos(VLonLat.Y * D2R)))
    );
  end;

  Inc(I);
  AItems[I].Visible := FTerrainConfig.ShowInStatusBar and FTerrainConfig.ElevationInfoAvailable;
  if AItems[I].Visible then begin
    AItems[I].Text := FTerrainInfo.GetElevationInfoStr(VLonLat, VProjection.Zoom);
  end;

  Inc(I);
  AItems[I].Visible := FConfig.TimeZoneInfoAvailable and FConfig.ViewTimeZoneTimeInfo;
  if AItems[I].Visible then begin
    AItems[I].Text := FTimeZoneInfo.GetStatusBarTzInfo(VLonLat);
  end;

  Inc(I);
  AItems[I].Visible := FConfig.ViewDownloadedInfo;
  if AItems[I].Visible then begin
    AItems[I].Text :=
      SAS_STR_load + ' ' + IntToStr(FDownloadInfo.TileCount) +
      ' (' + VValueConverter.DataSizeConvert(FDownloadInfo.Size / 1024) + ')';
  end;

  Inc(I);
  AItems[I].Visible := FConfig.ViewHttpQueueInfo;
  if AItems[I].Visible then begin
    AItems[I].Text := SAS_STR_queue + ' ' + IntToStr(FGlobalInternetState.QueueCount);
  end;

  Inc(I);
  AItems[I].Visible := FConfig.ViewTilePathInfo;
  if AItems[I].Visible then begin
    VMapType := FMainMap.GetStatic;
    VMapProjection := VMapType.ProjectionSet.GetSuitableProjection(VProjection);
    if VMapProjection.ProjectionType.CheckLonLatPos(VLonLat) then begin
      VTile :=
        PointFromDoublePoint(
          VMapProjection.LonLat2TilePosFloat(VLonLat),
          prToTopLeft
        );
      AItems[I].Text :=
        VMapType.GetTileShowName(
          VTile,
          VProjection.Zoom,
          VMapType.VersionRequest.GetStatic.BaseVersion
        );
    end;
  end;
end;

end.
