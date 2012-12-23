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

unit u_MiniMapLayerViewRect;

interface

uses
  Types,
  Classes,
  Controls,
  GR32,
  GR32_Image,
  GR32_Layers,
  TBX,
  TB2Item,
  t_GeoTypes,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_MiniMapLayerConfig,
  i_MapTypeIconsList,
  i_ViewPortState,
  i_MapTypeGUIConfigList,
  u_WindowLayerWithPos;

type
  TMiniMapLayerViewRect = class(TWindowLayerWithLocationBase)
  private
    FParentMap: TImage32;
    FConfig: IMiniMapLayerConfig;
    FPosition: ILocalCoordConverterChangeable;
    FViewPortState: IViewPortState;
    FGUIConfigList: IMapTypeGUIConfigList;
    FPopup: TTBXPopupMenu;
    FIconsList: IMapTypeIconsList;

    FPosMoved: Boolean;
    FViewRectMoveDelta: TDoublePoint;

    procedure OnConfigChange;
    procedure OnPosChange;
    procedure DrawMainViewRect(
      ABuffer: TBitmap32;
      const AMiniMapConverter: ILocalCoordConverter;
      const AViewConverter: ILocalCoordConverter
    );

    procedure LayerMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure LayerMouseUP(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure LayerMouseMove(
      Sender: TObject;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure BuildPopUpMenu;
    procedure BuildMapsListUI(AMapssSubMenu, ALayersSubMenu: TTBCustomItem);
    procedure OnClickMapItem(Sender: TObject);
    procedure OnClickLayerItem(Sender: TObject);
  protected
    procedure DoUpdateLayerVisibility; override;
    function GetNewLayerLocation: TFloatRect; override;
    procedure PaintLayer(ABuffer: TBitmap32); override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AIconsList: IMapTypeIconsList;
      const APosition: ILocalCoordConverterChangeable;
      const AConfig: IMiniMapLayerConfig
    );
  end;

implementation

uses
  c_ZeroGUID,
  i_CoordConverter,
  i_MapTypes,
  i_ActiveMapsConfig,
  u_ListenerByEvent,
  u_MapTypeMenuItemsGeneratorBasic,
  u_GeoFun,
  u_ResStrings;

{ TMiniMapLayerTopBorder }

constructor TMiniMapLayerViewRect.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier, AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AIconsList: IMapTypeIconsList;
  const APosition: ILocalCoordConverterChangeable;
  const AConfig: IMiniMapLayerConfig);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TPositionedLayer.Create(AParentMap.Layers)
  );
  FConfig := AConfig;
  FParentMap := AParentMap;
  FPosition := APosition;
  FViewPortState := AViewPortState;
  FGUIConfigList := AGUIConfigList;
  FIconsList := AIconsList;

  Layer.OnMouseDown := LayerMouseDown;
  Layer.OnMouseUp := LayerMouseUP;
  Layer.OnMouseMove := LayerMouseMove;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );

  FPopup := TTBXPopupMenu.Create(AParentMap);
  FPopup.Name := 'PopupMiniMap';
  FPopup.Images := FIconsList.GetImageList;

  BuildPopUpMenu;
end;

procedure TMiniMapLayerViewRect.BuildMapsListUI(
  AMapssSubMenu, ALayersSubMenu: TTBCustomItem
);
var
  VGenerator: TMapMenuGeneratorBasic;
begin
  VGenerator := TMapMenuGeneratorBasic.Create(
    FGUIConfigList,
    FConfig.MapsConfig.GetMapsSet,
    FConfig.MapsConfig.GetMapSingleSet,
    AMapssSubMenu,
    Self.OnClickMapItem,
    FIconsList
  );
  try
    VGenerator.BuildControls;
  finally
    VGenerator.Free;
  end;
  VGenerator := TMapMenuGeneratorBasic.Create(
    FGUIConfigList,
    FConfig.MapsConfig.GetLayersSet,
    FConfig.MapsConfig.GetMapSingleSet,
    ALayersSubMenu,
    Self.OnClickLayerItem,
    FIconsList
  );
  try
    VGenerator.BuildControls;
  finally
    VGenerator.Free;
  end;
end;

procedure TMiniMapLayerViewRect.BuildPopUpMenu;
var
  VSubMenuItem: TTBXSubmenuItem;
  VLayersSubMenu: TTBXSubmenuItem;
begin
  VSubMenuItem := TTBXSubmenuItem.Create(FPopup);
  VSubMenuItem.Name := 'MiniMapLayers';
  VSubMenuItem.Caption := SAS_STR_Layers;
  VSubMenuItem.Hint := '';
  VSubMenuItem.SubMenuImages := FPopup.Images;
  FPopup.Items.Add(VSubMenuItem);
  VLayersSubMenu := VSubMenuItem;

  BuildMapsListUI(FPopup.Items, VLayersSubMenu);
end;

procedure TMiniMapLayerViewRect.DoUpdateLayerVisibility;
begin
  inherited;
  Layer.MouseEvents := Visible;
end;

procedure TMiniMapLayerViewRect.DrawMainViewRect(
  ABuffer: TBitmap32;
  const AMiniMapConverter: ILocalCoordConverter;
  const AViewConverter: ILocalCoordConverter
);
var
  VViewMapSourceRect: TDoubleRect;
  VZoomSource: Byte;
  VMiniMapRect: TDoubleRect;
  VBitmapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VGeoConvertSource: ICoordConverter;
  VGeoConvertMiniMap: ICoordConverter;
  VZoomDelta: Integer;
  VSize: TPoint;
  VViewSize: TPoint;
  VWidth: Integer;
  VFillColor: TColor32;
  VBorderColor: TColor32;
  VDrawRect: TRect;
begin
  VWidth := FConfig.Width;
  VSize.X := VWidth;
  VSize.Y := VWidth;
  VViewSize := AViewConverter.GetLocalRectSize;

  if (AViewConverter <> nil) and (AMiniMapConverter <> nil) then begin
    VZoomDelta := FConfig.ZoomDelta;
    if VZoomDelta > 0 then begin
      VViewMapSourceRect := AViewConverter.GetRectInMapPixelFloat;
      VZoomSource := AViewConverter.GetZoom;
      VGeoConvertSource := AViewConverter.GetGeoConverter;
      VGeoConvertSource.CheckPixelRectFloat(VViewMapSourceRect, VZoomSource);
      VLonLatRect := VGeoConvertSource.PixelRectFloat2LonLatRect(VViewMapSourceRect, VZoomSource);
      VGeoConvertMiniMap := AMiniMapConverter.GeoConverter;
      VGeoConvertMiniMap.CheckLonLatRect(VLonLatRect);
      VBitmapRect := AMiniMapConverter.LonLatRect2LocalRectFloat(VLonLatRect);

      VBitmapRect.Left := VBitmapRect.Left + FViewRectMoveDelta.X;
      VBitmapRect.Top := VBitmapRect.Top + FViewRectMoveDelta.Y;
      VBitmapRect.Right := VBitmapRect.Right + FViewRectMoveDelta.X;
      VBitmapRect.Bottom := VBitmapRect.Bottom + FViewRectMoveDelta.Y;

      VMiniMapRect := DoubleRect(AMiniMapConverter.GetLocalRect);
      if IsIntersecProjectedRect(VBitmapRect, VMiniMapRect) then begin
        if
          (VMiniMapRect.Left < VBitmapRect.Left) or
          (VMiniMapRect.Top < VBitmapRect.Top) or
          (VMiniMapRect.Right > VBitmapRect.Right) or
          (VMiniMapRect.Bottom > VBitmapRect.Bottom)
        then begin
          VFillColor := SetAlpha(clWhite32, (VZoomDelta) * 35);
          VBorderColor := SetAlpha(clNavy32, (VZoomDelta) * 43);
          VDrawRect := RectFromDoubleRect(VBitmapRect, rrClosest);
          ABuffer.FillRectTS(VDrawRect, VFillColor);
          ABuffer.FrameRectTS(VDrawRect, VBorderColor);
          ABuffer.FrameRectTS(
            VDrawRect.Left - 1,
            VDrawRect.Top + 1,
            VDrawRect.Right + 1,
            VDrawRect.Bottom - 1,
            VBorderColor
          );
          ABuffer.FrameRectTS(
            VDrawRect.Left + 1,
            VDrawRect.Top - 1,
            VDrawRect.Right - 1,
            VDrawRect.Bottom + 1,
            VBorderColor
          );
        end;
      end;
    end;
  end;
end;

function TMiniMapLayerViewRect.GetNewLayerLocation: TFloatRect;
var
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FPosition.GetStatic;
  if Visible and (VLocalConverter <> nil) then begin
    Result := FloatRect(VLocalConverter.GetLocalRect);
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;

procedure TMiniMapLayerViewRect.LayerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  VVisibleCenter: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
begin
  FParentMap.PopupMenu := nil;
  case Button of
    mbRight: begin
      FParentMap.PopupMenu := FPopup;
    end;
    mbLeft: begin
      VLocalConverter := FPosition.GetStatic;
      VVisibleCenter := RectCenter(VLocalConverter.GetLocalRect);
      FPosMoved := True;
      FViewRectMoveDelta := DoublePoint(X - VVisibleCenter.X, Y - VVisibleCenter.Y);
      Layer.Changed;
    end;
  end;
end;

procedure TMiniMapLayerViewRect.LayerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  VVisibleCenter: TDoublePoint;
  VLocalRect: TRect;
  VLocalConverter: ILocalCoordConverter;
begin
  if FPosMoved then begin
    VLocalConverter := FPosition.GetStatic;
    VLocalRect := VLocalConverter.GetLocalRect;
    VVisibleCenter := RectCenter(VLocalRect);

    if X < VLocalRect.Left then begin
      FViewRectMoveDelta.X := VLocalRect.Left - VVisibleCenter.X;
    end else if X > VLocalRect.Right then begin
      FViewRectMoveDelta.X := VLocalRect.Right - VVisibleCenter.X;
    end else begin
      FViewRectMoveDelta.X := X - VVisibleCenter.X;
    end;
    if Y < VLocalRect.Top then begin
      FViewRectMoveDelta.Y := VLocalRect.Top - VVisibleCenter.Y;
    end else if Y > VLocalRect.Bottom then begin
      FViewRectMoveDelta.Y := VLocalRect.Bottom - VVisibleCenter.Y;
    end else begin
      FViewRectMoveDelta.Y := Y - VVisibleCenter.Y;
    end;

    Layer.Changed;
  end;
end;

procedure TMiniMapLayerViewRect.LayerMouseUP(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  VLocalConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapPoint: TDoublePoint;
  VLonLat: TDoublePoint;
begin
  if FPosMoved then begin
    if Layer.HitTest(X, Y) then begin
      VLocalConverter := FPosition.GetStatic;
      VConverter := VLocalConverter.GetGeoConverter;
      VZoom := VLocalConverter.GetZoom;

      VMapPoint := VLocalConverter.LocalPixel2MapPixelFloat(Point(X, Y));
      VConverter.CheckPixelPosFloatStrict(VMapPoint, VZoom, False);
      VLonLat := VConverter.PixelPosFloat2LonLat(VMapPoint, VZoom);
      FViewRectMoveDelta := DoublePoint(0, 0);

      FViewPortState.ChangeLonLat(VLonLat);
    end else begin
      FViewRectMoveDelta := DoublePoint(0, 0);
      Layer.Changed;
    end;
  end;
  FPosMoved := False;
end;

procedure TMiniMapLayerViewRect.OnClickLayerItem(Sender: TObject);
var
  VSender: TTBCustomItem;
  VAtiveMap: IActiveMapSingle;
  VMap: IMapType;
begin
  if Sender is TTBCustomItem then begin
    VSender := TTBCustomItem(Sender);
    VAtiveMap := IActiveMapSingle(VSender.Tag);
    if VAtiveMap <> nil then begin
      VMap := VAtiveMap.GetMapType;
      if VMap <> nil then begin
        FConfig.MapsConfig.LockWrite;
        try
          if VAtiveMap.GetIsActive then begin
            FConfig.MapsConfig.UnSelectLayerByGUID(VMap.GUID);
          end else begin
            FConfig.MapsConfig.SelectLayerByGUID(VMap.GUID);
          end;
        finally
          FConfig.MapsConfig.UnlockWrite;
        end;
      end;
    end;
  end;
end;

procedure TMiniMapLayerViewRect.OnClickMapItem(Sender: TObject);
var
  VSender: TComponent;
  VAtiveMap: IActiveMapSingle;
  VMap: IMapType;
begin
  if Sender is TComponent then begin
    VSender := TComponent(Sender);
    VAtiveMap := IActiveMapSingle(VSender.Tag);
    if VAtiveMap <> nil then begin
      VMap := VAtiveMap.GetMapType;
      if VMap <> nil then begin
        FConfig.MapsConfig.SelectMainByGUID(VMap.GUID);
      end else begin
        FConfig.MapsConfig.SelectMainByGUID(CGUID_Zero);
      end;
    end;
  end;
end;

procedure TMiniMapLayerViewRect.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.Visible;
    SetNeedUpdateLayerLocation;
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayerViewRect.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerLocation;
    SetNeedFullRepaintLayer;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayerViewRect.PaintLayer(ABuffer: TBitmap32);
var
  VMiniMapConverter: ILocalCoordConverter;
  VViewConverter: ILocalCoordConverter;
begin
  VMiniMapConverter := FPosition.GetStatic;
  VViewConverter := FViewPortState.View.GetStatic;
  if (VMiniMapConverter <> nil) and (VViewConverter <> nil) then begin
    DrawMainViewRect(ABuffer, VMiniMapConverter, VViewConverter);
  end;
end;

procedure TMiniMapLayerViewRect.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
