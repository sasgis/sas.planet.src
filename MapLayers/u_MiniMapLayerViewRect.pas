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
  u_WindowLayerBasic;

type
  TMiniMapLayerViewRect = class(TWindowLayerAbstract)
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

    FLayer: TPositionedLayer;
    procedure OnConfigChange;
    procedure OnPosChange;
    procedure DrawMainViewRect(
      ABuffer: TBitmap32;
      const AMiniMapConverter: ILocalCoordConverter;
      const AViewConverter: ILocalCoordConverter
    );

    procedure OnPaintLayer(
      Sender: TObject;
      Buffer: TBitmap32
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
    procedure UpdateLayerLocation(
      const AViewSize: TPoint;
      const AMiniMapWidth: Integer;
      const ABottomMargin: Integer
    );
  protected
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
  GR32_Polygons,
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
    AAppClosingNotifier
  );
  FConfig := AConfig;
  FPosition := APosition;
  FLayer := TPositionedLayer.Create(AParentMap.Layers);
  FLayer.Visible := False;
  FLayer.MouseEvents := false;
  FLayer.OnMouseDown := LayerMouseDown;
  FLayer.OnMouseUp := LayerMouseUP;
  FLayer.OnMouseMove := LayerMouseMove;
  FLayer.OnPaint := OnPaintLayer;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );

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
    FConfig.MapsConfig.GetActiveMapsSet,
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
    FConfig.MapsConfig.GetActiveLayersSet,
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

procedure TMiniMapLayerViewRect.DrawMainViewRect(
  ABuffer: TBitmap32;
  const AMiniMapConverter: ILocalCoordConverter;
  const AViewConverter: ILocalCoordConverter
);
var
  VLoadedRect: TDoubleRect;
  VZoomSource: Byte;
  VZoom: Byte;
  VMiniMapRect: TDoubleRect;
  VBitmapRect: TDoubleRect;
  VRelRect: TDoubleRect;
  VPolygon: TPolygon32;
  VBitmapSize: TPoint;
  VGeoConvert: ICoordConverter;
  VZoomDelta: Integer;
  VSize: TPoint;
  VViewSize: TPoint;
  VWidth: Integer;
begin
  VWidth := FConfig.Width;
  VSize.X := VWidth;
  VSize.Y := VWidth;
  VViewSize := AViewConverter.GetLocalRectSize;

  if (AViewConverter <> nil) and (AMiniMapConverter <> nil) then begin
    VGeoConvert := AViewConverter.GetGeoConverter;
    VZoomDelta := FConfig.ZoomDelta;
    if VZoomDelta > 0 then begin
      VLoadedRect := AViewConverter.GetRectInMapPixelFloat;
      VZoomSource := AMiniMapConverter.GetZoom;
      VZoom := AViewConverter.GetZoom;
      VGeoConvert.CheckPixelRectFloat(VLoadedRect, VZoom);
      VRelRect := VGeoConvert.PixelRectFloat2RelativeRect(VLoadedRect, VZoom);
      VMiniMapRect := VGeoConvert.RelativeRect2PixelRectFloat(VRelRect, VZoomSource);
      VBitmapRect := AMiniMapConverter.MapRectFloat2LocalRectFloat(VMiniMapRect);
      VBitmapRect.Left := VBitmapRect.Left + FViewRectMoveDelta.X;
      VBitmapRect.Top := VBitmapRect.Top + FViewRectMoveDelta.Y;
      VBitmapRect.Right := VBitmapRect.Right + FViewRectMoveDelta.X;
      VBitmapRect.Bottom := VBitmapRect.Bottom + FViewRectMoveDelta.Y;

      VBitmapSize := AMiniMapConverter.GetLocalRectSize;
      if (VBitmapRect.Left >= 0) or (VBitmapRect.Top >= 0) or (VBitmapRect.Right <= VBitmapSize.X) or (VBitmapRect.Bottom <= VBitmapSize.Y) then begin
        VPolygon := TPolygon32.Create;
        try
          VPolygon.Antialiased := true;
          VPolygon.Add(FixedPoint(VBitmapRect.Left, VBitmapRect.Top));
          VPolygon.Add(FixedPoint(VBitmapRect.Right, VBitmapRect.Top));
          VPolygon.Add(FixedPoint(VBitmapRect.Right, VBitmapRect.Bottom));
          VPolygon.Add(FixedPoint(VBitmapRect.Left, VBitmapRect.Bottom));
          VPolygon.DrawFill(ABuffer, SetAlpha(clWhite32, (VZoomDelta) * 35));
          with VPolygon.Outline do begin
            try
              with Grow(Fixed(3.2 / 2), 0.5) do begin
                try
                  FillMode := pfWinding;
                  DrawFill(ABuffer, SetAlpha(clNavy32, (VZoomDelta) * 43));
                finally
                  Free;
                end;
              end;
            finally
              Free;
            end;
          end;
        finally
          VPolygon.Free;
        end;
      end;
    end;
  end;
end;

procedure TMiniMapLayerViewRect.LayerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  VLayerSize: TPoint;
  VBitmapCenter: TDoublePoint;
  VVisibleCenter: TDoublePoint;
  Vlocation: TFloatRect;
  VLocalConverter: ILocalCoordConverter;
begin
  FParentMap.PopupMenu := nil;
  case button of
    mbRight: begin
      FParentMap.PopupMenu := FPopup;
    end;
    mbLeft: begin
      VLocalConverter := FPosition.GetStatic;
      VLayerSize := VLocalConverter.GetLocalRectSize;
      VBitmapCenter := DoublePoint(VLayerSize.X / 2, VLayerSize.Y / 2);
      Vlocation := FLayer.Location;
      VVisibleCenter.X := VBitmapCenter.X + Vlocation.Left;
      VVisibleCenter.Y := VBitmapCenter.Y + Vlocation.Top;
      FPosMoved := True;
      FViewRectMoveDelta := DoublePoint(X - VVisibleCenter.X, Y - VVisibleCenter.Y);
      FLayer.Changed;
    end;
  end;
end;

procedure TMiniMapLayerViewRect.LayerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  VBitmapSize: TPoint;
  VBitmapCenter: TDoublePoint;
  VVisibleCenter: TDoublePoint;
  VLocation: TFloatRect;
  VLocalConverter: ILocalCoordConverter;
begin
  if FPosMoved then begin
    VLocalConverter := FPosition.GetStatic;
    VBitmapSize := VLocalConverter.GetLocalRectSize;
    VBitmapCenter := DoublePoint(VBitmapSize.X / 2, VBitmapSize.Y / 2);

    VLocation := FLayer.Location;

    VVisibleCenter.X := VLocation.Left + VBitmapCenter.X;
    VVisibleCenter.Y := VLocation.Top + VBitmapCenter.Y;

    if X < VLocation.Left then begin
      FViewRectMoveDelta.X := VLocation.Left - VVisibleCenter.X;
    end else if X > VLocation.Right then begin
      FViewRectMoveDelta.X := VLocation.Right - VVisibleCenter.X;
    end else begin
      FViewRectMoveDelta.X := X - VVisibleCenter.X;
    end;
    if Y < VLocation.Top then begin
      FViewRectMoveDelta.Y := VLocation.Top - VVisibleCenter.Y;
    end else if Y > VLocation.Bottom then begin
      FViewRectMoveDelta.Y := VLocation.Bottom - VVisibleCenter.Y;
    end else begin
      FViewRectMoveDelta.Y := Y - VVisibleCenter.Y;
    end;

    FLayer.Changed;
  end;
end;

procedure TMiniMapLayerViewRect.LayerMouseUP(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  VBitmapCoordConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VBitmapPos: TDoublePoint;
  Vlocation: TFloatRect;
  VMapPoint: TDoublePoint;
  VLonLat: TDoublePoint;
begin
  if FPosMoved then begin
    if FLayer.HitTest(X, Y) then begin
      VBitmapCoordConverter := FPosition.GetStatic;
      Vlocation := FLayer.Location;
      VBitmapPos.X := X - Vlocation.Left;
      VBitmapPos.Y := Y - Vlocation.Top;
      VConverter := VBitmapCoordConverter.GetGeoConverter;
      VZoom := VBitmapCoordConverter.GetZoom;

      VMapPoint := VBitmapCoordConverter.LocalPixelFloat2MapPixelFloat(VBitmapPos);
      VConverter.CheckPixelPosFloatStrict(VMapPoint, VZoom, false);
      VLonLat := VConverter.PixelPosFloat2LonLat(VMapPoint, VZoom);
      FViewRectMoveDelta := DoublePoint(0, 0);

      FViewPortState.ChangeLonLat(VLonLat);
    end else begin
      FViewRectMoveDelta := DoublePoint(0, 0);
      FLayer.Changed;
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
          if not FConfig.MapsConfig.GetActiveLayersSet.IsGUIDSelected(VMap.GUID) then begin
            FConfig.MapsConfig.SelectLayerByGUID(VMap.GUID);
          end else begin
            FConfig.MapsConfig.UnSelectLayerByGUID(VMap.GUID);
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
var
  VLocalConverter: ILocalCoordConverter;
begin
  if FConfig.Visible then begin
    FLayer.Visible := True;
    FLayer.MouseEvents := True;
    VLocalConverter := FViewPortState.Position.GetStatic;
    if VLocalConverter <> nil then begin
      UpdateLayerLocation(VLocalConverter.GetLocalRectSize, FConfig.Width, FConfig.BottomMargin);
      FLayer.Changed;
    end;
  end else begin
    FLayer.Visible := False;
    FLayer.MouseEvents := False;
  end;
end;

procedure TMiniMapLayerViewRect.OnPaintLayer(
  Sender: TObject;
  Buffer: TBitmap32
);
var
  VMiniMapConverter: ILocalCoordConverter;
  VViewConverter: ILocalCoordConverter;
begin
  VMiniMapConverter := FPosition.GetStatic;
  VViewConverter := FViewPortState.Position.GetStatic;
  if (VMiniMapConverter <> nil) and (VViewConverter <> nil) then begin
    DrawMainViewRect(Buffer, VMiniMapConverter, VViewConverter);
  end;
end;

procedure TMiniMapLayerViewRect.OnPosChange;
var
  VLocalConverter: ILocalCoordConverter;
begin
  if FConfig.Visible then begin
    VLocalConverter := FViewPortState.Position.GetStatic;
    if VLocalConverter <> nil then begin
      UpdateLayerLocation(VLocalConverter.GetLocalRectSize, FConfig.Width, FConfig.BottomMargin);
      FLayer.Changed;
    end;
  end;
end;

procedure TMiniMapLayerViewRect.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

procedure TMiniMapLayerViewRect.UpdateLayerLocation(
  const AViewSize: TPoint;
  const AMiniMapWidth, ABottomMargin: Integer
);
var
  VLocation: TRect;
begin
  VLocation.Right := AViewSize.X;
  VLocation.Bottom := AViewSize.Y - ABottomMargin;
  VLocation.Left := VLocation.Left - AMiniMapWidth;
  VLocation.Top := VLocation.Bottom - AMiniMapWidth;
  FLayer.Location := FloatRect(VLocation);
end;

end.
