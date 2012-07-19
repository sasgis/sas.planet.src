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

unit u_MiniMapLayerPlusButton;

interface

uses
  Types,
  Classes,
  Controls,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_NotifierOperation,
  i_MarkerDrawable,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_MiniMapLayerConfig,
  u_WindowLayerBasic;

type
  TMiniMapLayerPlusButton = class(TWindowLayerAbstract)
  private
    FConfig: IMiniMapLayerConfig;
    FMarker: IMarkerDrawableChangeable;
    FPosition: ILocalCoordConverterChangeable;

    FLayer: TPositionedLayer;
    FButtonPressed: Boolean;
    procedure OnConfigChange;
    procedure OnPosChange;
    procedure OnPaintLayer(
      Sender: TObject;
      Buffer: TBitmap32
    );
    procedure UpdateLayerLocation(
      const AViewSize: TPoint;
      const AMiniMapWidth: Integer;
      const ABottomMargin: Integer
    );
    procedure MouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure MouseUP(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );

  protected
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
      const AMarker: IMarkerDrawableChangeable;
      const AConfig: IMiniMapLayerConfig
    );
  end;

implementation

uses
  t_GeoTypes,
  u_ListenerByEvent;

{ TMiniMapLayerPlusButton }

constructor TMiniMapLayerPlusButton.Create(
  const APerfList: IInternalPerformanceCounterList; const AAppStartedNotifier,
  AAppClosingNotifier: INotifierOneOperation; AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AMarker: IMarkerDrawableChangeable;
  const AConfig: IMiniMapLayerConfig);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier
  );
  FConfig := AConfig;
  FPosition := APosition;
  FMarker := AMarker;
  FLayer := TPositionedLayer.Create(AParentMap.Layers);
  FLayer.MouseEvents := false;
  FLayer.OnMouseDown := MouseDown;
  FLayer.OnMouseUp := MouseUP;
  FLayer.Cursor := crHandPoint;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FMarker.ChangeNotifier
  );
end;

procedure TMiniMapLayerPlusButton.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    FButtonPressed := True;
  end;
end;

procedure TMiniMapLayerPlusButton.MouseUP(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    if FButtonPressed then begin
      if FLayer.HitTest(X, Y) then begin
        FConfig.LockWrite;
        try
          FConfig.ZoomDelta := FConfig.ZoomDelta - 1;
        finally
          FConfig.UnlockWrite;
        end;
      end;
      FButtonPressed := False;
    end;
  end;
end;

procedure TMiniMapLayerPlusButton.OnConfigChange;
var
  VVisible: Boolean;
  VWidth: Integer;
  VBottomMargin: Integer;
  VLocalConverter: ILocalCoordConverter;
begin
  FConfig.LockRead;
  try
    VVisible := FConfig.Visible;
    VWidth := FConfig.Width;
    VBottomMargin := FConfig.BottomMargin;
  finally
    FConfig.UnlockRead;
  end;
  if VVisible then begin
    FLayer.Visible := True;
    FLayer.MouseEvents := True;
    VLocalConverter := FPosition.GetStatic;
    if VLocalConverter <> nil then begin
      UpdateLayerLocation(
        VLocalConverter.GetLocalRectSize,
        VWidth,
        VBottomMargin
      );
    end;
    FLayer.Changed;
  end else begin
    FLayer.Visible := False;
    FLayer.MouseEvents := False;
  end;
end;

procedure TMiniMapLayerPlusButton.OnPaintLayer(Sender: TObject;
  Buffer: TBitmap32);
var
  VCenterPos: TDoublePoint;
  VLocation: TFloatRect;
  VMarker: IMarkerDrawable;
begin
  VMarker := FMarker.GetStatic;
  if VMarker <> nil then begin
    VLocation := FLayer.Location;
    VCenterPos.X := (VLocation.Left + VLocation.Right) / 2;
    VCenterPos.Y := (VLocation.Top + VLocation.Bottom) / 2;
    VMarker.DrawToBitmap(Buffer, VCenterPos);
  end;
end;

procedure TMiniMapLayerPlusButton.OnPosChange;
var
  VVisible: Boolean;
  VWidth: Integer;
  VBottomMargin: Integer;
  VLocalConverter: ILocalCoordConverter;
begin
  FConfig.LockRead;
  try
    VVisible := FConfig.Visible;
    VWidth := FConfig.Width;
    VBottomMargin := FConfig.BottomMargin;
  finally
    FConfig.UnlockRead;
  end;
  if VVisible then begin
    VLocalConverter := FPosition.GetStatic;
    if VLocalConverter <> nil then begin
      UpdateLayerLocation(
        VLocalConverter.GetLocalRectSize,
        VWidth,
        VBottomMargin
      );
    end;
  end;
end;

procedure TMiniMapLayerPlusButton.StartThreads;
begin
  inherited;
  FLayer.OnPaint := OnPaintLayer;
  OnConfigChange;
end;

procedure TMiniMapLayerPlusButton.UpdateLayerLocation(
  const AViewSize: TPoint;
  const AMiniMapWidth: Integer;
  const ABottomMargin: Integer
);
var
  VLocation: TRect;
begin
  VLocation.Left := AViewSize.X - AMiniMapWidth + 6;
  VLocation.Top := AViewSize.Y - ABottomMargin - AMiniMapWidth + 6;
  VLocation.Right := VLocation.Left + 12;
  VLocation.Bottom := VLocation.Top + 12;
  FLayer.Location := FloatRect(VLocation);
end;

end.
