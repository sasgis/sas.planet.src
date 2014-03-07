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

unit u_MiniMapLayerLeftBorder;

interface

uses
  Types,
  Classes,
  Controls,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_MiniMapLayerConfig,
  u_WindowLayerWithPos;

type
  TMiniMapLayerLeftBorder = class(TWindowLayerWithBitmapBase)
  private
    FConfig: IMiniMapLayerConfig;
    FPosition: ILocalCoordConverterChangeable;

    FLeftBorderMoved: Boolean;
    FLeftBorderMovedClickDelta: Double;
    procedure OnConfigChange;
    procedure OnPosChange;
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
    procedure MouseMove(
      Sender: TObject;
      Shift: TShiftState;
      X, Y: Integer
    );
  protected
    function GetNewBitmapSize: TPoint; override;
    procedure DoUpdateLayerVisibility; override;
    function GetNewLayerLocation: TFloatRect; override;
    procedure DoUpdateBitmapDraw; override;
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const APosition: ILocalCoordConverterChangeable;
      const AConfig: IMiniMapLayerConfig
    );
  end;

implementation

uses
  u_ListenerByEvent;

{ TMiniMapLayerTopBorder }

constructor TMiniMapLayerLeftBorder.Create(
  const APerfList: IInternalPerformanceCounterList; const AAppStartedNotifier,
  AAppClosingNotifier: INotifierOneOperation; AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const AConfig: IMiniMapLayerConfig);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TBitmapLayer.Create(AParentMap.Layers)
  );
  FConfig := AConfig;
  FPosition := APosition;
  Layer.Cursor := crSizeNWSE;
  Layer.Bitmap.DrawMode := dmBlend;
  Layer.OnMouseDown := MouseDown;
  Layer.OnMouseUp := MouseUP;
  Layer.OnMouseMove := MouseMove;
  Layer.Bitmap.DrawMode := dmBlend;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );

end;

procedure TMiniMapLayerLeftBorder.DoUpdateBitmapDraw;
var
  VSize: TPoint;
  VCenterPos: TPoint;
begin
  inherited;
  if Layer.Bitmap.Empty then begin
    Exit;
  end;
  Layer.Bitmap.Clear(clLightGray32);
  VSize := Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
  Layer.Bitmap.VertLineS(0, 0, VSize.Y - 1, clBlack32);
  Layer.Bitmap.VertLineS(VSize.X - 1, VSize.X - 1, VSize.Y - 1, clBlack32);
  Layer.Bitmap.HorzLineS(0, 0, VSize.X - 1, clBlack32);

  VCenterPos.X := VSize.X div 2;
  VCenterPos.Y := VSize.X + (VSize.Y - VSize.X) div 2;
  Layer.Bitmap.Pixel[VCenterPos.X, VCenterPos.Y - 6] := 0;
  Layer.Bitmap.Pixel[VCenterPos.X, VCenterPos.Y - 2] := 0;
  Layer.Bitmap.Pixel[VCenterPos.X, VCenterPos.Y + 2] := 0;
  Layer.Bitmap.Pixel[VCenterPos.X, VCenterPos.Y + 6] := 0;
end;

procedure TMiniMapLayerLeftBorder.DoUpdateLayerVisibility;
begin
  inherited;
  Layer.MouseEvents := Visible;
end;

function TMiniMapLayerLeftBorder.GetNewBitmapSize: TPoint;
var
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FPosition.GetStatic;
  if VLocalConverter <> nil then begin
    Result := Point(5, VLocalConverter.GetLocalRectSize.Y + 5);
  end else begin
    Result := Point(0, 0);
  end;
end;

function TMiniMapLayerLeftBorder.GetNewLayerLocation: TFloatRect;
var
  VLocalConverter: ILocalCoordConverter;
  VMiniMapRect: TRect;
begin
  VLocalConverter := FPosition.GetStatic;
  if VLocalConverter <> nil then begin
    VMiniMapRect := VLocalConverter.GetLocalRect;
    Result.Right := VMiniMapRect.Left;
    Result.Bottom := VMiniMapRect.Bottom;
    Result.Left := Result.Right - Layer.Bitmap.Width;
    Result.Top := Result.Bottom - Layer.Bitmap.Height;
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;

procedure TMiniMapLayerLeftBorder.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    FLeftBorderMoved := true;
    FLeftBorderMovedClickDelta := X - Layer.Location.Left;
  end;
end;

procedure TMiniMapLayerLeftBorder.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  VNewWidth: Integer;
  VVisibleSize: TPoint;
begin
  if FLeftBorderMoved then begin
    VVisibleSize := FPosition.GetStatic.GetLocalRect.BottomRight;
    VNewWidth := Trunc(VVisibleSize.X - X - FLeftBorderMovedClickDelta);
    if VNewWidth < 40 then begin
      VNewWidth := 40;
    end;
    if VNewWidth > VVisibleSize.X then begin
      VNewWidth := VVisibleSize.X;
    end;
    if VNewWidth > VVisibleSize.Y then begin
      VNewWidth := VVisibleSize.Y;
    end;
    FConfig.LocationConfig.Width := VNewWidth;
  end;
end;

procedure TMiniMapLayerLeftBorder.MouseUP(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FLeftBorderMoved then begin
    FLeftBorderMoved := False;
  end;
end;

procedure TMiniMapLayerLeftBorder.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.LocationConfig.GetStatic.Visible;
    SetNeedUpdateBitmapSize;
    SetNeedUpdateBitmapDraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayerLeftBorder.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateBitmapSize;
    SetNeedUpdateLayerLocation;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayerLeftBorder.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.

