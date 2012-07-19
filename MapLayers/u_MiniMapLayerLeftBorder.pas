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
  u_WindowLayerBasic;

type
  TMiniMapLayerLeftBorder = class(TWindowLayerAbstract)
  private
    FConfig: IMiniMapLayerConfig;
    FPosition: ILocalCoordConverterChangeable;

    FLayer: TBitmapLayer;
    FLeftBorderMoved: Boolean;
    FLeftBorderMovedClickDelta: Double;
    procedure OnConfigChange;
    procedure OnPosChange;
    procedure UpdateLayerSize(
      const ASize: TPoint;
      const AMasterAlfa: Integer
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
    procedure MouseMove(
      Sender: TObject;
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
    AAppClosingNotifier
  );
  FConfig := AConfig;
  FPosition := APosition;
  FLayer := TBitmapLayer.Create(AParentMap.Layers);
  FLayer.Visible := False;
  FLayer.MouseEvents := false;
  FLayer.Cursor := crSizeNWSE;
  FLayer.Bitmap.DrawMode := dmBlend;
  FLayer.OnMouseDown := MouseDown;
  FLayer.OnMouseUp := MouseUP;
  FLayer.OnMouseMove := MouseMove;
  FLayer.Bitmap.DrawMode := dmBlend;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );

end;

procedure TMiniMapLayerLeftBorder.MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    FLeftBorderMoved := true;
    FLeftBorderMovedClickDelta := X - FLayer.Location.Left;
  end;
end;

procedure TMiniMapLayerLeftBorder.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  VNewWidth: Integer;
  VVisibleSize: TPoint;
begin
  if FLeftBorderMoved then begin
    VVisibleSize := FPosition.GetStatic.GetLocalRectSize;
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
    FConfig.Width := VNewWidth;
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
var
  VVisible: Boolean;
  VWidth: Integer;
  VMasterAlfa: Integer;
  VBorderWidth: Integer;
  VBottomMargin: Integer;
  VLocalConverter: ILocalCoordConverter;
  VLayerSize: TPoint;
begin
  VBorderWidth := 5;
  FConfig.LockRead;
  try
    VVisible := FConfig.Visible;
    VWidth := FConfig.Width;
    VMasterAlfa := FConfig.MasterAlpha;
    VBottomMargin := FConfig.BottomMargin;
  finally
    FConfig.UnlockRead;
  end;
  if VVisible then begin
    FLayer.Visible := True;
    FLayer.MouseEvents := True;
    VLayerSize.X := VBorderWidth;
    VLayerSize.Y := VWidth + VBorderWidth;
    UpdateLayerSize(VLayerSize, VMasterAlfa);
    VLocalConverter := FPosition.GetStatic;
    if VLocalConverter <> nil then begin
      UpdateLayerLocation(
        VLocalConverter.GetLocalRectSize,
        VWidth,
        VBottomMargin
      );
    end;
  end else begin
    FLayer.Visible := False;
    FLayer.MouseEvents := False;
  end;
end;

procedure TMiniMapLayerLeftBorder.OnPosChange;
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

procedure TMiniMapLayerLeftBorder.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

procedure TMiniMapLayerLeftBorder.UpdateLayerLocation(
  const AViewSize: TPoint;
  const AMiniMapWidth: Integer;
  const ABottomMargin: Integer
);
var
  VLocation: TRect;
begin
  VLocation.Right := AViewSize.X - AMiniMapWidth;
  VLocation.Bottom := AViewSize.Y - ABottomMargin;
  VLocation.Left := VLocation.Right - FLayer.Bitmap.Width;
  VLocation.Top := VLocation.Bottom - FLayer.Bitmap.Height;
  FLayer.Location := FloatRect(VLocation);
end;

procedure TMiniMapLayerLeftBorder.UpdateLayerSize(
  const ASize: TPoint;
  const AMasterAlfa: Integer
);
var
  VCenterPos: TPoint;
begin
  if (FLayer.Bitmap.Width <> ASize.X) or (FLayer.Bitmap.Height <> ASize.Y) then begin
    FLayer.Bitmap.SetSize(ASize.X, ASize.Y);
    FLayer.Bitmap.Clear(clLightGray32);

    FLayer.Bitmap.VertLineS(0, 0, ASize.Y - 1, clBlack32);
    FLayer.Bitmap.VertLineS(ASize.X - 1, ASize.X - 1, ASize.Y - 1, clBlack32);
    FLayer.Bitmap.HorzLineS(0, 0, ASize.X - 1, clBlack32);
    VCenterPos.X := ASize.X div 2;
    VCenterPos.Y := ASize.X + (ASize.Y - ASize.X) div 2;
    FLayer.bitmap.Pixel[VCenterPos.X, VCenterPos.Y - 6] := 0;
    FLayer.bitmap.Pixel[VCenterPos.X, VCenterPos.Y - 2] := 0;
    FLayer.bitmap.Pixel[VCenterPos.X, VCenterPos.Y + 2] := 0;
    FLayer.bitmap.Pixel[VCenterPos.X, VCenterPos.Y + 6] := 0;
    FLayer.Bitmap.MasterAlpha := AMasterAlfa;
  end;
end;

end.

