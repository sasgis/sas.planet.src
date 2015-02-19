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
  i_Bitmap32Static,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_MiniMapLayerConfig,
  u_WindowLayerWithBitmapBase;

type
  TMiniMapLayerPlusButton = class(TWindowLayerWithBitmapBase)
  private
    FConfig: IMiniMapLayerConfig;
    FPosition: ILocalCoordConverterChangeable;
    FBitmap: IBitmapChangeable;

    FButtonPressed: Boolean;
    procedure OnConfigChange;
    procedure OnBitmapChange;
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
      const ABitmap: IBitmapChangeable;
      const AConfig: IMiniMapLayerConfig
    );
  end;

implementation

uses
  u_BitmapFunc,
  u_ListenerByEvent;

{ TMiniMapLayerPlusButton }

constructor TMiniMapLayerPlusButton.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier,
  AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const APosition: ILocalCoordConverterChangeable;
  const ABitmap: IBitmapChangeable;
  const AConfig: IMiniMapLayerConfig
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    TBitmapLayer.Create(AParentMap.Layers)
  );
  FConfig := AConfig;
  FPosition := APosition;
  FBitmap := ABitmap;

  Layer.OnMouseDown := MouseDown;
  Layer.OnMouseUp := MouseUP;
  Layer.Cursor := crHandPoint;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnPosChange),
    FPosition.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnBitmapChange),
    FBitmap.ChangeNotifier
  );
end;

procedure TMiniMapLayerPlusButton.DoUpdateBitmapDraw;
var
  VBitmap: IBitmap32Static;
begin
  inherited;
  VBitmap := FBitmap.GetStatic;
  if VBitmap <> nil then begin
    BlockTransferFull(
      Layer.Bitmap,
      0, 0,
      VBitmap,
      dmOpaque
    );
  end;
end;

procedure TMiniMapLayerPlusButton.DoUpdateLayerVisibility;
begin
  inherited;
  Layer.MouseEvents := Visible;
end;

function TMiniMapLayerPlusButton.GetNewBitmapSize: TPoint;
var
  VBitmap: IBitmap32Static;
begin
  VBitmap := FBitmap.GetStatic;
  if VBitmap <> nil then begin
    Result := VBitmap.Size;
  end else begin
    Result := Point(0, 0);
  end;
end;

function TMiniMapLayerPlusButton.GetNewLayerLocation: TFloatRect;
var
  VLocalConverter: ILocalCoordConverter;
  VMiniMapRect: TRect;
begin
  VLocalConverter := FPosition.GetStatic;
  if VLocalConverter <> nil then begin
    VMiniMapRect := VLocalConverter.GetLocalRect;
    Result.Left := VMiniMapRect.Left + 6;
    Result.Top := VMiniMapRect.Top + 6;
    Result.Right := Result.Left + Layer.Bitmap.Width;
    Result.Bottom := Result.Top + Layer.Bitmap.Height;
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;

procedure TMiniMapLayerPlusButton.MouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  if Button = mbLeft then begin
    FButtonPressed := True;
  end;
end;

procedure TMiniMapLayerPlusButton.MouseUP(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  if Button = mbLeft then begin
    if FButtonPressed then begin
      if Layer.HitTest(X, Y) then begin
        FConfig.LocationConfig.LockWrite;
        try
          FConfig.LocationConfig.ZoomDelta := FConfig.LocationConfig.ZoomDelta - 1;
        finally
          FConfig.LocationConfig.UnlockWrite;
        end;
      end;
      FButtonPressed := False;
    end;
  end;
end;

procedure TMiniMapLayerPlusButton.OnBitmapChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateBitmapSize;
    SetNeedUpdateBitmapDraw;
    SetNeedUpdateLayerLocation;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayerPlusButton.OnConfigChange;
begin
  ViewUpdateLock;
  try
    Visible := FConfig.LocationConfig.GetStatic.Visible;
    SetNeedUpdateLayerLocation;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayerPlusButton.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateLayerLocation;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayerPlusButton.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
