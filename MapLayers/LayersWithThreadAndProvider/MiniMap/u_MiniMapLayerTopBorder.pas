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

unit u_MiniMapLayerTopBorder;

interface

uses
  Types,
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
  TMiniMapLayerTopBorder = class(TWindowLayerWithBitmapBase)
  private
    FConfig: IMiniMapLayerConfig;
    FPosition: ILocalCoordConverterChangeable;

    procedure OnConfigChange;
    procedure OnPosChange;
  protected
    function GetNewBitmapSize: TPoint; override;
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

constructor TMiniMapLayerTopBorder.Create(
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

procedure TMiniMapLayerTopBorder.DoUpdateBitmapDraw;
var
  VSize: TPoint;
begin
  inherited;
  VSize := Types.Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
  Layer.Bitmap.Clear(clLightGray32);
  Layer.Bitmap.HorzLineS(0, 0, VSize.X - 1, clBlack32);
  Layer.Bitmap.HorzLineS(0, VSize.Y - 1, VSize.X - 1, clBlack32);
end;

function TMiniMapLayerTopBorder.GetNewBitmapSize: TPoint;
var
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FPosition.GetStatic;
  if VLocalConverter <> nil then begin
    Result := Types.Point(VLocalConverter.GetLocalRectSize.X, 5);
  end else begin
    Result := Types.Point(0, 0);
  end;
end;

function TMiniMapLayerTopBorder.GetNewLayerLocation: TFloatRect;
var
  VLocalConverter: ILocalCoordConverter;
  VMiniMapRect: TRect;
begin
  VLocalConverter := FPosition.GetStatic;
  if VLocalConverter <> nil then begin
    VMiniMapRect := VLocalConverter.GetLocalRect;
    Result.Right := VMiniMapRect.Right;
    Result.Bottom := VMiniMapRect.Top;
    Result.Left := Result.Right - Layer.Bitmap.Width;
    Result.Top := Result.Bottom - Layer.Bitmap.Height;
  end else begin
    Result := FloatRect(0, 0, 0, 0);
  end;
end;

procedure TMiniMapLayerTopBorder.OnConfigChange;
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

procedure TMiniMapLayerTopBorder.OnPosChange;
begin
  ViewUpdateLock;
  try
    SetNeedUpdateBitmapSize;
    SetNeedUpdateLayerLocation;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayerTopBorder.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
