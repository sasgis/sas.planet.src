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
  u_WindowLayerBasic;

type
  TMiniMapLayerTopBorder = class(TWindowLayerAbstract)
  private
    FConfig: IMiniMapLayerConfig;
    FPosition: ILocalCoordConverterChangeable;

    FLayer: TBitmapLayer;
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

constructor TMiniMapLayerTopBorder.Create(
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
  FLayer.MouseEvents := false;
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

procedure TMiniMapLayerTopBorder.OnConfigChange;
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
    VLayerSize.X := VWidth;
    VLayerSize.Y := VBorderWidth;
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
  end;
end;

procedure TMiniMapLayerTopBorder.OnPosChange;
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

procedure TMiniMapLayerTopBorder.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

procedure TMiniMapLayerTopBorder.UpdateLayerLocation(
  const AViewSize: TPoint;
  const AMiniMapWidth: Integer;
  const ABottomMargin: Integer
);
var
  VLocation: TRect;
begin
  VLocation.Right := AViewSize.X;
  VLocation.Bottom := AViewSize.Y - ABottomMargin - AMiniMapWidth;
  VLocation.Left := VLocation.Right - FLayer.Bitmap.Width;
  VLocation.Top := VLocation.Bottom - FLayer.Bitmap.Height;
  FLayer.Location := FloatRect(VLocation);
end;

procedure TMiniMapLayerTopBorder.UpdateLayerSize(
  const ASize: TPoint;
  const AMasterAlfa: Integer
);
begin
  FLayer.Bitmap.SetSize(ASize.X, ASize.Y);
  FLayer.Bitmap.Clear(clLightGray32);
  FLayer.Bitmap.HorzLineS(0, 0, ASize.X, clBlack32);
  FLayer.Bitmap.HorzLineS(0, ASize.Y - 1, ASize.X, clBlack32);
  FLayer.Bitmap.MasterAlpha := AMasterAlfa;
end;

end.
