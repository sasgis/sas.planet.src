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

unit u_MapViewGoto;

interface

uses
  Types,
  t_GeoTypes,
  i_Notifier,
  i_ViewPortState,
  i_MapViewGoto,
  i_MarksSimple,
  u_BaseInterfacedObject;

type
  TGotoPosStatic = class(TBaseInterfacedObject, IGotoPosStatic)
  private
    FLonLat: TDoublePoint;
    FZoom: Byte;
    FGotoTime: TDateTime;
  private
    function GetLonLat: TDoublePoint;
    function GetZoom: Byte;
    function GetGotoTime: TDateTime;
  public
    constructor Create(
      ALonLat: TDoublePoint;
      AZoom: Byte;
      AGotoTime: TDateTime
    );
  end;

  TMapViewGoto = class(TBaseInterfacedObject, IMapViewGoto)
  private
    FViewPortState: IViewPortState;
    FLastGotoPos: IGotoPosStatic;
    FChangeNotifier: INotifierInternal;
  private
    procedure GotoPos(
      const ALonLat: TDoublePoint;
      const AZoom: Byte
    );
    procedure FitRectToScreen(
      const ALonLatRect: TDoubleRect;
      AShowMarker: boolean
    );
    procedure FitLineToScreen(
      const ALonLatPath: IMarkLine;
      AShowMarker: boolean
    );
    function GetLastGotoPos: IGotoPosStatic;
    function GetChangeNotifier: INotifier;
  public
    constructor Create(const AViewPortState: IViewPortState);
  end;

implementation

uses
  Math,
  SysUtils,
  i_CoordConverter,
  i_LocalCoordConverter,
  u_Notifier,
  u_GeoFun;

{ TMapViewGoto }

constructor TMapViewGoto.Create(const AViewPortState: IViewPortState);
begin
  inherited Create;
  FViewPortState := AViewPortState;
  FChangeNotifier := TNotifierBase.Create;
  FLastGotoPos := TGotoPosStatic.Create(CEmptyDoublePoint, 0, NaN);
end;

procedure TMapViewGoto.FitRectToScreen(const ALonLatRect: TDoubleRect; AShowMarker: boolean);
var
  VCenterLonLat: TDoublePoint;
  VLLRect: TDoubleRect;
  VGeoConverter: ICoordConverter;
  VScreenSize: TPoint;
  VRelativeRect: TDoubleRect;
  VTargetZoom: Byte;
  VZoom: Byte;
  VMarkMapRect: TDoubleRect;
  VMarkMapSize: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
begin
  if PointIsEmpty(ALonLatRect.TopLeft) or PointIsEmpty(ALonLatRect.BottomRight) then begin
    Exit;
  end;
  if DoublePointsEqual(ALonLatRect.TopLeft, ALonLatRect.BottomRight) then begin
    Exit;
  end;
  VCenterLonLat.X := (ALonLatRect.Left + ALonLatRect.Right) / 2;
  VCenterLonLat.Y := (ALonLatRect.Top + ALonLatRect.Bottom) / 2;
  VLLRect := ALonLatRect;
  VLocalConverter := FViewPortState.View.GetStatic;
  VGeoConverter := VLocalConverter.GeoConverter;
  VScreenSize := VLocalConverter.GetLocalRectSize;

  VGeoConverter.CheckLonLatRect(VLLRect);
  VRelativeRect := VGeoConverter.LonLatRect2RelativeRect(VLLRect);

  VTargetZoom := 23;
  for VZoom := 1 to 23 do begin
    VMarkMapRect := VGeoConverter.RelativeRect2PixelRectFloat(VRelativeRect, VZoom);
    VMarkMapSize.X := VMarkMapRect.Right - VMarkMapRect.Left;
    VMarkMapSize.Y := VMarkMapRect.Bottom - VMarkMapRect.Top;
    if (VMarkMapSize.X > VScreenSize.X) or (VMarkMapSize.Y > VScreenSize.Y) then begin
      VTargetZoom := VZoom - 1;
      Break;
    end;
  end;
  VGeoConverter.CheckZoom(VTargetZoom);
  VGeoConverter.CheckLonLatPos(VCenterLonLat);
  FViewPortState.ChangeLonLatAndZoom(VTargetZoom, VCenterLonLat);
  if AShowMarker then begin
    FLastGotoPos := TGotoPosStatic.Create(VCenterLonLat, VTargetZoom, Now);
    FChangeNotifier.Notify(nil);
  end;
end;

procedure TMapViewGoto.FitLineToScreen(const ALonLatPath: IMarkLine; AShowMarker: boolean);
var
  VCenterLonLat: TDoublePoint;
  VLLRect: TDoubleRect;
  VGeoConverter: ICoordConverter;
  VScreenSize: TPoint;
  VRelativeRect: TDoubleRect;
  VTargetZoom: Byte;
  VZoom: Byte;
  VMarkMapRect: TDoubleRect;
  VMarkMapSize: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
begin
  VLLRect:= ALonLatPath.Line.Bounds.Rect ;
  if PointIsEmpty(VLLRect.TopLeft) or PointIsEmpty(VLLRect.BottomRight) then begin
    Exit;
  end;
  if DoublePointsEqual(VLLRect.TopLeft, VLLRect.BottomRight) then begin
    Exit;
  end;
  VCenterLonLat.X := (VLLRect.Left + VLLRect.Right) / 2;
  VCenterLonLat.Y := (VLLRect.Top + VLLRect.Bottom) / 2;
  VLocalConverter := FViewPortState.View.GetStatic;
  VGeoConverter := VLocalConverter.GeoConverter;
  VScreenSize := VLocalConverter.GetLocalRectSize;

  VGeoConverter.CheckLonLatRect(VLLRect);
  VRelativeRect := VGeoConverter.LonLatRect2RelativeRect(VLLRect);

  VTargetZoom := 23;
  for VZoom := 1 to 23 do begin
    VMarkMapRect := VGeoConverter.RelativeRect2PixelRectFloat(VRelativeRect, VZoom);
    VMarkMapSize.X := VMarkMapRect.Right - VMarkMapRect.Left;
    VMarkMapSize.Y := VMarkMapRect.Bottom - VMarkMapRect.Top;
    if (VMarkMapSize.X > VScreenSize.X) or (VMarkMapSize.Y > VScreenSize.Y) then begin
      VTargetZoom := VZoom - 1;
      Break;
    end;
  end;
  VGeoConverter.CheckZoom(VTargetZoom);
  VGeoConverter.CheckLonLatPos(VCenterLonLat);
  FViewPortState.ChangeLonLatAndZoom(VTargetZoom, VCenterLonLat);
  if AShowMarker then begin
    FLastGotoPos := TGotoPosStatic.Create(ALonLatPath.GetGoToLonLat, VTargetZoom, Now);
    FChangeNotifier.Notify(nil);
  end;
end;


function TMapViewGoto.GetChangeNotifier: INotifier;
begin
  Result := FChangeNotifier;
end;

function TMapViewGoto.GetLastGotoPos: IGotoPosStatic;
begin
  Result := FLastGotoPos;
end;

procedure TMapViewGoto.GotoPos(
  const ALonLat: TDoublePoint;
  const AZoom: Byte
);
begin
  FLastGotoPos := TGotoPosStatic.Create(ALonLat, AZoom, Now);
  FViewPortState.ChangeLonLatAndZoom(AZoom, ALonLat);
  FChangeNotifier.Notify(nil);
end;

{ TGotoPosStatic }

constructor TGotoPosStatic.Create(
  ALonLat: TDoublePoint;
  AZoom: Byte;
  AGotoTime: TDateTime
);
begin
  inherited Create;
  FLonLat := ALonLat;
  FZoom := AZoom;
  FGotoTime := AGotoTime;
end;

function TGotoPosStatic.GetGotoTime: TDateTime;
begin
  Result := FGotoTime;
end;

function TGotoPosStatic.GetLonLat: TDoublePoint;
begin
  Result := FLonLat;
end;

function TGotoPosStatic.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
