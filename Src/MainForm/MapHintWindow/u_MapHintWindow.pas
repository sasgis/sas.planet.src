{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_MapHintWindow;

interface

uses
  Types,
  Classes,
  Controls,
  GR32_Image,
  t_GeoTypes,
  i_GeoCalc,
  i_GeometryProjectedProvider,
  i_GeometryHintInfoProvider,
  i_LocalCoordConverterChangeable,
  i_ValueToStringConverter,
  i_MapViewGoto,
  i_VectorItemSubset,
  i_VectorDataItemSimple;

type
  TMapHintWindow = class
  private
    FOwner: TComponent;
    FMap: TImage32;
    FMapGoTo: IMapViewGoto;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FLocalConverter: ILocalCoordConverterChangeable;

    FHintWindow: THintWindow;
    FHintInfoProvider: IGeometryHintInfoProvider;
    FCanHideGotoMarker: Boolean;

    FCursorOffset: TPoint;

    function MakeHintText(const AItems: IVectorItemSubset): string; overload;
    function MakeHintText(const AItem: IVectorDataItem; const AInfo: TLineHintInfo): string; overload;
    function MakeHintText(const AItem: IVectorDataItem; const AInfo: TPolyHintInfo): string; overload;

    procedure ShowHintText(
      const APos: TPoint;
      const AText: string;
      const AShowHintAboveCursor: Boolean
    );

    procedure ShowPointMarker(const ALonLat: TDoublePoint);
    procedure HidePointMarker;

    procedure ShowHintExt(
      const AMousePos: TPoint;
      const AItems: IVectorItemSubset
    );
  public
    procedure ShowHint(
      const AMousePos: TPoint;
      const AItems: IVectorItemSubset;
      const AShowExtendedHint: Boolean
    );
    procedure HideHint;
  public
    constructor Create(
      const AOwner: TComponent;
      const AMap: TImage32;
      const AGeoCalc: IGeoCalcChangeable;
      const AProjectedProvider: IGeometryProjectedProvider;
      const AMapGoTo: IMapViewGoto;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const ALocalConverter: ILocalCoordConverterChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  Forms,
  Math,
  SysUtils,
  Graphics,
  gnugettext,
  i_GeometryLonLat,
  i_LocalCoordConverter,
  u_ResStrings,
  u_GeometryHintInfoProvider;

{ TMapHintWindow }

constructor TMapHintWindow.Create(
  const AOwner: TComponent;
  const AMap: TImage32;
  const AGeoCalc: IGeoCalcChangeable;
  const AProjectedProvider: IGeometryProjectedProvider;
  const AMapGoTo: IMapViewGoto;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const ALocalConverter: ILocalCoordConverterChangeable
);
begin
  inherited Create;
  FOwner := AOwner;
  FMap := AMap;
  FMapGoTo := AMapGoTo;
  FValueToStringConverter := AValueToStringConverter;
  FLocalConverter := ALocalConverter;

  FHintInfoProvider := TGeometryHintInfoProvider.Create(AGeoCalc, AProjectedProvider);
end;

destructor TMapHintWindow.Destroy;
begin
  HideHint;
  inherited Destroy;
end;

procedure TMapHintWindow.ShowHint(
  const AMousePos: TPoint;
  const AItems: IVectorItemSubset;
  const AShowExtendedHint: Boolean
);
begin
  FCursorOffset.X := Mouse.CursorPos.X - AMousePos.X;
  FCursorOffset.Y := Mouse.CursorPos.Y - AMousePos.Y;

  if AShowExtendedHint then begin
    ShowHintExt(AMousePos, AItems);
  end else begin
    ShowHintText(AMousePos, MakeHintText(AItems), False);
  end;
end;

procedure TMapHintWindow.ShowHintExt(
  const AMousePos: TPoint;
  const AItems: IVectorItemSubset
);
var
  I: Integer;
  VItem: IVectorDataItem;
  VText: string;
  VHintPos: TPoint;
  VLine: IGeometryLonLatLine;
  VLineInfo: TLineHintInfo;
  VPoly: IGeometryLonLatPolygon;
  VPolyInfo: TPolyHintInfo;
  VLocalConverter: ILocalCoordConverter;
begin
  Assert(AItems <> nil);

  VLocalConverter := FLocalConverter.GetStatic;

  for I := 0 to AItems.Count - 1 do begin
    VItem := AItems[I];
    if Supports(VItem.Geometry, IGeometryLonLatLine, VLine) then begin
      if FHintInfoProvider.GetLineHintInfo(VLocalConverter, VLine, AMousePos, VLineInfo) then begin
        VText := MakeHintText(VItem, VLineInfo);
        VHintPos := VLocalConverter.LonLat2LocalPixel(VLineInfo.LonLatPos, prToTopLeft);
        Dec(VHintPos.Y, 24);
        ShowHintText(VHintPos, VText, True);
        ShowPointMarker(VLineInfo.LonLatPos);
        Exit;
      end;
    end else
    if Supports(VItem.Geometry, IGeometryLonLatPolygon, VPoly) then begin
      if FHintInfoProvider.GetPolyHintInfo(VLocalConverter, VPoly, AMousePos, VPolyInfo) then begin
        VText := MakeHintText(VItem, VPolyInfo);
        ShowHintText(AMousePos, VText, False);
        Exit;
      end;
    end;
  end;

  ShowHintText(AMousePos, MakeHintText(AItems), False);
end;

procedure TMapHintWindow.ShowHintText(
  const APos: TPoint;
  const AText: string;
  const AShowHintAboveCursor: Boolean
);
var
  VHintRect: TRect;
  VShift: TPoint;
begin
  HidePointMarker;

  if AText = '' then begin
    HideHint;
    Exit;
  end;

  if FMap.Cursor = crDefault then begin
    FMap.Cursor := crHandPoint;
  end;

  if FHintWindow = nil then begin
    FHintWindow := THintWindow.Create(FOwner);
    FHintWindow.Brush.Color := clInfoBk;
  end;

  VHintRect := FHintWindow.CalcHintRect(Screen.Width, AText, nil);

  VShift := Point(13, 13);

  if AShowHintAboveCursor then begin
    VShift.Y := Abs(VHintRect.Top - VHintRect.Bottom);
  end;

  FHintWindow.ActivateHint(
    Bounds(
      FCursorOffset.X + APos.X + VShift.X,
      FCursorOffset.Y + APos.Y - VShift.Y,
      Abs(VHintRect.Right - VHintRect.Left),
      Abs(VHintRect.Top - VHintRect.Bottom)
    ),
    AText
  );

  FHintWindow.Repaint;
end;

procedure TMapHintWindow.HideHint;
begin
  if FHintWindow <> nil then begin
    FHintWindow.ReleaseHandle;
    FreeAndNil(FHintWindow);
    if FMap.Cursor = crHandPoint then begin
      FMap.Cursor := crDefault;
    end;
    HidePointMarker;
  end;
end;

procedure TMapHintWindow.ShowPointMarker(const ALonLat: TDoublePoint);
begin
  FCanHideGotoMarker := True;
  FMapGoTo.ShowMarker(ALonLat);
end;

procedure TMapHintWindow.HidePointMarker;
begin
  if FCanHideGotoMarker then begin
    FCanHideGotoMarker := False;
    FMapGoTo.HideMarker;
  end;
end;

function TMapHintWindow.MakeHintText(const AItem: IVectorDataItem; const AInfo: TLineHintInfo): string;
var
  VName: string;
  VDistance: string;
  VElevation: string;
  VTimeStamp: string;
  VSpeed: string;
  VConverter: IValueToStringConverter;
begin
  VConverter := FValueToStringConverter.GetStatic;

  VName := Trim(AItem.Name);
  if VName <> '' then begin
    VName := VName + #13#10;
  end;

  VDistance := Format(_('Distance: %s'), [VConverter.DistConvert(AInfo.Distance)]);

  VElevation := '';
  if not IsNan(AInfo.Elevation) then begin
    VElevation := #13#10 + Format(_('Elevation: %s'), [VConverter.AltitudeConvert(AInfo.Elevation)]);
  end;

  VTimeStamp := '';
  if AInfo.TimeStamp <> 0 then begin
    VTimeStamp := #13#10 + Format(_('Time: %s'), [DateTimeToStr(AInfo.TimeStamp)]);
  end;

  VSpeed := '';
  if not IsNan(AInfo.Speed) then begin
    VSpeed := #13#10 + Format(_('Speed: %s'), [VConverter.SpeedConvert(AInfo.Speed)]);
  end;

  Result := VName + VDistance + VElevation + VTimeStamp + VSpeed;
end;

function TMapHintWindow.MakeHintText(const AItem: IVectorDataItem; const AInfo: TPolyHintInfo): string;
var
  VName: string;
  VPart: string;
  VConverter: IValueToStringConverter;
begin
  VConverter := FValueToStringConverter.GetStatic;

  VName := Trim(AItem.Name);
  if VName <> '' then begin
    VName := VName + #13#10;
  end;

  if AInfo.ContoursCount > 1 then begin
    VPart := Format(_('Part: %d/%d'), [AInfo.CurrentContour, AInfo.ContoursCount]) + #13#10;
  end else begin
    VPart := '';
  end;

  Result :=
    VName +
    VPart +
    Format(SAS_STR_Area, [VConverter.AreaConvert(AInfo.Area)]) + #13#10 +
    Format(SAS_STR_Perimeter, [VConverter.DistConvert(AInfo.Perimeter)]);
end;

function TMapHintWindow.MakeHintText(const AItems: IVectorItemSubset): string;
const
  CSep = #13#10 + '----------------' + #13#10;
var
  I: Integer;
begin
  Result := '';
  if AItems <> nil then begin
    for I := 0 to AItems.Count - 1 do begin
      if Result = '' then begin
        Result := AItems[I].GetHintText;
      end else begin
        Result := Result + CSep + AItems[I].GetHintText;
      end;
    end;
  end;
end;

end.
