{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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
  i_Datum,
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

    function MakeHintText(const AItems: IVectorItemSubset): string; overload;
    function MakeHintText(const AItem: IVectorDataItem; const AInfo: TPolyHintInfo): string; overload;

    procedure ShowHintText(const AMousePos: TPoint; const AText: string);
  public
    procedure ShowHint(
      const AMousePos: TPoint;
      const AItems: IVectorItemSubset
    );
    procedure ShowHintExt(
      const AMousePos: TPoint;
      const AItems: IVectorItemSubset
    );
    procedure HideHint;
  public
    constructor Create(
      const AOwner: TComponent;
      const AMap: TImage32;
      const ADatum: IDatum;
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
  SysUtils,
  Graphics,
  gnugettext,
  t_GeoTypes,
  i_GeometryLonLat,
  i_LocalCoordConverter,
  u_GeometryHintInfoProvider;

{ TMapHintWindow }

constructor TMapHintWindow.Create(
  const AOwner: TComponent;
  const AMap: TImage32;
  const ADatum: IDatum;
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

  FHintInfoProvider := TGeometryHintInfoProvider.Create(ADatum, AProjectedProvider);
end;

destructor TMapHintWindow.Destroy;
begin
  HideHint;
  inherited Destroy;
end;

procedure TMapHintWindow.ShowHint(
  const AMousePos: TPoint;
  const AItems: IVectorItemSubset
);
begin
  ShowHintText(AMousePos, MakeHintText(AItems));
end;

procedure TMapHintWindow.ShowHintExt(
  const AMousePos: TPoint;
  const AItems: IVectorItemSubset
);
var
  I: Integer;
  VItem: IVectorDataItem;
  VText: string;
  VPoly: IGeometryLonLatPolygon;
  VPolyInfo: TPolyHintInfo;
  VLocalConverter: ILocalCoordConverter;
begin
  Assert(AItems <> nil);

  VLocalConverter := FLocalConverter.GetStatic;

  for I := 0 to AItems.Count - 1 do begin
    VItem := AItems[I];
    if Supports(VItem.Geometry, IGeometryLonLatLine) then begin
      // todo
    end else
    if Supports(VItem.Geometry, IGeometryLonLatPolygon, VPoly) then begin
      if FHintInfoProvider.GetPolyHintInfo(VLocalConverter, VPoly, AMousePos, VPolyInfo) then begin
        VText := MakeHintText(VItem, VPolyInfo);
        ShowHintText(AMousePos, VText);
        Exit;
      end;
    end;
  end;

  ShowHint(AMousePos, AItems);
end;

procedure TMapHintWindow.ShowHintText(const AMousePos: TPoint; const AText: string);
var
  VHintRect: TRect;
begin
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

  FHintWindow.ActivateHint(
    Bounds(
      AMousePos.X + 13,
      AMousePos.Y - 13,
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
  end;
end;

function TMapHintWindow.MakeHintText(const AItem: IVectorDataItem; const AInfo: TPolyHintInfo): string;
var
  VPart: string;
  VConverter: IValueToStringConverter;
begin
  VConverter := FValueToStringConverter.GetStatic;

  if AInfo.ContoursCount > 1 then begin
    VPart := Format(_('Part: %d/%d'), [AInfo.CurrentContour, AInfo.ContoursCount]) + #13#10;
  end else begin
    VPart := '';
  end;

  Result :=
    AItem.Name + #13#10 +
    VPart +
    Format(_('Area: %s'), [VConverter.AreaConvert(AInfo.Area)]) + #13#10 +
    Format(_('Perimeter: %s'), [VConverter.DistConvert(AInfo.Perimeter)]);
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
