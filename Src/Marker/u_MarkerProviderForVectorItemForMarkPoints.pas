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

unit u_MarkerProviderForVectorItemForMarkPoints;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkerDrawable,
  i_VectorDataItemSimple,
  i_MarkerProviderByAppearancePointIcon,
  i_MarksDrawConfig,
  i_Bitmap32Static,
  i_BitmapMarker,
  i_TextDrawerBasic,
  i_MarkerProviderForVectorItem,
  u_BaseInterfacedObject;

type
  TMarkerProviderForVectorItemForMarkPoints = class(TBaseInterfacedObject, IMarkerProviderForVectorItem)
  private
    FTextDrawerBasic: ITextDrawerBasic;
    FMarkerProviderByAppearancePointIcon: IMarkerProviderByAppearancePointIcon;

    function GetCaptionMarker(
      const ACaption: string;
      AFontSize: Integer;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      ASolidBgDraw: Boolean;
      AAnchorDelta: TDoublePoint
    ): IMarkerDrawable;

  private
    function GetMarker(
      const AConfig: ICaptionDrawConfigStatic;
      const AItem: IVectorDataItem
    ): IMarkerDrawable;
  public
    constructor Create(
      const ATextDrawerBasic: ITextDrawerBasic;
      const AMarkerProviderByAppearancePointIcon: IMarkerProviderByAppearancePointIcon
    );
  end;

implementation

uses
  Types,
  SysUtils,
  i_AppearanceOfVectorItem,
  u_MarkerDrawableByBitmapMarker,
  u_MarkerDrawableByBitmap32Static,
  u_MarkerDrawableComplex,
  u_GeoFunc;

{ TMarkerProviderForVectorItemForMarkPoints }

constructor TMarkerProviderForVectorItemForMarkPoints.Create(
  const ATextDrawerBasic: ITextDrawerBasic;
  const AMarkerProviderByAppearancePointIcon: IMarkerProviderByAppearancePointIcon
);
begin
  Assert(Assigned(ATextDrawerBasic));
  Assert(Assigned(AMarkerProviderByAppearancePointIcon));
  inherited Create;
  FTextDrawerBasic := ATextDrawerBasic;
  FMarkerProviderByAppearancePointIcon := AMarkerProviderByAppearancePointIcon;
end;

function TMarkerProviderForVectorItemForMarkPoints.GetCaptionMarker(
  const ACaption: string;
  AFontSize: Integer;
  ATextColor, ATextBgColor: TColor32;
  ASolidBgDraw: Boolean;
  AAnchorDelta: TDoublePoint
): IMarkerDrawable;
var
  VBitmapStatic: IBitmap32Static;
  VAnchorPoint: TDoublePoint;
begin
  Result := nil;
  VBitmapStatic := FTextDrawerBasic.DrawText(ACaption, AFontSize, ATextColor, ATextBgColor, ASolidBgDraw);
  if VBitmapStatic <> nil then begin
    VAnchorPoint := DoublePoint(-AAnchorDelta.X, -AAnchorDelta.Y + VBitmapStatic.Size.Y / 2);
    Result := TMarkerDrawableByBitmap32Static.Create(VBitmapStatic, VAnchorPoint);
  end;
end;

function TMarkerProviderForVectorItemForMarkPoints.GetMarker(
  const AConfig: ICaptionDrawConfigStatic;
  const AItem: IVectorDataItem
): IMarkerDrawable;
var
  VAppearanceIcon: IAppearancePointIcon;
  VAppearanceCaption: IAppearancePointCaption;
  VMarkerIcon: IBitmapMarker;
  VMarkerCaption: IMarkerDrawable;
  VMarkerSize: TPoint;
  VAnchorPoint: TDoublePoint;
  VAnchorDelta: TDoublePoint;
begin
  Result := nil;
  VMarkerIcon := nil;
  if Supports(AItem.Appearance, IAppearancePointIcon, VAppearanceIcon) then begin
    VMarkerIcon := FMarkerProviderByAppearancePointIcon.GetMarker(VAppearanceIcon);
  end;

  if Assigned(VMarkerIcon) then begin
    VMarkerSize := VMarkerIcon.Size;
    VAnchorPoint := VMarkerIcon.AnchorPoint;
    VAnchorDelta := DoublePoint(VMarkerSize.X - VAnchorPoint.X, VMarkerSize.Y / 2 - VAnchorPoint.Y);
  end else begin
    VAnchorDelta := DoublePoint(0, 0);
  end;

  VMarkerCaption := nil;
  if AConfig.ShowPointCaption then begin
    if Supports(AItem.Appearance, IAppearancePointCaption, VAppearanceCaption) then begin
      VMarkerCaption :=
        GetCaptionMarker(
          AItem.Name,
          VAppearanceCaption.FontSize,
          VAppearanceCaption.TextColor,
          VAppearanceCaption.TextBgColor,
          AConfig.UseSolidCaptionBackground,
          VAnchorDelta
        );
    end;
  end;

  if (VMarkerCaption <> nil) and (VMarkerIcon <> nil) then begin
    Result := TMarkerDrawableComplex.Create(TMarkerDrawableByBitmapMarker.Create(VMarkerIcon), VMarkerCaption);
  end else if VMarkerCaption <> nil then begin
    Result := VMarkerCaption;
  end else if VMarkerIcon <> nil then begin
    Result := TMarkerDrawableByBitmapMarker.Create(VMarkerIcon);
  end;
end;

end.
