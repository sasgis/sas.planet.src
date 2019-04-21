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
  i_Bitmap32BufferFactory,
  i_BitmapMarker,
  i_MarkerProviderForVectorItem,
  u_BaseInterfacedObject;

type
  TMarkerProviderForVectorItemForMarkPoints = class(TBaseInterfacedObject, IMarkerProviderForVectorItem)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FMarkerProviderByAppearancePointIcon: IMarkerProviderByAppearancePointIcon;

    FBitmapWithText: TBitmap32;

    function GetCaptionBitmap(
      const ACaption: string;
      AFontSize: Integer;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      ASolidBgDraw: Boolean
    ): IBitmap32Static;
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
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AMarkerProviderByAppearancePointIcon: IMarkerProviderByAppearancePointIcon
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  SysUtils,
  GR32_Resamplers,
  i_AppearanceOfVectorItem,
  u_Bitmap32ByStaticBitmap,
  u_MarkerDrawableByBitmapMarker,
  u_MarkerDrawableByBitmap32Static,
  u_MarkerDrawableComplex,
  u_GeoFunc;

{ TMarkerProviderForVectorItemForMarkPoints }

constructor TMarkerProviderForVectorItemForMarkPoints.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AMarkerProviderByAppearancePointIcon: IMarkerProviderByAppearancePointIcon
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AMarkerProviderByAppearancePointIcon));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FMarkerProviderByAppearancePointIcon := AMarkerProviderByAppearancePointIcon;

  FBitmapWithText := TBitmap32.Create;
  FBitmapWithText.Font.Name := 'Tahoma';
  FBitmapWithText.Font.Style := [];
  FBitmapWithText.DrawMode := dmBlend;
  FBitmapWithText.CombineMode := cmMerge;
  FBitmapWithText.Resampler := TLinearResampler.Create;
end;

destructor TMarkerProviderForVectorItemForMarkPoints.Destroy;
begin
  FreeAndNil(FBitmapWithText);
  inherited;
end;

function TMarkerProviderForVectorItemForMarkPoints.GetCaptionBitmap(
  const ACaption: string;
  AFontSize: Integer;
  ATextColor, ATextBgColor: TColor32;
  ASolidBgDraw: Boolean
): IBitmap32Static;
var
  VTextSize: TSize;
  VBitmap: TBitmap32ByStaticBitmap;
begin
  Result := nil;
  if (AFontSize > 0) and (ACaption <> '') then begin
    FBitmapWithText.MasterAlpha := AlphaComponent(ATextColor);
    FBitmapWithText.Font.Size := AFontSize;
    VTextSize := FBitmapWithText.TextExtent(ACaption);
    VTextSize.cx := VTextSize.cx + 2;
    VTextSize.cy := VTextSize.cy + 2;
    FBitmapWithText.SetSize(VTextSize.cx + 2, VTextSize.cy + 2);
    if ASolidBgDraw then begin
      FBitmapWithText.Clear(ATextBgColor);
      FBitmapWithText.RenderText(2, 2, ACaption, 1, SetAlpha(ATextColor, 255));
    end else begin
      FBitmapWithText.Clear(0);
      FBitmapWithText.RenderText(2, 2, ACaption, 1, SetAlpha(ATextBgColor, 255));
      FBitmapWithText.RenderText(1, 1, ACaption, 1, SetAlpha(ATextColor, 255));
    end;
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
    try
      VBitmap.SetSizeFrom(FBitmapWithText);
      VBitmap.Clear(0);
      VBitmap.Draw(0, 0, FBitmapWithText);
      Result := VBitmap.MakeAndClear;
    finally
      VBitmap.Free;
    end;
  end;
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
  VBitmapStatic := GetCaptionBitmap(ACaption, AFontSize, ATextColor, ATextBgColor, ASolidBgDraw);
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
