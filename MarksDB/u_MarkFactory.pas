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

unit u_MarkFactory;

interface

uses
  t_Bitmap32,
  i_GeometryLonLat,
  i_Appearance,
  i_AppearanceOfMarkFactory,
  i_VectorDataItemSimple,
  i_HashFunction,
  i_ImportConfig,
  i_MarkPicture,
  i_MarkFactoryConfig,
  i_Category,
  i_MarkTemplate,
  i_HtmlToHintTextConverter,
  i_MarkFactory,
  u_BaseInterfacedObject;

type

  TMarkFactory = class(TBaseInterfacedObject, IMarkFactory)
  private
    FHashFunction: IHashFunction;
    FConfig: IMarkFactoryConfig;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FHintConverter: IHtmlToHintTextConverter;

    FMarkPictureList: IMarkPictureList;
    function PrepareAppearence(
      const ASource: IAppearance;
      const AParams: IImportMarkParams
    ): IAppearance;
    function PreparePointAppearence(
      const ASource: IAppearance;
      const AParams: IImportPointParams
    ): IAppearance;
    function PrepareLineAppearence(
      const ASource: IAppearance;
      const AParams: IImportLineParams
    ): IAppearance;
    function PreparePolyAppearence(
      const ASource: IAppearance;
      const AParams: IImportPolyParams
    ): IAppearance;
  private
    function CreateNewMark(
      const AGeometry: IGeometryLonLat;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplate = nil
    ): IVectorDataItem;
    function CreateMark(
      const AGeometry: IGeometryLonLat;
      const AName: string;
      const ADesc: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IVectorDataItem;

    function ReplaceCategory(
      const AMark: IVectorDataItem;
      const ACategory: ICategory
    ): IVectorDataItem;

    function ModifyGeometry(
      const ASource: IVectorDataItem;
      const AGeometry: IGeometryLonLat;
      const ADesc: string = ''
    ): IVectorDataItem;

    function PrepareMark(
      const AItem: IVectorDataItem;
      const AName: string;
      const AParams: IImportMarkParams;
      const ACategory: ICategory
    ): IVectorDataItem;

    function GetMarkPictureList: IMarkPictureList;
    function GetConfig: IMarkFactoryConfig;
  public
    constructor Create(
      const AConfig: IMarkFactoryConfig;
      const AMarkPictureList: IMarkPictureList;
      const AHashFunction: IHashFunction;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

implementation

uses
  SysUtils,
  t_Hash,
  i_AppearanceOfVectorItem,
  u_MarkFullBase,
  u_VectorDataItemBase;

{ TMarkFactory }

constructor TMarkFactory.Create(
  const AConfig: IMarkFactoryConfig;
  const AMarkPictureList: IMarkPictureList;
  const AHashFunction: IHashFunction;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  Assert(Assigned(AHashFunction));
  inherited Create;
  FConfig := AConfig;
  FHashFunction := AHashFunction;
  FHashFunction := AHashFunction;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FHintConverter := AHintConverter;
  FMarkPictureList := AMarkPictureList;
end;

function TMarkFactory.CreateNewMark(
  const AGeometry: IGeometryLonLat;
  const AName: string;
  const ADesc: string;
  const ATemplate: IMarkTemplate = nil
): IVectorDataItem;
var
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatMultiLine;
  VPoly: IGeometryLonLatMultiPolygon;
  VTemplate: IMarkTemplate;
  VName: string;
  VCategory: ICategory;
begin
  Result := nil;
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
      VTemplate := FConfig.PointTemplateConfig.DefaultTemplate;
    end else if Supports(AGeometry, IGeometryLonLatLine, VLine) then begin
      VTemplate := FConfig.LineTemplateConfig.DefaultTemplate;
    end else if Supports(AGeometry, IGeometryLonLatMultiLine, VLine) then begin
      VTemplate := FConfig.LineTemplateConfig.DefaultTemplate;
    end else if Supports(AGeometry, IGeometryLonLatPolygon, VPoly) then begin
      VTemplate := FConfig.PolyTemplateConfig.DefaultTemplate;
    end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VPoly) then begin
      VTemplate := FConfig.PolyTemplateConfig.DefaultTemplate;
    end;
  end;

  VName := AName;
  if VName = '' then begin
    VName := VTemplate.GetNewName;
  end;

  VCategory := VTemplate.Category;

  Result :=
    CreateMark(
      AGeometry,
      VName,
      ADesc,
      VCategory,
      VTemplate.Appearance
    );
end;

function TMarkFactory.CreateMark(
  const AGeometry: IGeometryLonLat;
  const AName, ADesc: string;
  const ACategory: ICategory;
  const AAppearance: IAppearance
): IVectorDataItem;
var
  VHash: THashValue;
  VMainInfo: IVectorDataItemMainInfo;
begin
  Assert(Assigned(AGeometry));
  Assert(Assigned(AAppearance));

  VHash := FHashFunction.CalcHashByString(AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  VMainInfo :=
    TMarkMainInfo.Create(
      VHash,
      FHintConverter,
      AName,
      ACategory,
      ADesc
    );

  VHash := AGeometry.Hash;
  FHashFunction.UpdateHashByHash(VHash, VMainInfo.Hash);
  FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);

  Result :=
    TVectorDataItem.Create(
      VHash,
      AAppearance,
      VMainInfo,
      AGeometry
    );
end;

function TMarkFactory.ModifyGeometry(
  const ASource: IVectorDataItem;
  const AGeometry: IGeometryLonLat;
  const ADesc: string
): IVectorDataItem;
var
  VDesc: string;
  VCategory: ICategory;
  VMarkWithCategory: IVectorDataItemWithCategory;
begin
  VDesc := ADesc;
  if ADesc = '' then begin
    VDesc := ASource.Desc;
  end;
  VCategory := nil;
  if Supports(ASource.MainInfo, IVectorDataItemWithCategory, VMarkWithCategory) then begin
    VCategory := VMarkWithCategory.Category;
  end;
  Result :=
    CreateMark(
      AGeometry,
      ASource.Name,
      ASource.Desc,
      VCategory,
      ASource.Appearance
    );
end;

function TMarkFactory.PrepareAppearence(
  const ASource: IAppearance;
  const AParams: IImportMarkParams
): IAppearance;
var
  VPointParams: IImportPointParams;
  VLineParams: IImportLineParams;
  VPolyParams: IImportPolyParams;
begin
  Result := nil;
  if AParams <> nil then begin
    if Supports(AParams, IImportPointParams, VPointParams) then begin
      Result := PreparePointAppearence(ASource, VPointParams);
    end else if Supports(AParams, IImportLineParams, VLineParams) then begin
      Result := PrepareLineAppearence(ASource, VLineParams);
    end else if Supports(AParams, IImportPolyParams, VPolyParams) then begin
      Result := PreparePolyAppearence(ASource, VPolyParams);
    end;
  end;
end;

function TMarkFactory.PreparePointAppearence(
  const ASource: IAppearance;
  const AParams: IImportPointParams
): IAppearance;
var
  VPic: IMarkPicture;
  VPicName: string;
  VTextColor, VTextBgColor: TColor32;
  VFontSize, VMarkerSize: Integer;
  VAppearanceCaption: IAppearancePointCaption;
  VAppearanceIcon: IAppearancePointIcon;
begin
  Result := nil;
  if AParams <> nil then begin
    VPic := AParams.IconAppearance.Pic;
    VPicName := AParams.IconAppearance.PicName;
    VFontSize := AParams.CaptionAppearance.FontSize;
    VMarkerSize := AParams.IconAppearance.MarkerSize;
    VTextColor := AParams.CaptionAppearance.TextColor;
    VTextBgColor := AParams.CaptionAppearance.TextBgColor;
    if Supports(ASource, IAppearancePointIcon, VAppearanceIcon) then begin
      if not AParams.IsForcePicName then begin
        VPic := VAppearanceIcon.Pic;
        VPicName := VAppearanceIcon.PicName;
      end;
      if not AParams.IsForceMarkerSize then begin
        VMarkerSize := VAppearanceIcon.MarkerSize;
      end;
    end;
    if Supports(ASource, IAppearancePointCaption, VAppearanceCaption) then begin
      if not AParams.IsForceTextColor then begin
        VTextColor := VAppearanceCaption.TextColor;
      end;
      if not AParams.IsForceTextBgColor then begin
        VTextBgColor := VAppearanceCaption.TextBgColor;
      end;
      if not AParams.IsForceFontSize then begin
        VFontSize := VAppearanceCaption.FontSize;
      end;
    end;
    Result :=
      FAppearanceOfMarkFactory.CreatePointAppearance(
        VTextColor,
        VTextBgColor,
        VFontSize,
        VPicName,
        VPic,
        VMarkerSize
      );
  end;
end;

function TMarkFactory.PrepareLineAppearence(
  const ASource: IAppearance;
  const AParams: IImportLineParams
): IAppearance;
var
  VLineColor: TColor32;
  VLineWidth: Integer;
  VAppearanceLine: IAppearanceLine;
begin
  Result := nil;
  if AParams <> nil then begin
    VLineColor := AParams.LineAppearance.LineColor;
    VLineWidth := AParams.LineAppearance.LineWidth;
    if Supports(ASource, IAppearanceLine, VAppearanceLine) then begin
      if not AParams.IsForceLineColor then begin
        VLineColor := VAppearanceLine.LineColor;
      end;
      if not AParams.IsForceLineWidth then begin
        VLineWidth := VAppearanceLine.LineWidth;
      end;
    end;
    Result :=
      FAppearanceOfMarkFactory.CreateLineAppearance(
        VLineColor,
        VLineWidth
      );
  end;
end;

function TMarkFactory.PrepareMark(
  const AItem: IVectorDataItem;
  const AName: string;
  const AParams: IImportMarkParams;
  const ACategory: ICategory
): IVectorDataItem;
var
  VAppearance: IAppearance;
begin
  Result := nil;
  if AParams <> nil then begin
    VAppearance := PrepareAppearence(AItem.Appearance, AParams);
    Result :=
      CreateMark(
        AItem.Geometry,
        AName,
        AItem.Desc,
        ACategory,
        VAppearance
      );
  end;
end;

function TMarkFactory.PreparePolyAppearence(
  const ASource: IAppearance;
  const AParams: IImportPolyParams
): IAppearance;
var
  VLineColor: TColor32;
  VLineWidth: Integer;
  VFillColor: TColor32;
  VAppearanceBorder: IAppearancePolygonBorder;
  VAppearanceFill: IAppearancePolygonFill;
begin
  Result := nil;
  if AParams <> nil then begin
    VLineColor := AParams.BorderAppearance.LineColor;
    VLineWidth := AParams.BorderAppearance.LineWidth;
    VFillColor := AParams.FillAppearance.FillColor;
    if Supports(ASource, IAppearancePolygonBorder, VAppearanceBorder) then begin
      if not AParams.IsForceLineColor then begin
        VLineColor := VAppearanceBorder.LineColor;
      end;
      if not AParams.IsForceLineWidth then begin
        VLineWidth := VAppearanceBorder.LineWidth;
      end;
    end;
    if Supports(ASource, IAppearancePolygonFill, VAppearanceFill) then begin
      if not AParams.IsForceFillColor then begin
        VFillColor := VAppearanceFill.FillColor;
      end;
    end;
    Result :=
      FAppearanceOfMarkFactory.CreatePolygonAppearance(
        VLineColor,
        VLineWidth,
        VFillColor
      );
  end;
end;

function TMarkFactory.ReplaceCategory(
  const AMark: IVectorDataItem;
  const ACategory: ICategory
): IVectorDataItem;
begin
  Result := nil;
  if AMark = nil then begin
    Exit;
  end;
  Result :=
    CreateMark(
      AMark.Geometry,
      AMark.Name,
      AMark.Desc,
      ACategory,
      AMark.Appearance
    );
end;

function TMarkFactory.GetConfig: IMarkFactoryConfig;
begin
  Result := FConfig;
end;

function TMarkFactory.GetMarkPictureList: IMarkPictureList;
begin
  Result := FMarkPictureList;
end;

end.
