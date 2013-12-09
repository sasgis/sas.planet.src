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

unit u_MarkFactory;

interface

uses
  GR32,
  t_GeoTypes,
  i_VectorItemLonLat,
  i_Appearance,
  i_AppearanceOfMarkFactory,
  i_VectorDataItemSimple,
  i_HashFunction,
  i_ImportConfig,
  i_MarkPicture,
  i_MarkFactoryConfig,
  i_Category,
  i_Mark,
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
  private
    function CreateNewPoint(
      const APoint: TDoublePoint;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplatePoint = nil
    ): IMarkPoint;
    function CreateNewLine(
      const ALine: IGeometryLonLatMultiLine;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplateLine = nil
    ): IMarkLine;
    function CreateNewPoly(
      const ALine: IGeometryLonLatMultiPolygon;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplatePoly = nil
    ): IMarkPoly;

    function CreatePoint(
      const APoint: TDoublePoint;
      const AName: string;
      const ADesc: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IMarkPoint;
    function CreateLine(
      const ALine: IGeometryLonLatMultiLine;
      const AName: string;
      const ADesc: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IMarkLine;
    function CreatePoly(
      const ALine: IGeometryLonLatMultiPolygon;
      const AName: string;
      const ADesc: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IMarkPoly;

    function ReplaceCategory(
      const AMark: IMark;
      const ACategory: ICategory
    ): IMark;

    function SimpleModifyPoint(
      const ASource: IMarkPoint;
      const ALonLat: TDoublePoint
    ): IMarkPoint;
    function SimpleModifyLine(
      const ASource: IMarkLine;
      const ALine: IGeometryLonLatMultiLine;
      const ADesc: string
    ): IMarkLine;
    function SimpleModifyPoly(
      const ASource: IMarkPoly;
      const ALine: IGeometryLonLatMultiPolygon
    ): IMarkPoly;

    function PreparePoint(
      const AItem: IVectorDataItemPoint;
      const AName: string;
      const AParams: IImportPointParams;
      const ACategory: ICategory
    ): IMarkPoint;
    function PrepareLine(
      const AItem: IVectorDataItemLine;
      const AName: string;
      const AParams: IImportLineParams;
      const ACategory: ICategory
    ): IMarkLine;
    function PreparePoly(
      const AItem: IVectorDataItemPoly;
      const AName: string;
      const AParams: IImportPolyParams;
      const ACategory: ICategory
    ): IMarkPoly;

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
  u_GeoFun,
  u_MarkPoint,
  u_MarkLine,
  u_MarkPoly;

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

function TMarkFactory.CreateNewLine(
  const ALine: IGeometryLonLatMultiLine;
  const AName, ADesc: string;
  const ATemplate: IMarkTemplateLine
): IMarkLine;
var
  VTemplate: IMarkTemplateLine;
  VName: string;
  VCategory: ICategory;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FConfig.LineTemplateConfig.DefaultTemplate;
  end;

  VName := AName;
  if VName = '' then begin
    VName := VTemplate.GetNewName;
  end;

  VCategory := VTemplate.Category;

  Result :=
    CreateLine(
      ALine,
      VName,
      ADesc,
      VCategory,
      VTemplate.Appearance
    );
end;

function TMarkFactory.CreateNewPoint(
  const APoint: TDoublePoint;
  const AName, ADesc: string;
  const ATemplate: IMarkTemplatePoint
): IMarkPoint;
var
  VTemplate: IMarkTemplatePoint;
  VName: string;
  VCategory: ICategory;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FConfig.PointTemplateConfig.DefaultTemplate;
  end;

  VName := AName;
  if VName = '' then begin
    VName := VTemplate.GetNewName;
  end;

  VCategory := VTemplate.Category;

  Result :=
    CreatePoint(
      APoint,
      VName,
      ADesc,
      VCategory,
      VTemplate.Appearance
    );
end;

function TMarkFactory.CreateNewPoly(
  const ALine: IGeometryLonLatMultiPolygon;
  const AName, ADesc: string;
  const ATemplate: IMarkTemplatePoly
): IMarkPoly;
var
  VTemplate: IMarkTemplatePoly;
  VName: string;
  VCategory: ICategory;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FConfig.PolyTemplateConfig.DefaultTemplate;
  end;

  VName := AName;
  if VName = '' then begin
    VName := VTemplate.GetNewName;
  end;

  VCategory := VTemplate.Category;

  Result :=
    CreatePoly(
      ALine,
      VName,
      ADesc,
      VCategory,
      VTemplate.Appearance
    );
end;

function TMarkFactory.CreatePoint(
  const APoint: TDoublePoint;
  const AName: string;
  const ADesc: string;
  const ACategory: ICategory;
  const AAppearance: IAppearance
): IMarkPoint;
var
  VHash: THashValue;
begin
  Assert(not PointIsEmpty(APoint));
  Assert(Assigned(AAppearance));

  VHash := FHashFunction.CalcHashByDoublePoint(APoint);
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);

  Result :=
    TMarkPoint.Create(
      VHash,
      FHintConverter,
      AName,
      AAppearance,
      ACategory,
      ADesc,
      APoint
    );
end;

function TMarkFactory.CreateLine(
  const ALine: IGeometryLonLatMultiLine;
  const AName: string;
  const ADesc: string;
  const ACategory: ICategory;
  const AAppearance: IAppearance
): IMarkLine;
var
  VHash: THashValue;
begin
  Assert(Assigned(ALine));
  Assert(Assigned(AAppearance));

  VHash := ALine.Hash;
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);
  Result :=
    TMarkLine.Create(
      VHash,
      FHintConverter,
      AName,
      AAppearance,
      ACategory,
      ADesc,
      ALine
    );
end;

function TMarkFactory.CreatePoly(
  const ALine: IGeometryLonLatMultiPolygon;
  const AName: string;
  const ADesc: string;
  const ACategory: ICategory;
  const AAppearance: IAppearance
): IMarkPoly;
var
  VHash: THashValue;
begin
  Assert(Assigned(ALine));
  Assert(Assigned(AAppearance));

  VHash := ALine.Hash;
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);
  Result :=
    TMarkPoly.Create(
      VHash,
      FHintConverter,
      AName,
      AAppearance,
      ACategory,
      ADesc,
      ALine
    );
end;

function TMarkFactory.SimpleModifyLine(
  const ASource: IMarkLine;
  const ALine: IGeometryLonLatMultiLine;
  const ADesc: string
): IMarkLine;
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
  if Supports(ASource, IVectorDataItemWithCategory, VMarkWithCategory) then begin
    VCategory := VMarkWithCategory.Category;
  end;

  Result :=
    CreateLine(
      ALine,
      ASource.Name,
      VDesc,
      VCategory,
      ASource.Appearance
    );
end;

function TMarkFactory.SimpleModifyPoint(
  const ASource: IMarkPoint;
  const ALonLat: TDoublePoint
): IMarkPoint;
var
  VCategory: ICategory;
  VMarkWithCategory: IVectorDataItemWithCategory;
begin
  VCategory := nil;
  if Supports(ASource, IVectorDataItemWithCategory, VMarkWithCategory) then begin
    VCategory := VMarkWithCategory.Category;
  end;
  Result :=
    CreatePoint(
      ALonLat,
      ASource.Name,
      ASource.Desc,
      VCategory,
      ASource.Appearance
    );
end;

function TMarkFactory.SimpleModifyPoly(
  const ASource: IMarkPoly;
  const ALine: IGeometryLonLatMultiPolygon
): IMarkPoly;
var
  VCategory: ICategory;
  VMarkWithCategory: IVectorDataItemWithCategory;
begin
  VCategory := nil;
  if Supports(ASource, IVectorDataItemWithCategory, VMarkWithCategory) then begin
    VCategory := VMarkWithCategory.Category;
  end;
  Result :=
    CreatePoly(
      ALine,
      ASource.Name,
      ASource.Desc,
      VCategory,
      ASource.Appearance
    );
end;

function TMarkFactory.PreparePoint(
  const AItem: IVectorDataItemPoint;
  const AName: string;
  const AParams: IImportPointParams;
  const ACategory: ICategory
): IMarkPoint;
var
  VPic: IMarkPicture;
  VPicName: string;
  VTextColor, VTextBgColor: TColor32;
  VFontSize, VMarkerSize: Integer;
  VAppearanceCaption: IAppearancePointCaption;
  VAppearanceIcon: IAppearancePointIcon;
  VAppearance: IAppearance;
begin
  Result := nil;
  if AParams <> nil then begin
    VPic := AParams.IconAppearance.Pic;
    VPicName := AParams.IconAppearance.PicName;
    VFontSize := AParams.CaptionAppearance.FontSize;
    VMarkerSize := AParams.IconAppearance.MarkerSize;
    VTextColor := AParams.CaptionAppearance.TextColor;
    VTextBgColor := AParams.CaptionAppearance.TextBgColor;
    if Supports(AItem.Appearance, IAppearancePointIcon, VAppearanceIcon) then begin
      if not AParams.IsForcePicName then begin
        VPic := VAppearanceIcon.Pic;
        VPicName := VAppearanceIcon.PicName;
      end;
      if not AParams.IsForceMarkerSize then begin
        VMarkerSize := VAppearanceIcon.MarkerSize;
      end;
    end;
    if Supports(AItem.Appearance, IAppearancePointCaption, VAppearanceCaption) then begin
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
    VAppearance :=
      FAppearanceOfMarkFactory.CreatePointAppearance(
        VTextColor,
        VTextBgColor,
        VFontSize,
        VPicName,
        VPic,
        VMarkerSize
      );
    Result :=
      CreatePoint(
        AItem.Point,
        AName,
        AItem.Desc,
        ACategory,
        VAppearance
      );
  end;
end;

function TMarkFactory.PrepareLine(
  const AItem: IVectorDataItemLine;
  const AName: string;
  const AParams: IImportLineParams;
  const ACategory: ICategory
): IMarkLine;
var
  VLineColor: TColor32;
  VLineWidth: Integer;
  VAppearanceLine: IAppearanceLine;
  VAppearance: IAppearance;
begin
  Result := nil;
  if AParams <> nil then begin
    VLineColor := AParams.LineAppearance.LineColor;
    VLineWidth := AParams.LineAppearance.LineWidth;
    if Supports(AItem.Appearance, IAppearanceLine, VAppearanceLine) then begin
      if not AParams.IsForceLineColor then begin
        VLineColor := VAppearanceLine.LineColor;
      end;
      if not AParams.IsForceLineWidth then begin
        VLineWidth := VAppearanceLine.LineWidth;
      end;
    end;
    VAppearance :=
      FAppearanceOfMarkFactory.CreateLineAppearance(
        VLineColor,
        VLineWidth
      );
    Result :=
      CreateLine(
        AItem.Line,
        AName,
        AItem.Desc,
        ACategory,
        VAppearance
      );
  end;
end;

function TMarkFactory.PreparePoly(const AItem: IVectorDataItemPoly;
  const AName: string; const AParams: IImportPolyParams;
  const ACategory: ICategory): IMarkPoly;
var
  VLineColor: TColor32;
  VLineWidth: Integer;
  VFillColor: TColor32;
  VAppearanceBorder: IAppearancePolygonBorder;
  VAppearanceFill: IAppearancePolygonFill;
  VAppearance: IAppearance;
begin
  Result := nil;
  if AParams <> nil then begin
    VLineColor := AParams.BorderAppearance.LineColor;
    VLineWidth := AParams.BorderAppearance.LineWidth;
    VFillColor := AParams.FillAppearance.FillColor;
    if Supports(AItem.Appearance, IAppearancePolygonBorder, VAppearanceBorder) then begin
      if not AParams.IsForceLineColor then begin
        VLineColor := VAppearanceBorder.LineColor;
      end;
      if not AParams.IsForceLineWidth then begin
        VLineWidth := VAppearanceBorder.LineWidth;
      end;
    end;
    if Supports(AItem, IAppearancePolygonFill, VAppearanceFill) then begin
      if not AParams.IsForceFillColor then begin
        VFillColor := VAppearanceFill.FillColor;
      end;
    end;
    VAppearance :=
      FAppearanceOfMarkFactory.CreatePolygonAppearance(
        VLineColor,
        VLineWidth,
        VFillColor
      );
    Result :=
      CreatePoly(
        AItem.Line,
        AName,
        AItem.Desc,
        ACategory,
        VAppearance
      );
  end;
end;

function TMarkFactory.ReplaceCategory(
  const AMark: IMark;
  const ACategory: ICategory
): IMark;
var
  VMarkPoint: IMarkPoint;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
begin
  Result := nil;
  if AMark = nil then begin
    Exit;
  end;
  if Supports(AMark, IMarkPoint, VMarkPoint) then begin
    Result :=
      CreatePoint(
        VMarkPoint.Point,
        VMarkPoint.Name,
        VMarkPoint.Desc,
        ACategory,
        VMarkPoint.Appearance
      );
  end else if Supports(AMark, IMarkLine, VMarkLine) then begin
    Result :=
      CreateLine(
        VMarkLine.Line,
        VMarkLine.Name,
        VMarkLine.Desc,
        ACategory,
        AMark.Appearance
      );
  end else if Supports(AMark, IMarkPoly, VMarkPoly) then begin
    Result :=
      CreatePoly(
        VMarkPoly.Line,
        VMarkPoly.Name,
        VMarkPoly.Desc,
        ACategory,
        AMark.Appearance
      );
  end;
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
