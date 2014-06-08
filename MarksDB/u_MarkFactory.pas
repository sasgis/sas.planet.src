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
      const ADesc: string
    ): IVectorDataItemSimple;
    function CreateNewPoint(
      const APoint: IGeometryLonLatPoint;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplatePoint = nil
    ): IVectorDataItemPoint;
    function CreateNewLine(
      const ALine: IGeometryLonLatMultiLine;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplateLine = nil
    ): IVectorDataItemLine;
    function CreateNewPoly(
      const ALine: IGeometryLonLatMultiPolygon;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplatePoly = nil
    ): IVectorDataItemPoly;

    function CreateMark(
      const AGeometry: IGeometryLonLat;
      const AName: string;
      const ADesc: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IVectorDataItemSimple;
    function CreatePoint(
      const APoint: IGeometryLonLatPoint;
      const AName: string;
      const ADesc: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IVectorDataItemPoint;
    function CreateLine(
      const ALine: IGeometryLonLatMultiLine;
      const AName: string;
      const ADesc: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IVectorDataItemLine;
    function CreatePoly(
      const ALine: IGeometryLonLatMultiPolygon;
      const AName: string;
      const ADesc: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IVectorDataItemPoly;

    function ReplaceCategory(
      const AMark: IVectorDataItemSimple;
      const ACategory: ICategory
    ): IVectorDataItemSimple;

    function ModifyGeometry(
      const ASource: IVectorDataItemSimple;
      const AGeometry: IGeometryLonLat;
      const ADesc: string
    ): IVectorDataItemSimple;
    function SimpleModifyPoint(
      const ASource: IVectorDataItemPoint;
      const ALonLat: IGeometryLonLatPoint
    ): IVectorDataItemPoint;
    function SimpleModifyLine(
      const ASource: IVectorDataItemLine;
      const ALine: IGeometryLonLatMultiLine;
      const ADesc: string
    ): IVectorDataItemLine;
    function SimpleModifyPoly(
      const ASource: IVectorDataItemPoly;
      const ALine: IGeometryLonLatMultiPolygon
    ): IVectorDataItemPoly;

    function PrepareMark(
      const AItem: IVectorDataItemSimple;
      const AName: string;
      const AParams: IImportMarkParams;
      const ACategory: ICategory
    ): IVectorDataItemSimple;
    function PreparePoint(
      const AItem: IVectorDataItemPoint;
      const AName: string;
      const AParams: IImportPointParams;
      const ACategory: ICategory
    ): IVectorDataItemPoint;
    function PrepareLine(
      const AItem: IVectorDataItemLine;
      const AName: string;
      const AParams: IImportLineParams;
      const ACategory: ICategory
    ): IVectorDataItemLine;
    function PreparePoly(
      const AItem: IVectorDataItemPoly;
      const AName: string;
      const AParams: IImportPolyParams;
      const ACategory: ICategory
    ): IVectorDataItemPoly;

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
  u_VectorDataItemPoint,
  u_VectorDataItemPolygon;

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
): IVectorDataItemLine;
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

function TMarkFactory.CreateNewMark(
  const AGeometry: IGeometryLonLat;
  const AName, ADesc: string
): IVectorDataItemSimple;
var
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatMultiLine;
  VPoly: IGeometryLonLatMultiPolygon;
begin
  Result := nil;
  if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
    Result := CreateNewPoint(VPoint, AName, ADesc);
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VLine) then begin
    Result := CreateNewLine(VLine, AName, ADesc);
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VPoly) then begin
    Result := CreateNewPoly(VPoly, AName, ADesc);
  end;
end;

function TMarkFactory.CreateNewPoint(
  const APoint: IGeometryLonLatPoint;
  const AName, ADesc: string;
  const ATemplate: IMarkTemplatePoint
): IVectorDataItemPoint;
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
): IVectorDataItemPoly;
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
  const APoint: IGeometryLonLatPoint;
  const AName: string;
  const ADesc: string;
  const ACategory: ICategory;
  const AAppearance: IAppearance
): IVectorDataItemPoint;
var
  VHash: THashValue;
  VMainInfo: IVectorDataItemMainInfo;
begin
  Assert(Assigned(APoint));
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

  VHash := APoint.Hash;
  FHashFunction.UpdateHashByHash(VHash, VMainInfo.Hash);
  FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);

  Result :=
    TVectorDataItemPoint.Create(
      VHash,
      AAppearance,
      VMainInfo,
      APoint
    );
end;

function TMarkFactory.CreateLine(
  const ALine: IGeometryLonLatMultiLine;
  const AName: string;
  const ADesc: string;
  const ACategory: ICategory;
  const AAppearance: IAppearance
): IVectorDataItemLine;
var
  VHash: THashValue;
  VMainInfo: IVectorDataItemMainInfo;
begin
  Assert(Assigned(ALine));
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

  VHash := ALine.Hash;
  FHashFunction.UpdateHashByHash(VHash, VMainInfo.Hash);
  FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);
  Result :=
    TVectorDataItemPath.Create(
      VHash,
      AAppearance,
      VMainInfo,
      ALine
    );
end;

function TMarkFactory.CreateMark(
  const AGeometry: IGeometryLonLat;
  const AName, ADesc: string;
  const ACategory: ICategory;
  const AAppearance: IAppearance
): IVectorDataItemSimple;
var
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatMultiLine;
  VPoly: IGeometryLonLatMultiPolygon;
begin
  Result := nil;
  if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
    Result := CreatePoint(VPoint, AName, ADesc, ACategory, AAppearance);
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VLine) then begin
    Result := CreateLine(VLine, AName, ADesc, ACategory, AAppearance);
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VPoly) then begin
    Result := CreatePoly(VPoly, AName, ADesc, ACategory, AAppearance);
  end;
end;

function TMarkFactory.CreatePoly(
  const ALine: IGeometryLonLatMultiPolygon;
  const AName: string;
  const ADesc: string;
  const ACategory: ICategory;
  const AAppearance: IAppearance
): IVectorDataItemPoly;
var
  VHash: THashValue;
  VMainInfo: IVectorDataItemMainInfo;
begin
  Assert(Assigned(ALine));
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

  VHash := ALine.Hash;
  FHashFunction.UpdateHashByHash(VHash, VMainInfo.Hash);
  FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);
  Result :=
    TVectorDataItemPoly.Create(
      VHash,
      AAppearance,
      VMainInfo,
      ALine
    );
end;

function TMarkFactory.ModifyGeometry(
  const ASource: IVectorDataItemSimple;
  const AGeometry: IGeometryLonLat;
  const ADesc: string
): IVectorDataItemSimple;
var
  VPoint: IGeometryLonLatPoint;
  VLine: IGeometryLonLatMultiLine;
  VPoly: IGeometryLonLatMultiPolygon;
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
  Result := nil;
  if Supports(AGeometry, IGeometryLonLatPoint, VPoint) then begin
    Assert(Supports(ASource.Geometry, IGeometryLonLatPoint));
    Result :=
      CreatePoint(
        VPoint,
        ASource.Name,
        VDesc,
        VCategory,
        ASource.Appearance
      );
  end else if Supports(AGeometry, IGeometryLonLatMultiLine, VLine) then begin
    Assert(Supports(ASource.Geometry, IGeometryLonLatMultiLine));
    Result :=
      CreateLine(
        VLine,
        ASource.Name,
        VDesc,
        VCategory,
        ASource.Appearance
      );
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VPoly) then begin
    Assert(Supports(ASource.Geometry, IGeometryLonLatMultiPolygon));
    Result :=
      CreatePoly(
        VPoly,
        ASource.Name,
        ASource.Desc,
        VCategory,
        ASource.Appearance
      );
  end;
end;

function TMarkFactory.SimpleModifyLine(
  const ASource: IVectorDataItemLine;
  const ALine: IGeometryLonLatMultiLine;
  const ADesc: string
): IVectorDataItemLine;
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
    CreateLine(
      ALine,
      ASource.Name,
      VDesc,
      VCategory,
      ASource.Appearance
    );
end;

function TMarkFactory.SimpleModifyPoint(
  const ASource: IVectorDataItemPoint;
  const ALonLat: IGeometryLonLatPoint
): IVectorDataItemPoint;
var
  VCategory: ICategory;
  VMarkWithCategory: IVectorDataItemWithCategory;
begin
  VCategory := nil;
  if Supports(ASource.MainInfo, IVectorDataItemWithCategory, VMarkWithCategory) then begin
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
  const ASource: IVectorDataItemPoly;
  const ALine: IGeometryLonLatMultiPolygon
): IVectorDataItemPoly;
var
  VCategory: ICategory;
  VMarkWithCategory: IVectorDataItemWithCategory;
begin
  VCategory := nil;
  if Supports(ASource.MainInfo, IVectorDataItemWithCategory, VMarkWithCategory) then begin
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
): IVectorDataItemPoint;
var
  VAppearance: IAppearance;
begin
  Result := nil;
  if AParams <> nil then begin
    VAppearance := PreparePointAppearence(AItem.Appearance, AParams);
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

function TMarkFactory.PrepareLine(
  const AItem: IVectorDataItemLine;
  const AName: string;
  const AParams: IImportLineParams;
  const ACategory: ICategory
): IVectorDataItemLine;
var
  VAppearance: IAppearance;
begin
  Result := nil;
  if AParams <> nil then begin
    VAppearance := PrepareLineAppearence(AItem.Appearance, AParams);
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
  const AItem: IVectorDataItemSimple;
  const AName: string;
  const AParams: IImportMarkParams;
  const ACategory: ICategory
): IVectorDataItemSimple;
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

function TMarkFactory.PreparePoly(
  const AItem: IVectorDataItemPoly;
  const AName: string;
  const AParams: IImportPolyParams;
  const ACategory: ICategory
): IVectorDataItemPoly;
var
  VAppearance: IAppearance;
begin
  Result := nil;
  if AParams <> nil then begin
    VAppearance := PreparePolyAppearence(AItem.Appearance, AParams);
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
  const AMark: IVectorDataItemSimple;
  const ACategory: ICategory
): IVectorDataItemSimple;
var
  VMarkPoint: IVectorDataItemPoint;
  VMarkLine: IVectorDataItemLine;
  VMarkPoly: IVectorDataItemPoly;
begin
  Result := nil;
  if AMark = nil then begin
    Exit;
  end;
  if Supports(AMark, IVectorDataItemPoint, VMarkPoint) then begin
    Result :=
      CreatePoint(
        VMarkPoint.Point,
        VMarkPoint.Name,
        VMarkPoint.Desc,
        ACategory,
        VMarkPoint.Appearance
      );
  end else if Supports(AMark, IVectorDataItemLine, VMarkLine) then begin
    Result :=
      CreateLine(
        VMarkLine.Line,
        VMarkLine.Name,
        VMarkLine.Desc,
        ACategory,
        AMark.Appearance
      );
  end else if Supports(AMark, IVectorDataItemPoly, VMarkPoly) then begin
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
