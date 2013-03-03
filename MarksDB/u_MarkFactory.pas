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
  i_VectorItemsFactory,
  i_VectorDataItemSimple,
  i_ImportConfig,
  i_MarkPicture,
  i_MarksFactoryConfig,
  i_Category,
  i_MarksSimple,
  i_MarkTemplate,
  i_HtmlToHintTextConverter,
  i_MarkFactory,
  u_BaseInterfacedObject;

type

  TMarkFactory = class(TBaseInterfacedObject, IMarkFactory)
  private
    FConfig: IMarksFactoryConfig;
    FFactory: IVectorItemsFactory;
    FHintConverter: IHtmlToHintTextConverter;

    FMarkPictureList: IMarkPictureList;

    function CreatePoint(
      const AName: string;
      const APic: IMarkPicture;
      const ACategory: ICategory;
      const ADesc: string;
      const APoint: TDoublePoint;
      ATextColor, ATextBgColor: TColor32;
      AFontSize, AMarkerSize: Integer
    ): IMarkPoint;
    function CreateLine(
      const AName: string;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: ILonLatPath;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IMarkLine;
    function CreatePoly(
      const AName: string;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: ILonLatPolygon;
      ABorderColor, AFillColor: TColor32;
      ALineWidth: Integer
    ): IMarkPoly;
  private
    function CreateNewPoint(
      const APoint: TDoublePoint;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplatePoint = nil
    ): IMarkPoint;
    function CreateNewLine(
      const ALine: ILonLatPath;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplateLine = nil
    ): IMarkLine;
    function CreateNewPoly(
      const ALine: ILonLatPolygon;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplatePoly = nil
    ): IMarkPoly;

    function ModifyPoint(
      const ASource: IMarkPoint;
      const AName: string;
      const APic: IMarkPicture;
      const ACategory: ICategory;
      const ADesc: string;
      const APoint: TDoublePoint;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      AFontSize: Integer;
      AMarkerSize: Integer
    ): IMarkPoint;
    function ModifyLine(
      const ASource: IMarkLine;
      const AName: string;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: ILonLatPath;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IMarkLine;
    function ModifyPoly(
      const ASource: IMarkPoly;
      const AName: string;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: ILonLatPolygon;
      ABorderColor: TColor32;
      AFillColor: TColor32;
      ALineWidth: Integer
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
      const ALine: ILonLatPath;
      const ADesc: string
    ): IMarkLine;
    function SimpleModifyPoly(
      const ASource: IMarkPoly;
      const ALine: ILonLatPolygon
    ): IMarkPoly;

    function PreparePoint(
      const AItem: IVectorDataItemPoint;
      const AName: string;
      const AParams: IImportPointParams;
      const ACategory: ICategory;
      const ADefaultPic: IMarkPicture
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
    function GetConfig: IMarksFactoryConfig;
  public
    constructor Create(
      const AConfig: IMarksFactoryConfig;
      const AMarkPictureList: IMarkPictureList;
      const AFactory: IVectorItemsFactory;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

implementation

uses
  SysUtils,
  u_MarkPoint,
  u_MarkLine,
  u_MarkPoly;

{ TMarkFactory }

constructor TMarkFactory.Create(
  const AConfig: IMarksFactoryConfig;
  const AMarkPictureList: IMarkPictureList;
  const AFactory: IVectorItemsFactory;
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  inherited Create;
  FConfig := AConfig;
  FFactory := AFactory;
  FHintConverter := AHintConverter;
  FMarkPictureList := AMarkPictureList;
end;

function TMarkFactory.CreateNewLine(
  const ALine: ILonLatPath;
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
      VName,
      VCategory,
      ADesc,
      ALine,
      VTemplate.LineColor,
      VTemplate.LineWidth
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
  VPicName: string;
  VPic: IMarkPicture;
  VPicIndex: Integer;
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

  VPic := nil;
  VPicName := VTemplate.PicName;
  if VPicName <> '' then begin
    VPicIndex := FMarkPictureList.GetIndexByName(VPicName);
    if VPicIndex >= 0 then begin
      VPic := FMarkPictureList.Get(VPicIndex);
    end;
  end else begin
    VPic := FMarkPictureList.GetDefaultPicture;
  end;

  Result :=
    CreatePoint(
      VName,
      VPic,
      VCategory,
      ADesc,
      APoint,
      VTemplate.TextColor,
      VTemplate.TextBgColor,
      VTemplate.FontSize,
      VTemplate.MarkerSize
    );
end;

function TMarkFactory.CreateNewPoly(
  const ALine: ILonLatPolygon;
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
      VName,
      VCategory,
      ADesc,
      ALine,
      VTemplate.LineColor,
      VTemplate.FillColor,
      VTemplate.LineWidth
    );
end;

function TMarkFactory.CreatePoint(
  const AName: string;
  const APic: IMarkPicture;
  const ACategory: ICategory;
  const ADesc: string;
  const APoint: TDoublePoint;
  ATextColor, ATextBgColor: TColor32;
  AFontSize, AMarkerSize: Integer
): IMarkPoint;
begin
  Result :=
    TMarkPoint.Create(
      FHintConverter,
      AName,
      APic,
      ACategory,
      ADesc,
      APoint,
      ATextColor,
      ATextBgColor,
      AFontSize,
      AMarkerSize
    );
end;

function TMarkFactory.CreateLine(
  const AName: string;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPath;
  ALineColor: TColor32;
  ALineWidth: Integer
): IMarkLine;
begin
  Result :=
    TMarkLine.Create(
      FHintConverter,
      AName,
      ACategory,
      ADesc,
      ALine,
      ALineColor,
      ALineWidth
    );
end;

function TMarkFactory.CreatePoly(
  const AName: string;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPolygon;
  ABorderColor, AFillColor: TColor32;
  ALineWidth: Integer
): IMarkPoly;
begin
  Result :=
    TMarkPoly.Create(
      FHintConverter,
      AName,
      ACategory,
      ADesc,
      ALine,
      ABorderColor,
      AFillColor,
      ALineWidth
    );
end;

function TMarkFactory.SimpleModifyLine(
  const ASource: IMarkLine;
  const ALine: ILonLatPath;
  const ADesc: string
): IMarkLine;
var
  VDesc: string;
begin
  VDesc := ADesc;
  if ADesc = '' then begin
    VDesc := ASource.Desc;
  end;

  Result :=
    CreateLine(
      ASource.Name,
      ASource.Category,
      VDesc,
      ALine,
      ASource.LineColor,
      ASource.LineWidth
    );
end;

function TMarkFactory.SimpleModifyPoint(
  const ASource: IMarkPoint;
  const ALonLat: TDoublePoint
): IMarkPoint;
begin
  Result :=
    CreatePoint(
      ASource.Name,
      ASource.Pic,
      ASource.Category,
      ASource.Desc,
      ALonLat,
      ASource.TextColor,
      ASource.TextBgColor,
      ASource.FontSize,
      ASource.MarkerSize
    );
end;

function TMarkFactory.SimpleModifyPoly(
  const ASource: IMarkPoly;
  const ALine: ILonLatPolygon
): IMarkPoly;
begin
  Result :=
    CreatePoly(
      ASource.Name,
      ASource.Category,
      ASource.Desc,
      ALine,
      ASource.LineColor,
      ASource.FillColor,
      ASource.LineWidth
    );
end;

function TMarkFactory.ModifyPoint(
  const ASource: IMarkPoint;
  const AName: string;
  const APic: IMarkPicture;
  const ACategory: ICategory;
  const ADesc: string;
  const APoint: TDoublePoint;
  ATextColor: TColor32;
  ATextBgColor: TColor32;
  AFontSize: Integer;
  AMarkerSize: Integer
): IMarkPoint;
begin
  Result :=
    CreatePoint(
      AName,
      APic,
      ACategory,
      ADesc,
      APoint,
      ATextColor,
      ATextBgColor,
      AFontSize,
      AMarkerSize
    );
end;

function TMarkFactory.ModifyLine(
  const ASource: IMarkLine;
  const AName: string;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPath;
  ALineColor: TColor32;
  ALineWidth: Integer
): IMarkLine;
begin
  Result :=
    CreateLine(
      AName,
      ACategory,
      ADesc,
      ALine,
      ALineColor,
      ALineWidth
    );
end;

function TMarkFactory.ModifyPoly(
  const ASource: IMarkPoly;
  const AName: string;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPolygon;
  ABorderColor: TColor32;
  AFillColor: TColor32;
  ALineWidth: Integer
): IMarkPoly;
begin
  Result :=
    CreatePoly(
      AName,
      ACategory,
      ADesc,
      ALine,
      ABorderColor,
      AFillColor,
      ALineWidth
    );
end;

function TMarkFactory.PreparePoint(const AItem: IVectorDataItemPoint;
  const AName: string; const AParams: IImportPointParams;
  const ACategory: ICategory; const ADefaultPic: IMarkPicture): IMarkPoint;
var
  VName: string;
  VPic: IMarkPicture;
  VTextColor, VTextBgColor: TColor32;
  VFontSize, VMarkerSize: Integer;
  VItemWithIconParams: IVectorDataItemPointWithIconParams;
  VItemWithCaptionParams: IVectorDataItemPointWithCaptionParams;
begin
  Result := nil;
  if AParams <> nil then begin
    VName := AName;
    if VName = '' then begin
      VName := AParams.Template.GetNewName;
    end;
    VPic := ADefaultPic;
    VFontSize := AParams.Template.FontSize;
    VMarkerSize := AParams.Template.MarkerSize;
    VTextColor := AParams.Template.TextColor;
    VTextBgColor := AParams.Template.TextBgColor;
    if Supports(AItem, IVectorDataItemPointWithIconParams, VItemWithIconParams) then begin
      if not AParams.IsForcePicName then begin
        VPic := VItemWithIconParams.Pic;
      end;
      if not AParams.IsForceMarkerSize then begin
        VMarkerSize := VItemWithIconParams.MarkerSize;
      end;
    end;
    if Supports(AItem, IVectorDataItemPointWithCaptionParams, VItemWithCaptionParams) then begin
      if not AParams.IsForceTextColor then begin
        VTextColor := VItemWithCaptionParams.TextColor;
      end;
      if not AParams.IsForceTextBgColor then begin
        VTextBgColor := VItemWithCaptionParams.TextBgColor;
      end;
      if not AParams.IsForceFontSize then begin
        VFontSize := VItemWithCaptionParams.FontSize;
      end;
    end;
    Result :=
      CreatePoint(
        VName,
        VPic,
        ACategory,
        AItem.Desc,
        AItem.Point,
        VTextColor,
        VTextBgColor,
        VFontSize,
        VMarkerSize
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
  VName: string;
  VLineColor: TColor32;
  VLineWidth: Integer;
  VItemWithLineParams: IVectorDataItemWithLineParams;
begin
  Result := nil;
  if AParams <> nil then begin
    VName := AName;
    if VName = '' then begin
      VName := AParams.Template.GetNewName;
    end;
    VLineColor := AParams.Template.LineColor;
    VLineWidth := AParams.Template.LineWidth;
    if Supports(AItem, IVectorDataItemWithLineParams, VItemWithLineParams) then begin
      if not AParams.IsForceLineColor then begin
        VLineColor := VItemWithLineParams.LineColor;
      end;
      if not AParams.IsForceLineWidth then begin
        VLineWidth := VItemWithLineParams.LineWidth;
      end;
    end;
    Result :=
      CreateLine(
        VName,
        ACategory,
        AItem.Desc,
        AItem.Line,
        VLineColor,
        VLineWidth
      );
  end;
end;

function TMarkFactory.PreparePoly(const AItem: IVectorDataItemPoly;
  const AName: string; const AParams: IImportPolyParams;
  const ACategory: ICategory): IMarkPoly;
var
  VName: string;
  VLineColor: TColor32;
  VLineWidth: Integer;
  VFillColor: TColor32;
  VItemWithLineParams: IVectorDataItemWithLineParams;
  VItemWithFillParams: IVectorDataItemPolyWithFillParams;
begin
  Result := nil;
  if AParams <> nil then begin
    VName := AName;
    if VName = '' then begin
      VName := AParams.Template.GetNewName;
    end;
    VLineColor := AParams.Template.LineColor;
    VLineWidth := AParams.Template.LineWidth;
    VFillColor := AParams.Template.FillColor;
    if Supports(AItem, IVectorDataItemWithLineParams, VItemWithLineParams) then begin
      if not AParams.IsForceLineColor then begin
        VLineColor := VItemWithLineParams.LineColor;
      end;
      if not AParams.IsForceLineWidth then begin
        VLineWidth := VItemWithLineParams.LineWidth;
      end;
    end;
    if Supports(AItem, IVectorDataItemPolyWithFillParams, VItemWithFillParams) then begin
      if not AParams.IsForceFillColor then begin
        VFillColor := VItemWithFillParams.FillColor;
      end;
    end;
    Result :=
      CreatePoly(
        VName,
        ACategory,
        AItem.Desc,
        AItem.Line,
        VLineColor,
        VFillColor,
        VLineWidth
      );
  end;
end;

function TMarkFactory.ReplaceCategory(const AMark: IMark;
  const ACategory: ICategory): IMark;
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
      ModifyPoint(
        VMarkPoint,
        VMarkPoint.Name,
        VMarkPoint.Pic,
        ACategory,
        VMarkPoint.Desc,
        VMarkPoint.Point,
        VMarkPoint.TextColor,
        VMarkPoint.TextBgColor,
        VMarkPoint.FontSize,
        VMarkPoint.MarkerSize
      );
  end else if Supports(AMark, IMarkLine, VMarkLine) then begin
    Result :=
      ModifyLine(
        VMarkLine,
        VMarkLine.Name,
        ACategory,
        VMarkLine.Desc,
        VMarkLine.Line,
        VMarkLine.LineColor,
        VMarkLine.LineWidth
      );
  end else if Supports(AMark, IMarkPoly, VMarkPoly) then begin
    Result :=
      ModifyPoly(
        VMarkPoly,
        VMarkPoly.Name,
        ACategory,
        VMarkPoly.Desc,
        VMarkPoly.Line,
        VMarkPoly.LineColor,
        VMarkPoly.FillColor,
        VMarkPoly.LineWidth
      );
  end;
end;

function TMarkFactory.GetConfig: IMarksFactoryConfig;
begin
  Result := FConfig;
end;

function TMarkFactory.GetMarkPictureList: IMarkPictureList;
begin
  Result := FMarkPictureList;
end;

end.
