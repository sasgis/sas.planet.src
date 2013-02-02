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
  i_MarkPicture,
  i_MarksFactoryConfig,
  i_MarkCategory,
  i_MarkCategoryDBSmlInternal,
  i_MarksSimple,
  i_MarkTemplate,
  i_HtmlToHintTextConverter,
  i_MarkFactory,
  i_MarkFactorySmlInternal,
  u_BaseInterfacedObject;

type

  TMarkFactory = class(TBaseInterfacedObject, IMarkFactory, IMarkFactorySmlInternal)
  private
    FConfig: IMarksFactoryConfig;
    FFactory: IVectorItemsFactory;
    FCategoryDB: IMarkCategoryDBSmlInternal;
    FHintConverter: IHtmlToHintTextConverter;

    FMarkPictureList: IMarkPictureList;

    function CreatePoint(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      const APicName: string;
      const APic: IMarkPicture;
      ACategoryId: Integer;
      const ACategory: ICategory;
      const ADesc: string;
      const APoint: TDoublePoint;
      ATextColor, ATextBgColor: TColor32;
      AFontSize, AMarkerSize: Integer
    ): IMarkPoint;
    function CreateLine(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: ILonLatPath;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IMarkLine;
    function CreatePoly(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      ACategoryId: Integer;
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
      AVisible: Boolean;
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
      AVisible: Boolean;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: ILonLatPath;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IMarkLine;
    function ModifyPoly(
      const ASource: IMarkPoly;
      const AName: string;
      AVisible: Boolean;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: ILonLatPolygon;
      ABorderColor: TColor32;
      AFillColor: TColor32;
      ALineWidth: Integer
    ): IMarkPoly;

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

    function GetMarkPictureList: IMarkPictureList;
    function GetConfig: IMarksFactoryConfig;
  private
    function CreateMark(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      const APicName: string;
      ACategoryId: Integer;
      const ADesc: string;
      const APoints: PDoublePointArray;
      APointCount: Integer;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMark;
  public
    constructor Create(
      const AConfig: IMarksFactoryConfig;
      const AMarkPictureList: IMarkPictureList;
      const AFactory: IVectorItemsFactory;
      const AHintConverter: IHtmlToHintTextConverter;
      const ACategoryDB: IMarkCategoryDBSmlInternal
    );
  end;

implementation

uses
  SysUtils,
  i_MarksDbSmlInternal,
  i_MarkCategoryFactoryDbInternal,
  u_GeoFun,
  u_MarkPoint,
  u_MarkLine,
  u_MarkPoly;

{ TMarkFactory }

constructor TMarkFactory.Create(
  const AConfig: IMarksFactoryConfig;
  const AMarkPictureList: IMarkPictureList;
  const AFactory: IVectorItemsFactory;
  const AHintConverter: IHtmlToHintTextConverter;
  const ACategoryDB: IMarkCategoryDBSmlInternal
);
begin
  inherited Create;
  FConfig := AConfig;
  FFactory := AFactory;
  FHintConverter := AHintConverter;
  FCategoryDB := ACategoryDB;
  FMarkPictureList := AMarkPictureList;
end;

function TMarkFactory.CreateNewLine(
  const ALine: ILonLatPath;
  const AName, ADesc: string;
  const ATemplate: IMarkTemplateLine
): IMarkLine;
var
  VTemplate: IMarkTemplateLine;
  VCategoryId: Integer;
  VCategoryStringId: string;
  VName: string;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FConfig.LineTemplateConfig.DefaultTemplate;
  end;

  VName := AName;
  if VName = '' then begin
    VName := VTemplate.GetNewName;
  end;

  VCategoryId := CNotExistCategoryID;
  VCategoryStringId := VTemplate.CategoryStringID;
  if VCategoryStringId <> '' then begin
    VCategoryId := StrToIntDef(VCategoryStringID, VCategoryId);
  end;

  Result :=
    CreateLine(
      CNotExistMarkID,
      VName,
      True,
      VCategoryId,
      nil,
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
  VCategoryStringId: string;
  VName: string;
  VCategoryId: Integer;
  VPicName: string;
  VPic: IMarkPicture;
  VPicIndex: Integer;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FConfig.PointTemplateConfig.DefaultTemplate;
  end;

  VName := AName;
  if VName = '' then begin
    VName := VTemplate.GetNewName;
  end;

  VCategoryId := CNotExistCategoryID;
  VCategoryStringId := VTemplate.CategoryStringID;
  if VCategoryStringId <> '' then begin
    VCategoryId := StrToIntDef(VCategoryStringID, VCategoryId);
  end;

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
      CNotExistMarkID,
      VName,
      True,
      VPicName,
      VPic,
      VCategoryId,
      nil,
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
  VCategoryStringId: string;
  VName: string;
  VCategoryId: Integer;
begin
  VTemplate := ATemplate;
  if VTemplate = nil then begin
    VTemplate := FConfig.PolyTemplateConfig.DefaultTemplate;
  end;

  VName := AName;
  if VName = '' then begin
    VName := VTemplate.GetNewName;
  end;

  VCategoryId := CNotExistCategoryID;
  VCategoryStringId := VTemplate.CategoryStringID;
  if VCategoryStringId <> '' then begin
    VCategoryId := StrToIntDef(VCategoryStringID, VCategoryId);
  end;

  Result :=
    CreatePoly(
      CNotExistMarkID,
      VName,
      True,
      VCategoryId,
      nil,
      ADesc,
      ALine,
      VTemplate.BorderColor,
      VTemplate.FillColor,
      VTemplate.LineWidth
    );
end;

function TMarkFactory.CreatePoint(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  const APicName: string;
  const APic: IMarkPicture;
  ACategoryId: Integer;
  const ACategory: ICategory;
  const ADesc: string;
  const APoint: TDoublePoint;
  ATextColor, ATextBgColor: TColor32;
  AFontSize, AMarkerSize: Integer
): IMarkPoint;
var
  VPicIndex: Integer;
  VPic: IMarkPicture;
  VPicName: string;
  VCategory: ICategory;
begin
  VPic := APic;
  if VPic = nil then begin
    VPicName := APicName;
    VPicIndex := FMarkPictureList.GetIndexByName(APicName);
    if VPicIndex < 0 then begin
      VPic := nil;
    end else begin
      VPic := FMarkPictureList.Get(VPicIndex);
    end;
  end else begin
    VPicName := VPic.GetName;
  end;

  VCategory := ACategory;
  if VCategory = nil then begin
    VCategory := FCategoryDB.GetCategoryByID(ACategoryId);
  end;

  Result :=
    TMarkPoint.Create(
      FHintConverter,
      AName,
      AId,
      AVisible,
      VPicName,
      VPic,
      VCategory,
      ADesc,
      APoint,
      ATextColor,
      ATextBgColor,
      AFontSize,
      AMarkerSize
    );
end;

function TMarkFactory.CreateLine(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  ACategoryId: Integer;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPath;
  ALineColor: TColor32;
  ALineWidth: Integer
): IMarkLine;
var
  VCategory: ICategory;
begin
  VCategory := ACategory;
  if VCategory = nil then begin
    VCategory := FCategoryDB.GetCategoryByID(ACategoryId);
  end;

  Result :=
    TMarkLine.Create(
      FHintConverter,
      AName,
      AId,
      AVisible,
      VCategory,
      ADesc,
      ALine,
      ALineColor,
      ALineWidth
    );
end;

function TMarkFactory.CreatePoly(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  ACategoryId: Integer;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPolygon;
  ABorderColor, AFillColor: TColor32;
  ALineWidth: Integer
): IMarkPoly;
var
  VCategory: ICategory;
begin
  VCategory := ACategory;
  if VCategory = nil then begin
    VCategory := FCategoryDB.GetCategoryByID(ACategoryId);
  end;

  Result :=
    TMarkPoly.Create(
      FHintConverter,
      AName,
      AId,
      AVisible,
      VCategory,
      ADesc,
      ALine,
      ABorderColor,
      AFillColor,
      ALineWidth
    );
end;

function TMarkFactory.CreateMark(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  const APicName: string;
  ACategoryId: Integer;
  const ADesc: string;
  const APoints: PDoublePointArray;
  APointCount: Integer;
  AColor1, AColor2: TColor32;
  AScale1, AScale2: Integer
): IMark;
var
  VPolygon: ILonLatPolygon;
  VPath: ILonLatPath;
begin
  Result := nil;
  if APointCount > 0 then begin
    if APointCount = 1 then begin
      if not PointIsEmpty(APoints[0]) then begin
        Result :=
          CreatePoint(
            AId,
            AName,
            AVisible,
            APicName,
            nil,
            ACategoryId,
            nil,
            ADesc,
            APoints[0],
            AColor1,
            AColor2,
            AScale1,
            AScale2
          );
      end;
    end else begin
      if DoublePointsEqual(APoints[0], APoints[APointCount - 1]) then begin
        VPolygon := FFactory.CreateLonLatPolygon(APoints, APointCount);
        if VPolygon.Count <> 0 then begin
          Result :=
            CreatePoly(
              AId,
              AName,
              AVisible,
              ACategoryId,
              nil,
              ADesc,
              VPolygon,
              AColor1,
              AColor2,
              AScale1
            );
        end;
      end else begin
        VPath := FFactory.CreateLonLatPath(APoints, APointCount);
        if VPath.Count <> 0 then begin
          Result :=
            CreateLine(
              AId,
              AName,
              AVisible,
              ACategoryId,
              nil,
              ADesc,
              VPath,
              AColor1,
              AScale1
            );
        end;
      end;
    end;
  end;
end;

function TMarkFactory.SimpleModifyLine(
  const ASource: IMarkLine;
  const ALine: ILonLatPath;
  const ADesc: string
): IMarkLine;
var
  VId: Integer;
  VCategoryId: Integer;
  VDesc: string;
  VVisible: Boolean;
  VMarkInternal: IMarkSMLInternal;
begin
  VVisible := True;
  VId := CNotExistMarkID;
  VCategoryId := CNotExistCategoryID;
  if Supports(ASource, IMarkSMLInternal, VMarkInternal) then begin
    VVisible := VMarkInternal.Visible;
    VId := VMarkInternal.Id;
    VCategoryId := VMarkInternal.CategoryId;
  end;
  VDesc := ADesc;
  if ADesc = '' then begin
    VDesc := ASource.Desc;
  end;

  Result :=
    CreateLine(
      VId,
      ASource.Name,
      VVisible,
      VCategoryId,
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
var
  VVisible: Boolean;
  VId: Integer;
  VCategoryId: Integer;
  VPicName: string;
  VMarkInternal: IMarkPointSMLInternal;
begin
  VVisible := True;
  VId := CNotExistMarkID;
  VCategoryId := CNotExistCategoryID;
  if Supports(ASource, IMarkPointSMLInternal, VMarkInternal) then begin
    VVisible := VMarkInternal.Visible;
    VId := VMarkInternal.Id;
    VCategoryId := VMarkInternal.CategoryId;
    VPicName := VMarkInternal.PicName;
  end;

  Result :=
    CreatePoint(
      VId,
      ASource.Name,
      VVisible,
      VPicName,
      ASource.Pic,
      VCategoryId,
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
var
  VVisible: Boolean;
  VId: Integer;
  VCategoryId: Integer;
  VMarkInternal: IMarkSMLInternal;
begin
  VVisible := True;
  VId := CNotExistMarkID;
  VCategoryId := CNotExistCategoryID;
  if Supports(ASource, IMarkSMLInternal, VMarkInternal) then begin
    VVisible := VMarkInternal.Visible;
    VId := VMarkInternal.Id;
    VCategoryId := VMarkInternal.CategoryId;
  end;

  Result :=
    CreatePoly(
      VId,
      ASource.Name,
      VVisible,
      VCategoryId,
      ASource.Category,
      ASource.Desc,
      ALine,
      ASource.BorderColor,
      ASource.FillColor,
      ASource.LineWidth
    );
end;

function TMarkFactory.ModifyPoint(
  const ASource: IMarkPoint;
  const AName: string;
  AVisible: Boolean;
  const APic: IMarkPicture;
  const ACategory: ICategory;
  const ADesc: string;
  const APoint: TDoublePoint;
  ATextColor: TColor32;
  ATextBgColor: TColor32;
  AFontSize: Integer;
  AMarkerSize: Integer
): IMarkPoint;
var
  VID: Integer;
  VCategoryId: Integer;
  VPicName: string;
  VCategoryInternal: IMarkCategorySMLInternal;
  VMarkInternal: IMarkSMLInternal;
  VMarkPointInternal: IMarkPointSMLInternal;
begin
  VID := CNotExistMarkID;
  if ASource <> nil then begin
    if Supports(ASource, IMarkSMLInternal, VMarkInternal) then begin
      VID := VMarkInternal.Id;
    end;
    if Supports(ASource, IMarkPointSMLInternal, VMarkPointInternal) then begin
      VPicName := VMarkPointInternal.PicName;
    end;
  end;
  VCategoryId := CNotExistCategoryID;
  if ACategory <> nil then begin
    if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
      VCategoryId := VCategoryInternal.Id;
    end;
  end;

  Result :=
    CreatePoint(
      VID,
      AName,
      AVisible,
      VPicName,
      APic,
      VCategoryId,
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
  AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPath;
  ALineColor: TColor32;
  ALineWidth: Integer
): IMarkLine;
var
  VId: Integer;
  VCategoryId: Integer;
  VCategoryInternal: IMarkCategorySMLInternal;
  VMarkInternal: IMarkSMLInternal;
begin
  VId := CNotExistMarkID;
  if ASource <> nil then begin
    if Supports(ASource, IMarkSMLInternal, VMarkInternal) then begin
      VId := VMarkInternal.Id;
    end;
  end;
  VCategoryId := CNotExistCategoryID;
  if ACategory <> nil then begin
    if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
      VCategoryId := VCategoryInternal.Id;
    end;
  end;

  Result :=
    CreateLine(
      VId,
      AName,
      AVisible,
      VCategoryId,
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
  AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPolygon;
  ABorderColor: TColor32;
  AFillColor: TColor32;
  ALineWidth: Integer
): IMarkPoly;
var
  VID: Integer;
  VCategoryId: Integer;
  VCategoryInternal: IMarkCategorySMLInternal;
  VMarkInternal: IMarkSMLInternal;
begin
  VID := CNotExistMarkID;
  if ASource <> nil then begin
    if Supports(ASource, IMarkSMLInternal, VMarkInternal) then begin
      VID := VMarkInternal.Id;
    end;
  end;
  VCategoryId := CNotExistCategoryID;
  if ACategory <> nil then begin
    if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
      VCategoryId := VCategoryInternal.Id;
    end;
  end;

  Result :=
    CreatePoly(
      VID,
      AName,
      AVisible,
      VCategoryId,
      ACategory,
      ADesc,
      ALine,
      ABorderColor,
      AFillColor,
      ALineWidth
    );
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
