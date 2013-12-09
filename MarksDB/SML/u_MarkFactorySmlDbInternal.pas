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

unit u_MarkFactorySmlDbInternal;

interface

uses
  GR32,
  t_GeoTypes,
  i_HashFunction,
  i_AppearanceOfMarkFactory,
  i_VectorItemLonLat,
  i_VectorItemsFactory,
  i_MarkPicture,
  i_Category,
  i_MarkCategoryDBSmlInternal,
  i_Mark,
  i_MarkFactory,
  i_HtmlToHintTextConverter,
  i_MarkFactorySmlInternal,
  u_BaseInterfacedObject;

type
  TMarkFactorySmlDbInternal = class(TBaseInterfacedObject, IMarkFactorySmlInternal)
  private
    FDbId: Integer;
    FHashFunction: IHashFunction;
    FVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FMarkFactory: IMarkFactory;
    FCategoryDB: IMarkCategoryDBSmlInternal;
    FHintConverter: IHtmlToHintTextConverter;

    FMarkPictureList: IMarkPictureList;
  private
    function CreatePoint(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      const APicName: string;
      const APic: IMarkPicture;
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
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: IGeometryLonLatMultiLine;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IMarkLine;
    function CreatePoly(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: IGeometryLonLatMultiPolygon;
      ABorderColor, AFillColor: TColor32;
      ALineWidth: Integer
    ): IMarkPoly;
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
    function CreateInternalMark(const AMark: IMark): IMark;
  public
    constructor Create(
      const ADbId: Integer;
      const AMarkPictureList: IMarkPictureList;
      const AVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkFactory: IMarkFactory;
      const AHashFunction: IHashFunction;
      const AHintConverter: IHtmlToHintTextConverter;
      const ACategoryDB: IMarkCategoryDBSmlInternal
    );
  end;

implementation

uses
  SysUtils,
  t_Hash,
  i_MarkDbSmlInternal,
  i_Appearance,
  i_VectorDataItemSimple,
  u_GeoFun,
  u_MarkPointSml,
  u_MarkLineSml,
  u_MarkPolySml;

{ TMarkFactorySmlDbInternal }

constructor TMarkFactorySmlDbInternal.Create(
  const ADbId: Integer;
  const AMarkPictureList: IMarkPictureList;
  const AVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const AHashFunction: IHashFunction;
  const AHintConverter: IHtmlToHintTextConverter;
  const ACategoryDB: IMarkCategoryDBSmlInternal
);
begin
  inherited Create;
  FDbId := ADbId;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FMarkFactory := AMarkFactory;
  FHashFunction := AHashFunction;
  FHintConverter := AHintConverter;
  FCategoryDB := ACategoryDB;
  FMarkPictureList := AMarkPictureList;
end;

function TMarkFactorySmlDbInternal.CreatePoint(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  const APicName: string;
  const APic: IMarkPicture;
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
  VHash: THashValue;
  VAppearance: IAppearance;
begin
  VPic := APic;
  if VPic = nil then begin
    VPicName := APicName;
    VPicIndex := FMarkPictureList.GetIndexByName(APicName);
    if VPicIndex >= 0 then begin
      VPic := FMarkPictureList.Get(VPicIndex);
    end;
  end else begin
    VPicName := VPic.GetName;
  end;
  VAppearance :=
    FAppearanceOfMarkFactory.CreatePointAppearance(
      ATextColor,
      ATextBgColor,
      AFontSize,
      VPicName,
      VPic,
      AMarkerSize
    );

  VHash := FHashFunction.CalcHashByDoublePoint(APoint);
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  FHashFunction.UpdateHashByHash(VHash, VAppearance.Hash);

  Result :=
    TMarkPointSml.Create(
      VHash,
      FHintConverter,
      AName,
      AId,
      FDbId,
      AVisible,
      VAppearance,
      ACategory,
      ADesc,
      APoint
    );
end;

function TMarkFactorySmlDbInternal.CreateInternalMark(const AMark: IMark): IMark;
var
  VCategory: ICategory;
  VMarkInternal: IMarkSMLInternal;
  VMarkWithCategory: IVectorDataItemWithCategory;
begin
  Assert(Assigned(AMark));

  if Supports(AMark, IMarkSMLInternal, VMarkInternal) then begin
    if VMarkInternal.DbId = FDbId then begin
      Result := AMark;
      Exit;
    end;
  end;
  VCategory := nil;
  if Supports(AMark, IVectorDataItemWithCategory, VMarkWithCategory) then begin
    VCategory := VMarkWithCategory.Category;
    if Assigned(VCategory) then begin
      if not FCategoryDB.IsCategoryFromThisDb(VCategory) then begin
        VCategory := FCategoryDB.GetCategoryByName(VCategory.Name);
        if Assigned(VCategory) then begin
          Result := FMarkFactory.ReplaceCategory(AMark, VCategory);
        end;
      end;
    end;
  end;
  Assert(Assigned(VCategory), 'Corresponding category in this DB have not been found');
  Result := AMark;
end;

function TMarkFactorySmlDbInternal.CreateLine(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: IGeometryLonLatMultiLine;
  ALineColor: TColor32;
  ALineWidth: Integer
): IMarkLine;
var
  VHash: THashValue;
  VAppearance: IAppearance;
begin
  Assert(Assigned(ALine));
  VAppearance :=
    FAppearanceOfMarkFactory.CreateLineAppearance(
      ALineColor,
      ALineWidth
    );

  VHash := ALine.Hash;
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  FHashFunction.UpdateHashByHash(VHash, VAppearance.Hash);
  Result :=
    TMarkLineSml.Create(
      VHash,
      FHintConverter,
      AName,
      AId,
      FDbId,
      AVisible,
      VAppearance,
      ACategory,
      ADesc,
      ALine
    );
end;

function TMarkFactorySmlDbInternal.CreatePoly(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: IGeometryLonLatMultiPolygon;
  ABorderColor, AFillColor: TColor32;
  ALineWidth: Integer
): IMarkPoly;
var
  VHash: THashValue;
  VAppearance: IAppearance;
begin
  Assert(Assigned(ALine));
  VAppearance :=
    FAppearanceOfMarkFactory.CreatePolygonAppearance(
      ABorderColor,
      ALineWidth,
      AFillColor
    );
  VHash := ALine.Hash;
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  FHashFunction.UpdateHashByHash(VHash, VAppearance.Hash);
  Result :=
    TMarkPolySml.Create(
      VHash,
      FHintConverter,
      AName,
      AId,
      FDbId,
      AVisible,
      VAppearance,
      ACategory,
      ADesc,
      ALine
    );
end;

function TMarkFactorySmlDbInternal.CreateMark(
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
  VPolygon: IGeometryLonLatMultiPolygon;
  VPath: IGeometryLonLatMultiLine;
  VCategory: ICategory;
begin
  Result := nil;
  if APointCount > 0 then begin
    if VCategory = nil then begin
      VCategory := FCategoryDB.GetCategoryByID(ACategoryId);
    end;

    if APointCount = 1 then begin
      if not PointIsEmpty(APoints[0]) then begin
        Result :=
          CreatePoint(
            AId,
            AName,
            AVisible,
            APicName,
            nil,
            VCategory,
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
        VPolygon := FVectorGeometryLonLatFactory.CreateLonLatPolygon(APoints, APointCount - 1);
        if VPolygon.Count <> 0 then begin
          Result :=
            CreatePoly(
              AId,
              AName,
              AVisible,
              VCategory,
              ADesc,
              VPolygon,
              AColor1,
              AColor2,
              AScale1
            );
        end;
      end else begin
        VPath := FVectorGeometryLonLatFactory.CreateLonLatPath(APoints, APointCount);
        if VPath.Count <> 0 then begin
          Result :=
            CreateLine(
              AId,
              AName,
              AVisible,
              VCategory,
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

end.
