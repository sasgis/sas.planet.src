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

unit u_MarkFactorySmlDbInternal;

interface

uses
  t_Bitmap32,
  i_HashFunction,
  i_AppearanceOfMarkFactory,
  i_GeometryLonLat,
  i_MarkPicture,
  i_Category,
  i_MarkCategoryDBSmlInternal,
  i_VectorDataItemSimple,
  i_MarkFactory,
  i_HtmlToHintTextConverter,
  i_MarkFactorySmlInternal,
  u_BaseInterfacedObject;

type
  TMarkFactorySmlDbInternal = class(TBaseInterfacedObject, IMarkFactorySmlInternal)
  private
    FDbId: Integer;
    FHashFunction: IHashFunction;
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
      const ACategory: ICategory;
      const ADesc: string;
      const AGeometry: IGeometryLonLat;
      ATextColor, ATextBgColor: TColor32;
      AFontSize, AMarkerSize: Integer
    ): IVectorDataItem;
    function CreateLine(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      const ACategory: ICategory;
      const ADesc: string;
      const AGeometry: IGeometryLonLat;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IVectorDataItem;
    function CreatePoly(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      const ACategory: ICategory;
      const ADesc: string;
      const AGeometry: IGeometryLonLat;
      ABorderColor, AFillColor: TColor32;
      ALineWidth: Integer
    ): IVectorDataItem;
  private
    function CreateMark(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      const APicName: string;
      ACategoryId: Integer;
      const ADesc: string;
      const AGeometry: IGeometryLonLat;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IVectorDataItem;
    function CreateInternalMark(const AMark: IVectorDataItem): IVectorDataItem;
  public
    constructor Create(
      const ADbId: Integer;
      const AMarkPictureList: IMarkPictureList;
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
  i_MarkId,
  i_MarkDbSmlInternal,
  i_Appearance,
  u_MarkId,
  u_VectorDataItemBase;

function CalcMultiGeometryCount(const AGeometry: IGeometryLonLat): Integer;
var
  VLine: IGeometryLonLatMultiLine;
  VPoly: IGeometryLonLatMultiPolygon;
begin
  if Supports(AGeometry, IGeometryLonLatMultiLine, VLine) then begin
    Result := VLine.Count;
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VPoly) then begin
    Result := VPoly.Count;
  end else begin
    Result := 0;
  end;
end;

{ TMarkFactorySmlDbInternal }

constructor TMarkFactorySmlDbInternal.Create(
  const ADbId: Integer;
  const AMarkPictureList: IMarkPictureList;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const AHashFunction: IHashFunction;
  const AHintConverter: IHtmlToHintTextConverter;
  const ACategoryDB: IMarkCategoryDBSmlInternal
);
begin
  inherited Create;
  FDbId := ADbId;
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
  const ACategory: ICategory;
  const ADesc: string;
  const AGeometry: IGeometryLonLat;
  ATextColor, ATextBgColor: TColor32;
  AFontSize, AMarkerSize: Integer
): IVectorDataItem;
var
  VPicIndex: Integer;
  VPic: IMarkPicture;
  VPicName: string;
  VHash: THashValue;
  VAppearance: IAppearance;
  VMainInfo: IVectorDataItemMainInfo;
begin
  VPicName := APicName;
  VPicIndex := FMarkPictureList.GetIndexByName(APicName);
  if VPicIndex >= 0 then begin
    VPic := FMarkPictureList.Get(VPicIndex);
    VPicName := VPic.GetName;
  end else begin
    VPic := nil;
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
  VHash := FHashFunction.CalcHashByString(AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  VMainInfo :=
    TMarkId.Create(
      FHintConverter,
      VHash,
      midPoint,
      AName,
      ADesc,
      AId,
      FDbId,
      CalcMultiGeometryCount(AGeometry),
      ACategory,
      AVisible
    );

  VHash := AGeometry.Hash;
  FHashFunction.UpdateHashByHash(VHash, VMainInfo.Hash);
  FHashFunction.UpdateHashByHash(VHash, VAppearance.Hash);

  Result :=
    TVectorDataItem.Create(
      VHash,
      VAppearance,
      VMainInfo,
      AGeometry
    );
end;

function TMarkFactorySmlDbInternal.CreateInternalMark(
  const AMark: IVectorDataItem
): IVectorDataItem;
var
  VCategory: ICategory;
  VMarkInternal: IMarkSMLInternal;
  VMarkWithCategory: IVectorDataItemWithCategory;
begin
  Assert(Assigned(AMark));

  if Assigned(AMark) and Supports(AMark.MainInfo, IMarkSMLInternal, VMarkInternal) then begin
    if VMarkInternal.DbId = FDbId then begin
      Result := AMark;
      Exit;
    end;
  end;
  VCategory := nil;
  if Assigned(AMark) and Supports(AMark.MainInfo, IVectorDataItemWithCategory, VMarkWithCategory) then begin
    VCategory := VMarkWithCategory.Category;
    if Assigned(VCategory) then begin
      if not FCategoryDB.IsCategoryFromThisDb(VCategory) then begin
        VCategory := FCategoryDB.GetCategoryByName(VCategory.Name);
        if Assigned(VCategory) then begin
          Result := FMarkFactory.ReplaceCategory(AMark, VCategory);
          Exit;
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
  const AGeometry: IGeometryLonLat;
  ALineColor: TColor32;
  ALineWidth: Integer
): IVectorDataItem;
var
  VHash: THashValue;
  VAppearance: IAppearance;
  VMainInfo: IVectorDataItemMainInfo;
begin
  Assert(Assigned(AGeometry));
  VAppearance :=
    FAppearanceOfMarkFactory.CreateLineAppearance(
      ALineColor,
      ALineWidth
    );

  VHash := FHashFunction.CalcHashByString(AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  VMainInfo :=
    TMarkId.Create(
      FHintConverter,
      VHash,
      midLine,
      AName,
      ADesc,
      AId,
      FDbId,
      CalcMultiGeometryCount(AGeometry),
      ACategory,
      AVisible
    );

  VHash := AGeometry.Hash;
  FHashFunction.UpdateHashByHash(VHash, VMainInfo.Hash);
  FHashFunction.UpdateHashByHash(VHash, VAppearance.Hash);
  Result :=
    TVectorDataItem.Create(
      VHash,
      VAppearance,
      VMainInfo,
      AGeometry
    );
end;

function TMarkFactorySmlDbInternal.CreatePoly(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const AGeometry: IGeometryLonLat;
  ABorderColor, AFillColor: TColor32;
  ALineWidth: Integer
): IVectorDataItem;
var
  VHash: THashValue;
  VAppearance: IAppearance;
  VMainInfo: IVectorDataItemMainInfo;
begin
  Assert(Assigned(AGeometry));
  VAppearance :=
    FAppearanceOfMarkFactory.CreatePolygonAppearance(
      ABorderColor,
      ALineWidth,
      AFillColor
    );

  VHash := FHashFunction.CalcHashByString(AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  VMainInfo :=
    TMarkId.Create(
      FHintConverter,
      VHash,
      midPoly,
      AName,
      ADesc,
      AId,
      FDbId,
      CalcMultiGeometryCount(AGeometry),
      ACategory,
      AVisible
    );

  VHash := AGeometry.Hash;
  FHashFunction.UpdateHashByHash(VHash, VMainInfo.Hash);
  FHashFunction.UpdateHashByHash(VHash, VAppearance.Hash);
  Result :=
    TVectorDataItem.Create(
      VHash,
      VAppearance,
      VMainInfo,
      AGeometry
    );
end;

function TMarkFactorySmlDbInternal.CreateMark(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  const APicName: string;
  ACategoryId: Integer;
  const ADesc: string;
  const AGeometry: IGeometryLonLat;
  AColor1, AColor2: TColor32;
  AScale1, AScale2: Integer
): IVectorDataItem;
var
  VCategory: ICategory;
begin
  Result := nil;
  if Assigned(AGeometry) then begin
    VCategory := FCategoryDB.GetCategoryByID(ACategoryId);
    if Supports(AGeometry, IGeometryLonLatPoint) then begin
      Result :=
        CreatePoint(
          AId,
          AName,
          AVisible,
          APicName,
          VCategory,
          ADesc,
          AGeometry,
          AColor1,
          AColor2,
          AScale1,
          AScale2
        );
    end else if Supports(AGeometry, IGeometryLonLatLine) then begin
      Result :=
        CreateLine(
          AId,
          AName,
          AVisible,
          VCategory,
          ADesc,
          AGeometry,
          AColor1,
          AScale1
        );
    end else if Supports(AGeometry, IGeometryLonLatPolygon) then begin
      Result :=
        CreatePoly(
          AId,
          AName,
          AVisible,
          VCategory,
          ADesc,
          AGeometry,
          AColor1,
          AColor2,
          AScale1
        );
    end;
  end;
end;

end.
