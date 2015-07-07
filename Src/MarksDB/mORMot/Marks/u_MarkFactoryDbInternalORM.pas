{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_MarkFactoryDbInternalORM;

interface

uses
  t_MarkSystemORM,
  t_Bitmap32,
  i_HashFunction,
  i_AppearanceOfMarkFactory,
  i_GeometryLonLat,
  i_MarkPicture,
  i_Category,
  i_MarkId,
  i_MarkFactory,
  i_MarkFactoryDbInternalORM,
  i_MarkCategoryDbInternalORM,
  i_VectorDataItemSimple,
  i_HtmlToHintTextConverter,
  u_MarkSystemORMModel,
  u_BaseInterfacedObject;

type
  TMarkFactoryDbInternalORM = class(TBaseInterfacedObject, IMarkFactoryDbInternalORM)
  private
    FDbId: Integer;
    FHashFunction: IHashFunction;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FMarkFactory: IMarkFactory;
    FCategoryDB: IMarkCategoryDbInternalORM;
    FHintConverter: IHtmlToHintTextConverter;
    FMarkPictureList: IMarkPictureList;
  private
    function CreatePoint(
      const AId: TID;
      const AName: string;
      const AVisible: Boolean;
      const APicName: string;
      const ACategory: ICategory;
      const ADesc: string;
      const AGeometry: IGeometryLonLat;
      const ATextColor, ATextBgColor: TColor32;
      const AFontSize, AMarkerSize: Integer
    ): IVectorDataItem;
    function CreateLine(
      const AId: TID;
      const AName: string;
      const AVisible: Boolean;
      const ACategory: ICategory;
      const ADesc: string;
      const AGeometry: IGeometryLonLat;
      const ALineColor: TColor32;
      const ALineWidth: Integer
    ): IVectorDataItem;
    function CreatePoly(
      const AId: TID;
      const AName: string;
      const AVisible: Boolean;
      const ACategory: ICategory;
      const ADesc: string;
      const AGeometry: IGeometryLonLat;
      const ABorderColor, AFillColor: TColor32;
      const ALineWidth: Integer
    ): IVectorDataItem;
  private
    { IMarkFactoryDbInternalORM }
    function CreateMark(
      const AID: TID;
      const AName: string;
      const AVisible: Boolean;
      const APicName: string;
      const ACategoryId: TID;
      const ADesc: string;
      const AGeometry: IGeometryLonLat;
      const AColor1: TColor32;
      const AColor2: TColor32;
      const AScale1: Integer;
      const AScale2: Integer
    ): IVectorDataItem; overload;

    function CreateMark(
      const AMarkRec: TSQLMarkRec
    ): IVectorDataItem; overload;

    function CreateMarkId(
      const AMarkRec: TSQLMarkRec
    ): IMarkId;

    function CreateInternalMark(
      const AMark: IVectorDataItem
    ): IVectorDataItem;
  public
    constructor Create(
      const ADbId: Integer;
      const AMarkPictureList: IMarkPictureList;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkFactory: IMarkFactory;
      const AHashFunction: IHashFunction;
      const AHintConverter: IHtmlToHintTextConverter;
      const ACategoryDB: IMarkCategoryDbInternalORM
    );
  end;

implementation

uses
  SysUtils,
  t_Hash,
  i_MarkDbInternalORM,
  i_Appearance,
  u_MarkIdORM,
  u_MarkSystemORMTools,
  u_VectorDataItemBase;

{ TMarkFactoryDbInternalORM }

constructor TMarkFactoryDbInternalORM.Create(
  const ADbId: Integer;
  const AMarkPictureList: IMarkPictureList;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const AHashFunction: IHashFunction;
  const AHintConverter: IHtmlToHintTextConverter;
  const ACategoryDB: IMarkCategoryDbInternalORM
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

function TMarkFactoryDbInternalORM.CreatePoint(
  const AId: TID;
  const AName: string;
  const AVisible: Boolean;
  const APicName: string;
  const ACategory: ICategory;
  const ADesc: string;
  const AGeometry: IGeometryLonLat;
  const ATextColor, ATextBgColor: TColor32;
  const AFontSize, AMarkerSize: Integer
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
    TMarkIdORM.Create(
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

function TMarkFactoryDbInternalORM.CreateLine(
  const AId: TID;
  const AName: string;
  const AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const AGeometry: IGeometryLonLat;
  const ALineColor: TColor32;
  const ALineWidth: Integer
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
    TMarkIdORM.Create(
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

function TMarkFactoryDbInternalORM.CreatePoly(
  const AId: TID;
  const AName: string;
  const AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const AGeometry: IGeometryLonLat;
  const ABorderColor, AFillColor: TColor32;
  const ALineWidth: Integer
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
    TMarkIdORM.Create(
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

function TMarkFactoryDbInternalORM.CreateMark(
  const AID: TID;
  const AName: string;
  const AVisible: Boolean;
  const APicName: string;
  const ACategoryId: TID;
  const ADesc: string;
  const AGeometry: IGeometryLonLat;
  const AColor1: TColor32;
  const AColor2: TColor32;
  const AScale1: Integer;
  const AScale2: Integer
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

function TMarkFactoryDbInternalORM.CreateMark(
  const AMarkRec: TSQLMarkRec
): IVectorDataItem;
begin
  Result :=
    CreateMark(
      AMarkRec.FMarkId,
      AMarkRec.FName,
      AMarkRec.FVisible,
      AMarkRec.FPicName,
      AMarkRec.FCategoryId,
      AMarkRec.FDesc,
      AMarkRec.FGeometry,
      AMarkRec.FColor1,
      AMarkRec.FColor2,
      AMarkRec.FScale1,
      AMarkRec.FScale2
    );
end;

function GeoTypeToMarkType(const AGeoType: TSQLGeoType): TMarkIdType;
begin
  case AGeoType of
    gtPoint: Result := midPoint;
    gtLine:  Result := midLine;
    gtPoly:  Result := midPoly;
    gtUndef: raise Exception.Create('MarkSystemORM: Undefined GeoType!');
  else
    raise Exception.Create('MarkSystemORM: Unknown GeoType!');
  end;
end;

function TMarkFactoryDbInternalORM.CreateMarkId(
  const AMarkRec: TSQLMarkRec
): IMarkId;
var
  VHash: THashValue;
  VCategory: ICategory;
begin
  VHash := FHashFunction.CalcHashByString(AMarkRec.FName);
  FHashFunction.UpdateHashByString(VHash, AMarkRec.FDesc);

  VCategory := FCategoryDB.GetCategoryByID(AMarkRec.FCategoryId);

  Result :=
    TMarkIdORM.Create(
      FHintConverter,
      VHash,
      GeoTypeToMarkType(AMarkRec.FGeoType),
      AMarkRec.FName,
      AMarkRec.FDesc,
      AMarkRec.FMarkId,
      FDbId,
      AMarkRec.FGeoCount,
      VCategory,
      AMarkRec.FVisible
    );
end;

function TMarkFactoryDbInternalORM.CreateInternalMark(
  const AMark: IVectorDataItem
): IVectorDataItem;
var
  VCategory: ICategory;
  VMarkInternal: IMarkInternalORM;
  VMarkWithCategory: IVectorDataItemWithCategory;
begin
  Assert(Assigned(AMark));

  if Assigned(AMark) and Supports(AMark.MainInfo, IMarkInternalORM, VMarkInternal) then begin
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

end.
