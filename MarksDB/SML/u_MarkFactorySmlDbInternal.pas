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
  i_VectorItemLonLat,
  i_VectorItemsFactory,
  i_MarkPicture,
  i_Category,
  i_MarkCategoryDBSmlInternal,
  i_Mark,
  i_HtmlToHintTextConverter,
  i_MarkFactorySmlInternal,
  u_BaseInterfacedObject;

type
  TMarkFactorySmlDbInternal = class(TBaseInterfacedObject, IMarkFactorySmlInternal)
  private
    FDbId: Integer;
    FHashFunction: IHashFunction;
    FFactory: IVectorItemsFactory;
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
      const ALine: ILonLatPath;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IMarkLine;
    function CreatePoly(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: ILonLatPolygon;
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
    function CreateInternalMark(AMark: IMark): IMark;
    function GetMarkPictureList: IMarkPictureList;
  public
    constructor Create(
      const ADbId: Integer;
      const AMarkPictureList: IMarkPictureList;
      const AFactory: IVectorItemsFactory;
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
  u_GeoFun,
  u_MarkPointSml,
  u_MarkLineSml,
  u_MarkPolySml;

{ TMarkFactorySmlDbInternal }

constructor TMarkFactorySmlDbInternal.Create(
  const ADbId: Integer;
  const AMarkPictureList: IMarkPictureList;
  const AFactory: IVectorItemsFactory;
  const AHashFunction: IHashFunction;
  const AHintConverter: IHtmlToHintTextConverter;
  const ACategoryDB: IMarkCategoryDBSmlInternal
);
begin
  inherited Create;
  FDbId := ADbId;
  FFactory := AFactory;
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
  VPicHash: THashValue;
begin
  VPic := APic;
  if VPic = nil then begin
    VPicName := APicName;
    VPicIndex := FMarkPictureList.GetIndexByName(APicName);
    if VPicIndex < 0 then begin
      VPic := nil;
      VPicHash := 0;
    end else begin
      VPic := FMarkPictureList.Get(VPicIndex);
      VPicHash := VPic.Hash;
    end;
  end else begin
    VPicName := VPic.GetName;
    VPicHash := VPic.Hash;
  end;

  VHash := FHashFunction.CalcHashByDoublePoint(APoint);
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  FHashFunction.UpdateHashByHash(VHash, VPicHash);
  FHashFunction.UpdateHashByInteger(VHash, AMarkerSize);
  FHashFunction.UpdateHashByInteger(VHash, ATextColor);
  FHashFunction.UpdateHashByInteger(VHash, ATextBgColor);
  FHashFunction.UpdateHashByInteger(VHash, AFontSize);

  Result :=
    TMarkPointSml.Create(
      VHash,
      FHintConverter,
      AName,
      AId,
      FDbId,
      AVisible,
      VPicName,
      VPic,
      ACategory,
      ADesc,
      APoint,
      ATextColor,
      ATextBgColor,
      AFontSize,
      AMarkerSize
    );
end;

function TMarkFactorySmlDbInternal.CreateInternalMark(AMark: IMark): IMark;
var
  VCategory: ICategory;
  VMarkInternal: IMarkSMLInternal;
  VCategoryInternal: IMarkCategorySMLInternal;
  VMarkPoint: IMarkPoint;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
  VVisible: Boolean;
begin
  Assert(Assigned(AMark));
  Assert(Assigned(AMark.Category));

  VVisible := True;
  if Supports(AMark, IMarkSMLInternal, VMarkInternal) then begin
    VVisible := VMarkInternal.Visible;
    if VMarkInternal.DbId = FDbId then begin
      Result := AMark;
      Exit;
    end;
  end;
  VCategory := nil;
  if Supports(AMark.Category, IMarkCategorySMLInternal, VCategoryInternal) then begin
    if VCategoryInternal.DbId = FDbId then begin
      VCategory := AMark.Category;
    end;
  end;
  if VCategory = nil then begin
    if AMark.Category <> nil then begin
      VCategory := FCategoryDB.GetCategoryByName(AMark.Category.Name);
    end;
  end;
  Assert(Assigned(VCategory), 'Corresponding category in this DB have not been found');

  if Supports(AMark, IMarkPoint, VMarkPoint) then begin
    Result :=
      CreatePoint(
        -1,
        VMarkPoint.Name,
        VVisible,
        '',
        VMarkPoint.Pic,
        VCategory,
        VMarkPoint.Desc,
        VMarkPoint.Point,
        VMarkPoint.TextColor,
        VMarkPoint.TextBgColor,
        VMarkPoint.FontSize,
        VMarkPoint.MarkerSize
      );
  end else if Supports(AMark, IMarkLine, VMarkLine) then begin
    Result :=
      CreateLine(
        -1,
        VMarkLine.Name,
        VVisible,
        VCategory,
        VMarkLine.Desc,
        VMarkLine.Line,
        VMarkLine.LineColor,
        VMarkLine.LineWidth
      );
  end else if Supports(AMark, IMarkPoly, VMarkPoly) then begin
    Result :=
      CreatePoly(
        -1,
        VMarkPoly.Name,
        VVisible,
        VCategory,
        VMarkPoly.Desc,
        VMarkPoly.Line,
        VMarkPoly.LineColor,
        VMarkPoly.FillColor,
        VMarkPoly.LineWidth
      );
  end else begin
    Assert(False, 'Unknown Mark type');
  end;
end;

function TMarkFactorySmlDbInternal.CreateLine(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPath;
  ALineColor: TColor32;
  ALineWidth: Integer
): IMarkLine;
var
  VHash: THashValue;
begin
  VHash := ALine.Hash;
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  FHashFunction.UpdateHashByInteger(VHash, ALineColor);
  FHashFunction.UpdateHashByInteger(VHash, ALineWidth);
  Result :=
    TMarkLineSml.Create(
      VHash,
      FHintConverter,
      AName,
      AId,
      FDbId,
      AVisible,
      ACategory,
      ADesc,
      ALine,
      ALineColor,
      ALineWidth
    );
end;

function TMarkFactorySmlDbInternal.CreatePoly(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPolygon;
  ABorderColor, AFillColor: TColor32;
  ALineWidth: Integer
): IMarkPoly;
var
  VHash: THashValue;
begin
  VHash := ALine.Hash;
  FHashFunction.UpdateHashByString(VHash, AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  FHashFunction.UpdateHashByInteger(VHash, AFillColor);
  FHashFunction.UpdateHashByInteger(VHash, ABorderColor);
  FHashFunction.UpdateHashByInteger(VHash, ALineWidth);
  Result :=
    TMarkPolySml.Create(
      VHash,
      FHintConverter,
      AName,
      AId,
      FDbId,
      AVisible,
      ACategory,
      ADesc,
      ALine,
      ABorderColor,
      AFillColor,
      ALineWidth
    );
end;

function TMarkFactorySmlDbInternal.GetMarkPictureList: IMarkPictureList;
begin
  Result := FMarkPictureList;
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
  VPolygon: ILonLatPolygon;
  VPath: ILonLatPath;
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
        VPolygon := FFactory.CreateLonLatPolygon(APoints, APointCount - 1);
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
        VPath := FFactory.CreateLonLatPath(APoints, APointCount);
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
