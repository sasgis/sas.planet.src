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
  i_VectorItemLonLat,
  i_VectorItemsFactory,
  i_MarkPicture,
  i_Category,
  i_MarkCategoryDBSmlInternal,
  i_MarksSimple,
  i_HtmlToHintTextConverter,
  i_MarkFactorySmlInternal,
  u_BaseInterfacedObject;

type
  TMarkFactorySmlDbInternal = class(TBaseInterfacedObject, IMarkFactorySmlInternal)
  private
    FDbId: Integer;
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
      const AHintConverter: IHtmlToHintTextConverter;
      const ACategoryDB: IMarkCategoryDBSmlInternal
    );
  end;

implementation

uses
  SysUtils,
  i_MarksDbSmlInternal,
  
  u_GeoFun,
  u_MarkPointSml,
  u_MarkLineSml,
  u_MarkPolySml;

{ TMarkFactorySmlDbInternal }

constructor TMarkFactorySmlDbInternal.Create(
  const ADbId: Integer;
  const AMarkPictureList: IMarkPictureList;
  const AFactory: IVectorItemsFactory;
  const AHintConverter: IHtmlToHintTextConverter;
  const ACategoryDB: IMarkCategoryDBSmlInternal
);
begin
  inherited Create;
  FDbId := ADbId;
  FFactory := AFactory;
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

  Result :=
    TMarkPointSml.Create(
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
begin
  Result :=
    TMarkLineSml.Create(
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
begin
  Result :=
    TMarkPolySml.Create(
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
