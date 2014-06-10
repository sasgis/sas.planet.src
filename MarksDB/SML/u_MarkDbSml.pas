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

unit u_MarkDbSml;

interface

uses
  Windows,
  DBClient,
  SysUtils,
  Classes,
  t_GeoTypes,
  i_IDList,
  i_SimpleFlag,
  i_VectorItemSubsetBuilder,
  i_InternalPerformanceCounter,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_Category,
  i_VectorDataItemSimple,
  i_MarkId,
  i_VectorItemSubset,
  i_MarkDbSmlInternal,
  i_MarkDbImpl,
  i_MarkFactorySmlInternal,
  i_ReadWriteStateInternal,
  u_ConfigDataElementBase;

type
  TMarkDbSml = class(TConfigDataElementBaseEmptySaveLoad, IMarkDbSmlInternal, IMarkDbImpl)
  private
    FDbId: Integer;
    FStateInternal: IReadWriteStateInternal;
    FFileName: string;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FFactoryDbInternal: IMarkFactorySmlInternal;
    FLoadDbCounter: IInternalPerformanceCounter;
    FSaveDbCounter: IInternalPerformanceCounter;

    FStream: TStream;
    FCdsMarks: TClientDataSet;
    FMarkList: IIDInterfaceList;
    FByCategoryList: IIDInterfaceList;
    FNeedSaveFlag: ISimpleFlag;

    function ReadCurrentMark: IVectorDataItem;
    procedure WriteCurrentMarkId(const AMark: IMarkId);
    procedure WriteCurrentMark(const AMark: IVectorDataItem);

    function GetMarksBackUpFileName: string;
    procedure InitEmptyDS(ACdsMarks: TClientDataSet);
    function GetCategoryID(const ACategory: ICategory): Integer;
    function GetFilterTextByCategory(const ACategory: ICategory): string;
    function _UpdateMark(
      const AOldMark: IInterface;
      const ANewMark: IInterface
    ): IVectorDataItem;
    procedure _AddMarksToListByRect(
      const ASourceList: IIDInterfaceList;
      const ARect: TDoubleRect;
      AIgnoreVisible: Boolean;
      const AResultList: IVectorItemSubsetBuilder
    );
    procedure _AddMarksToList(
      const ASourceList: IIDInterfaceList;
      AIgnoreVisible: Boolean;
      const AResultList: IVectorItemSubsetBuilder
    );
    function SaveMarks2File: boolean;
    procedure LoadMarksFromFile;
  private
    function GetById(AId: Integer): IVectorDataItem;
  private
    function UpdateMark(
      const AOldMark: IVectorDataItem;
      const ANewMark: IVectorDataItem
    ): IVectorDataItem;
    function UpdateMarkList(
      const AOldMarkList: IInterfaceListStatic;
      const ANewMarkList: IInterfaceListStatic
    ): IInterfaceListStatic;

    function GetMarkByID(const AMarkId: IMarkId): IVectorDataItem;
    function GetMarkByName(
      const AName: string;
      const ACategory: ICategory
    ): IVectorDataItem;

    procedure SetMarkVisibleByID(
      const AMark: IMarkId;
      AVisible: Boolean
    );
    procedure SetMarkVisible(
      const AMark: IVectorDataItem;
      AVisible: Boolean
    );
    procedure SetMarkVisibleByIDList(
      const AMarkList: IInterfaceListStatic;
      AVisible: Boolean
    );
    procedure ToggleMarkVisibleByIDList(
      const AMarkList: IInterfaceListStatic
    );
    function GetMarkVisibleByID(const AMark: IMarkId): Boolean;
    function GetMarkVisible(const AMark: IVectorDataItem): Boolean;
    function GetAllMarkIdList: IInterfaceListStatic;
    function GetMarkIdListByCategory(const ACategory: ICategory): IInterfaceListStatic;

    procedure SetAllMarksInCategoryVisible(
      const ACategory: ICategory;
      ANewVisible: Boolean
    );

    function GetMarkSubsetByCategoryList(
      const ACategoryList: IInterfaceListStatic;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemSubset;
    function GetMarkSubsetByCategory(
      const ACategory: ICategory;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemSubset;
    function GetMarkSubsetByCategoryListInRect(
      const ARect: TDoubleRect;
      const ACategoryList: IInterfaceListStatic;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemSubset;
    function GetMarkSubsetByCategoryInRect(
      const ARect: TDoubleRect;
      const ACategory: ICategory;
      const AIncludeHiddenMarks: Boolean
    ): IVectorItemSubset;
    function FindMarks(
      const ASearch: string;
      const AMaxCount: Integer;
      const AIncludeHiddenMarks: Boolean;
      const ASearchInDescription: Boolean
    ): IVectorItemSubset;
  public
    constructor Create(
      const ADbId: Integer;
      const AStateInternal: IReadWriteStateInternal;
      const AFileName: string;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AFactoryDbInternal: IMarkFactorySmlInternal;
      const ALoadDbCounter: IInternalPerformanceCounter;
      const ASaveDbCounter: IInternalPerformanceCounter
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  DB,
  Math,
  StrUtils,
  t_Bitmap32,
  t_CommonTypes,
  i_EnumDoublePoint,
  i_AppearanceOfVectorItem,
  i_MarkCategoryFactoryDbInternal,
  u_IDInterfaceList,
  u_InterfaceListSimple,
  i_DoublePointsAggregator,
  i_GeometryLonLat,
  u_DoublePointsAggregator,
  u_SimpleFlagWithInterlock,
  u_GeoFunc;

type
  TExtendedPoint = record
    X, Y: Extended;
  end;


procedure Blob2ExtArr(
  ABlobField: TField;
  const AAggregator: IDoublePointsAggregator
);
const
  CMaxDegres: Extended = 360;
  CMinDegres: Extended = -360;
var
  VSize: Integer;
  VPointsCount: Integer;
  VField: TBlobfield;
  VStream: TStream;
  i: Integer;
  VPoint: TExtendedPoint;
  VDoublePoint: TDoublePoint;
begin
  VField := TBlobfield(ABlobField);
  VStream := VField.DataSet.CreateBlobStream(VField, bmRead);
  try
    VSize := VStream.Size;
    VPointsCount := VSize div SizeOf(TExtendedPoint);
    for i := 0 to VPointsCount - 1 do begin
      VStream.ReadBuffer(VPoint, SizeOf(TExtendedPoint));
      try
        if IsNan(VPoint.X) or IsNan(VPoint.Y) then begin
          VDoublePoint := CEmptyDoublePoint;
        end else if (VPoint.X >= CMaxDegres) or (VPoint.X <= CMinDegres) or (VPoint.Y >= CMaxDegres) or (VPoint.Y <= CMinDegres) then begin
          VDoublePoint := CEmptyDoublePoint;
        end else begin
          VDoublePoint := DoublePoint(VPoint.X, VPoint.Y);
        end;
      except
        VDoublePoint := CEmptyDoublePoint;
      end;
      AAggregator.Add(VDoublePoint);
    end;
  finally
    VStream.Free;
  end;
end;

procedure BlobFromPoint(
  const APoint: IGeometryLonLatPoint;
  ABlobField: TField
);
var
  VField: TBlobfield;
  VStream: TStream;
  VPoint: TExtendedPoint;
begin
  VField := TBlobfield(ABlobField);
  VStream := VField.DataSet.CreateBlobStream(VField, bmWrite);
  try
    VPoint.X := APoint.Point.X;
    VPoint.Y := APoint.Point.Y;
    VStream.Write(VPoint, SizeOf(VPoint));
  finally
    VStream.Free;
  end;
end;

procedure BlobFromPath(
  const APath: IGeometryLonLatMultiLine;
  ABlobField: TField
);
var
  VField: TBlobfield;
  VStream: TStream;
  i: Integer;
  VPoint: TExtendedPoint;
  VEnum: IEnumDoublePoint;
  VFirstPoint: TDoublePoint;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
begin
  VField := TBlobfield(ABlobField);
  VStream := VField.DataSet.CreateBlobStream(VField, bmWrite);
  try
    VEnum := APath.GetEnum;
    i := 0;
    if VEnum.Next(VFirstPoint) then begin
      VCurrPoint := VFirstPoint;
      VPrevPoint := VCurrPoint;
      VPoint.X := VCurrPoint.X;
      VPoint.Y := VCurrPoint.Y;
      VStream.Write(VPoint, SizeOf(VPoint));
      Inc(i);
      while VEnum.Next(VCurrPoint) do begin
        VPoint.X := VCurrPoint.X;
        VPoint.Y := VCurrPoint.Y;
        VStream.Write(VPoint, SizeOf(VPoint));
        VPrevPoint := VCurrPoint;
        Inc(i);
      end;
    end;
    if (i = 1) or ((i > 1) and DoublePointsEqual(VFirstPoint, VPrevPoint)) then begin
      VPoint.X := CEmptyDoublePoint.X;
      VPoint.Y := CEmptyDoublePoint.Y;
      VStream.Write(VPoint, SizeOf(VPoint));
    end;
  finally
    VStream.Free;
  end;
end;

procedure BlobFromPolygon(
  const APolygon: IGeometryLonLatMultiPolygon;
  ABlobField: TField
);
var
  VField: TBlobfield;
  VStream: TStream;
  VPoint: TExtendedPoint;
  VEnum: IEnumDoublePoint;
  VCurrPoint: TDoublePoint;
  VLine: IGeometryLonLatSinglePolygon;
begin
  VField := TBlobfield(ABlobField);
  VStream := VField.DataSet.CreateBlobStream(VField, bmWrite);
  try
    if APolygon.Count > 0 then begin
      VEnum := APolygon.GetEnum;
      while VEnum.Next(VCurrPoint) do begin
        VPoint.X := VCurrPoint.X;
        VPoint.Y := VCurrPoint.Y;
        VStream.Write(VPoint, SizeOf(VPoint));
      end;
      VLine := APolygon.Item[0];
      if VLine.Count > 1 then begin
        VCurrPoint := VLine.Points[0];
        VPoint.X := VCurrPoint.X;
        VPoint.Y := VCurrPoint.Y;
        VStream.Write(VPoint, SizeOf(VPoint));
      end;
    end;
  finally
    VStream.Free;
  end;
end;

constructor TMarkDbSml.Create(
  const ADbId: Integer;
  const AStateInternal: IReadWriteStateInternal;
  const AFileName: string;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AFactoryDbInternal: IMarkFactorySmlInternal;
  const ALoadDbCounter: IInternalPerformanceCounter;
  const ASaveDbCounter: IInternalPerformanceCounter
);
begin
  inherited Create;
  FDbId := ADbId;
  FFileName := AFileName;
  FStateInternal := AStateInternal;
  FFactoryDbInternal := AFactoryDbInternal;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;

  FMarkList := TIDInterfaceList.Create;
  FByCategoryList := TIDInterfaceList.Create;
  FNeedSaveFlag := TSimpleFlagWithInterlock.Create;

  FLoadDbCounter := ALoadDbCounter;
  FSaveDbCounter := ASaveDbCounter;

  FCdsMarks := TClientDataSet.Create(nil);
  FCdsMarks.Name := 'CDSmarks';
  FCdsMarks.DisableControls;
  InitEmptyDS(FCdsMarks);
  LoadMarksFromFile;
end;

destructor TMarkDbSml.Destroy;
begin
  if Assigned(FCdsMarks) then begin
    SaveMarks2File;
  end;
  FreeAndNil(FStream);
  FreeAndNil(FCdsMarks);
  FByCategoryList := nil;
  FMarkList := nil;
  FFactoryDbInternal := nil;
  inherited;
end;

function TMarkDbSml.GetMarkByName(
  const AName: string;
  const ACategory: ICategory
): IVectorDataItem;
var
  VCategory: IMarkCategorySMLInternal;
  VList: IIDInterfaceList;
  VEnum: IEnumUnknown;
  VItem: IInterface;
  VCnt: Integer;
  VMark: IVectorDataItem;
begin
  Result := nil;
  if not Supports(ACategory, IMarkCategorySMLInternal, VCategory) then begin
    VCategory := nil;
  end;
  if VCategory <> nil then begin
    if (VCategory.DbId <> FDbId) or (VCategory.Id = CNotExistCategoryID) then begin
      VCategory := nil;
    end;
  end;
  LockRead;
  try
    if VCategory <> nil then begin
      VList := IIDInterfaceList(FByCategoryList.GetByID(VCategory.Id));
    end else begin
      VList := FMarkList;
    end;
    if VList <> nil then begin
      VEnum := VList.GetEnumUnknown;
      while VEnum.Next(1, VItem, @VCnt) = S_OK do begin
        if Supports(VItem, IVectorDataItem, VMark) then begin
          if VMark.Name = AName then begin
            Result := VMark;
            Exit;
          end;
        end;
      end;
    end;
  finally
    UnlockRead;
  end;
end;

function TMarkDbSml._UpdateMark(
  const AOldMark: IInterface;
  const ANewMark: IInterface
): IVectorDataItem;
var
  VIdOld: Integer;
  VIdNew: Integer;
  VMarkInternal: IMarkSMLInternal;
  VLocated: Boolean;
  VOldMark: IVectorDataItem;
  VNewMark: IVectorDataItem;
  VList: IIDInterfaceList;
  VCategoryIdOld: Integer;
  VCategoryIdNew: Integer;
begin
  Result := nil;
  VIdOld := CNotExistMarkID;
  if Supports(AOldMark, IMarkSMLInternal, VMarkInternal) then begin
    if VMarkInternal.DbId = FDbId then begin
      VIdOld := VMarkInternal.Id;
    end else begin
      Assert(False, 'Error type of old mark object');
      Exit;
    end;
  end else if Supports(AOldMark, IVectorDataItem, VOldMark) then begin
    if Supports(VOldMark.MainInfo, IMarkSMLInternal, VMarkInternal) then begin
      if VMarkInternal.DbId = FDbId then begin
        VIdOld := VMarkInternal.Id;
      end else begin
        Assert(False, 'Error type of old mark object');
        Exit;
      end;
    end;
  end else begin
    Assert(not Assigned(AOldMark), 'Error type of old mark object');
    if Assigned(AOldMark) then begin
      Exit;
    end;
  end;

  if Supports(ANewMark, IVectorDataItem, VNewMark) then begin
    VNewMark := FFactoryDbInternal.CreateInternalMark(VNewMark);
    Assert(Assigned(VNewMark), 'Error type of new mark object');
    if not Assigned(VNewMark) then begin
      Exit;
    end;
  end else begin
    Assert(not Assigned(ANewMark), 'Error type of new mark object');
    if Assigned(ANewMark) then begin
      Exit;
    end;
    VNewMark := nil;
  end;

  VLocated := False;
  VOldMark := nil;
  if VIdOld <> CNotExistMarkID then begin
    VOldMark := IVectorDataItem(FMarkList.GetByID(VIdOld));
    if (VOldMark <> nil) and (VNewMark <> nil) then begin
      if VOldMark.IsEqual(VNewMark) then begin
        Result := VOldMark;
        Exit;
      end;
    end;
    FCdsMarks.Filtered := false;
    if FCdsMarks.Locate('id', VIdOld, []) then begin
      VLocated := True;
    end;
  end;
  if VLocated then begin
    if VNewMark <> nil then begin
      FCdsMarks.Edit;
      WriteCurrentMark(VNewMark);
      FCdsMarks.Post;
      Result := ReadCurrentMark;
    end else begin
      FCdsMarks.Delete;
    end;
    SetChanged;
    FNeedSaveFlag.SetFlag;
  end else begin
    if VNewMark <> nil then begin
      FCdsMarks.Insert;
      WriteCurrentMark(VNewMark);
      FCdsMarks.Post;
      Result := ReadCurrentMark;
      SetChanged;
      FNeedSaveFlag.SetFlag;
    end;
  end;

  VIdNew := CNotExistMarkID;
  VCategoryIdNew := CNotExistCategoryID;
  if Assigned(Result) and Supports(Result.MainInfo, IMarkSMLInternal, VMarkInternal) then begin
    VIdNew := VMarkInternal.Id;
    VCategoryIdNew := VMarkInternal.CategoryId;
  end;

  VCategoryIdOld := CNotExistCategoryID;
  if Assigned(VOldMark) and Supports(VOldMark.MainInfo, IMarkSMLInternal, VMarkInternal) then begin
    VCategoryIdOld := VMarkInternal.CategoryId;
  end;

  if VIdOld = VIdNew then begin
    if VOldMark <> nil then begin
      if Result <> nil then begin
        FMarkList.Replace(VIdOld, Result);
        if VCategoryIdOld <> VCategoryIdNew then begin
          VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryIdOld));
          if VList <> nil then begin
            VList.Remove(VIdOld);
          end;
          VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryIdNew));
          if VList = nil then begin
            VList := TIDInterfaceList.Create;
            FByCategoryList.Add(VCategoryIdNew, VList);
          end;
          VList.Add(VIdNew, Result);
        end else begin
          VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryIdOld));
          if VList <> nil then begin
            VList.Replace(VIdNew, Result);
          end;
        end;
      end else begin
        FMarkList.Remove(VIdOld);
        VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryIdOld));
        if VList <> nil then begin
          VList.Remove(VIdOld);
        end;
      end;
    end else begin
      if Result <> nil then begin
        FMarkList.Add(VIdNew, Result);
        VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryIdNew));
        if VList = nil then begin
          VList := TIDInterfaceList.Create;
          FByCategoryList.Add(VCategoryIdNew, VList);
        end;
        VList.Add(VIdNew, Result);
      end;
    end;
  end else begin
    if VOldMark <> nil then begin
      FMarkList.Remove(VIdOld);
      VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryIdOld));
      if VList <> nil then begin
        VList.Remove(VIdOld);
      end;
    end;
    if Result <> nil then begin
      FMarkList.Add(VIdNew, Result);
      VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryIdNew));
      if VList = nil then begin
        VList := TIDInterfaceList.Create;
        FByCategoryList.Add(VCategoryIdNew, VList);
      end;
      VList.Add(VIdNew, Result);
    end;
  end;
end;

function TMarkDbSml.UpdateMark(
  const AOldMark: IVectorDataItem;
  const ANewMark: IVectorDataItem
): IVectorDataItem;
begin
  Assert((AOldMark <> nil) or (ANewMark <> nil));
  LockWrite;
  try
    Result := _UpdateMark(AOldMark, ANewMark);
  finally
    UnlockWrite;
  end;
  SaveMarks2File;
end;

function TMarkDbSml.UpdateMarkList(
  const AOldMarkList, ANewMarkList: IInterfaceListStatic
): IInterfaceListStatic;
var
  i: Integer;
  VNew: IInterface;
  VOld: IInterface;
  VResult: IVectorDataItem;
  VMinCount: Integer;
  VMaxCount: Integer;
  VTemp: IInterfaceListSimple;
begin
  Result := nil;
  if ANewMarkList <> nil then begin
    VTemp := TInterfaceListSimple.Create;
    VTemp.Capacity := ANewMarkList.Count;

    LockWrite;
    try
      if (AOldMarkList <> nil) then begin
        if AOldMarkList.Count < ANewMarkList.Count then begin
          VMinCount := AOldMarkList.Count;
          VMaxCount := ANewMarkList.Count;
        end else begin
          VMinCount := ANewMarkList.Count;
          VMaxCount := AOldMarkList.Count;
        end;
      end else begin
        VMinCount := 0;
        VMaxCount := ANewMarkList.Count;
      end;
      for i := 0 to VMinCount - 1 do begin
        VOld := AOldMarkList[i];
        VNew := ANewMarkList[i];
        VResult := _UpdateMark(VOld, VNew);
        VTemp.Add(VResult);
      end;
      for i := VMinCount to VMaxCount - 1 do begin
        VOld := nil;
        if (AOldMarkList <> nil) and (i < AOldMarkList.Count) then begin
          VOld := AOldMarkList[i];
        end;
        VNew := nil;
        if (i < ANewMarkList.Count) then begin
          VNew := ANewMarkList[i];
        end;
        VResult := _UpdateMark(VOld, VNew);
        if i < VTemp.Capacity then begin
          VTemp.Add(VResult);
        end;
      end;
    finally
      UnlockWrite;
    end;
    SaveMarks2File;
    Result := VTemp.MakeStaticAndClear;
  end else begin
    LockWrite;
    try
      for i := 0 to AOldMarkList.Count - 1 do begin
        _UpdateMark(AOldMarkList[i], nil);
      end;
    finally
      UnlockWrite;
    end;
    SaveMarks2File;
  end;
end;

function TMarkDbSml.ReadCurrentMark: IVectorDataItem;
var
  VPicName: string;
  AId: Integer;
  VName: string;
  VVisible: Boolean;
  VPoints: IDoublePointsAggregator;
  VCategoryId: Integer;
  VDesc: string;
  VColor1: TColor32;
  VColor2: TColor32;
  VScale1: Integer;
  VScale2: Integer;
begin
  VPoints := TDoublePointsAggregator.Create;
  AId := FCdsMarks.fieldbyname('id').AsInteger;
  VName := FCdsMarks.FieldByName('name').AsString;
  VVisible := FCdsMarks.FieldByName('Visible').AsBoolean;
  Blob2ExtArr(FCdsMarks.FieldByName('LonLatArr'), VPoints);
  VCategoryId := FCdsMarks.FieldByName('categoryid').AsInteger;
  VDesc := FCdsMarks.FieldByName('descr').AsString;
  VPicName := FCdsMarks.FieldByName('PicName').AsString;
  VColor1 := TColor32(FCdsMarks.FieldByName('Color1').AsInteger);
  VColor2 := TColor32(FCdsMarks.FieldByName('Color2').AsInteger);
  VScale1 := FCdsMarks.FieldByName('Scale1').AsInteger;
  VScale2 := FCdsMarks.FieldByName('Scale2').AsInteger;

  Result :=
    FFactoryDbInternal.CreateMark(
      AId,
      VName,
      VVisible,
      VPicName,
      VCategoryId,
      VDesc,
      VPoints.Points,
      VPoints.Count,
      VColor1,
      VColor2,
      VScale1,
      VScale2
    );
end;

procedure TMarkDbSml.WriteCurrentMarkId(const AMark: IMarkId);
begin
  FCdsMarks.FieldByName('name').AsString := AMark.Name;
  FCdsMarks.FieldByName('Visible').AsBoolean := GetMarkVisibleByID(AMark);
end;

procedure TMarkDbSml.WriteCurrentMark(const AMark: IVectorDataItem);
var
  VMarkSMLInternal: IMarkSMLInternal;
  VPicName: string;
  VCategoryId: Integer;
  VVisible: Boolean;
  VGeometryPoint: IGeometryLonLatPoint;
  VGeometryLine: IGeometryLonLatMultiLine;
  VGeometryPoly: IGeometryLonLatMultiPolygon;
  VAppearanceIcon: IAppearancePointIcon;
  VAppearanceCaption: IAppearancePointCaption;
  VAppearanceLine: IAppearanceLine;
  VAppearanceBorder: IAppearancePolygonBorder;
  VAppearanceFill: IAppearancePolygonFill;
  VTextColor: TColor32;
  VTextBgColor: TColor32;
  VFontSize: Integer;
  VMarkerSize: Integer;
  VLineColor: TColor32;
  VLineWidth: Integer;
  VFillColor: TColor32;
  VMarkWithCategory: IVectorDataItemWithCategory;
  VCategory: IMarkCategorySMLInternal;
  VRect: TDoubleRect;
begin
  Assert(Assigned(AMark));
  VVisible := True;
  VCategoryId := CNotExistCategoryID;
  if Supports(AMark.MainInfo, IMarkSMLInternal, VMarkSMLInternal) then begin
    VVisible := VMarkSMLInternal.Visible;
    VCategoryId := VMarkSMLInternal.CategoryId;
  end else begin
    if Supports(AMark.MainInfo, IVectorDataItemWithCategory, VMarkWithCategory) then begin
      if Supports(VMarkWithCategory.Category, IMarkCategorySMLInternal, VCategory) then begin
        VCategoryId := VCategory.Id;
      end;
    end;
  end;
  Assert(VCategoryId <> CNotExistCategoryID);

  FCdsMarks.FieldByName('Visible').AsBoolean := VVisible;
  FCdsMarks.FieldByName('name').AsString := AMark.Name;
  FCdsMarks.FieldByName('categoryid').AsInteger := VCategoryId;
  FCdsMarks.FieldByName('descr').AsString := AMark.Desc;
  VRect := AMark.Geometry.Bounds.Rect;
  FCdsMarks.FieldByName('LonL').AsFloat := VRect.Left;
  FCdsMarks.FieldByName('LatT').AsFloat := VRect.Top;
  FCdsMarks.FieldByName('LonR').AsFloat := VRect.Right;
  FCdsMarks.FieldByName('LatB').AsFloat := VRect.Bottom;

  if Supports(AMark.Geometry, IGeometryLonLatPoint, VGeometryPoint) then begin
    VTextColor := 0;
    VTextBgColor := 0;
    VFontSize := 0;
    VMarkerSize := 0;
    VPicName := '';
    if Supports(AMark.Appearance, IAppearancePointCaption, VAppearanceCaption) then begin
      VTextColor := VAppearanceCaption.TextColor;
      VTextBgColor := VAppearanceCaption.TextBgColor;
      VFontSize := VAppearanceCaption.FontSize;
    end;
    if Supports(AMark.Appearance, IAppearancePointIcon, VAppearanceIcon) then begin
      VMarkerSize := VAppearanceIcon.MarkerSize;
      VPicName := VAppearanceIcon.PicName;
    end;
    FCdsMarks.FieldByName('PicName').AsString := VPicName;
    BlobFromPoint(VGeometryPoint, FCdsMarks.FieldByName('LonLatArr'));
    FCdsMarks.FieldByName('Color1').AsInteger := VTextColor;
    FCdsMarks.FieldByName('Color2').AsInteger := VTextBgColor;
    FCdsMarks.FieldByName('Scale1').AsInteger := VFontSize;
    FCdsMarks.FieldByName('Scale2').AsInteger := VMarkerSize;
  end else if Supports(AMark.Geometry, IGeometryLonLatMultiLine, VGeometryLine) then begin
    FCdsMarks.FieldByName('PicName').AsString := '';
    BlobFromPath(VGeometryLine, FCdsMarks.FieldByName('LonLatArr'));
    VLineColor := 0;
    VLineWidth := 0;
    if Supports(AMark.Appearance, IAppearanceLine, VAppearanceLine) then begin
      VLineColor := VAppearanceLine.LineColor;
      VLineWidth := VAppearanceLine.LineWidth;
    end;
    FCdsMarks.FieldByName('Color1').AsInteger := VLineColor;
    FCdsMarks.FieldByName('Color2').AsInteger := 0;
    FCdsMarks.FieldByName('Scale1').AsInteger := VLineWidth;
    FCdsMarks.FieldByName('Scale2').AsInteger := 0;
  end else if Supports(AMark.Geometry, IGeometryLonLatMultiPolygon, VGeometryPoly) then begin
    FCdsMarks.FieldByName('PicName').AsString := '';
    BlobFromPolygon(VGeometryPoly, FCdsMarks.FieldByName('LonLatArr'));
    VLineColor := 0;
    VLineWidth := 0;
    VFillColor := 0;
    if Supports(AMark.Appearance, IAppearancePolygonBorder, VAppearanceBorder) then begin
      VLineColor := VAppearanceBorder.LineColor;
      VLineWidth := VAppearanceBorder.LineWidth;
    end;
    if Supports(AMark.Appearance, IAppearancePolygonFill, VAppearanceFill) then begin
      VFillColor := VAppearanceFill.FillColor;
    end;
    FCdsMarks.FieldByName('Color1').AsInteger := VLineColor;
    FCdsMarks.FieldByName('Color2').AsInteger := VFillColor;
    FCdsMarks.FieldByName('Scale1').AsInteger := VLineWidth;
    FCdsMarks.FieldByName('Scale2').AsInteger := 0;
  end;
end;

function TMarkDbSml.GetMarkByID(const AMarkId: IMarkId): IVectorDataItem;
var
  AId: Integer;
  VMarkVisible: IMarkSMLInternal;
begin
  Result := nil;
  if AMarkId <> nil then begin
    AId := CNotExistMarkID;
    if Supports(AMarkId, IMarkSMLInternal, VMarkVisible) then begin
      AId := VMarkVisible.Id;
    end;
    if AId <> CNotExistMarkID then begin
      LockRead;
      try
        Result := IVectorDataItem(FMarkList.GetByID(AId));
      finally
        UnlockRead;
      end;
    end;
  end;
end;

function TMarkDbSml.GetMarkVisible(const AMark: IVectorDataItem): Boolean;
var
  VMarkVisible: IMarkSMLInternal;
begin
  Result := True;
  if AMark <> nil then begin
    if Supports(AMark.MainInfo, IMarkSMLInternal, VMarkVisible) then begin
      Result := VMarkVisible.Visible;
    end;
  end;
end;

function TMarkDbSml.GetMarkVisibleByID(const AMark: IMarkId): Boolean;
var
  VMarkInternal: IMarkSMLInternal;
begin
  Result := True;
  if AMark <> nil then begin
    if Supports(AMark, IMarkSMLInternal, VMarkInternal) then begin
      Result := VMarkInternal.Visible;
    end;
  end;
end;

procedure TMarkDbSml.SetAllMarksInCategoryVisible(
  const ACategory: ICategory;
  ANewVisible: Boolean
);
var
  VVisible: Boolean;
  VFilter: string;
  VCategoryId: Integer;
  VList: IIDInterfaceList;
  VEnum: IEnumUnknown;
  VCnt: Cardinal;
  VMarkInternal: IMarkSMLInternal;
  VItem: IVectorDataItem;
begin
  VFilter := GetFilterTextByCategory(ACategory);
  if VFilter <> '' then begin
    LockWrite;
    try
      FCdsMarks.Filtered := false;
      FCdsMarks.Filter := VFilter;
      FCdsMarks.Filtered := true;
      FCdsMarks.First;
      while not (FCdsMarks.Eof) do begin
        VVisible := FCdsMarks.FieldByName('Visible').AsBoolean;
        if VVisible <> ANewVisible then begin
          FCdsMarks.Edit;
          FCdsMarks.FieldByName('Visible').AsBoolean := ANewVisible;
          FCdsMarks.Post;
          SetChanged;
          FNeedSaveFlag.SetFlag;
        end;
        FCdsMarks.Next;
      end;
      VCategoryId := GetCategoryID(ACategory);
      VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryId));
      if VList <> nil then begin
        VEnum := VList.GetEnumUnknown;
        while VEnum.Next(1, VItem, @VCnt) = S_OK do begin
          if Supports(VItem.MainInfo, IMarkSMLInternal, VMarkInternal) then begin
            VMarkInternal.Visible := ANewVisible;
          end;
        end;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TMarkDbSml.SetMarkVisible(const AMark: IVectorDataItem; AVisible: Boolean);
var
  VMarkVisible: IMarkSMLInternal;
  AId: Integer;
  VMarkInternal: IMarkSMLInternal;
begin
  if AMark <> nil then begin
    AId := CNotExistMarkID;
    if Supports(AMark.MainInfo, IMarkSMLInternal, VMarkVisible) then begin
      AId := VMarkVisible.Id;
      VMarkVisible.Visible := AVisible;
    end;
    if AId <> CNotExistMarkID then begin
      LockWrite;
      try
        FCdsMarks.Filtered := false;
        if FCdsMarks.Locate('id', AId, []) then begin
          FCdsMarks.Edit;
          WriteCurrentMarkId(AMark.MainInfo as IMarkId);
          FCdsMarks.Post;
          SetChanged;
          FNeedSaveFlag.SetFlag;
        end;
        if Supports(IVectorDataItem(FMarkList.GetByID(AId)).MainInfo, IMarkSMLInternal, VMarkInternal) then begin
          VMarkInternal.Visible := AVisible;
        end;
      finally
        UnlockWrite;
      end;
    end;
  end;
end;

procedure TMarkDbSml.SetMarkVisibleByID(
  const AMark: IMarkId;
  AVisible: Boolean
);
var
  VMarkVisible: IMarkSMLInternal;
  AId: Integer;
  VMarkInternal: IMarkSMLInternal;
begin
  if AMark <> nil then begin
    AId := CNotExistMarkID;
    if Supports(AMark, IMarkSMLInternal, VMarkVisible) then begin
      AId := VMarkVisible.Id;
      VMarkVisible.Visible := AVisible;
    end;
    if AId <> CNotExistMarkID then begin
      LockWrite;
      try
        FCdsMarks.Filtered := false;
        if FCdsMarks.Locate('id', AId, []) then begin
          FCdsMarks.Edit;
          WriteCurrentMarkId(AMark);
          FCdsMarks.Post;
          SetChanged;
          FNeedSaveFlag.SetFlag;
        end;
        if Supports(IVectorDataItem(FMarkList.GetByID(AId)).MainInfo, IMarkSMLInternal, VMarkInternal) then begin
          VMarkInternal.Visible := AVisible;
        end;
      finally
        UnlockWrite;
      end;
    end;
  end;
end;

procedure TMarkDbSml.SetMarkVisibleByIDList(
  const AMarkList: IInterfaceListStatic;
  AVisible: Boolean
);
var
  i: Integer;
  VMarkVisible: IMarkSMLInternal;
  AId: Integer;
  VMarkInternal: IMarkSMLInternal;
begin
  if (AMarkList <> nil) and (AMarkList.Count > 0) then begin
    LockWrite;
    try
      for i := 0 to AMarkList.Count - 1 do begin
        AId := CNotExistMarkID;
        if Supports(AMarkList.Items[i], IMarkSMLInternal, VMarkVisible) then begin
          AId := VMarkVisible.Id;
          VMarkVisible.Visible := AVisible;
        end;
        if AId <> CNotExistMarkID then begin
          if Supports(IVectorDataItem(FMarkList.GetByID(AId)).MainInfo, IMarkSMLInternal, VMarkInternal) then begin
            VMarkInternal.Visible := AVisible;
            FCdsMarks.Filtered := false;
            if FCdsMarks.Locate('id', AId, []) then begin
              FCdsMarks.Edit;
              WriteCurrentMarkId(VMarkInternal as IMarkId);
              FCdsMarks.Post;
              SetChanged;
              FNeedSaveFlag.SetFlag;
            end;
          end;
        end;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TMarkDbSml.ToggleMarkVisibleByIDList(const AMarkList: IInterfaceListStatic);
var
  i: Integer;
  VMarkVisible: IMarkSMLInternal;
  AId: Integer;
  VMarkInternal: IMarkSMLInternal;
  VVisible: Boolean;
  VVisibleCount: Integer;
  VInvisibleCount: Integer;
begin
  if (AMarkList <> nil) and (AMarkList.Count > 0) then begin
    VVisibleCount := 0;
    VInvisibleCount := 0;
    for i := 0 to AMarkList.Count - 1 do begin
      if Supports(AMarkList.Items[i], IMarkSMLInternal, VMarkVisible) then begin
        if VMarkVisible.Visible then begin
          Inc(VVisibleCount);
        end else begin
          Inc(VInvisibleCount);
        end;
      end;
    end;
    VVisible := VVisibleCount < VInvisibleCount;

    LockWrite;
    try
      for i := 0 to AMarkList.Count - 1 do begin
        AId := CNotExistMarkID;
        if Supports(AMarkList.Items[i], IMarkSMLInternal, VMarkVisible) then begin
          AId := VMarkVisible.Id;
          VMarkVisible.Visible := VVisible;
        end;
        if AId <> CNotExistMarkID then begin
          if Supports(IVectorDataItem(FMarkList.GetByID(AId)).MainInfo, IMarkSMLInternal, VMarkInternal) then begin
            VMarkInternal.Visible := VVisible;
            FCdsMarks.Filtered := false;
            if FCdsMarks.Locate('id', AId, []) then begin
              FCdsMarks.Edit;
              WriteCurrentMarkId(VMarkInternal as IMarkId);
              FCdsMarks.Post;
              SetChanged;
              FNeedSaveFlag.SetFlag;
            end;
          end;
        end;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

function TMarkDbSml.GetAllMarkIdList: IInterfaceListStatic;
var
  VEnum: IEnumUnknown;
  VCnt: Cardinal;
  VItem: IVectorDataItem;
  VMarkId: IMarkId;
  VTemp: IInterfaceListSimple;
begin
  Result := nil;
  VTemp := TInterfaceListSimple.Create;
  LockRead;
  try
    VEnum := FMarkList.GetEnumUnknown;
    while VEnum.Next(1, VItem, @VCnt) = S_OK do begin
      if Supports(VItem.MainInfo, IMarkId, VMarkId) then begin
        VTemp.Add(VMarkId);
      end;
    end;
  finally
    UnlockRead;
  end;
  Result := VTemp.MakeStaticAndClear;
end;

function TMarkDbSml.GetById(AId: Integer): IVectorDataItem;
begin
  Result := nil;
  if AId >= 0 then begin
    LockRead;
    try
      Result := IVectorDataItem(FMarkList.GetByID(AId));
    finally
      UnlockRead;
    end;
  end;
end;

function TMarkDbSml.GetCategoryID(const ACategory: ICategory): Integer;
var
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  Assert(ACategory <> nil);
  Result := CNotExistCategoryID;
  if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
    Result := VCategoryInternal.Id;
  end;
end;

function TMarkDbSml.GetMarkIdListByCategory(const ACategory: ICategory): IInterfaceListStatic;
var
  VMarkId: IMarkId;
  VCategoryId: Integer;
  VList: IIDInterfaceList;
  VEnum: IEnumUnknown;
  VCnt: Cardinal;
  VItem: IVectorDataItem;
  VTemp: IInterfaceListSimple;
begin
  Result := nil;
  VCategoryId := GetCategoryID(ACategory);
  if Supports(FByCategoryList.GetByID(VCategoryId), IIDInterfaceList, VList) then begin
    VTemp := TInterfaceListSimple.Create;
    VTemp.Capacity := VList.Count;
    VEnum := VList.GetEnumUnknown;
    while VEnum.Next(1, VItem, @VCnt) = S_OK do begin
      if Supports(VItem.MainInfo, IMarkId, VMarkId) then begin
        VTemp.Add(VMarkId);
      end;
    end;
    Result := VTemp.MakeStaticAndClear;
  end;
end;

procedure TMarkDbSml.InitEmptyDS(ACdsMarks: TClientDataSet);
begin
  ACdsMarks.Close;
  ACdsMarks.XMLData :=
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>' +
    '<DATAPACKET Version="2.0">' +
    '   <METADATA>' +
    '           <FIELDS>' +
    '                   <FIELD attrname="id" fieldtype="i4" readonly="true" SUBTYPE="Autoinc"/>' +
    '                   <FIELD attrname="name" fieldtype="string" WIDTH="255"/>' +
    '                   <FIELD attrname="descr" fieldtype="bin.hex" SUBTYPE="Text"/>' +
    '                   <FIELD attrname="scale1" fieldtype="i4"/>' +
    '           <FIELD attrname="scale2" fieldtype="i4"/>' +
    '                   <FIELD attrname="lonlatarr" fieldtype="bin.hex" SUBTYPE="Binary"/>' +
    '                   <FIELD attrname="lonL" fieldtype="r8"/>' +
    '                   <FIELD attrname="latT" fieldtype="r8"/>' +
    '                   <FIELD attrname="LonR" fieldtype="r8"/>' +
    '                   <FIELD attrname="LatB" fieldtype="r8"/>' +
    '                   <FIELD attrname="color1" fieldtype="i4"/>' +
    '                   <FIELD attrname="color2" fieldtype="i4"/>' +
    '                   <FIELD attrname="visible" fieldtype="boolean"/>' +
    '                   <FIELD attrname="picname" fieldtype="string" WIDTH="20"/>' +
    '                   <FIELD attrname="categoryid" fieldtype="i4"/>' +
    '           </FIELDS>' +
    '           <PARAMS AUTOINCVALUE="1"/>' +
    '   </METADATA>' +
    '   <ROWDATA/>' +
    '</DATAPACKET>';
  ACdsMarks.IndexFieldNames := 'categoryid;LonR;LonL;LatT;LatB;visible';
  ACdsMarks.Open;
end;

function TMarkDbSml.GetFilterTextByCategory(const ACategory: ICategory): string;
var
  VCategoryID: Integer;
begin
  Result := '';
  if (ACategory <> nil) then begin
    VCategoryID := GetCategoryID(ACategory);
    if VCategoryID >= 0 then begin
      Result := '(categoryid = ' + IntToStr(VCategoryID) + ')';
    end;
  end;
end;

procedure TMarkDbSml._AddMarksToList(
  const ASourceList: IIDInterfaceList;
  AIgnoreVisible: Boolean;
  const AResultList: IVectorItemSubsetBuilder
);
var
  VMark: IVectorDataItem;
  VEnum: IEnumUnknown;
  VCnt: Cardinal;
  VMarkInternal: IMarkSMLInternal;
  VNewCapacity: Integer;
begin
  if AIgnoreVisible then begin
    VNewCapacity := AResultList.Count + ASourceList.Count;
    if AResultList.Capacity < VNewCapacity then begin
      AResultList.Capacity := VNewCapacity;
    end;
  end;
  VEnum := ASourceList.GetEnumUnknown;
  while VEnum.Next(1, VMark, @VCnt) = S_OK do begin
    if not AIgnoreVisible then begin
      if Supports(VMark.MainInfo, IMarkSMLInternal, VMarkInternal) then begin
        if VMarkInternal.Visible then begin
          AResultList.Add(VMark);
        end;
      end;
    end else begin
      AResultList.Add(VMark);
    end;
  end;
end;

procedure TMarkDbSml._AddMarksToListByRect(
  const ASourceList: IIDInterfaceList;
  const ARect: TDoubleRect;
  AIgnoreVisible: Boolean;
  const AResultList: IVectorItemSubsetBuilder
);
var
  VMark: IVectorDataItem;
  VEnum: IEnumUnknown;
  VCnt: Cardinal;
  VMarkInternal: IMarkSMLInternal;
begin
  VEnum := ASourceList.GetEnumUnknown;
  while VEnum.Next(1, VMark, @VCnt) = S_OK do begin
    if VMark.Geometry.Bounds.IsIntersecWithRect(ARect) then begin
      if not AIgnoreVisible then begin
        if Supports(VMark.MainInfo, IMarkSMLInternal, VMarkInternal) then begin
          if VMarkInternal.Visible then begin
            AResultList.Add(VMark);
          end;
        end;
      end else begin
        AResultList.Add(VMark);
      end;
    end;
  end;
end;

function TMarkDbSml.GetMarkSubsetByCategoryList(
  const ACategoryList: IInterfaceListStatic;
  const AIncludeHiddenMarks: Boolean): IVectorItemSubset;
var
  VResultList: IVectorItemSubsetBuilder;
  i: Integer;
  VCategoryID: Integer;
  VList: IIDInterfaceList;
begin
  Result := nil;
  VResultList := FVectorItemSubsetBuilderFactory.Build;
    LockRead;
    try
      if (ACategoryList = nil) then begin
        _AddMarksToList(FMarkList, AIncludeHiddenMarks, VResultList);
      end else begin
        for i := 0 to ACategoryList.Count - 1 do begin
          VCategoryID := GetCategoryID(ICategory(ACategoryList[i]));
          VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryID));
          if VList <> nil then begin
            _AddMarksToList(VList, AIncludeHiddenMarks, VResultList);
          end;
        end;
      end;
    finally
      UnlockRead;
    end;
  Result := VResultList.MakeStaticAndClear;
end;

function TMarkDbSml.GetMarkSubsetByCategoryListInRect(
  const ARect: TDoubleRect;
  const ACategoryList: IInterfaceListStatic;
  const AIncludeHiddenMarks: Boolean
): IVectorItemSubset;
var
  VResultList: IVectorItemSubsetBuilder;
  i: Integer;
  VCategoryID: Integer;
  VList: IIDInterfaceList;
begin
  Result := nil;
  VResultList := FVectorItemSubsetBuilderFactory.Build;
    LockRead;
    try
      if (ACategoryList = nil) then begin
        _AddMarksToListByRect(FMarkList, ARect, AIncludeHiddenMarks, VResultList);
      end else begin
        for i := 0 to ACategoryList.Count - 1 do begin
          VCategoryID := GetCategoryID(ICategory(ACategoryList[i]));
          VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryID));
          if VList <> nil then begin
            _AddMarksToListByRect(VList, ARect, AIncludeHiddenMarks, VResultList);
          end;
        end;
      end;
    finally
      UnlockRead;
    end;
  Result := VResultList.MakeStaticAndClear;
end;

function TMarkDbSml.FindMarks(
  const ASearch: string;
  const AMaxCount: Integer;
  const AIncludeHiddenMarks: Boolean;
  const ASearchInDescription: Boolean
): IVectorItemSubset;
var
  VResultList: IVectorItemSubsetBuilder;
  VList: IIDInterfaceList;
  VMark: IVectorDataItem;
  VEnum: IEnumUnknown;
  VCnt: Cardinal;
  VMarkInternal: IMarkSMLInternal;
begin
  Result := nil;
  VResultList := FVectorItemSubsetBuilderFactory.Build;
  VList := FMarkList;
  VEnum := VList.GetEnumUnknown;
  while VEnum.Next(1, VMark, @VCnt) = S_OK do begin
    if ContainsText(VMark.Name, ASearch) or (ASearchInDescription) and ContainsText(VMark.Desc, ASearch) then begin
      if not AIncludeHiddenMarks then begin
        if Supports(VMark.MainInfo, IMarkSMLInternal, VMarkInternal) then begin
          if VMarkInternal.Visible then begin
            VResultList.Add(VMark);
          end;
        end;
      end else begin
        VResultList.Add(VMark);
      end;
      if (AMaxCount > 0) and (VResultList.Count >= AMaxCount) then begin
        Break;
      end;
    end;
  end;
  Result := VResultList.MakeStaticAndClear;
end;

function TMarkDbSml.GetMarkSubsetByCategory(const ACategory: ICategory;
  const AIncludeHiddenMarks: Boolean): IVectorItemSubset;
var
  VResultList: IVectorItemSubsetBuilder;
  VCategoryId: Integer;
  VList: IIDInterfaceList;
begin
  Result := nil;
  VResultList := FVectorItemSubsetBuilderFactory.Build;
    if ACategory = nil then begin
      VList := FMarkList;
    end else begin
      VCategoryId := GetCategoryID(ACategory);
      VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryId));
    end;
    if VList <> nil then begin
      LockRead;
      try
        _AddMarksToList(VList, AIncludeHiddenMarks, VResultList);
      finally
        UnlockRead;
      end;
    end;
  Result := VResultList.MakeStaticAndClear;
end;

function TMarkDbSml.GetMarkSubsetByCategoryInRect(
  const ARect: TDoubleRect;
  const ACategory: ICategory;
  const AIncludeHiddenMarks: Boolean
): IVectorItemSubset;
var
  VResultList: IVectorItemSubsetBuilder;
  VCategoryId: Integer;
  VList: IIDInterfaceList;
begin
  Result := nil;
  VResultList := FVectorItemSubsetBuilderFactory.Build;
    if ACategory = nil then begin
      VList := FMarkList;
    end else begin
      VCategoryId := GetCategoryID(ACategory);
      VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryId));
    end;
    if VList <> nil then begin
      LockRead;
      try
        _AddMarksToListByRect(VList, ARect, AIncludeHiddenMarks, VResultList);
      finally
        UnlockRead;
      end;
    end;
  Result := VResultList.MakeStaticAndClear;
end;

function TMarkDbSml.GetMarksBackUpFileName: string;
begin
  Result := ChangeFileExt(FFileName, '.~sml');
end;

procedure TMarkDbSml.LoadMarksFromFile;
var
  VFileName: string;
  VMark: IVectorDataItem;
  VIdNew: Integer;
  VCategoryIdNew: Integer;
  VList: IIDInterfaceList;
  VMarkInternal: IMarkSMLInternal;
  VStream: TStream;
  XML: AnsiString;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  if FLoadDbCounter <> nil then begin
    VCounterContext := FLoadDbCounter.StartOperation;
  end else begin
    VCounterContext := 0;
  end;
  try
    VFileName := FFileName;
    FStateInternal.LockWrite;
    try
      LockWrite;
      try
        InitEmptyDS(FCdsMarks);
        FMarkList.Clear;
        FByCategoryList.Clear;
        if FStateInternal.ReadAccess <> asDisabled then begin
          VStream := nil;
          try
            if FileExists(VFileName) then begin
              if FStateInternal.WriteAccess <> asDisabled then begin
                try
                  VStream := TFileStream.Create(VFileName, fmOpenReadWrite + fmShareDenyWrite);
                  FStateInternal.WriteAccess := asEnabled;
                except
                  VStream := nil;
                  FStateInternal.WriteAccess := asDisabled;
                end;
              end;
              if VStream = nil then begin
                try
                  VStream := TFileStream.Create(VFileName, fmOpenRead + fmShareDenyNone);
                  FStateInternal.ReadAccess := asEnabled;
                except
                  FStateInternal.ReadAccess := asDisabled;
                  VStream := nil;
                end;
              end;
              if VStream <> nil then begin
                try
                  SetLength(XML, VStream.Size);
                  VStream.ReadBuffer(XML[1], length(XML));
                except
                  FStateInternal.ReadAccess := asDisabled;
                  VStream.Free;
                  VStream := nil;
                end;
              end;

              if length(XML) > 0 then begin
                try
                  FCdsMarks.XMLData := XML;
                except
                  InitEmptyDS(FCdsMarks);
                end;
              end;

              if FCdsMarks.RecordCount > 0 then begin
                if FStateInternal.WriteAccess = asEnabled then begin
                  CopyFile(PChar(VFileName), PChar(GetMarksBackUpFileName), false);
                end;
              end;

              FCdsMarks.Filtered := False;
              FCdsMarks.First;
              while not FCdsMarks.Eof do begin
                VMark := ReadCurrentMark;
                if Assigned(VMark) and Supports(VMark.MainInfo, IMarkSMLInternal, VMarkInternal) then begin
                  VIdNew := VMarkInternal.Id;
                  VCategoryIdNew := VMarkInternal.CategoryId;
                  FMarkList.Add(VIdNew, VMark);
                  VList := IIDInterfaceList(FByCategoryList.GetByID(VCategoryIdNew));
                  if VList = nil then begin
                    VList := TIDInterfaceList.Create;
                    FByCategoryList.Add(VCategoryIdNew, VList);
                  end;
                  VList.Add(VIdNew, VMark);
                end;
                FCdsMarks.Next;
              end;
            end else begin
              if FStateInternal.WriteAccess <> asDisabled then begin
                try
                  VStream := TFileStream.Create(VFileName, fmCreate);
                  VStream.Free;
                  VStream := nil;
                except
                  FStateInternal.WriteAccess := asDisabled;
                  VStream := nil;
                end;
                if FStateInternal.WriteAccess <> asDisabled then begin
                  try
                    VStream := TFileStream.Create(VFileName, fmOpenReadWrite + fmShareDenyWrite);
                    FStateInternal.WriteAccess := asEnabled;
                  except
                    VStream := nil;
                    FStateInternal.WriteAccess := asDisabled;
                  end;
                end;
              end;
            end;
            if FStream <> nil then begin
              FreeAndNil(FStream);
            end;
            if FStateInternal.WriteAccess = asEnabled then begin
              FStream := VStream;
              VStream := nil;
            end;
          finally
            VStream.Free;
          end;
        end;
      finally
        UnlockWrite
      end;
    finally
      FStateInternal.UnlockWrite;
    end;
  finally
    if VCounterContext <> 0 then begin
      FLoadDbCounter.FinishOperation(VCounterContext);
    end;
  end;
end;

function TMarkDbSml.SaveMarks2File: boolean;
var
  XML: AnsiString;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  result := true;
  if FNeedSaveFlag.CheckFlagAndReset then begin
    if FSaveDbCounter <> nil then begin
      VCounterContext := FSaveDbCounter.StartOperation;
    end else begin
      VCounterContext := 0;
    end;
    try
      try
        FStateInternal.LockRead;
        try
          if FStateInternal.WriteAccess = asEnabled then begin
            LockRead;
            try
              if FStream <> nil then begin
                FCdsMarks.MergeChangeLog;
                XML := FCdsMarks.XMLData;
                FStream.Size := length(XML);
                FStream.Position := 0;
                FStream.WriteBuffer(XML[1], length(XML));
              end else begin
                FNeedSaveFlag.SetFlag;
              end;
            finally
              UnlockRead;
            end;
          end else begin
            FNeedSaveFlag.SetFlag;
          end;
        finally
          FStateInternal.UnlockRead;
        end;
      except
        result := false;
        FNeedSaveFlag.SetFlag;
      end;
    finally
      if VCounterContext <> 0 then begin
        FSaveDbCounter.FinishOperation(VCounterContext);
      end;
    end;
  end;
end;

end.
