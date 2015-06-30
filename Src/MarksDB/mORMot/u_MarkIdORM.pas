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

unit u_MarkIdORM;

interface

uses
  t_Hash,
  t_MarkSystemORM,
  i_MarkId,
  i_VectorDataItemSimple,
  i_Category,
  i_HtmlToHintTextConverter,
  i_MarkDbInternalORM,
  u_BaseInterfacedObject;

type
  TMarkIdORM = class(
    TBaseInterfacedObject,
    IMarkId,
    IMarkInternalORM,
    IVectorDataItemMainInfo,
    IVectorDataItemWithCategory
  )
  private
    FHash: THashValue;
    FName: string;
    FDesc: string;
    FId: TID;
    FDbId: Integer;
    FCategory: ICategory;
    FCategoryId: TID;
    FVisible: Boolean;
    FType: TMarkIdType;
    FMultiGeometryCount: Integer;
    FHintConverter: IHtmlToHintTextConverter;
  protected
    function IsEqualInternal(const AMarkInternal: IMarkInternalORM): Boolean;
    function GetStringID: string;
  protected
    function GetHash: THashValue;
    function GetName: string;
    function GetDesc: string;
    function GetHintText: string;
    function GetInfoHTML: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
    function GetMarkType: TMarkIdType;
    function GetMultiGeometryCount: Integer;
  protected
    function GetId: TID;
    function GetDbId: Integer;
    function GetCategory: ICategory;
    function GetCategoryId: TID;
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
    function IsSameId(const AMarkId: IMarkId): Boolean;
    function IsSameMark(const AMark: IVectorDataItem): Boolean;
    function IsEqual(const AValue: IVectorDataItemMainInfo): Boolean;
  public
    constructor Create(
      const AHintConverter: IHtmlToHintTextConverter;
      const AHash: THashValue;
      const AType: TMarkIdType;
      const AName: string;
      const ADesc: string;
      const AId: TID;
      const ADbId: Integer;
      const AMultiGeometryCount: Integer;
      const ACategory: ICategory;
      const AVisible: Boolean
    );
  end;

implementation

uses
  SysUtils,
  c_InternalBrowser,
  i_MarkCategoryInternalORM;

{ TMarkIdORM }

constructor TMarkIdORM.Create(
  const AHintConverter: IHtmlToHintTextConverter;
  const AHash: THashValue;
  const AType: TMarkIdType;
  const AName: string;
  const ADesc: string;
  const AId: TID;
  const ADbId: Integer;
  const AMultiGeometryCount: Integer;
  const ACategory: ICategory;
  const AVisible: Boolean
);
var
  VCategory: IMarkCategoryInternalORM;
begin
  Assert(AId > 0);
  inherited Create;
  FHintConverter := AHintConverter;
  FHash := AHash;
  FType := AType;
  FName := AName;
  FDesc := ADesc;
  FId := AId;
  FDbId := ADbId;
  FMultiGeometryCount := AMultiGeometryCount;
  FCategory := ACategory;
  FCategoryId := 0;
  if FCategory <> nil then begin
    if Supports(FCategory, IMarkCategoryInternalORM, VCategory) then begin
      FCategoryId := VCategory.Id;
    end;
  end;
  FVisible := AVisible;
end;

function TMarkIdORM.GetCategory: ICategory;
begin
  Result := FCategory;
end;

function TMarkIdORM.GetCategoryId: TID;
begin
  Result := FCategoryId;
end;

function TMarkIdORM.GetDbId: Integer;
begin
  Result := FDbId;
end;

function TMarkIdORM.GetDesc: string;
begin
  Result := FDesc;
end;

function TMarkIdORM.GetHash: THashValue;
begin
  Result := FHash;
end;

function TMarkIdORM.GetHintText: string;
begin
  Result := FHintConverter.Convert(FName, FDesc);
end;

function TMarkIdORM.GetId: TID;
begin
  Result := FId;
end;

function TMarkIdORM.GetInfoCaption: string;
begin
  Result := FName;
end;

function TMarkIdORM.GetInfoHTML: string;
begin
  Result := '';
  if FDesc <> '' then begin
    Result := '<HTML><BODY>';
    Result := Result + FDesc;
    Result := Result + '</BODY></HTML>';
  end;
end;

function TMarkIdORM.GetInfoUrl: string;
begin
  Result := GetStringID;
  if Result <> '' then begin
    Result := CMarksSystemInternalURL + Result + '/';
  end;
end;

function TMarkIdORM.GetMarkType: TMarkIdType;
begin
  Result := FType;
end;

function TMarkIdORM.GetMultiGeometryCount: Integer;
begin
  Result := FMultiGeometryCount;
end;

function TMarkIdORM.GetName: string;
begin
  Result := FName;
end;

function TMarkIdORM.GetStringID: string;
begin
  Result := IntToStr(FId);
end;

function TMarkIdORM.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TMarkIdORM.IsEqualInternal(const AMarkInternal: IMarkInternalORM): Boolean;
begin
  Result := True;
  if FDbId <> AMarkInternal.DbId then begin
    Result := False;
    Exit;
  end;
  if FCategoryId <> AMarkInternal.CategoryId then begin
    Result := False;
    Exit;
  end;
  if FId <> AMarkInternal.Id then begin
    Result := False;
    Exit;
  end;
  if FVisible <> AMarkInternal.Visible then begin
    Result := False;
    Exit;
  end;
  if FName <> AMarkInternal.Name then begin
    Result := False;
    Exit;
  end;
end;

function TMarkIdORM.IsSameId(const AMarkId: IMarkId): Boolean;
var
  VMarkInternal: IMarkInternalORM;
begin
  Result := False;
  if AMarkId <> nil then begin
    if Supports(AMarkId, IMarkInternalORM, VMarkInternal) then begin
      Result := (FId = VMarkInternal.Id) and (FDbId = VMarkInternal.DbId);
    end;
  end;
end;

function TMarkIdORM.IsSameMark(const AMark: IVectorDataItem): Boolean;
var
  VMarkInternal: IMarkInternalORM;
begin
  if not Assigned(AMark) then begin
    Result := False;
    Exit;
  end;
  Result := False;
  if Supports(AMark.MainInfo, IMarkInternalORM, VMarkInternal) then begin
    Result := (FId = VMarkInternal.Id) and (FDbId = VMarkInternal.DbId);
  end;
end;

function TMarkIdORM.IsEqual(const AValue: IVectorDataItemMainInfo): Boolean;
var
  VVectorDataItemWithCategory: IVectorDataItemWithCategory;
begin
  if not Assigned(AValue) then begin
    Result := False;
    Exit;
  end;
  if AValue = IVectorDataItemMainInfo(Self) then begin
    Result := True;
    Exit;
  end;
  if (AValue.Hash <> 0) and (FHash <> 0) and (AValue.Hash <> FHash) then begin
    Result := False;
    Exit;
  end;
  if FName <> AValue.Name then begin
    Result := False;
    Exit;
  end;
  if FDesc <> AValue.Desc then begin
    Result := False;
    Exit;
  end;
  if Supports(AValue, IVectorDataItemWithCategory, VVectorDataItemWithCategory) then begin
    if FCategory <> nil then begin
      if not FCategory.IsSame(VVectorDataItemWithCategory.Category) then begin
        Result := False;
        Exit;
      end;
    end else begin
      if VVectorDataItemWithCategory.Category <> nil then begin
        Result := False;
        Exit;
      end;
    end;
  end else begin
    if Assigned(FCategory) then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TMarkIdORM.SetVisible(const AValue: Boolean);
begin
  FVisible := AValue;
end;

end.
