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

unit u_MarkId;

interface

uses
  t_Hash,
  i_MarkId,
  i_VectorDataItemSimple,
  i_Category,
  i_HtmlToHintTextConverter,
  i_MarkDbSmlInternal,
  u_BaseInterfacedObject;

type
  TMarkIdType = (midPoint, midLine, midPoly);

  TMarkId = class(TBaseInterfacedObject, IMarkId, IMarkSMLInternal, IVectorDataItemMainInfo, IVectorDataItemWithCategory)
  private
    FHash: THashValue;
    FName: string;
    FDesc: string;
    FId: Integer;
    FDbId: Integer;
    FCategory: ICategory;
    FCategoryId: Integer;
    FVisible: Boolean;
    FType: TMarkIdType;
    FHintConverter: IHtmlToHintTextConverter;
  protected
    function IsEqualInternal(const AMarkInternal: IMarkSMLInternal): Boolean;
    function GetStringID: string;
  protected
    function GetHash: THashValue;
    function GetName: string;
    function GetDesc: string;
    function GetHintText: string;
    function GetInfoHTML: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
    function GetMarkType: TGUID;
  protected
    function GetId: Integer;
    function GetDbId: integer;
    function GetCategory: ICategory;
    function GetCategoryId: Integer;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    function IsSameId(const AMarkId: IMarkId): Boolean;
    function IsSameMark(const AMark: IVectorDataItemSimple): Boolean;
    function IsEqual(const AValue: IVectorDataItemMainInfo): Boolean;
  public
    constructor Create(
      const AHintConverter: IHtmlToHintTextConverter;
      const AHash: THashValue;
      const AType: TMarkIdType;
      const AName: string;
      const ADesc: string;
      AId: Integer;
      ADbId: Integer;
      const ACategory: ICategory;
      AVisible: Boolean
    );
  end;

implementation

uses
  SysUtils,
  c_InternalBrowser,
  i_MarkCategoryFactoryDbInternal;

{ TMarkId }

constructor TMarkId.Create(
  const AHintConverter: IHtmlToHintTextConverter;
  const AHash: THashValue;
  const AType: TMarkIdType;
  const AName: string;
  const ADesc: string;
  AId: Integer;
  ADbId: Integer;
  const ACategory: ICategory;
  AVisible: Boolean
);
var
  VCategory: IMarkCategorySMLInternal;
begin
  Assert(AId >= 0);
  inherited Create;
  FHintConverter := AHintConverter;
  FHash := AHash;
  FType := AType;
  FName := AName;
  FDesc := ADesc;
  FId := AId;
  FDbId := ADbId;
  FCategory := ACategory;
  FCategoryId := CNotExistCategoryID;
  if FCategory <> nil then begin
    if Supports(FCategory, IMarkCategorySMLInternal, VCategory) then begin
      FCategoryId := VCategory.Id;
    end;
  end;
  FVisible := AVisible;
end;

function TMarkId.GetCategory: ICategory;
begin
  Result := FCategory;
end;

function TMarkId.GetCategoryId: Integer;
begin
  Result := FCategoryId;
end;

function TMarkId.GetDbId: integer;
begin
  Result := FDbId;
end;

function TMarkId.GetDesc: string;
begin
  Result := FDesc;
end;

function TMarkId.GetHash: THashValue;
begin
  Result := FHash;
end;

function TMarkId.GetHintText: string;
begin
  Result := FHintConverter.Convert(FName, FDesc);
end;

function TMarkId.GetId: Integer;
begin
  Result := FId;
end;

function TMarkId.GetInfoCaption: string;
begin
  Result := FName;
end;

function TMarkId.GetInfoHTML: string;
begin
  Result := '';
  if FDesc <> '' then begin
    Result := '<HTML><BODY>';
    Result := Result + FDesc;
    Result := Result + '</BODY></HTML>';
  end;
end;

function TMarkId.GetInfoUrl: string;
begin
  Result := GetStringID;
  if Result <> '' then begin
    Result := CMarksSystemInternalURL + Result + '/';
  end;
end;

function TMarkId.GetMarkType: TGUID;
begin
  case FType of
    midPoint: Result := IVectorDataItemPoint;
    midLine: Result := IVectorDataItemLine;
    midPoly: Result := IVectorDataItemPoly;
  end;
end;

function TMarkId.GetName: string;
begin
  Result := FName;
end;

function TMarkId.GetStringID: string;
begin
  Result := IntToStr(FId);
end;

function TMarkId.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TMarkId.IsEqualInternal(const AMarkInternal: IMarkSMLInternal): Boolean;
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

function TMarkId.IsSameId(const AMarkId: IMarkId): Boolean;
var
  VMarkInternal: IMarkSMLInternal;
begin
  Result := False;
  if AMarkId <> nil then begin
    if Supports(AMarkId, IMarkSMLInternal, VMarkInternal) then begin
      Result := (FId = VMarkInternal.Id) and (FDbId = VMarkInternal.DbId);
    end;
  end;
end;

function TMarkId.IsSameMark(const AMark: IVectorDataItemSimple): Boolean;
var
  VMarkInternal: IMarkSMLInternal;
begin
  if not Assigned(AMark) then begin
    Result := False;
    Exit;
  end;
  Result := False;
  if Supports(AMark.MainInfo, IMarkSMLInternal, VMarkInternal) then begin
    Result := (FId = VMarkInternal.Id) and (FDbId = VMarkInternal.DbId);
  end;
end;

function TMarkId.IsEqual(const AValue: IVectorDataItemMainInfo): Boolean;
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

procedure TMarkId.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
end;

end.
