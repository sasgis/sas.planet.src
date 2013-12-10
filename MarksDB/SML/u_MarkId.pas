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

unit u_MarkId;

interface

uses
  i_MarkId,
  i_VectorDataItemSimple,
  i_Category,
  i_MarkDbSmlInternal,
  u_BaseInterfacedObject;

type
  TMarkId = class(TBaseInterfacedObject, IMarkId, IMarkSMLInternal)
  private
    FName: string;
    FId: Integer;
    FDbId: Integer;
    FCategory: ICategory;
    FCategoryId: Integer;
    FVisible: Boolean;
  protected
    function IsEqualInternal(const AMarkInternal: IMarkSMLInternal): Boolean;
    function GetStringID: string;
  protected
    function GetName: string;
    function GetMarkType: TGUID; virtual; abstract;
  protected
    function GetId: Integer;
    function GetDbId: integer;
    function GetCategory: ICategory;
    function GetCategoryId: Integer;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    function IsSameId(const AMarkId: IMarkId): Boolean;
    function IsSameMark(const AMark: IVectorDataItemSimple): Boolean;
  public
    constructor Create(
      const AName: string;
      AId: Integer;
      ADbId: Integer;
      const ACategory: ICategory;
      AVisible: Boolean
    );
  end;

implementation

uses
  SysUtils,
  i_MarkCategoryFactoryDbInternal;

{ TMarkId }

constructor TMarkId.Create(
  const AName: string;
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
  FName := AName;
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

function TMarkId.GetId: Integer;
begin
  Result := FId;
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
  Result := False;
  if AMark <> nil then begin
    if Supports(AMark, IMarkSMLInternal, VMarkInternal) then begin
      Result := (FId = VMarkInternal.Id) and (FDbId = VMarkInternal.DbId);
    end;
  end;
end;

procedure TMarkId.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
end;

end.
