{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  i_MarksSimple,
  i_MarkCategory,
  i_MarksDbSmlInternal;

type
  TMarkId = class(TInterfacedObject, IMarkID, IMarkSMLInternal)
  private
    FDbCode: Integer;
    FName: string;
    FId: Integer;
    FCategory: ICategory;
    FCategoryId: Integer;
    FVisible: Boolean;
  protected
    function GetName: string;
  protected
    function GetDbCode: Integer;
    function GetId: Integer;
    function GetCategory: ICategory;
    function GetCategoryId: Integer;
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    function IsSameId(const AMarkId: IMarkID): Boolean;
  public
    constructor Create(
      ADbCode: Integer;
      const AName: string;
      AId: Integer;
      const ACategory: ICategory;
      AVisible: Boolean
    );
  end;

implementation

uses
  SysUtils;

{ TMarkId }

constructor TMarkId.Create(
  ADbCode: Integer;
  const AName: string;
  AId: Integer;
  const ACategory: ICategory;
  AVisible: Boolean
);
var
  VCategory: IMarkCategorySMLInternal;
begin
  FDbCode := ADbCode;
  FName := AName;
  FId := AId;
  FCategory := ACategory;
  FCategoryId := -1;
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

function TMarkId.GetDbCode: Integer;
begin
  Result := FDbCode;
end;

function TMarkId.GetId: Integer;
begin
  Result := FId;
end;

function TMarkId.GetName: string;
begin
  Result := FName;
end;

function TMarkId.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TMarkId.IsSameId(const AMarkId: IMarkID): Boolean;
var
  VMarkInternal: IMarkSMLInternal;
begin
  Result := False;
  if AMarkId <> nil then begin
    if Supports(AMarkId, IMarkSMLInternal, VMarkInternal) then begin
      Result := FId = VMarkInternal.Id;
    end;
  end;
end;

procedure TMarkId.SetVisible(AValue: Boolean);
begin
  FVisible := AValue;
end;

end.
