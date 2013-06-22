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

unit u_MarkCategorySmlDbInternal;

interface

uses
  i_Category,
  i_MarkCategory,
  i_MarkDbSmlInternal,
  u_BaseInterfacedObject;

type
  TMarkCategorySmlDbInternal = class(TBaseInterfacedObject, ICategory, IMarkCategory, IMarkCategorySMLInternal)
  private
    FId: Integer;
    FDbId: Integer;
    FName: string;
    FVisible: Boolean;
    FAfterScale: integer;
    FBeforeScale: integer;
  private
    function GetId: integer;
    function GetDbId: integer;
  private
    function GetName: string;
    function IsSame(const ACategory: ICategory): Boolean;
    function IsEqual(const ACategory: ICategory): Boolean;
  private
    function GetVisible: boolean;
    function GetAfterScale: integer;
    function GetBeforeScale: integer;
  public
    constructor Create(
      AId: Integer;
      ADbId: Integer;
      const AName: string;
      AVisible: Boolean;
      AAfterScale: integer;
      ABeforeScale: integer
    );
  end;

implementation

uses
  SysUtils;

{ TMarkCategorySmlDbInternal }

constructor TMarkCategorySmlDbInternal.Create(
  AId: Integer;
  ADbId: Integer;
  const AName: string;
  AVisible: Boolean;
  AAfterScale, ABeforeScale: Integer
);
begin
  Assert(AId >= 0);
  Assert(ADbId <> 0);
  Assert(AName <> '');
  Assert(AAfterScale >= 0);
  Assert(ABeforeScale >= 0);
  inherited Create;
  FId := AId;
  FDbId := ADbId;
  FName := AName;
  FVisible := AVisible;
  FAfterScale := AAfterScale;
  FBeforeScale := ABeforeScale;
end;

function TMarkCategorySmlDbInternal.GetAfterScale: integer;
begin
  Result := FAfterScale;
end;

function TMarkCategorySmlDbInternal.GetBeforeScale: integer;
begin
  Result := FBeforeScale;
end;

function TMarkCategorySmlDbInternal.GetDbId: integer;
begin
  Result := FDbId;
end;

function TMarkCategorySmlDbInternal.GetId: integer;
begin
  Result := FId;
end;

function TMarkCategorySmlDbInternal.GetName: string;
begin
  Result := FName;
end;

function TMarkCategorySmlDbInternal.GetVisible: boolean;
begin
  Result := FVisible;
end;

function TMarkCategorySmlDbInternal.IsEqual(const ACategory: ICategory): Boolean;
var
  VCategory: IMarkCategory;
begin
  if ACategory = nil then begin
    Result := False;
    Exit;
  end;
  if ACategory = ICategory(Self) then begin
    Result := True;
    Exit;
  end;
  if ACategory.Name <> FName then begin
    Result := False;
    Exit;
  end;
  if Supports(ACategory, IMarkCategory, VCategory) then begin
    Result :=
      (VCategory.Visible = FVisible) and
      (VCategory.AfterScale = FAfterScale) and
      (VCategory.BeforeScale = FBeforeScale);
  end else begin
    Result := False;
  end;
end;

function TMarkCategorySmlDbInternal.IsSame(const ACategory: ICategory): Boolean;
var
  VCategoryInternal: IMarkCategorySMLInternal;
begin
  Result := False;
  if ACategory <> nil then begin
    if Supports(ACategory, IMarkCategorySMLInternal, VCategoryInternal) then begin
      Result := FId = VCategoryInternal.Id;
    end;
  end;
end;

end.
