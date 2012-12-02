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

unit u_MarkCategory;

interface

uses
  i_MarkCategory,
  i_MarksDbSmlInternal,
  u_BaseInterfacedObject;

type
  TMarkCategory = class(TBaseInterfacedObject, ICategory, IMarkCategory, IMarkCategorySMLInternal)
  private
    FId: Integer;
    FName: string;
    FVisible: Boolean;
    FAfterScale: integer;
    FBeforeScale: integer;
  private
    function GetId: integer; stdcall;
  private
    function GetName: string; stdcall;
    function IsSame(const ACategory: ICategory): Boolean;
    function IsEqual(const ACategory: ICategory): Boolean;
  private
    function GetVisible: boolean; stdcall;
    function GetAfterScale: integer; stdcall;
    function GetBeforeScale: integer; stdcall;
  public
    constructor Create(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      AAfterScale: integer;
      ABeforeScale: integer
    );
  end;

implementation

uses
  SysUtils;

{ TMarkCategory }

constructor TMarkCategory.Create(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  AAfterScale, ABeforeScale: integer
);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FVisible := AVisible;
  FAfterScale := AAfterScale;
  FBeforeScale := ABeforeScale;
end;

function TMarkCategory.GetAfterScale: integer;
begin
  Result := FAfterScale;
end;

function TMarkCategory.GetBeforeScale: integer;
begin
  Result := FBeforeScale;
end;

function TMarkCategory.GetId: integer;
begin
  Result := FId;
end;

function TMarkCategory.GetName: string;
begin
  Result := FName;
end;

function TMarkCategory.GetVisible: boolean;
begin
  Result := FVisible;
end;

function TMarkCategory.IsEqual(const ACategory: ICategory): Boolean;
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

function TMarkCategory.IsSame(const ACategory: ICategory): Boolean;
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
