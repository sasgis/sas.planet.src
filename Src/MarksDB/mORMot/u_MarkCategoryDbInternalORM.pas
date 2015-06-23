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

unit u_MarkCategoryDbInternalORM;

interface

uses
  t_MarkSystemORM,
  i_Category,
  i_MarkCategory,
  i_MarkCategoryInternalORM,
  u_BaseInterfacedObject;

type
  TMarkCategoryDbInternalORM = class(
    TBaseInterfacedObject,
    ICategory,
    IMarkCategory,
    IMarkCategoryInternalORM
  )
  private
    FId: TID;
    FDbId: Integer;
    FName: string;
    FVisible: Boolean;
    FAfterScale: Integer;
    FBeforeScale: Integer;
  private
    { IMarkCategoryInternalORM }
    function GetId: TID;
    function GetDbId: Integer;
  private
    { ICategory }
    function GetName: string;
    function IsSame(const ACategory: ICategory): Boolean;
    function IsEqual(const ACategory: ICategory): Boolean;
  private
    { IMarkCategory }
    function GetVisible: Boolean;
    function GetAfterScale: Integer;
    function GetBeforeScale: Integer;
  public
    constructor Create(
      const AId: TID;
      const ADbId: Integer;
      const AName: string;
      const AVisible: Boolean;
      const AAfterScale: Integer;
      const ABeforeScale: Integer
    );
  end;

implementation

uses
  SysUtils;

{ TMarkCategoryDbInternalORM }

constructor TMarkCategoryDbInternalORM.Create(
  const AId: TID;
  const ADbId: Integer;
  const AName: string;
  const AVisible: Boolean;
  const AAfterScale: Integer;
  const ABeforeScale: Integer
);
begin
  Assert(AId > 0);
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

function TMarkCategoryDbInternalORM.GetAfterScale: Integer;
begin
  Result := FAfterScale;
end;

function TMarkCategoryDbInternalORM.GetBeforeScale: Integer;
begin
  Result := FBeforeScale;
end;

function TMarkCategoryDbInternalORM.GetDbId: Integer;
begin
  Result := FDbId;
end;

function TMarkCategoryDbInternalORM.GetId: TID;
begin
  Result := FId;
end;

function TMarkCategoryDbInternalORM.GetName: string;
begin
  Result := FName;
end;

function TMarkCategoryDbInternalORM.GetVisible: Boolean;
begin
  Result := FVisible;
end;

function TMarkCategoryDbInternalORM.IsEqual(const ACategory: ICategory): Boolean;
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

function TMarkCategoryDbInternalORM.IsSame(const ACategory: ICategory): Boolean;
var
  VCategoryInternal: IMarkCategoryInternalORM;
begin
  Result := False;
  if ACategory <> nil then begin
    if Supports(ACategory, IMarkCategoryInternalORM, VCategoryInternal) then begin
      Result := (FId = VCategoryInternal.Id);
    end;
  end;
end;

end.
