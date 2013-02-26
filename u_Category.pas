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

unit u_Category;

interface

uses
  i_Category,
  u_BaseInterfacedObject;

type
  TCategory = class(TBaseInterfacedObject, ICategory)
  private
    FName: string;
  private
    function GetName: string;
    function IsSame(const ACategory: ICategory): Boolean;
    function IsEqual(const ACategory: ICategory): Boolean;
  public
    constructor Create(
      const AName: string
    );
  end;

implementation

{ TMarkCategory }

constructor TCategory.Create(
  const AName: string
);
begin
  inherited Create;
  FName := AName;
end;

function TCategory.GetName: string;
begin
  Result := FName;
end;

function TCategory.IsEqual(const ACategory: ICategory): Boolean;
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
  Result := True;
end;

function TCategory.IsSame(const ACategory: ICategory): Boolean;
begin
  Result := ACategory = ICategory(Self);
end;

end.
