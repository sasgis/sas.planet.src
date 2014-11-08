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

unit u_MarkCategoryFactorySmlDbInternal;

interface

uses
  i_MarkCategory,
  i_MarkCategoryFactoryDbInternal,
  u_BaseInterfacedObject;

type
  TMarkCategoryFactorySmlDbInternal = class(TBaseInterfacedObject, IMarkCategoryFactoryDbInternal)
  private
    FDbId: Integer;
  private
    function CreateCategory(
      AId: Integer;
      const AName: string;
      AVisible: Boolean;
      AAfterScale: integer;
      ABeforeScale: integer
    ): IMarkCategory;
  public
    constructor Create(const ADbId: Integer);
  end;

implementation

uses
  u_MarkCategorySmlDbInternal;

{ TMarkCategoryFactorySmlDbInternal }

constructor TMarkCategoryFactorySmlDbInternal.Create(const ADbId: Integer);
begin
  inherited Create;
  FDbId := ADbId;
end;

function TMarkCategoryFactorySmlDbInternal.CreateCategory(
  AId: Integer;
  const AName: string;
  AVisible: Boolean;
  AAfterScale, ABeforeScale: integer
): IMarkCategory;
begin
  Result :=
    TMarkCategorySmlDbInternal.Create(
      AId,
      FDbId,
      AName,
      AVisible,
      AAfterScale,
      ABeforeScale
    );
end;

end.
