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

unit i_MarkCategoryFactory;

interface

uses
  i_MarkCategory,
  i_MarkCategoryFactoryConfig;

type
  IMarkCategoryFactory = interface
    ['{A5AC81FF-F5FE-4085-8E9A-29318FDBBBA3}']
    function CreateNew(const AName: string): IMarkCategory;
    function Modify(
      const ASource: IMarkCategory;
      const AName: string;
      AVisible: Boolean;
      AAfterScale: integer;
      ABeforeScale: integer
    ): IMarkCategory;
    function ModifyVisible(
      const ASource: IMarkCategory;
      AVisible: Boolean
    ): IMarkCategory;

    function GetConfig: IMarkCategoryFactoryConfig;
    property Config: IMarkCategoryFactoryConfig read GetConfig;
  end;

implementation

end.
