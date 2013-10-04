{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit i_LanguageListStatic;

interface

type
  ILanguageListStatic = interface
    ['{EE282D06-5C24-4A10-A3DD-610A2ADEB515}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetCode(const AIndex: Integer): string;
    property Code[const AIndex: Integer]: string read GetCode;

    function FindCode(
      const ACode: string;
      out AIndex: Integer
    ): Boolean;
  end;

implementation

end.
