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

unit i_LanguageManager;

interface

uses
  i_ConfigDataElement,
  i_LanguageListStatic;

type
  ILanguageManager = interface(IConfigDataElement)
    ['{F8D76CED-2681-4DD4-AB24-4C6ECE89CE4D}']
    function GetCurrentLanguageCode: string;
    procedure SetCurrentLanguageCode(const ACode: string);
    property CurrentLanguageCode: string read GetCurrentLanguageCode write SetCurrentLanguageCode;

    function GetCurrentLanguageIndex: Integer;
    procedure SetCurrentLanguageIndex(const AValue: Integer);
    property CurrentLanguageIndex: Integer read GetCurrentLanguageIndex write SetCurrentLanguageIndex;

    function GetLanguageList: ILanguageListStatic;
    property LanguageList: ILanguageListStatic read GetLanguageList;

    function GetLangNameByIndex(const AIndex: Integer): string;
  end;

implementation

end.
