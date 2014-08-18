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

unit i_Dictionary;

interface

type
  IDictionaryStringToIntegerStatic = interface
    ['{6134CA9D-48BE-43ED-AF87-644F31EE0302}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetKey(AIndex: Integer): String;
    property Keys[AIndex: Integer]: String read GetKey;

    function GetValue(AIndex: Integer): Integer;
    property Values[AIndex: Integer]: Integer read GetValue;

    function FindIndex(const AKey: string): Integer;
  end;

  IDictionaryIntegerToIntegerStatic = interface
    ['{785B719D-C22C-4F8C-A62E-79F93AB42F4E}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetKey(AIndex: Integer): Integer;
    property Keys[AIndex: Integer]: Integer read GetKey;

    function GetValue(AIndex: Integer): Integer;
    property Values[AIndex: Integer]: Integer read GetValue;

    function FindIndex(const AKey: Integer): Integer;
  end;

implementation

end.
