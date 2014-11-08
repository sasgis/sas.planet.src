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

unit i_DictionaryObjects;

interface

type
  IDictionaryGuidToInterfaceStatic = interface
    ['{190F57D6-5201-4540-8019-E4FD9B3F7501}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetKey(AIndex: Integer): TGUID;
    property Keys[AIndex: Integer]: TGUID read GetKey;

    function GetValue(AIndex: Integer): IInterface;
    property Values[AIndex: Integer]: IInterface read GetValue;

    function FindIndex(const AKey: TGUID): Integer;
  end;

  IDictionaryGuidToObjectStatic = interface
    ['{559992E4-E89B-4942-80A1-59F1822631AB}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetKey(AIndex: Integer): TGUID;
    property Keys[AIndex: Integer]: TGUID read GetKey;

    function GetValue(AIndex: Integer): TObject;
    property Values[AIndex: Integer]: TObject read GetValue;

    function FindIndex(const AKey: TGUID): Integer;
  end;

implementation

end.
