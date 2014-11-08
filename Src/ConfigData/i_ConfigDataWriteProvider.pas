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

unit i_ConfigDataWriteProvider;

interface

uses
  i_BinaryData,
  i_ConfigDataProvider;

type
  IConfigDataWriteProvider = interface(IConfigDataProvider)
    ['{2AA51ACD-3056-4328-BDF9-D458E50F5734}']
    function GetOrCreateSubItem(const AIdent: string): IConfigDataWriteProvider;
    procedure DeleteSubItem(const AIdent: string);
    procedure DeleteValue(const AIdent: string);
    procedure DeleteValues;
    procedure WriteBinary(
      const AIdent: string;
      const AValue: IBinaryData
    );
    procedure WriteString(
      const AIdent: string;
      const AValue: string
    );
    procedure WriteAnsiString(
      const AIdent: string;
      const AValue: AnsiString
    );
    procedure WriteInteger(
      const AIdent: string;
      const AValue: Longint
    );
    procedure WriteBool(
      const AIdent: string;
      const AValue: Boolean
    );
    procedure WriteDate(
      const AIdent: string;
      const AValue: TDateTime
    );
    procedure WriteDateTime(
      const AIdent: string;
      const AValue: TDateTime
    );
    procedure WriteFloat(
      const AIdent: string;
      const AValue: Double
    );
    procedure WriteTime(
      const AIdent: string;
      const AValue: TDateTime
    );
  end;

implementation

end.
