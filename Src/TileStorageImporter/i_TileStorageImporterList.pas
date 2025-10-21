{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit i_TileStorageImporterList;

interface

uses
  Types,
  i_Changeable,
  i_TileStorageImporter;

type
  ITileStorageImporterListItem = interface
    ['{D70D6118-32B7-483E-9ACE-E59C2FA7A6BE}']
    function GetImporter: ITileStorageImporter;
    property Importer: ITileStorageImporter read GetImporter;

    function GetSupportedExt: TStringDynArray;
    property SupportedExt: TStringDynArray read GetSupportedExt;

    function GetName: string;
    property Name: string read GetName;
  end;

  ITileStorageImporterListStatic = interface
    ['{9937C55A-7FD5-4712-8955-1798F67F6A23}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(const AIndex: Integer): ITileStorageImporterListItem;
    property Items[const AIndex: Integer]: ITileStorageImporterListItem read GetItem;

    function GetImporterByExt(const AExt: string): ITileStorageImporter;
  end;

  ITileStorageImporterListChangeable = interface(IChangeable)
    ['{E2C40B8E-3CA9-4F72-9FCB-F32CB2C059E3}']
    function GetStatic: ITileStorageImporterListStatic;
  end;

implementation

end.
