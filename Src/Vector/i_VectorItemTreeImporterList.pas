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

unit i_VectorItemTreeImporterList;

interface

uses
  Types,
  i_Changeable,
  i_VectorItemTreeImporter;

type
  IVectorItemTreeImporterListItem = interface
    ['{96787244-28A5-4D01-A00B-5F7057132BCB}']
    function GetImporter: IVectorItemTreeImporter;
    property Importer: IVectorItemTreeImporter read GetImporter;

    function GetSupportedExt: TStringDynArray;
    property SupportedExt: TStringDynArray read GetSupportedExt;

    function GetName: string;
    property Name: string read GetName;
  end;

  IVectorItemTreeImporterListStatic = interface
    ['{4D9CF72E-DCD2-48AD-890D-9B9627CF457F}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(const AIndex: Integer): IVectorItemTreeImporterListItem;
    property Items[const AIndex: Integer]: IVectorItemTreeImporterListItem read GetItem;

    function GetImporterByExt(const AExt: string): IVectorItemTreeImporter;
  end;

  IVectorItemTreeImporterListChangeable = interface(IChangeable)
    ['{6850E0F8-197F-452F-B46D-239DEA3B452C}']
    function GetStatic: IVectorItemTreeImporterListStatic;
  end;

implementation

end.
