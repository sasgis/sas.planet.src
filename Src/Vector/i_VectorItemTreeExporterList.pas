{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit i_VectorItemTreeExporterList;

interface

uses
  i_Changeable,
  i_ExportConfig,
  i_VectorItemTreeExporter;

type
  TExporterListItemOption = (
    elioSaveToFile,
    elioSaveToDir,
    elioAllowSeparateFiles
  );

  TExporterListItemOptions = set of TExporterListItemOption;

  IVectorItemTreeExporterListItem = interface
    ['{96787244-28A5-4D01-A00B-5F7057132BCB}']
    function GetExporter: IVectorItemTreeExporter;
    property Exporter: IVectorItemTreeExporter read GetExporter;

    function GetConfig: IExportConfig;
    property Config: IExportConfig read GetConfig;

    function GetOptions: TExporterListItemOptions;
    property Options: TExporterListItemOptions read GetOptions;

    function GetDefaultExt: string;
    property DefaultExt: string read GetDefaultExt;

    function GetName: string;
    property Name: string read GetName;
  end;

  IVectorItemTreeExporterListStatic = interface
    ['{4D9CF72E-DCD2-48AD-890D-9B9627CF457F}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetItem(const AIndex: Integer): IVectorItemTreeExporterListItem;
    property Items[const AIndex: Integer]: IVectorItemTreeExporterListItem read GetItem;
  end;

  IVectorItemTreeExporterListChangeable = interface(IChangeable)
    ['{6850E0F8-197F-452F-B46D-239DEA3B452C}']
    function GetStatic: IVectorItemTreeExporterListStatic;
  end;

implementation

end.
