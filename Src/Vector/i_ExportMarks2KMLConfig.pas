{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2018, SAS.Planet development team.                      *}
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

unit i_ExportMarks2KMLConfig;

interface

uses
  i_ConfigDataElement;

type
  TKmlSortingType = (kstNone = 0, kstByNameASC, kstByNameDESC);

  IExportMarks2KMLConfigStatic = interface
    ['{19E357D9-248B-44FB-93F0-9CCDBD1748FF}']
    function GetUseCoordFormatting: Boolean;
    property UseCoordFormatting: Boolean read GetUseCoordFormatting;

    function GetCoordPrecision: Integer;
    property CoordPrecision: Integer read GetCoordPrecision;

    function GetSortingType: TKmlSortingType;
    property SortingType: TKmlSortingType read GetSortingType;

    function GetUseAbsPathToIcon: Boolean;
    property UseAbsPathToIcon: Boolean read GetUseAbsPathToIcon;

    function GetAbsPathToIcon: string;
    property AbsPathToIcon: string read GetAbsPathToIcon;
  end;

  IExportMarks2KMLConfig = interface(IConfigDataElement)
    ['{D9088CC7-6B52-46C0-AFC7-6F905BF5A96F}']
    function GetUseCoordFormatting: Boolean;
    procedure SetUseCoordFormatting(const AValue: Boolean);
    property UseCoordFormatting: Boolean read GetUseCoordFormatting write SetUseCoordFormatting;

    function GetCoordPrecision: Integer;
    procedure SetCoordPrecision(const AValue: Integer);
    property CoordPrecision: Integer read GetCoordPrecision write SetCoordPrecision;

    function GetSortingType: TKmlSortingType;
    procedure SetSortingType(const AValue: TKmlSortingType);
    property SortingType: TKmlSortingType read GetSortingType write SetSortingType;

    function GetUseAbsPathToIcon: Boolean;
    procedure SetUseAbsPathToIcon(const AValue: Boolean);
    property UseAbsPathToIcon: Boolean read GetUseAbsPathToIcon write SetUseAbsPathToIcon;

    function GetAbsPathToIcon: string;
    procedure SetAbsPathToIcon(const AValue: string);
    property AbsPathToIcon: string read GetAbsPathToIcon write SetAbsPathToIcon;

    function GetStatic: IExportMarks2KMLConfigStatic;
  end;

implementation

end.
