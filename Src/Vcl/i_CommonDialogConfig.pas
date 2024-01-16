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

unit i_CommonDialogConfig;

interface

uses
  i_ConfigDataElement;

type
  ICommonDialogConfig = interface(IConfigDataElement)
    ['{1A73B6FA-097D-48BE-86A4-AF1AB10E5253}']
    function GetInitialDir: string;
    procedure SetInitialDir(const AValue: string);
    property InitialDir: string read GetInitialDir write SetInitialDir;

    function GetFilterIndex: Integer;
    procedure SetFilterIndex(const AValue: Integer);
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex;
  end;

implementation

end.
