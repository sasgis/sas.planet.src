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

unit i_RegionProcessConfig;

interface

uses
  i_StringListStatic,
  i_ConfigDataElement;

type
  IRegionProcessConfig = interface(IConfigDataElement)
    ['{11C683A8-5093-48B5-859F-CE4D1D108E2A}']
    function GetCopyBboxTemplates: IStringListStatic;
    procedure SetCopyBboxTemplates(const AValue: IStringListStatic);
    property CopyBboxTemplates: IStringListStatic read GetCopyBboxTemplates write SetCopyBboxTemplates;

    function GetCopyBboxTemplateActiveIndex: Integer;
    procedure SetCopyBboxTemplateActiveIndex(const AValue: Integer);
    property CopyBboxTemplateActiveIndex: Integer read GetCopyBboxTemplateActiveIndex write SetCopyBboxTemplateActiveIndex;
  end;

implementation

end.
