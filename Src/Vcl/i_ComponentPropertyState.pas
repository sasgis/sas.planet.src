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

unit i_ComponentPropertyState;

interface

uses
  Types,
  t_ComponentProperty;

type
  IComponentPropertyState = interface
    ['{0CA728CC-A021-418E-97F7-ECE8F72F80CE}']
    procedure Save;
    procedure Restore;

    procedure Include(const AComponentName: string; const AProperties: TStringDynArray);
    procedure Exclude(const AComponentName: string; const AProperties: TStringDynArray);
    procedure ExcludeAll(const AComponentName: string);

    function GetOptions: TComponentPropertyStateOptions;
    property Options: TComponentPropertyStateOptions read GetOptions;
  end;

implementation

end.
