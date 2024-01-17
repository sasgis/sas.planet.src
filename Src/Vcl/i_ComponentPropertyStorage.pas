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

unit i_ComponentPropertyStorage;

interface

uses
  Types,
  Classes,
  t_ComponentProperty;

type
  TComponentPropertyStorageCache = record
    FNamePath: string;
    FIgnoreNamePath: TStringDynArray;
    FTemporaryNamePath: TStringDynArray;
  end;

  ICustomPropertiesFilter = interface
    ['{FB2AEA04-83FC-4038-A290-ADEB9F986D46}']
    procedure Process(
      const AComponent: TComponent;
      var AProperties: TStringDynArray
    );
  end;

  IComponentPropertyStorage = interface
    ['{D161FB94-2629-4259-8DF5-DF55431F6C5C}']
    procedure Save(
      const AComponent: TComponent;
      const AIgnore: TComponentDynArray;
      const ATemporary: TComponentDynArray;
      var ACache: TComponentPropertyStorageCache;
      const AFilter: ICustomPropertiesFilter = nil
    );
    procedure Restore(
      const AComponent: TComponent;
      const AIgnore: TComponentDynArray;
      const ATemporary: TComponentDynArray;
      var ACache: TComponentPropertyStorageCache;
      const AFilter: ICustomPropertiesFilter = nil
    );
  end;

implementation

end.
