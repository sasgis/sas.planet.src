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

unit i_MapVersionFactory;

interface

uses
  i_Changeable,
  i_MapVersionInfo;

type
  IMapVersionFactory = interface
    ['{4E03F54E-C11D-443C-BF0E-D9A2B0D1299C}']
    function CreateByStoreString(const AValue: string): IMapVersionInfo;
    function CreateByMapVersion(const AValue: IMapVersionInfo): IMapVersionInfo;

    function IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
  end;

  IMapVersionFactoryChangeable = interface(IChangeable)
    ['{098EFB27-7733-41EC-88E5-9101B9729FB2}']
    function GetStatic: IMapVersionFactory;
  end;

  IMapVersionFactoryChangeableInternal = interface(IMapVersionFactoryChangeable)
    procedure SetFactory(const AValue: IMapVersionFactory);
  end;

implementation

end.
