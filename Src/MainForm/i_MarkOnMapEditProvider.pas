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

unit i_MarkOnMapEditProvider;

interface

uses
  i_VectorDataItemSimple;

type
  IMarkOnMapEditProvider = interface
    ['{907AB238-07D5-44D8-9332-CE9A8477EF99}']
    procedure ProcessOnMapEdit(const AItem: IVectorDataItem);
  end;

  IMarkOnMapEditProviderInternal = interface(IMarkOnMapEditProvider)
    ['{D81A6EA4-E7EC-45B8-84BE-65985F68E473}']
    procedure SetEnabled(const AValue: Boolean);
  end;

implementation

end.
