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

unit i_MarkDbInternalORM;

interface

uses
  t_MarkSystemORM,
  i_VectorDataItemSimple;

type
  IMarkInternalORM = interface
    ['{4BA721D5-8AD5-43EF-98A8-75DD3A9A2B1B}']
    function GetId: TID;
    property Id: TID read GetId;

    function GetDbId: NativeInt;
    property DbId: NativeInt read GetDbId;

    function GetCategoryId: TID;
    property CategoryId: TID read GetCategoryId;

    function GetName: string;
    property Name: string read GetName;

    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  IMarkDbInternalORM = interface
    ['{A5916723-34B7-43AD-8268-63626FC182AD}']
    function GetById(const AId: TID): IVectorDataItem;
  end;

implementation

end.
