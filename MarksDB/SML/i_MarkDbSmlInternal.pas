{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit i_MarkDbSmlInternal;

interface

uses
  i_VectorDataItemSimple;

type
  IMarkSMLInternal = interface
    ['{2611AAA5-10DA-472B-B3EE-31EA27EDD6CD}']
    function GetId: Integer;
    property Id: Integer read GetId;

    function GetDbId: integer;
    property DbId: integer read GetDbId;

    function GetCategoryId: Integer;
    property CategoryId: Integer read GetCategoryId;

    function GetName: string;
    property Name: string read GetName;

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  IMarkCategorySMLInternal = interface
    ['{08E68E71-FD75-4E7F-953F-485F034525AA}']
    function GetId: integer;
    property Id: integer read GetId;

    function GetDbId: integer;
    property DbId: integer read GetDbId;
  end;

  IMarkDbSmlInternal = interface
    ['{54D17191-A56C-4951-8838-7E492906213A}']
    function GetById(AId: Integer): IVectorDataItemSimple;
  end;

implementation

end.
