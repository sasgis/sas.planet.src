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

unit i_MapTypeGUIConfig;

interface

uses
  Classes,
  i_Bitmap32Static,
  i_StringConfigDataElement,
  i_ConfigDataElement;

type
  IMapTypeGUIConfigStatic = interface
    ['{CDB1236F-59D5-4B84-AF3D-764C1595E0AD}']
    function GetName: string;
    property Name: string read GetName;

    function GetSortIndex: Integer;
    property SortIndex: Integer read GetSortIndex;

    function GetHotKey: TShortCut;
    property HotKey: TShortCut read GetHotKey;

    function GetSeparator: Boolean;
    property Separator: Boolean read GetSeparator;

    function GetParentSubMenu: string;
    property ParentSubMenu: string read GetParentSubMenu;

    function GetEnabled: Boolean;
    property Enabled: Boolean read GetEnabled;

    function GetInfoUrl: string;
    property InfoUrl: string read GetInfoUrl;

    function GetBmp18: IBitmap32Static;
    property Bmp18: IBitmap32Static read GetBmp18;

    function GetBmp24: IBitmap32Static;
    property Bmp24: IBitmap32Static read GetBmp24;
  end;

  IMapTypeGUIConfig = interface(IConfigDataElement)
    ['{E597028D-5B2B-4771-A68D-1F9BD4111EC1}']
    function GetName: IStringConfigDataElement;
    property Name: IStringConfigDataElement read GetName;

    function GetSortIndex: Integer;
    procedure SetSortIndex(const AValue: Integer);
    property SortIndex: Integer read GetSortIndex write SetSortIndex;

    function GetHotKey: TShortCut;
    procedure SetHotKey(const AValue: TShortCut);
    property HotKey: TShortCut read GetHotKey write SetHotKey;

    function GetSeparator: Boolean;
    procedure SetSeparator(const AValue: Boolean);
    property Separator: Boolean read GetSeparator write SetSeparator;

    function GetParentSubMenu: IStringConfigDataElement;
    property ParentSubMenu: IStringConfigDataElement read GetParentSubMenu;

    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;

    function GetInfoUrl: IStringConfigDataElement;
    property InfoUrl: IStringConfigDataElement read GetInfoUrl;

    function GetBmp18: IBitmap32Static;
    property Bmp18: IBitmap32Static read GetBmp18;

    function GetBmp24: IBitmap32Static;
    property Bmp24: IBitmap32Static read GetBmp24;

    function GetStatic: IMapTypeGUIConfigStatic;
  end;

implementation

end.
