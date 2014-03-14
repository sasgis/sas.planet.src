{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit i_MainWindowPosition;

interface

uses
  Types,
  i_ConfigDataElement;

type
  IMainWindowPosition = interface(IConfigDataElement)
    ['{BD5C5719-02CB-4364-A670-B1DD75A5BAEE}']
    function GetIsFullScreen: Boolean;
    property IsFullScreen: Boolean read GetIsFullScreen;
    procedure SetFullScreen;
    procedure SetNoFullScreen;

    function GetIsMaximized: Boolean;
    property IsMaximized: Boolean read GetIsMaximized;
    procedure SetMaximized;
    procedure SetNormalWindow;

    function GetIsMinimized: Boolean;
    procedure SetMinimized;
    procedure SetNotMinimized;
    property IsMinimized: Boolean read GetIsMinimized;

    function GetBoundsRect: TRect;
    property BoundsRect: TRect read GetBoundsRect;
    procedure SetWindowPosition(const ARect: TRect);
  end;

implementation

end.
