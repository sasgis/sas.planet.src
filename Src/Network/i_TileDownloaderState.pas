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

unit i_TileDownloaderState;

interface

uses
  i_Changeable;

type
  ITileDownloaderStateStatic = interface
    ['{A0A51F5A-7F35-48A2-870D-203739666B64}']
    function GetEnabled: Boolean;
    property Enabled: Boolean read GetEnabled;

    function GetDisableReason: string;
    property DisableReason: string read GetDisableReason;
  end;

  ITileDownloaderStateChangeble = interface(IChangeable)
    ['{48AAA474-723C-443E-940E-2D4332ED41F7}']
    function GetStatic: ITileDownloaderStateStatic;
  end;

implementation

end.
