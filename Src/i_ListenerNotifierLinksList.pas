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

unit i_ListenerNotifierLinksList;

interface

uses
  i_Listener,
  i_Notifier,
  i_ListenerTime,
  i_NotifierTime;

type
  IListenerNotifierLinksList = interface
    ['{B197E296-150C-4961-9370-1BF73F0B8BB6}']
    procedure Add(
      const AListener: IListener;
      const ANotifier: INotifier
    ); overload;
    procedure Add(
      const AListener: IListenerTime;
      const ANotifier: INotifierTime
    ); overload;
    procedure ActivateLinks;
    procedure DeactivateLinks;
  end;

implementation

end.
