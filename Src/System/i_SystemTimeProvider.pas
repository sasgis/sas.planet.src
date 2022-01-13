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

unit i_SystemTimeProvider;

interface

uses
  i_Notifier;

type
  ISystemTimeProvider = interface
    ['{75FBE2A6-9385-4EBE-AF6E-5E4F44C340D6}']
    function GetLocalTime: TDateTime;
    function GetUTCTime: TDateTime;
    function UTCToLocalTime(const ASysTime: TDateTime): TDateTime;
    function LocalTimeToUTC(const ASysTime: TDateTime): TDateTime;

    function GetSystemTimeChangedNotifier: INotifier;
    property SystemTimeChangedNotifier: INotifier read GetSystemTimeChangedNotifier;
  end;

  ISystemTimeProviderInternal = interface(ISystemTimeProvider)
    procedure SystemTimeChanged;
  end;

implementation

end.
