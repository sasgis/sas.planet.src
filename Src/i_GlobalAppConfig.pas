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

unit i_GlobalAppConfig;

interface

uses
  i_ConfigDataElement;

type
  IGlobalAppConfig = interface(IConfigDataElement)
    ['{3DBA929F-BD4C-46A3-A64B-F61786D41FED}']
    // Показывать иконку в трее
    function GetIsShowIconInTray: Boolean;
    procedure SetIsShowIconInTray(AValue: Boolean);
    property IsShowIconInTray: Boolean read GetIsShowIconInTray write SetIsShowIconInTray;

    // Заходить на сайт автора при старте программы
    function GetIsSendStatistic: Boolean;
    procedure SetIsSendStatistic(AValue: Boolean);
    property IsSendStatistic: Boolean read GetIsSendStatistic write SetIsSendStatistic;
  end;

implementation

end.
