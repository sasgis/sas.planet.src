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

unit i_MainFormBehaviourByGPSConfig;

interface

uses
  i_ConfigDataElement;

type
  IMainFormBehaviourByGPSConfig = interface(IConfigDataElement)
    ['{2236C930-B6F6-4A6D-A8EB-FC83999973EA}']
    // Двигать карту при изменении gps-координат
    function GetMapMove: Boolean;
    procedure SetMapMove(AValue: Boolean);
    property MapMove: Boolean read GetMapMove write SetMapMove;

    // Центрировать карту на GPS позиции
    function GetMapMoveCentered: Boolean;
    procedure SetMapMoveCentered(AValue: Boolean);
    property MapMoveCentered: Boolean read GetMapMoveCentered write SetMapMoveCentered;

    // Расстояние, на которое должено измениться положение GPS что бы вызвать сдиг карты
    function GetMinMoveDelta: Double;
    procedure SetMinMoveDelta(AValue: Double);
    property MinMoveDelta: Double read GetMinMoveDelta write SetMinMoveDelta;

    // Скрывать/показывать панель датчиков при подключении/отключении GPS
    function GetSensorsAutoShow: Boolean;
    procedure SetSensorsAutoShow(AValue: Boolean);
    property SensorsAutoShow: Boolean read GetSensorsAutoShow write SetSensorsAutoShow;

    // Двигать карту и перерисовывать трек только если главное окно активно
    function GetProcessGPSIfActive: Boolean;
    procedure SetProcessGPSIfActive(AValue: Boolean);
    property ProcessGPSIfActive: Boolean read GetProcessGPSIfActive write SetProcessGPSIfActive;
  end;

implementation

end.
