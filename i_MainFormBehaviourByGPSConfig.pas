{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
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
