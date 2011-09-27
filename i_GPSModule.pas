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

unit i_GPSModule;

interface

uses
  i_JclNotify,
  i_GPS;

type
  IGPSModule = interface
    ['{B477FFBD-C36D-40C6-AF7F-B118E47A6815}']
    function GetPosition: IGPSPosition; safecall;
    property Position: IGPSPosition read GetPosition;

    function GetDataReciveNotifier: IJclNotifier; safecall;
    property DataReciveNotifier: IJclNotifier read GetDataReciveNotifier;

    function GetConnectingNotifier: IJclNotifier; safecall;
    property ConnectingNotifier: IJclNotifier read GetConnectingNotifier;

    function GetConnectedNotifier: IJclNotifier; safecall;
    property ConnectedNotifier: IJclNotifier read GetConnectedNotifier;

    function GetDisconnectingNotifier: IJclNotifier; safecall;
    property DisconnectingNotifier: IJclNotifier read GetDisconnectingNotifier;

    function GetDisconnectedNotifier: IJclNotifier; safecall;
    property DisconnectedNotifier: IJclNotifier read GetDisconnectedNotifier;

    function GetTimeOutNotifier: IJclNotifier; safecall;
    property TimeOutNotifier: IJclNotifier read GetTimeOutNotifier;

    function GetConnectErrorNotifier: IJclNotifier; safecall;
    property ConnectErrorNotifier: IJclNotifier read GetConnectErrorNotifier;
  end;

implementation

end.
