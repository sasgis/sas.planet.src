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

unit i_GPSModuleByCOMPortConfig;

interface

uses
  i_GPSModuleByCOMPortSettings,
  i_ConfigDataElement;

type
  IGPSModuleByCOMPortConfig = interface(IConfigDataElement)
    ['{75AC2DE2-4C88-4A0C-A1D1-D99E51995C78}']
    function GetPort: Integer;
    procedure SetPort(const AValue: Integer);
    property Port: Integer read GetPort write SetPort;

    function GetBaudRate: Integer;
    procedure SetBaudRate(const AValue: Integer);
    property BaudRate: Integer read GetBaudRate write SetBaudRate;

    function GetConnectionTimeout: Integer;
    procedure SetConnectionTimeout(const AValue: Integer);
    property ConnectionTimeout: Integer read GetConnectionTimeout write SetConnectionTimeout;

    function GetDelay: Integer;
    procedure SetDelay(const AValue: Integer);
    property Delay: Integer read GetDelay write SetDelay;

    function GetNMEALog: Boolean;
    procedure SetNMEALog(const AValue: Boolean);
    property NMEALog: Boolean read GetNMEALog write SetNMEALog;

    function GetLogPath: WideString;
    property LogPath: WideString read GetLogPath;

    function GetStatic: IGPSModuleByCOMPortSettings;
  end;

implementation

end.
