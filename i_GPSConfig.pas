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

unit i_GPSConfig;

interface

uses
  i_GPSModuleByCOMPortConfig,
  i_ConfigDataElement;

type
  IGPSConfig = interface(IConfigDataElement)
    ['{336A93F1-8E9C-4704-8384-214018758354}']
    function GetGPSEnabled: Boolean;
    procedure SetGPSEnabled(const AValue: Boolean);
    property GPSEnabled: Boolean read GetGPSEnabled write SetGPSEnabled;

    function GetNoDataTimeOut: Integer;
    procedure SetNoDataTimeOut(const AValue: Integer);
    property NoDataTimeOut: Integer read GetNoDataTimeOut write SetNoDataTimeOut;

    function GetWriteLog: Boolean;
    procedure SetWriteLog(const AValue: Boolean);
    property WriteLog: Boolean read GetWriteLog write SetWriteLog;

    function GetLogPath: WideString;
    property LogPath: WideString read GetLogPath;

    function GetModuleConfig: IGPSModuleByCOMPortConfig;
    property ModuleConfig: IGPSModuleByCOMPortConfig read GetModuleConfig;
  end;

implementation

end.
