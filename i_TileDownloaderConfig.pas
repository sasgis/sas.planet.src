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

unit i_TileDownloaderConfig;

interface

uses
  i_ConfigDataElement,
  i_InetConfig;

type
  ITileDownloaderConfigStatic = interface
    ['{FECD40CF-A0AF-479A-8CCC-E3363037773E}']
    function GetInetConfigStatic: IInetConfigStatic;
    property InetConfigStatic: IInetConfigStatic read GetInetConfigStatic;

    function GetWaitInterval: Cardinal;
    property WaitInterval: Cardinal read GetWaitInterval;

    function GetMaxConnectToServerCount: Cardinal;
    property MaxConnectToServerCount: Cardinal read GetMaxConnectToServerCount;

    function GetIgnoreMIMEType: Boolean;
    property IgnoreMIMEType: Boolean read GetIgnoreMIMEType;

    function GetExpectedMIMETypes: string;
    property ExpectedMIMETypes: string read GetExpectedMIMETypes;

    function GetDefaultMIMEType: string;
    property DefaultMIMEType: string read GetDefaultMIMEType;
  end;

  ITileDownloaderConfig = interface(IConfigDataElement)
    ['{FECD40CF-A0AF-479A-8CCC-E3363037773E}']
    function GetInetConfigStatic: IInetConfigStatic;
    property InetConfigStatic: IInetConfigStatic read GetInetConfigStatic;

    function GetWaitInterval: Cardinal;
    procedure SetWaitInterval(AValue: Cardinal);
    property WaitInterval: Cardinal read GetWaitInterval write SetWaitInterval;

    function GetMaxConnectToServerCount: Cardinal;
    procedure SetMaxConnectToServerCount(AValue: Cardinal);
    property MaxConnectToServerCount: Cardinal read GetMaxConnectToServerCount write SetMaxConnectToServerCount;

    function GetIgnoreMIMEType: Boolean;
    procedure SetIgnoreMIMEType(AValue: Boolean);
    property IgnoreMIMEType: Boolean read GetIgnoreMIMEType write SetIgnoreMIMEType;

    function GetExpectedMIMETypes: string;
    procedure SetExpectedMIMETypes(AValue: string);
    property ExpectedMIMETypes: string read GetExpectedMIMETypes write SetExpectedMIMETypes;

    function GetDefaultMIMEType: string;
    procedure SetDefaultMIMEType(AValue: string);
    property DefaultMIMEType: string read GetDefaultMIMEType write SetDefaultMIMEType;

    function GetStatic: ITileDownloaderConfigStatic;
  end;

implementation

end.
