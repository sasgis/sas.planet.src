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

unit i_GSMGeoCodeConfig;

interface

uses
  i_ConfigDataElement;
  
type
  IGSMGeoCodeConfig = interface(IConfigDataElement)
    ['{E60F1967-FE2D-4C3C-81D1-D99B50A0F21F}']
    function GetUseGSMByCOM: Boolean;
    procedure SetUseGSMByCOM(const AValue: Boolean);
    property UseGSMByCOM: Boolean read GetUseGSMByCOM write SetUseGSMByCOM;

    function GetPortName: string;
    procedure SetPortName(const AValue: string);
    property PortName: string read GetPortName write SetPortName;

    function GetBaudRate: Cardinal;
    procedure SetBaudRate(const AValue: Cardinal);
    property BaudRate: Cardinal read GetBaudRate write SetBaudRate;

    function GetWaitTime: Cardinal;
    procedure SetWaitTime(const AValue: Cardinal);
    property WaitTime: Cardinal read GetWaitTime write SetWaitTime;
  end;

implementation

end.
