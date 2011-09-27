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

unit i_DownloadResultTextProvider;

interface

type
  IDownloadResultTextProvider = interface
    ['{70C2269A-0A1C-4D26-B4F6-D65C16698C76}']
    function GetMessageBadContentType: string;
    function GetMessageBadProxyAuth: string;
    function GetMessageBanned: string;
    function GetMessageDataNotExistsByStatusCode: string;
    function GetMessageDataNotExistsZeroSize: string;
    function GetMessageLoadErrorByErrorCode: string;
    function GetMessageLoadErrorByStatusCode: string;
    function GetMessageLoadErrorByUnknownStatusCode: string;
    function GetMessageNoConnetctToServerByErrorCode: string;
    function GetMessageUnexpectedProxyAuth: string;
  end;

implementation

end.
