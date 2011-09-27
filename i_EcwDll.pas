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

unit i_EcwDll;

interface

uses
  ECWWriter;

type
  IEcwDll = interface
  ['{B5E36492-CA31-4114-A65F-4EC6E0B76DCC}']
    function GetCompressAllocClient: NCSEcwCompressAllocClient;
    property CompressAllocClient: NCSEcwCompressAllocClient read GetCompressAllocClient;

    function GetCompressOpen: NCSEcwCompressOpen;
    property CompressOpen: NCSEcwCompressOpen read GetCompressOpen;

    function GetCompress: NCSEcwCompress;
    property Compress: NCSEcwCompress read GetCompress;

    function GetCompressClose: NCSEcwCompressClose;
    property CompressClose: NCSEcwCompressClose read GetCompressClose;

    function GetCompressFreeClient: NCSEcwCompressFreeClient;
    property CompressFreeClient: NCSEcwCompressFreeClient read GetCompressFreeClient;
  end;

implementation

end.
