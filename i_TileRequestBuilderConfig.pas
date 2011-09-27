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

unit i_TileRequestBuilderConfig;

interface

uses
  i_ConfigDataElement;

type
  ITileRequestBuilderConfigStatic = interface
    ['{84B1A72C-951D-4591-80E4-3DA0CDC30ED7}']
    function  GetUrlBase: string;
    property UrlBase: string read GetUrlBase;

    function  GetRequestHeader: string;
    property RequestHeader: string read GetRequestHeader;
  end;


  ITileRequestBuilderConfig = interface(IConfigDataElement)
    ['{FA554C29-EDAF-4E3C-9B59-BC881502F33A}']
    function  GetUrlBase: string;
    procedure SetUrlBase(AValue: string);
    property UrlBase: string read GetUrlBase write SetUrlBase;

    function  GetRequestHeader: string;
    procedure SetRequestHeader(AValue: string);
    property RequestHeader: string read GetRequestHeader write SetRequestHeader;
  end;

implementation

end.
