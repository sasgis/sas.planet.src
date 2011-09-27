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

unit i_GeoCoderList;

interface

uses
  ActiveX,
  i_JclNotify,
  i_GeoCoder;

type
  IGeoCoderListEntity = interface
    ['{FB6DA76B-1706-4F85-A2A0-53E61F4AED2F}']
    function GetGUID: TGUID;
    function GetCaption: WideString;
    function GetGeoCoder: IGeoCoder;
  end;

  IGeoCoderList = interface
    ['{34A0BB9F-8C6B-4664-B299-4F78710E0996}']
    function GetGUIDEnum: IEnumGUID;
    function Get(AGUID: TGUID): IGeoCoderListEntity;
    function GetAddNotifier: IJclNotifier;
  end;


implementation

end.
