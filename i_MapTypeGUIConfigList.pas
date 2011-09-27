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

unit i_MapTypeGUIConfigList;

interface

uses
  i_GUIDListStatic,
  i_MapTypeHotKeyListStatic,
  i_ConfigDataElement;

type
  IMapTypeGUIConfigList = interface(IConfigDataElement)
    ['{6EAFA879-3A76-40CA-89A7-598D45E2C92E}']
    function GetOrderedMapGUIDList: IGUIDListStatic;
    property OrderedMapGUIDList: IGUIDListStatic read GetOrderedMapGUIDList;

    function GetHotKeyList: IMapTypeHotKeyListStatic;
    property HotKeyList: IMapTypeHotKeyListStatic read GetHotKeyList;
  end;

implementation

end.
