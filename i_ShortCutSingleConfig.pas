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

unit i_ShortCutSingleConfig;

interface

uses
  Classes,
  Graphics,
  i_ConfigDataElement;

type
  IShortCutSingleConfig = interface(IConfigDataElement)
    ['{B8B92915-98D2-4254-ACE7-92ACFC081513}']
    function GetCaption: String;
    property Caption: String read GetCaption;

    function GetIconBitmap: TBitmap;
    property IconBitmap: TBitmap read GetIconBitmap;

    function GetShortCut: TShortCut;
    procedure SetShortCut(AValue: TShortCut);
    property ShortCut: TShortCut read GetShortCut write SetShortCut;

    procedure ResetToDefault;

    procedure ResetShortCut;
    procedure ApplyShortCut;
  end;

implementation

end.
