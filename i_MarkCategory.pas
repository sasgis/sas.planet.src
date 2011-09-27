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

unit i_MarkCategory;

interface

type
  ICategory = interface
    ['{B870BAEC-8ADD-4D29-9A9E-B9131C0C5681}']
    function GetName: string; stdcall;
    property Name: string read GetName;

    function IsSame(ACategory: ICategory): Boolean;
  end;

  IMarkCategory = interface(ICategory)
  ['{00226B68-9915-41AA-90B7-3F2348E53527}']
    function GetVisible: boolean; stdcall;
    property Visible: boolean read GetVisible;

    function GetAfterScale: integer; stdcall;
    property AfterScale: integer read GetAfterScale;

    function GetBeforeScale: integer; stdcall;
    property BeforeScale: integer read GetBeforeScale;

    function IsNew: Boolean;
  end;

implementation

end.
