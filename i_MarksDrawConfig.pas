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

unit i_MarksDrawConfig;

interface

uses
  Types,
  i_ConfigDataElement;

type
  IMarksDrawConfigStatic = interface
    ['{2BC70BD2-74E8-4063-BB70-03445CBCFD00}']
    function GetShowPointCaption: Boolean;
    property ShowPointCaption: Boolean read GetShowPointCaption;

    function GetUseSimpleDrawOrder: Boolean;
    property UseSimpleDrawOrder: Boolean read GetUseSimpleDrawOrder;

    function GetOverSizeRect: TRect;
    property OverSizeRect: TRect read GetOverSizeRect;
  end;

  IMarksDrawConfig = interface(IConfigDataElement)
    ['{992DD23C-E0AA-4731-99A9-9049F55DFF6E}']
    function GetShowPointCaption: Boolean;
    procedure SetShowPointCaption(AValue: Boolean);
    property ShowPointCaption: Boolean read GetShowPointCaption write SetShowPointCaption;

    function GetUseSimpleDrawOrder: Boolean;
    procedure SetUseSimpleDrawOrder(AValue: Boolean);
    property UseSimpleDrawOrder: Boolean read GetUseSimpleDrawOrder write SetUseSimpleDrawOrder;

    function GetOverSizeRect: TRect;
    procedure SetOverSizeRect(AValue: TRect);
    property OverSizeRect: TRect read GetOverSizeRect write SetOverSizeRect;

    function GetMagnetDraw: Boolean;
    procedure SetMagnetDraw(AValue: Boolean);
    property MagnetDraw: Boolean read GetMagnetDraw write SetMagnetDraw;

    function GetStatic: IMarksDrawConfigStatic;
  end;

implementation

end.
