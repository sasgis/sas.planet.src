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

unit i_MapMovingConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapMovingConfig = interface(IConfigDataElement)
    ['{A322104E-A247-4EB3-83F6-C897F64E764C}']
    //Анимация инерции
    function GetAnimateMove: Boolean;
    procedure SetAnimateMove(AValue: Boolean);
    property AnimateMove: Boolean read GetAnimateMove write SetAnimateMove;

    function GetAnimateMoveTime: Cardinal;
    procedure SetAnimateMoveTime(AValue: Cardinal);
    property AnimateMoveTime: Cardinal read GetAnimateMoveTime write SetAnimateMoveTime;

    function GetAnimateMaxStartSpeed: Cardinal;
    procedure SetAnimateMaxStartSpeed(AValue: Cardinal);
    property AnimateMaxStartSpeed: Cardinal read GetAnimateMaxStartSpeed write SetAnimateMaxStartSpeed;

    function GetAnimateMinStartSpeed: Cardinal;
    procedure SetAnimateMinStartSpeed(AValue: Cardinal);
    property AnimateMinStartSpeed: Cardinal read GetAnimateMinStartSpeed write SetAnimateMinStartSpeed;
  end;

implementation

end.
