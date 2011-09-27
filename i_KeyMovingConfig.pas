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

unit i_KeyMovingConfig;

interface

uses
  i_ConfigDataElement;

type
  IKeyMovingConfig = interface(IConfigDataElement)
    ['{87769678-9D11-4E47-AAE5-88F4809B7406}']
    function GetFirstKeyPressDelta: Double;
    procedure SetFirstKeyPressDelta(AValue: Double);
    property FirstKeyPressDelta: Double read GetFirstKeyPressDelta write SetFirstKeyPressDelta;

    function GetMinPixelPerSecond: Double;
    procedure SetMinPixelPerSecond(AValue: Double);
    property MinPixelPerSecond: Double read GetMinPixelPerSecond write SetMinPixelPerSecond;

    function GetMaxPixelPerSecond: Double;
    procedure SetMaxPixelPerSecond(AValue: Double);
    property MaxPixelPerSecond: Double read GetMaxPixelPerSecond write SetMaxPixelPerSecond;

    function GetSpeedChangeTime: Double;
    procedure SetSpeedChangeTime(AValue: Double);
    property SpeedChangeTime: Double read GetSpeedChangeTime write SetSpeedChangeTime;
  end;

implementation

end.
