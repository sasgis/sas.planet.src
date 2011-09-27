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

unit i_GlobalViewMainConfig;

interface

uses
  Graphics,
  i_ConfigDataElement;

type
  IGlobalViewMainConfig = interface(IConfigDataElement)
    ['{9174C624-07F7-41B7-8533-C77D1E05B270}']
    function GetBackGroundColor: TColor;
    procedure SetBackGroundColor(const AValue: TColor);
    property BackGroundColor: TColor read GetBackGroundColor write SetBackGroundColor;

    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(const AValue: Boolean);
    property UsePrevZoomAtMap: Boolean read GetUsePrevZoomAtMap write SetUsePrevZoomAtMap;

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(const AValue: Boolean);
    property UsePrevZoomAtLayer: Boolean read GetUsePrevZoomAtLayer write SetUsePrevZoomAtLayer;
  end;

implementation

end.
