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

unit i_NavigationToPoint;

interface

uses
  t_GeoTypes,
  i_MarksSimple,
  i_ConfigDataElement;

type
  INavigationToPoint = interface(IConfigDataElement)
    ['{61EBB721-A3D9-402B-ACDC-FF3E2DA5C262}']
    function GetIsActive: Boolean;
    property IsActive: Boolean read GetIsActive;

    function GetMarkId: IMarkID;
    property MarkId: IMarkID read GetMarkId;

    function GetLonLat: TDoublePoint;
    property LonLat: TDoublePoint read GetLonLat;

    procedure StartNavToMark(AMarkId: IMarkID; APointLonLat: TDoublePoint);
    procedure StartNavLonLat(APointLonLat: TDoublePoint);
    procedure StopNav;
  end;
implementation

end.
