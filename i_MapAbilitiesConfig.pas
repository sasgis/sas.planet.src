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

unit i_MapAbilitiesConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapAbilitiesConfigStatic = interface
  ['{89BC8688-41A7-4ADE-A911-E90BAC6B5689}']
    function GetIsLayer: Boolean;
    property IsLayer: Boolean read GetIsLayer;

    function GetIsShowOnSmMap: Boolean;
    property IsShowOnSmMap: Boolean read GetIsShowOnSmMap;

    function GetIsUseStick: Boolean;
    property IsUseStick: Boolean read GetIsUseStick;

    function GetIsUseGenPrevious: Boolean;
    property IsUseGenPrevious: Boolean read GetIsUseGenPrevious;

    function GetUseDownload: Boolean;
    property UseDownload: Boolean read GetUseDownload;
  end;

  IMapAbilitiesConfig = interface(IConfigDataElement)
  ['{6CF60AD7-0284-4252-AC55-2A2C1ABAF4FC}']
    function GetIsLayer: Boolean;
    property IsLayer: Boolean read GetIsLayer;

    function GetIsShowOnSmMap: Boolean;
    procedure SetIsShowOnSmMap(AValue: Boolean);
    property IsShowOnSmMap: Boolean read GetIsShowOnSmMap write SetIsShowOnSmMap;

    function GetIsUseStick: Boolean;
    procedure SetIsUseStick(AValue: Boolean);
    property IsUseStick: Boolean read GetIsUseStick write SetIsUseStick;

    function GetIsUseGenPrevious: Boolean;
    procedure SetIsUseGenPrevious(AValue: Boolean);
    property IsUseGenPrevious: Boolean read GetIsUseGenPrevious write SetIsUseGenPrevious;

    function GetUseDownload: Boolean;
    procedure SetUseDownload(AValue: Boolean);
    property UseDownload: Boolean read GetUseDownload write SetUseDownload;

    function GetStatic: IMapAbilitiesConfigStatic;
  end;

implementation

end.
