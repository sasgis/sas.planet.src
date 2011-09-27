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

unit i_TileError;

interface

uses
  Types,
  u_MapType;

type
  ITileErrorInfo = interface
    ['{35CA6508-14F7-43D0-BA19-C6FE088936FB}']
    function GetMapType: TMapType;
    property MapType: TMapType read GetMapType;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetTile: TPoint;
    property Tile: TPoint read GetTile;

    function GetErrorText: string;
    property ErrorText: string read GetErrorText;
  end;

  ITileErrorLogger = interface
    ['{693E489D-A78E-42B8-87FB-F16EF5F53ACF}']
    procedure LogError(AValue: ITileErrorInfo);
  end;

implementation

end.
