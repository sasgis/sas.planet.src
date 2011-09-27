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

unit i_TileStorage;

interface

uses
  Types,
  Classes,
  GR32,
  i_TileInfoBasic,
  i_TileStorageInfo;

type
  ITileStorage = interface
    ['{80A0246E-68E0-4EA0-9B0F-3338472FDB3C}']
    function GetInfo: ITileStorageInfo;

    function GetTileFileName(AXY: TPoint; Azoom: byte; AVersion: Variant): string;
    function GetTileInfo(AXY: TPoint; Azoom: byte; AVersion: Variant): ITileInfoBasic;

    function LoadTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream; out ATileInfo: ITileInfoBasic): Boolean;
    function DeleteTile(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
    function DeleteTNE(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
    procedure SaveTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream);
    procedure SaveTNE(AXY: TPoint; Azoom: byte; AVersion: Variant);

    function LoadFillingMap(
      btm: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      ASourceZoom: byte;
      AVersion: Variant;
      IsStop: PBoolean
    ): boolean;
  end;


implementation

end.
