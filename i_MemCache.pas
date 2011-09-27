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

unit i_MemCache;

interface

uses
  Types;
type
  ICacheElement = interface
  ['{3E56D8A9-51CB-4CB0-AABF-CD09145CEFB1}']
    function GetIsEmpty: Boolean;
    function GetPutTime: TDateTime;
    function GetLastAccessTime: TDateTime;
    procedure GetTileCoord(var AXY: TPoint; var AZoom: Byte);
    function GetObject: TObject;
  end;

  IMemCache = interface
  ['{75B5851E-3BC3-41B4-9E2E-8804AB073BCB}']
    function GetByCoord(AXY: TPoint; AZoom: Byte): ICacheElement;
    procedure PutObject(AXY: TPoint; AZoom: Byte; AObj: TObject);
    procedure TrimByTimeToLive;
    procedure TrimByCount;
  end;
implementation

end.
