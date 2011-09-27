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

unit i_MemObjCache;

interface

uses
  GR32,
  i_VectorDataItemSimple;

type
  IMemObjCacheBitmap = interface
    ['{1CF92025-6BA9-4264-91E8-73766E698A6D}']
    procedure Clear;
    procedure DeleteFileFromCache(AKey: string);
    procedure AddTileToCache(AObj: TCustomBitmap32; AKey: string);
    function TryLoadFileFromCache(AObj: TCustomBitmap32; AKey: string): boolean;
  end;

  IMemObjCacheVector = interface
    ['{0BB1598E-A00C-4BBE-9AA8-08F94974EAB2}']
    procedure Clear;
    procedure DeleteFileFromCache(AKey: string);
    procedure AddTileToCache(AObj: IVectorDataItemList; AKey: string);
    function TryLoadFileFromCache(var AObj: IVectorDataItemList; AKey: string): boolean;
  end;

implementation

end.
 