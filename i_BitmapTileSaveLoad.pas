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

unit i_BitmapTileSaveLoad;

interface

uses
  Classes,
  GR32;

type

  ///	<summary>Интерфейс загрузчика растровых тайлов</summary>
  IBitmapTileLoader = interface
    ['{07D84005-DD59-4750-BCCE-A02330734539}']

    {$REGION 'Documentation'}
    ///	<summary>Загрузка растра из файла</summary>
    ///	<remarks>Рекомендуется реализовывать через загрузку при помощи
    ///	стрима</remarks>
    {$ENDREGION}
    procedure LoadFromFile(const AFileName: string; ABtm: TCustomBitmap32);

    ///	<summary>Загрузка растра из стрима</summary>
    procedure LoadFromStream(AStream: TStream; ABtm: TCustomBitmap32);
  end;

  IBitmapTileSaver = interface
    ['{00853113-0F3E-441D-974E-CCBC2F5C6E10}']
    procedure SaveToFile(ABtm: TCustomBitmap32; const AFileName: string);
    procedure SaveToStream(ABtm: TCustomBitmap32; AStream: TStream);
  end;

implementation

end.
 