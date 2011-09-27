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

unit i_ImageResamplerFactory;

interface

uses
  GR32;

type
  IImageResamplerFactory = interface
    ['{4829EE36-667A-4A25-8CE0-1DAFDDC9B3D9}']
    function CreateResampler: TCustomResampler;
  end;

  IImageResamplerFactoryList = interface
    ['{CC888F5D-5DDA-427F-8127-93B0F1BD8CA5}']
    function Count: Integer;

    function Get(AIndex: Integer): IImageResamplerFactory;
    property Items[Index: Integer]: IImageResamplerFactory read Get; default;

    function GetCaption(AIndex: Integer): string;
    property Captions[Index: Integer]: string read GetCaption;
  end;

implementation

end.
