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

unit i_BitmapPostProcessingConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  IBitmapPostProcessingConfigStatic = interface
    ['{3DBBF6CA-6AA3-4578-8D23-3E04D1D42C34}']
    function GetInvertColor: boolean;
    property InvertColor: boolean read GetInvertColor;

    // Число для гамма преобразования тайлов перед отображением
    function GetGammaN: Integer;
    property GammaN: Integer read GetGammaN;

    // Число для изменения контрастности тайлов перед отображением
    function GetContrastN: Integer;
    property ContrastN: Integer read GetContrastN;

    procedure ProcessBitmap(Bitmap: TCustomBitmap32);
  end;

  IBitmapPostProcessingConfig = interface(IConfigDataElement)
    ['{3CF3CE21-3488-495C-9A17-A2164763342E}']
    function GetInvertColor: boolean;
    procedure SetInvertColor(const AValue: boolean);
    property InvertColor: boolean read GetInvertColor write SetInvertColor;

    function GetGammaN: Integer;
    procedure SetGammaN(const AValue: Integer);
    property GammaN: Integer read GetGammaN write SetGammaN;

    function GetContrastN: Integer;
    procedure SetContrastN(const AValue: Integer);
    property ContrastN: Integer read GetContrastN write SetContrastN;

    function GetStatic: IBitmapPostProcessingConfigStatic;
  end;

implementation

end.
