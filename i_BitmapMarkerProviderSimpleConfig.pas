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

unit i_BitmapMarkerProviderSimpleConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  IBitmapMarkerProviderSimpleConfigStatic = interface
    ['{EBE59B49-48A8-4657-AF1D-9C0951D5AEA9}']
    function GetMarkerSize: Integer;
    property MarkerSize: Integer read GetMarkerSize;

    function GetMarkerColor: TColor32;
    property MarkerColor: TColor32 read GetMarkerColor;

    function GetBorderColor: TColor32;
    property BorderColor: TColor32 read GetBorderColor;
  end;

  IBitmapMarkerProviderSimpleConfig = interface(IConfigDataElement)
    ['{77A05655-3105-400E-90A2-CF24DE062F0A}']
    function GetMarkerSize: Integer;
    procedure SetMarkerSize(AValue: Integer);
    property MarkerSize: Integer read GetMarkerSize write SetMarkerSize;

    function GetMarkerColor: TColor32;
    procedure SetMarkerColor(AValue: TColor32);
    property MarkerColor: TColor32 read GetMarkerColor write SetMarkerColor;

    function GetBorderColor: TColor32;
    procedure SetBorderColor(AValue: TColor32);
    property BorderColor: TColor32 read GetBorderColor write SetBorderColor;

    function GetStatic: IBitmapMarkerProviderSimpleConfigStatic;
  end;

implementation

end.
