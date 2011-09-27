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

unit i_SelectionRectLayerConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  ISelectionRectLayerConfig = interface(IConfigDataElement)
    ['{B8253870-8613-444C-B45C-47FD420B7EFB}']
    function GetFillColor: TColor32;
    procedure SetFillColor(AValue: TColor32);
    property FillColor: TColor32 read GetFillColor write SetFillColor;

    function GetBorderColor: TColor32;
    procedure SetBorderColor(AValue: TColor32);
    property BorderColor: TColor32 read GetBorderColor write SetBorderColor;

    function GetZoomDeltaCount: Integer;
    procedure SetZoomDeltaCount(AValue: Integer);
    property ZoomDeltaCount: Integer read GetZoomDeltaCount write SetZoomDeltaCount;

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
    property FontSize: Integer read GetFontSize write SetFontSize;

    function GetZoomDeltaColor(AIndex: Integer): TColor32;
    procedure SetZoomDeltaColor(AIndex: Integer; AValue: TColor32);
    property ZoomDeltaColor[AIndex: Integer]: TColor32 read GetZoomDeltaColor write SetZoomDeltaColor;

    function GetZoomDeltaColors: TArrayOfColor32;
    property ZoomDeltaColors: TArrayOfColor32 read GetZoomDeltaColors;
  end;

implementation

end.
