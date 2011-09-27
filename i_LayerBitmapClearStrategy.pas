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

unit i_LayerBitmapClearStrategy;

interface

uses
  GR32,
  i_LocalCoordConverter;

type
  ILayerBitmapClearStrategy =  interface
    ['{F2E51CCC-D584-4D88-98E7-0057F3825F63}']
    procedure Clear(ABitmap: TCustomBitmap32);
  end;

  ILayerBitmapClearStrategyFactory = interface
    ['{9F14B47C-2D9C-4974-B78E-E3E3E6B74725}']
    function GetStrategy(
      ASourceConverter, ATargetConverter: ILocalCoordConverter;
      ASourceBitmap: TCustomBitmap32;
      APrevStrategy: ILayerBitmapClearStrategy
    ): ILayerBitmapClearStrategy;
  end;

implementation

end.
