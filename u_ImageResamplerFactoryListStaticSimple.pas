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

unit u_ImageResamplerFactoryListStaticSimple;

interface

uses
  u_ImageResamplerFactoryListStatic;

type
  TImageResamplerFactoryListStaticSimple = class(TImageResamplerFactoryListStatic)
  public
    constructor Create;
  end;

implementation

uses
  GR32_Resamplers,
  u_ImageResamplerFactory;

{ TImageResamplerFactoryListStaticSimple }

constructor TImageResamplerFactoryListStaticSimple.Create;
begin
  inherited;
  Add(TImageResamplerFactoryNil.Create, 'Nearest');
  Add(TImageResamplerFactoryLinear.Create, 'Linear');
  Add(TImageResamplerFactoryKernel.Create(nil), 'Box');
  Add(TImageResamplerFactoryKernel.Create(TCosineKernel), 'Cosine');
  Add(TImageResamplerFactoryKernel.Create(TSplineKernel), 'Spline');
  Add(TImageResamplerFactoryKernel.Create(TCubicKernel), 'Cubic');
  Add(TImageResamplerFactoryKernel.Create(TMitchellKernel), 'Mitchell');
  Add(TImageResamplerFactoryKernel.Create(TAlbrechtKernel), 'Albrecht');
  Add(TImageResamplerFactoryKernel.Create(TLanczosKernel), 'Lanczos');
  Add(TImageResamplerFactoryKernel.Create(TGaussianKernel), 'Gaussian');
  Add(TImageResamplerFactoryKernel.Create(TBlackmanKernel), 'Blackman');
  Add(TImageResamplerFactoryKernel.Create(THannKernel), 'Hann');
  Add(TImageResamplerFactoryKernel.Create(THammingKernel), 'Hamming');
  Add(TImageResamplerFactoryKernel.Create(TSinshKernel), 'Sinsh');
  Add(TImageResamplerFactoryKernel.Create(THermiteKernel), 'Hermite');
end;

end.
