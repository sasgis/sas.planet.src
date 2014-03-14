{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
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
  Add(TImageResamplerFactoryNearest.Create, 'Nearest');
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
