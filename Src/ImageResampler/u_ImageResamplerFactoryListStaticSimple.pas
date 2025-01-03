{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
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
  c_ImageResampler,
  i_InterfaceListSimple,
  i_ImageResamplerFactory,
  u_InterfaceListSimple,
  u_ImageResamplerFactory;

{ TImageResamplerFactoryListStaticSimple }

constructor TImageResamplerFactoryListStaticSimple.Create;
var
  VList: IInterfaceListSimple;
  VItem: IImageResamplerFactoryListEntry;
begin
  VList := TInterfaceListSimple.Create;
  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryNearest.Create,
      'Nearest',
      CResamplerNearestGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryLinear.Create,
      'Linear',
      CResamplerLinearGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(nil),
      'Box',
      CResamplerBoxGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(TCosineKernel),
      'Cosine',
      CResamplerCosineGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(TSplineKernel),
      'Spline',
      CResamplerSplineGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(TCubicKernel),
      'Cubic',
      CResamplerCubicGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(TMitchellKernel),
      'Mitchell',
      CResamplerMitchellGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(TAlbrechtKernel),
      'Albrecht',
      CResamplerAlbrechtGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(TLanczosKernel),
      'Lanczos',
      CResamplerLanczosGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(TGaussianKernel),
      'Gaussian',
      CResamplerGaussianGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(TBlackmanKernel),
      'Blackman',
      CResamplerBlackmanGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(THannKernel),
      'Hann',
      CResamplerHannGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(THammingKernel),
      'Hamming',
      CResamplerHammingGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(TSinshKernel),
      'Sinsh',
      CResamplerSinshGUID
    );
  VList.Add(VItem);

  VItem :=
    TImageResamplerFactoryListStaticEntry.Create(
      TImageResamplerFactoryKernel.Create(THermiteKernel),
      'Hermite',
      CResamplerHermiteGUID
    );
  VList.Add(VItem);

  inherited Create(VList.MakeStaticAndClear);
end;

end.
