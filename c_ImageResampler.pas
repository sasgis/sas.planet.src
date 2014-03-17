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

unit c_ImageResampler;

interface

const
  CResamplerNearestGUID: TGUID = '{1CCCFBD8-3DB5-4387-A86B-3690D6E21914}';
  CResamplerLinearGUID: TGUID = '{39AE1D08-B463-4CFB-A040-EB1CCB0A8F35}';
  CResamplerBoxGUID: TGUID = '{939FAFBA-D34A-4941-AF6B-50A6F431BDA2}';
  CResamplerCosineGUID: TGUID = '{554E2271-BD61-4EA7-AD3E-7BED8FD8B145}';
  CResamplerSplineGUID: TGUID = '{653C8817-CCD7-45CB-A1DC-D12F7FA9A890}';
  CResamplerCubicGUID: TGUID = '{5081A8A4-887F-4035-9D0F-173A92022C1B}';
  CResamplerMitchellGUID: TGUID = '{3B24A711-6D96-40CE-82A1-F710ED5DBA1B}';
  CResamplerAlbrechtGUID: TGUID = '{1B4DC573-9C60-4107-977D-F1AC4B4D9AC5}';
  CResamplerLanczosGUID: TGUID = '{77533101-C009-4A79-8D90-293D83E88337}';
  CResamplerGaussianGUID: TGUID = '{310E83A1-4F3A-4AB3-8390-0F512C27A467}';
  CResamplerBlackmanGUID: TGUID = '{1BBD612B-0FCF-4EAE-AE46-65BDAF63FB54}';
  CResamplerHannGUID: TGUID = '{1439CA42-F813-4FC4-8E5E-6C3A72165E1B}';
  CResamplerHammingGUID: TGUID = '{B377031D-E87A-4C95-8F68-863322791A64}';
  CResamplerSinshGUID: TGUID = '{6C73B7A8-7AC5-440C-89E8-3D4D73EFD828}';
  CResamplerHermiteGUID: TGUID = '{2F694E07-5121-405A-9A06-E07DADF46BD2}';

implementation

end.
