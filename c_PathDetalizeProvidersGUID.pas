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

unit c_PathDetalizeProvidersGUID;

interface

const
  CPathDetalizeProviderCloudMadeFastestByCar: TGUID = '{5F79B0F0-71E4-42CF-A3FB-0B3AD4AC0EAE}';
  CPathDetalizeProviderCloudMadeFastestByFoot: TGUID = '{F320F215-E0A9-411D-9F59-12931F830334}';
  CPathDetalizeProviderCloudMadeFastestByBicycle: TGUID = '{44FDA766-BB46-4FB8-9641-20F6A5FC5748}';
  CPathDetalizeProviderCloudMadeShortestByCar: TGUID = '{A313276B-EF1D-4030-A34B-C311CA9EBFFE}';
  CPathDetalizeProviderCloudMadeShortestByFoot: TGUID = '{565E73B3-5F02-4D03-87A7-7EA3A1F6BD35}';
  CPathDetalizeProviderCloudMadeShortestByBicycle: TGUID = '{1EBC7CC5-A107-41AB-B5AF-86BFD61ADBC0}';

  CPathDetalizeProviderMailRuShortest: TGUID = '{B64FB2B3-9862-493B-86DB-868D20817DB0}';
  CPathDetalizeProviderMailRuFastest: TGUID = '{F88747AD-E946-4652-9B28-E8B32EACF726}';
  CPathDetalizeProviderMailRuFastestWithTraffic: TGUID = '{4E7DE0A5-C868-42FA-9667-4B72EAACFA4B}';

  CPathDetalizeProviderYourNavigationFastestByCar: TGUID = '{49F567B9-9E8A-4878-86FD-BDDDB7098928}';
  CPathDetalizeProviderYourNavigationShortestByCar: TGUID = '{3789A641-00FA-4DFD-BE57-BF53D162B0E7}';
  CPathDetalizeProviderYourNavigationFastestByBicycle: TGUID = '{FB9561C9-0AC4-4E4D-9F9E-CE81A5D1CA45}';
  CPathDetalizeProviderYourNavigationShortestByBicycle: TGUID = '{FB699DA8-87AF-4B6B-BB4D-7C714510396B}';


implementation

end.
