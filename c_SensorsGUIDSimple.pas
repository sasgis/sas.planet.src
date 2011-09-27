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

unit c_SensorsGUIDSimple;

interface

const
  CSensorLastSpeedGUID: TGUID = '{9C8ABE63-50B0-4784-83E9-8B43245514B3}';
  CSensorAvgSpeedGUID: TGUID = '{89D0A61B-831B-4FA7-B672-371DB65A1920}';
  CSensorMaxSpeedGUID: TGUID = '{99DD78E5-D14A-4AA3-A47C-0D477D2B0CE1}';

  CSensorDistGUID: TGUID = '{A38E3D2F-C22A-4805-AAFB-FC65C1869EF3}';
  CSensorOdometer1GUID: TGUID = '{27CEEABE-7BFB-4D56-8920-167AB133B686}';
  CSensorOdometer2GUID: TGUID = '{0686326A-261D-40E2-A036-F846EC5778E7}';

  CSensorDistToMarkGUID: TGUID = '{5B360A05-F62A-4FAD-A129-8FC0A7C4CA69}';
  CSensorLastAltitudeGUID: TGUID = '{F45D4FAF-F0FC-402D-A5AB-4349215B77F5}';
  CSensorBatteryGUID: TGUID = '{E763D7A8-EF72-452A-9A97-5BFBF1C7EE4D}';

  CSensorHeadingGUID: TGUID = '{8E6C8F38-A5AD-4D2E-9EC0-71866FE9FDCF}';
  CSensorSatellitesGUID: TGUID = '{52681DB0-C9F1-488D-BA11-391004033D28}';

implementation

end.
