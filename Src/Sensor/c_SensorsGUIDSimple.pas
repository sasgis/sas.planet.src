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

  CSensorHDOPGUID: TGUID = '{98BAEF18-A8E6-4BCD-A005-5D61E50BC622}';
  CSensorVDOPGUID: TGUID = '{32D784F0-5DF0-419E-9A9A-3E2BCF73A724}';

  CSensorUTCTimeGUID: TGUID = '{F2D88B5C-1A5B-44C3-9877-D54B0C79DE21}';
  CSensorLocalTimeGUID: TGUID = '{CCE40E26-2F1D-42D0-BB65-55106C9C5E36}';
  CSensorSunRiseTimeGUID: TGUID = '{DA61B759-A9B0-477B-8D66-DC455E918D4B}';
  CSensorSunSetTimeGUID: TGUID = '{2E034C90-1369-448A-8070-AA881B1D2523}';

  CSensorDGPSGUID: TGUID = '{D59E70E7-E064-4D7E-BECD-E268924CEF15}';
  CSensorGPSUnitInfoGUID: TGUID = '{1525B03B-F499-428E-8919-9858F9DD0F9B}';

  CSensorGPSSatellitesGUID: TGUID = '{0B620DF1-26D9-485F-A83C-4191D1C7619B}';

implementation

end.
