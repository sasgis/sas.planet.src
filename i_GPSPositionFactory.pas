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

unit i_GPSPositionFactory;

interface

uses
  ActiveX,
  t_GeoTypes,
  i_GPS;

type
  IGPSPositionFactory = interface
    ['{542F6E48-EC4E-4C8D-9A53-8B392B0E8EA6}']
    function BuildSatelliteInfo(
      const APseudoRandomCode: Integer;
      const AElevation: Integer;
      const AAzimuth: Integer;
      const ASignalToNoiseRatio: Integer;
      const AIsFix: Boolean
    ): IGPSSatelliteInfo;

    function BuildSatellitesInViewEmpty: IGPSSatellitesInView;
    function BuildSatellitesInView(
      const AFixCount: Integer;
      const AItemsCount: Integer;
      const AItems: PUnknownList
    ): IGPSSatellitesInView;

    function BuildPositionEmpty: IGPSPosition;
    function BuildPosition(
      const APosition: TDoublePoint;
      const AAltitude: Double;
      const ASpeed_KMH: Double;
      const AHeading: Double;
      const AUTCDateTime: TDateTime;
      const ALocalDateTime: TDateTime;
      const AIsFix: Word;
      const AHDOP: Double;
      const AVDOP: Double;
      const APDOP: Double;
      const ASatellites: IGPSSatellitesInView
    ): IGPSPosition;
  end;

implementation

end.
