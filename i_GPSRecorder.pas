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

unit i_GPSRecorder;

interface

uses
  t_GeoTypes,
  i_GPS,
  i_ConfigDataElement;

type
  TGPSTrackPoint = record
    Point: TDoublePoint;
    Speed: Double;
    Time: TDateTime;
  end;

  TGPSTrackPointArray = array of TGPSTrackPoint;

  IGPSRecorder = interface(IConfigDataElement)
    ['{E8525CFD-243B-4454-82AA-C66108A74B8F}']
    procedure AddPoint(APosition: IGPSPosition);
    procedure AddEmptyPoint;
    procedure ClearTrack;
    function IsEmpty: Boolean;
    function LastPoints(const AMaxCount: Integer; var APoints: TGPSTrackPointArray): Integer;
    function GetAllPoints: TArrayOfDoublePoint;
    function GetAllTracPoints: TGPSTrackPointArray;

    function GetOdometer1: Double;
    property Odometer1: Double read GetOdometer1;
    procedure ResetOdometer1;

    function GetOdometer2: Double;
    property Odometer2: Double read GetOdometer2;
    procedure ResetOdometer2;

    function GetDist: Double;
    property Dist: Double read GetDist;
    procedure ResetDist;

    function GetMaxSpeed: Double;
    property MaxSpeed: Double read GetMaxSpeed;
    procedure ResetMaxSpeed;

    function GetAvgSpeed: Double;
    property AvgSpeed: Double read GetAvgSpeed;
    procedure ResetAvgSpeed;

    function GetLastSpeed: Double;
    property LastSpeed: Double read GetLastSpeed;

    function GetLastAltitude: Double;
    property LastAltitude: Double read GetLastAltitude;

    function GetLastHeading: Double;
    property LastHeading: Double read GetLastHeading;

    function GetLastPosition: TDoublePoint;
    property LastPosition: TDoublePoint read GetLastPosition;

    function GetCurrentPosition: IGPSPosition;
    property CurrentPosition: IGPSPosition read GetCurrentPosition;
  end;

implementation

end.
