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

unit t_GeoTypes;

interface

uses
  Types;

type
  TDoublePoint = record
    X, Y: Double;
  end;

  TDoubleRect = packed record
    case Integer of
      0: (Left, Top: Double;
        Right, Bottom: Double);
      1: (TopLeft, BottomRight: TDoublePoint);
  end;


  PPointArray = ^TPointArray;
  TPointArray = array [0..0] of TPoint;
  PArrayOfPoint = ^TArrayOfPoint;
  TArrayOfPoint = array of TPoint;

  PDoublePointArray = ^TDoublePointArray;
  TDoublePointArray = array [0..0] of TDoublePoint;
  PArrayOfDoublePoint = ^TArrayOfDoublePoint;
  TArrayOfDoublePoint = array of TDoublePoint;

// Скопировал из ECWReader что бы не добавлять лишние зависимости от того юнита.
{$MINENUMSIZE 4}
type
  TCellSizeUnits =
    (
    // Invalid cell units
    CELL_UNITS_INVALID = 0,
    // Cell units are standard meters
    CELL_UNITS_METERS = 1,
    // Degrees
    CELL_UNITS_DEGREES = 2,
    // US Survey feet
    CELL_UNITS_FEET = 3,
    // Unknown cell units
    CELL_UNITS_UNKNOWN = 4
    );

implementation

end.
