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

unit i_Datum;

interface

uses
  t_GeoTypes;

type
  IDatum = interface
    ['{FF96E41C-41EC-4D87-BD1B-42F8E7CA3E15}']
    // Возвращает код EPSG для этого датума. Для нестандартных проекций и сфероидов будет возвращать 0
    function GetEPSG: integer; stdcall;
    property EPSG: Integer read GetEPSG;

    // Возвращает радиус сфероида.
    function GetSpheroidRadiusA: Double; stdcall;
    function GetSpheroidRadiusB: Double; stdcall;

    // Возвращает является ли другой конвертер эквивалентным текущему
    function IsSameDatum(ADatum: IDatum): Boolean; stdcall;

    function CalcPoligonArea(const polygon: TArrayOfDoublePoint): Double;
    function CalcDist(const AStart: TDoublePoint; const AFinish: TDoublePoint): Double;
  end;

implementation

end.

