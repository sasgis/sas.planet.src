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

unit i_Datum;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_EnumDoublePoint,
  i_NotifierOperation;

type
  IDatum = interface
    ['{FF96E41C-41EC-4D87-BD1B-42F8E7CA3E15}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    // Возвращает код EPSG для этого датума. Для нестандартных проекций и сфероидов будет возвращать 0
    function GetEPSG: integer; stdcall;
    property EPSG: Integer read GetEPSG;

    // Возвращает радиус сфероида.
    function GetSpheroidRadiusA: Double; stdcall;
    function GetSpheroidRadiusB: Double; stdcall;

    // Возвращает является ли другой конвертер эквивалентным текущему
    function IsSameDatum(const ADatum: IDatum): Boolean; stdcall;

    function CalcPolygonArea(
      const APoints: PDoublePointArray;
      const ACount: Integer;
      const ANotifier: INotifierOperation = nil;
      const AOperationID: Integer = 0
    ): Double;

    // Возвращает кратчайшее (вдоль геодезической линии) расстояние между двумя
    // заданными точками (в метрах).
    function CalcDist(
      const AStart: TDoublePoint;
      const AFinish: TDoublePoint
    ): Double; overload;

    // Решает так называемую вторую (обратную) геодезическую задачу: построить
    // кратчайший маршрут между двумя точками на картографируемой поверхности и
    // определелить расстояние и направление движения.
    function CalcDist(
      const AStart: TDoublePoint;
      const AFinish: TDoublePoint;
      out AInitialBearing: Double;
      out AFinalBearing: Double
    ): Double; overload;

    // Решает так называемую первую (прямую) геодезическую задачу: где мы
    // окажемся, если выйдем из указанной точки в указанном направлении и
    // продём, не сворачивая, указанное расстояние.
    function CalcFinishPosition(
      const AStart: TDoublePoint;
      const AInitialBearing: Double;
      const ADistance: Double
    ): TDoublePoint;

    // Вычисляет координаты середины отрезка между заданными точками
    function CalcMiddlePoint(
      const AStart: TDoublePoint;
      const AFinish: TDoublePoint
    ): TDoublePoint;

    // Генерирует промежуточные точки вдоль геодезической линии
    function GetLinePoints(
      const AStart: TDoublePoint;
      const AFinish: TDoublePoint;
      const APointCount: integer
    ): IEnumLonLatPoint;
  end;

implementation

end.
