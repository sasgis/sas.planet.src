{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit i_ProjectionType;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Datum;

type
  IProjectionType = interface
    ['{843F645C-F485-4392-A809-8139430FC974}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetDatum: IDatum;
    property Datum: IDatum read GetDatum;

    // Возвращает код EPSG для этой проекции. Для нестандартных проекций и сфероидов будет возвращать 0
    function GetProjectionEPSG: Integer;
    property ProjectionEPSG: Integer read GetProjectionEPSG;

    // Перобразует относительные координаты на карте в географические
    function Relative2LonLat(const APoint: TDoublePoint): TDoublePoint;
    // Перобразует прямоугольник с относительными координатами на карте в географические
    function RelativeRect2LonLatRect(const ARect: TDoubleRect): TDoubleRect;

    // Преобразует географические коодинаты в относительные координаты на карте
    function LonLat2Relative(const APoint: TDoublePoint): TDoublePoint;
    // Преобразует прямоугольник в географических коодинатах в относительные координаты на карте
    function LonLatRect2RelativeRect(const ARect: TDoubleRect): TDoubleRect;

    // Преобразует георафические координаты в метрические, и обратно
    function LonLat2Metr(const APoint: TDoublePoint): TDoublePoint;
    function Metr2LonLat(const APoint: TDoublePoint): TDoublePoint;

    procedure ValidateRelativePos(var APoint: TDoublePoint);
    procedure ValidateRelativeRect(var ARect: TDoubleRect);

    procedure ValidateLonLatPos(var APoint: TDoublePoint);
    procedure ValidateLonLatRect(var ARect: TDoubleRect);

    function CheckRelativePos(const APoint: TDoublePoint): boolean;
    function CheckRelativeRect(const ARect: TDoubleRect): boolean;

    function CheckLonLatPos(const APoint: TDoublePoint): boolean;
    function CheckLonLatRect(const ARect: TDoubleRect): boolean;

    // Возвращает является ли другой тип проекции эквивалентным текущему
    function IsSame(const AOther: IProjectionType): Boolean;
  end;

implementation

end.
