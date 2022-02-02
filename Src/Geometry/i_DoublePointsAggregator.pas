{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit i_DoublePointsAggregator;

interface

uses
  t_GeoTypes,
  i_DoublePoints;

type
  IDoublePointsAggregator = interface
    ['{2B653087-1769-4C76-A880-17A2E27BD282}']
    procedure Add(
      const APoint: TDoublePoint;
      const AMetaItem: PDoublePointsMetaItem = nil
    );
    procedure AddPoints(
      const APoints: PDoublePointArray;
      const AMeta: PDoublePointsMeta;
      const ACount: Integer
    );

    procedure Insert(
      const AIndex: Integer;
      const APoint: TDoublePoint;
      const AMetaItem: PDoublePointsMetaItem = nil
    );
    procedure InsertPoints(
      const AIndex: Integer;
      const APoints: PDoublePointArray;
      const AMeta: PDoublePointsMeta;
      const ACount: Integer
    );

    procedure Delete(const AIndex: Integer);
    procedure DeletePoints(
      const AIndex: Integer;
      const ACount: Integer
    );

    procedure Clear;

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function GetPoints: PDoublePointArray;
    property Points: PDoublePointArray read GetPoints;

    function GetMeta: PDoublePointsMeta;
    property Meta: PDoublePointsMeta read GetMeta;

    function MakeStaticAndClear: IDoublePoints;
    function MakeStaticCopy: IDoublePoints;
  end;

implementation

end.
