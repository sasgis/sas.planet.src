{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit i_MergePolygonsProgress;

interface

uses
  i_VectorDataItemSimple;

type
  IMergePolygonsProgress = interface
    ['{C764188E-C7D6-407C-980A-A6C509774316}']
    procedure ResetProgress;

    procedure GetProgress(
      out APolyCount: Integer;
      out AHolesCount: Integer;
      out ATime: Double;
      out AVectorItem: IVectorDataItem
    );

    procedure SetProgress(
      const APolyCount: Integer;
      const AHolesCount: Integer;
      const ATime: Double;
      const AVectorItem: IVectorDataItem
    );

    function GetFinished: Boolean;
    procedure SetFinished(const AValue: Boolean);
    property IsFinished: Boolean read GetFinished write SetFinished;

    function GetStartedAt: TDateTime;
    procedure SetStartedAt(const AValue: TDateTime);
    property StartedAt: TDateTime read GetStartedAt write SetStartedAt;
  end;

implementation

end.
