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

unit i_MapMovingConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapMovingConfig = interface(IConfigDataElement)
    ['{A322104E-A247-4EB3-83F6-C897F64E764C}']
    //Анимация инерции
    function GetAnimateMove: Boolean;
    procedure SetAnimateMove(AValue: Boolean);
    property AnimateMove: Boolean read GetAnimateMove write SetAnimateMove;

    function GetAnimateMoveTime: Cardinal;
    procedure SetAnimateMoveTime(AValue: Cardinal);
    property AnimateMoveTime: Cardinal read GetAnimateMoveTime write SetAnimateMoveTime;

    function GetAnimateMaxStartSpeed: Cardinal;
    procedure SetAnimateMaxStartSpeed(AValue: Cardinal);
    property AnimateMaxStartSpeed: Cardinal read GetAnimateMaxStartSpeed write SetAnimateMaxStartSpeed;

    function GetAnimateMinStartSpeed: Cardinal;
    procedure SetAnimateMinStartSpeed(AValue: Cardinal);
    property AnimateMinStartSpeed: Cardinal read GetAnimateMinStartSpeed write SetAnimateMinStartSpeed;
  end;

implementation

end.
