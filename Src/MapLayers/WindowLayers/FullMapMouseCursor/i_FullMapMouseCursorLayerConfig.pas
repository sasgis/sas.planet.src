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

unit i_FullMapMouseCursorLayerConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataElement;

type
  IFullMapMouseCursorLayerConfig = interface(IConfigDataElement)
    ['{B0B3C241-966E-47C7-9B8F-C80434B7868D}']
    function GetLineColor: TColor32;
    procedure SetLineColor(AValue: TColor32);
    property LineColor: TColor32 read GetLineColor write SetLineColor;

    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    property Enabled: Boolean read GetEnabled write SetEnabled;

    function GetShowAlways: Boolean;
    procedure SetShowAlways(AValue: Boolean);
    property ShowAlways: Boolean read GetShowAlways write SetShowAlways;
  end;

implementation

end.
