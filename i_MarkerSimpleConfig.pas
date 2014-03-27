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

unit i_MarkerSimpleConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataElement;

type
  IMarkerSimpleConfigStatic = interface
    ['{EBE59B49-48A8-4657-AF1D-9C0951D5AEA9}']
    function GetMarkerSize: Integer;
    property MarkerSize: Integer read GetMarkerSize;

    function GetMarkerColor: TColor32;
    property MarkerColor: TColor32 read GetMarkerColor;

    function GetBorderColor: TColor32;
    property BorderColor: TColor32 read GetBorderColor;
  end;

  IMarkerSimpleConfig = interface(IConfigDataElement)
    ['{77A05655-3105-400E-90A2-CF24DE062F0A}']
    function GetMarkerSize: Integer;
    procedure SetMarkerSize(AValue: Integer);
    property MarkerSize: Integer read GetMarkerSize write SetMarkerSize;

    function GetMarkerColor: TColor32;
    procedure SetMarkerColor(AValue: TColor32);
    property MarkerColor: TColor32 read GetMarkerColor write SetMarkerColor;

    function GetBorderColor: TColor32;
    procedure SetBorderColor(AValue: TColor32);
    property BorderColor: TColor32 read GetBorderColor write SetBorderColor;

    function GetStatic: IMarkerSimpleConfigStatic;
  end;

implementation

end.


