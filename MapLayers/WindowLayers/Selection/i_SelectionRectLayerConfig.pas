{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit i_SelectionRectLayerConfig;

interface

uses
  GR32,
  i_ConfigDataElement;

type
  ISelectionRectLayerConfig = interface(IConfigDataElement)
    ['{B8253870-8613-444C-B45C-47FD420B7EFB}']
    function GetFillColor: TColor32;
    procedure SetFillColor(AValue: TColor32);
    property FillColor: TColor32 read GetFillColor write SetFillColor;

    function GetBorderColor: TColor32;
    procedure SetBorderColor(AValue: TColor32);
    property BorderColor: TColor32 read GetBorderColor write SetBorderColor;

    function GetZoomDeltaCount: Integer;
    procedure SetZoomDeltaCount(AValue: Integer);
    property ZoomDeltaCount: Integer read GetZoomDeltaCount write SetZoomDeltaCount;

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
    property FontSize: Integer read GetFontSize write SetFontSize;

    function GetZoomDeltaColor(AIndex: Integer): TColor32;
    procedure SetZoomDeltaColor(
      AIndex: Integer;
      AValue: TColor32
    );
    property ZoomDeltaColor[AIndex: Integer]: TColor32 read GetZoomDeltaColor write SetZoomDeltaColor;

    function GetZoomDeltaColors: TArrayOfColor32;
    property ZoomDeltaColors: TArrayOfColor32 read GetZoomDeltaColors;
  end;

implementation

end.
