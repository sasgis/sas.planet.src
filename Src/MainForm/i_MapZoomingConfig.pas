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

unit i_MapZoomingConfig;

interface

uses
  i_ConfigDataElement;

type
  IMapZoomingConfig = interface(IConfigDataElement)
    ['{A322104E-A247-4EB3-83F6-C897F64E764C}']
    // Фиксировать центр изменения масштаба под курсором мыши
    function GetZoomingAtMousePos: Boolean;
    procedure SetZoomingAtMousePos(AValue: Boolean);
    property ZoomingAtMousePos: Boolean read GetZoomingAtMousePos write SetZoomingAtMousePos;

    //Анимированный зум
    function GetAnimateZoom: Boolean;
    procedure SetAnimateZoom(AValue: Boolean);
    property AnimateZoom: Boolean read GetAnimateZoom write SetAnimateZoom;

    function GetAnimateZoomTime: Cardinal;
    procedure SetAnimateZoomTime(AValue: Cardinal);
    property AnimateZoomTime: Cardinal read GetAnimateZoomTime write SetAnimateZoomTime;
  end;

implementation

end.
