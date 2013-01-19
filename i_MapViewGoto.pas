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

unit i_MapViewGoto;

interface

uses
  t_GeoTypes,
  i_Notifier,
  i_MarksSimple;

type
  IGotoPosStatic = interface
    ['{D9988166-EFD3-4C84-B43C-B0FE95194FB1}']
    function GetLonLat: TDoublePoint;
    property LonLat: TDoublePoint read GetLonLat;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetGotoTime: TDateTime;
    property GotoTime: TDateTime read GetGotoTime;
  end;

  IMapViewGoto = interface
    ['{33FDD537-B089-4ED6-8AB4-720E47B3C8B8}']
    procedure GotoPos(
      const ALonLat: TDoublePoint;
      const AZoom: Byte
    );
    procedure FitRectToScreen(
      const ALonLatRect: TDoubleRect;
      AShowMarker: boolean
    );
    procedure FitLineToScreen(
      const ALonLatPath: IMarkLine;
      AShowMarker: boolean
    );
    function GetLastGotoPos: IGotoPosStatic;
    property LastGotoPos: IGotoPosStatic read GetLastGotoPos;

    function GetChangeNotifier: INotifier;
    property ChangeNotifier: INotifier read GetChangeNotifier;
  end;

implementation

end.
