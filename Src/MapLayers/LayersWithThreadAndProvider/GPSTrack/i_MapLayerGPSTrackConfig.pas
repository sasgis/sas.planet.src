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

unit i_MapLayerGPSTrackConfig;

interface

uses
  t_Bitmap32,
  i_ThreadConfig,
  i_ConfigDataElement;

type
  ITrackColorerStatic = interface
    ['{8DB3AAF8-D665-40C1-AC21-B8027347F95A}']
    function GetColorForSpeed(const ASpeed: Double): TColor32;
  end;

  ISpeedRangeItem = interface
    ['{2D50B901-BFBA-42F8-8303-BEAB3342E865}']
    function GetSpeed: Double;
    function GetMinSpeedColor: TColor32;
    function GetMaxSpeedColor: TColor32;
  end;

  ITrackColorerConfig = interface(IConfigDataElement)
    ['{46E030C7-F9E1-45F5-914E-20B240238261}']
    function GetStatic: ITrackColorerStatic;

    function GetSpeedRangeCount: Integer;

    function GetSpeedRangeItem(AIndex: Integer): ISpeedRangeItem;
    function AddSpeedRangeItem(
      const ASpeed: Double;
      AMinColor, AMaxColor: TColor32
    ): Integer;
    procedure ClearItems;
  end;

  IMapLayerGPSTrackConfig = interface(IConfigDataElement)
    ['{5F9D5FD1-B40B-451A-B544-11C93A2B6532}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetLineWidth: Double;
    procedure SetLineWidth(AValue: Double);
    property LineWidth: Double read GetLineWidth write SetLineWidth;

    function GetLastPointCount: Integer;
    procedure SetLastPointCount(AValue: Integer);
    property LastPointCount: Integer read GetLastPointCount write SetLastPointCount;

    function GetTrackColorerConfig: ITrackColorerConfig;
    property TrackColorerConfig: ITrackColorerConfig read GetTrackColorerConfig;

    function GetThreadConfig: IThreadConfig;
    property ThreadConfig: IThreadConfig read GetThreadConfig;
  end;

implementation

end.
