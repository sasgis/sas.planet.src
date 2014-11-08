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

unit i_MapLayerGridsConfig;

interface

uses
  t_Bitmap32,
  t_GeoTypes,
  i_LocalCoordConverter,
  i_ThreadConfig,
  i_ConfigDataElement;

type
  IBaseGridConfig = interface(IConfigDataElement)
    ['{A1E36D4D-2237-474E-A554-E47434449AA3}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetGridColor: TColor32;
    procedure SetGridColor(AValue: TColor32);
    property GridColor: TColor32 read GetGridColor write SetGridColor;

    function GetShowText: Boolean;
    procedure SetShowText(AValue: Boolean);
    property ShowText: Boolean read GetShowText write SetShowText;

    function GetPointStickToGrid(
      const ALocalConverter: ILocalCoordConverter;
      const ASourceLonLat: TDoublePoint
    ): TDoublePoint;
    function GetRectStickToGrid(
      const ALocalConverter: ILocalCoordConverter;
      const ASourceRect: TDoubleRect
    ): TDoubleRect;
  end;

  ITileGridConfig = interface(IBaseGridConfig)
    ['{55B99C82-8734-450D-A4C7-8150A23FF39C}']
    function GetUseRelativeZoom: Boolean;
    procedure SetUseRelativeZoom(AValue: Boolean);
    property UseRelativeZoom: Boolean read GetUseRelativeZoom write SetUseRelativeZoom;

    function GetZoom: Integer;
    procedure SetZoom(AValue: Integer);
    property Zoom: Integer read GetZoom write SetZoom;

    function GetActualZoom(const ALocalConverter: ILocalCoordConverter): Byte;
  end;

  IGenShtabGridConfig = interface(IBaseGridConfig)
    ['{5E125B9F-5D31-421B-B6BF-B7535123B18F}']
    function GetScale: Integer;
    procedure SetScale(AValue: Integer);
    property Scale: Integer read GetScale write SetScale;
  end;

  IDegreeGridConfig = interface(IBaseGridConfig)
    ['{8592549C-0DC8-4B07-8262-2A802FF6CF24}']
    function GetScale: Double;
    procedure SetScale(AValue: Double);
    property Scale: Double read GetScale write SetScale;
  end;

  IMapLayerGridsConfig = interface(IConfigDataElement)
    ['{55B99C82-8734-450D-A4C7-8150A23FF39C}']
    function GetTileGrid: ITileGridConfig;
    property TileGrid: ITileGridConfig read GetTileGrid;

    function GetGenShtabGrid: IGenShtabGridConfig;
    property GenShtabGrid: IGenShtabGridConfig read GetGenShtabGrid;

    function GetDegreeGrid: IDegreeGridConfig;
    property DegreeGrid: IDegreeGridConfig read GetDegreeGrid;

    function GetThreadConfig: IThreadConfig;
    property ThreadConfig: IThreadConfig read GetThreadConfig;
  end;

implementation

end.
