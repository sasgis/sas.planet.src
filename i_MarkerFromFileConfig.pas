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

unit i_MarkerFromFileConfig;

interface


uses
  t_GeoTypes,
  i_ConfigDataElement;

type
  TAnchorType =
    (
      atCenterPoint = 0,
      atTopLeftCorner = 1,
      atCenterOfTopBorder = 2,
      atTopRightCorner = 3,
      atCenterOfRightBorder = 4,
      atBottomRightCorner = 5,
      atCenterOfBottomBorder = 6,
      atBottomLeftCorner = 7,
      atCenterOfLeftBorder = 8,
      atFixedPoint = 9
    );

  IMarkerFromFileConfigStatic = interface
    ['{EBE59B49-48A8-4657-AF1D-9C0951D5AEA9}']
    function GetFileName: string;
    property FileName: string read GetFileName;

    function GetAnchorType: TAnchorType;
    property AnchorType: TAnchorType read GetAnchorType;

    function GetFixedPoint: TDoublePoint;
    property FixedPoint: TDoublePoint read GetFixedPoint;
  end;

  IMarkerFromFileConfig = interface(IConfigDataElement)
    ['{77A05655-3105-400E-90A2-CF24DE062F0A}']
    function GetFileName: string;
    procedure SetFileName(const AValue: string);
    property FileName: string read GetFileName write SetFileName;

    function GetAnchorType: TAnchorType;
    procedure SetAnchorType(const AValue: TAnchorType);
    property AnchorType: TAnchorType read GetAnchorType write SetAnchorType;

    function GetFixedPoint: TDoublePoint;
    procedure SetFixedPoint(const AValue: TDoublePoint);
    property FixedPoint: TDoublePoint read GetFixedPoint write SetFixedPoint;

    function GetStatic: IMarkerFromFileConfigStatic;
  end;

implementation

end.
