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

unit i_NavigationToPoint;

interface

uses
  t_GeoTypes,
  i_ConfigDataElement;

type
  INavigationToPoint = interface(IConfigDataElement)
    ['{61EBB721-A3D9-402B-ACDC-FF3E2DA5C262}']
    function GetIsActive: Boolean;
    property IsActive: Boolean read GetIsActive;

    function GetMarkId: string;
    property MarkId: string read GetMarkId;

    function GetLonLat: TDoublePoint;
    property LonLat: TDoublePoint read GetLonLat;

    procedure StartNavToMark(
      const AMarkId: string;
      const APointLonLat: TDoublePoint
    );
    procedure StartNavLonLat(const APointLonLat: TDoublePoint);
    procedure StopNav;
  end;

implementation

end.
