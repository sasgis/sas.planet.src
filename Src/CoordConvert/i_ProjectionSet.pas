{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit i_ProjectionSet;

interface

uses
  t_Hash,
  i_ProjectionInfo;

type
  IProjectionSet = interface
    ['{4B23F7C1-A818-459C-BCD8-F28BA96EEC82}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function IsSame(const AProjectionSet: IProjectionSet): Boolean;

    function GetZoomCount: Byte;
    property ZoomCount: Byte read GetZoomCount;

    function GetZoom(const AIndex: Byte): IProjection;
    property Zooms[const AIndex: Byte]: IProjection read GetZoom; default;

    procedure ValidateZoom(var AZoom: Byte);
    function CheckZoom(const AZoom: Byte): Boolean;

    function GetSuitableProjection(const AProjection: IProjection): IProjection;
    function GetSuitableZoom(const AProjection: IProjection): Byte;
    function IsProjectionFromThisSet(const AProjection: IProjection): Boolean;
  end;

implementation

end.
