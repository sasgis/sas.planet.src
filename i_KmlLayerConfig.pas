{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit i_KmlLayerConfig;

interface

uses
  GR32,
  i_ThreadConfig,
  i_ConfigDataElement;

type
  IKmlLayerConfig = interface(IConfigDataElement)
    ['{6EA3D5D6-3D9D-40DB-AD53-989920190477}']
    function GetMainColor: TColor32;
    procedure SetMainColor(AValue: TColor32);
    property MainColor: TColor32 read GetMainColor write SetMainColor;

    function GetPointColor: TColor32;
    procedure SetPointColor(AValue: TColor32);
    property PointColor: TColor32 read GetPointColor write SetPointColor;

    function GetShadowColor: TColor32;
    procedure SetShadowColor(AValue: TColor32);
    property ShadowColor: TColor32 read GetShadowColor write SetShadowColor;

    function GetThreadConfig: IThreadConfig;
    property ThreadConfig: IThreadConfig read GetThreadConfig;
  end;

implementation

end.
