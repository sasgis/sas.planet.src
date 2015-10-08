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

unit i_GoogleEarthTerrain deprecated;

interface

const
  IID_IGoogleEarthTerrain: TGUID = '{A045855E-05DD-4BFD-9775-39567DD68444}';

type
  IGoogleEarthTerrain = interface(IInterface)
    ['{25229FC3-C973-462C-BF08-9ED5CC74E695}']
    procedure Open(
      const ATileData: PByte;
      const ATileSize: Integer
    ); safecall;
    function Elevation(
      const ALon: Double;
      const ALat: Double
    ): Single; safecall;
  end;

implementation

end.
