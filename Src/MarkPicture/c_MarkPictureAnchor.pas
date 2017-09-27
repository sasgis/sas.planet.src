{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit c_MarkPictureAnchor;

interface

uses
  t_GeoTypes;

// Anchor values
const
  cPicAnchorTopLeft:     TDoublePoint = (X: 0;   Y: 0);
  cPicAnchorTop:         TDoublePoint = (X: 0.5; Y: 0);
  cPicAnchorTopRight:    TDoublePoint = (X: 1;   Y: 0);

  cPicAnchorLeft:        TDoublePoint = (X: 0;   Y: 0.5);
  cPicAnchorCenter:      TDoublePoint = (X: 0.5; Y: 0.5);
  cPicAnchorRight:       TDoublePoint = (X: 1;   Y: 0.5);

  cPicAnchorBottomLeft:  TDoublePoint = (X: 0;   Y: 1);
  cPicAnchorBottom:      TDoublePoint = (X: 0.5; Y: 1);
  cPicAnchorBottomRight: TDoublePoint = (X: 1;   Y: 1);

  cPicAnchorDefault:     TDoublePoint = (X: 0.5; Y: 1); // cPicAnchorBottom

// Anchor readable names
const
  cPicAnchorTopLeftName     = 'TopLeft';
  cPicAnchorTopName         = 'Top';
  cPicAnchorTopRightName    = 'TopRight';

  cPicAnchorLeftName        = 'Left';
  cPicAnchorCenterName      = 'Center';
  cPicAnchorRightName       = 'Right';

  cPicAnchorBottomLeftName  = 'BottomLeft';
  cPicAnchorBottomName      = 'Bottom';
  cPicAnchorBottomRightName = 'BottomRight';

implementation

end.
