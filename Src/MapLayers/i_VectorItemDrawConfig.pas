{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit i_VectorItemDrawConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataElement;

type
  IVectorItemDrawConfigStatic = interface
    ['{8896BCE3-39C2-4481-B61C-197EAB16E3E9}']
    function GetMainColor: TColor32;
    property MainColor: TColor32 read GetMainColor;

    function GetShadowColor: TColor32;
    property ShadowColor: TColor32 read GetShadowColor;
  end;

  IVectorItemDrawConfig = interface(IConfigDataElement)
    ['{41BE92C7-D58B-4CBD-B14E-245C5C7AA9D6}']
    function GetMainColor: TColor32;
    procedure SetMainColor(AValue: TColor32);
    property MainColor: TColor32 read GetMainColor write SetMainColor;

    function GetShadowColor: TColor32;
    procedure SetShadowColor(AValue: TColor32);
    property ShadowColor: TColor32 read GetShadowColor write SetShadowColor;

    function GetStatic: IVectorItemDrawConfigStatic;
  end;

implementation

end.
