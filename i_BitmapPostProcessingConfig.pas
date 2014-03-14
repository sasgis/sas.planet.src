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

unit i_BitmapPostProcessingConfig;

interface

uses
  i_ConfigDataElement;

type
  IBitmapPostProcessingConfigStatic = interface
    ['{6CE9D4D8-4CCB-4E1A-8CEB-78BDA0FB65BF}']
    function GetInvertColor: boolean;
    property InvertColor: boolean read GetInvertColor;

    function GetGammaN: Integer;
    property GammaN: Integer read GetGammaN;

    function GetContrastN: Integer;
    property ContrastN: Integer read GetContrastN;
  end;

  IBitmapPostProcessingConfig = interface(IConfigDataElement)
    ['{3CF3CE21-3488-495C-9A17-A2164763342E}']
    function GetInvertColor: boolean;
    procedure SetInvertColor(const AValue: boolean);
    property InvertColor: boolean read GetInvertColor write SetInvertColor;

    function GetGammaN: Integer;
    procedure SetGammaN(const AValue: Integer);
    property GammaN: Integer read GetGammaN write SetGammaN;

    function GetContrastN: Integer;
    procedure SetContrastN(const AValue: Integer);
    property ContrastN: Integer read GetContrastN write SetContrastN;

    function GetStatic: IBitmapPostProcessingConfigStatic;
  end;

implementation

end.
