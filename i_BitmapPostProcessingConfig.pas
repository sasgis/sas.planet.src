{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit i_BitmapPostProcessingConfig;

interface

uses
  i_Bitmap32Static,
  i_ConfigDataElement;

type
  IBitmapPostProcessingConfigStatic = interface
    ['{3DBBF6CA-6AA3-4578-8D23-3E04D1D42C34}']
    function GetInvertColor: boolean;
    property InvertColor: boolean read GetInvertColor;

    // Число для гамма преобразования тайлов перед отображением
    function GetGammaN: Integer;
    property GammaN: Integer read GetGammaN;

    // Число для изменения контрастности тайлов перед отображением
    function GetContrastN: Integer;
    property ContrastN: Integer read GetContrastN;

    function Process(const ABitmap: IBitmap32Static): IBitmap32Static;
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
