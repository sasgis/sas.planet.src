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

unit i_CalcLineLayerConfig;

interface

uses
  GR32,
  i_PolyLineLayerConfig;

type
  ICalcLineLayerConfig = interface(IPolyLineLayerConfig)
    ['{6D7DFAAC-D654-4CB2-8073-5C33A0DBEE12}']
    function GetLenShow: Boolean;
    procedure SetLenShow(const AValue: Boolean);
    property LenShow: Boolean read GetLenShow write SetLenShow;

    function GetTextColor: TColor32;
    procedure SetTextColor(const AValue: TColor32);
    property TextColor: TColor32 read GetTextColor write SetTextColor;

    function GetTextBGColor: TColor32;
    procedure SetTextBGColor(const AValue: TColor32);
    property TextBGColor: TColor32 read GetTextBGColor write SetTextBGColor;
 end;

implementation

end.
