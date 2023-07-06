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

unit u_CoordRepresentation;

interface

uses
  t_CoordRepresentation;

type
  TCoordSysTypeCaption = array [TCoordSysType] of string;

function GetCoordSysTypeCaption: TCoordSysTypeCaption;
function GetCoordSysTypeCaptionShort: TCoordSysTypeCaption;

function GetUPSCoordSysTypeCaptionShort: string;

type
  TDegrShowFormatCaption = array [TDegrShowFormat] of string;

function GetDegrShowFormatCaption: TDegrShowFormatCaption;

function DegrShowFormatToId(const AValue: TDegrShowFormat): Integer;
function IdToDegrShowFormat(const AValue: Integer): TDegrShowFormat;

implementation

uses
  gnugettext;

function GetCoordSysTypeCaption: TCoordSysTypeCaption;
begin
  Result[cstWGS84]  := _('WGS 84 / Geographic');
  Result[cstUTM]    := _('WGS 84 / UTM (6 degree zones)');

  Result[cstSK42]   := _('SK-42 (Pulkovo-1942) / Geographic');
  Result[cstSK42GK] := _('SK-42 / Gauss-Kruger (6 degree zones)');
end;

function GetCoordSysTypeCaptionShort: TCoordSysTypeCaption;
begin
  Result[cstWGS84]  := 'WGS 84';
  Result[cstUTM]    := 'WGS 84 / UTM';

  Result[cstSK42]   := _('SK-42');
  Result[cstSK42GK] := _('SK-42 / GK');
end;

function GetUPSCoordSysTypeCaptionShort: string;
begin
  Result := 'WGS 84 / UPS';
end;

function GetDegrShowFormatCaption: TDegrShowFormatCaption;
begin
  Result[dshCharDegrMinSec] := _('WS deg.min.sec. (W12°12''12.1234")');
  Result[dshCharDegrMin]    := _('WS deg.min. (W12°12.123456'')');
  Result[dshCharDegr]       := _('WS deg. (W12.12345678°)');
  Result[dshCharDegr2]      := _('WS deg. (W12.12345678)');

  Result[dshSignDegrMinSec] := _('-- deg.min.sec. (-12°12''12.1234")');
  Result[dshSignDegrMin]    := _('-- deg.min. (-12°12.1234'')');
  Result[dshSignDegr]       := _('-- deg. (-12.12345678°)');
  Result[dshSignDegr2]      := _('-- deg. (-12.12345678)');
end;

const
  CDegrShowFormatId: array[TDegrShowFormat] of Integer = (
    0, 1, 2, 21, 3, 4, 5, 51
  );

function DegrShowFormatToId(const AValue: TDegrShowFormat): Integer;
begin
  Result := CDegrShowFormatId[AValue];
end;

function IdToDegrShowFormat(const AValue: Integer): TDegrShowFormat;
var
  I: TDegrShowFormat;
begin
  for I := Low(TDegrShowFormat) to High(TDegrShowFormat) do begin
    if CDegrShowFormatId[I] = AValue then begin
      Result := I;
      Exit;
    end;
  end;

  Result := Low(TDegrShowFormat);
end;

end.
