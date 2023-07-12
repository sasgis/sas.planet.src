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
  TGeogCoordShowFormatCaption = array [TGeogCoordShowFormat] of string;

function GetGeogCoordShowFormatCaption: TGeogCoordShowFormatCaption;

function GeogCoordShowFormatToInteger(const AValue: TGeogCoordShowFormat): Integer;
function IntegerToGeogCoordShowFormat(const AValue: Integer): TGeogCoordShowFormat;

type
  TProjCoordShowFormatCaption = array [TProjCoordShowFormat] of string;

function GetProjCoordShowFormatCaption: TProjCoordShowFormatCaption;

function ProjCoordShowFormatToInteger(const AValue: TProjCoordShowFormat): Integer;
function IntegerToProjCoordShowFormat(const AValue: Integer): TProjCoordShowFormat;

implementation

uses
  gnugettext;

function GetCoordSysTypeCaption: TCoordSysTypeCaption;
begin
  Result[cstWGS84]  := _('WGS 84 / Geographic');

  Result[cstSK42]   := _('SK-42 (Pulkovo-1942) / Geographic');
  Result[cstSK42GK] := _('SK-42 / Gauss-Kruger (6 degree zones)');

  Result[cstUTM]    := _('WGS 84 / UTM (6 degree zones)');
  Result[cstMGRS]   := _('WGS 84 / Military Grid Reference System');
end;

function GetCoordSysTypeCaptionShort: TCoordSysTypeCaption;
begin
  Result[cstWGS84]  := 'WGS 84';

  Result[cstSK42]   := _('SK-42');
  Result[cstSK42GK] := _('SK-42 / GK');

  Result[cstUTM]    := 'WGS 84 / UTM';
  Result[cstMGRS]   := 'WGS 84 / MGRS';
end;

function GetUPSCoordSysTypeCaptionShort: string;
begin
  Result := 'WGS 84 / UPS';
end;

function GetGeogCoordShowFormatCaption: TGeogCoordShowFormatCaption;
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
  CGeogCoordShowFormatId: array[TGeogCoordShowFormat] of Integer = (
    0, 1, 2, 21, 3, 4, 5, 51
  );

function GeogCoordShowFormatToInteger(const AValue: TGeogCoordShowFormat): Integer;
begin
  Result := CGeogCoordShowFormatId[AValue];
end;

function IntegerToGeogCoordShowFormat(const AValue: Integer): TGeogCoordShowFormat;
var
  I: TGeogCoordShowFormat;
begin
  for I := Low(TGeogCoordShowFormat) to High(TGeogCoordShowFormat) do begin
    if CGeogCoordShowFormatId[I] = AValue then begin
      Result := I;
      Exit;
    end;
  end;

  Result := Low(TGeogCoordShowFormat);
end;

function GetProjCoordShowFormatCaption: TProjCoordShowFormatCaption;
begin
  Result[csfWhole]         := '12345';
  Result[csfWholeWithAxis] := '12345N';

  Result[csfExact]         := '12345.000';
  Result[csfExactWithAxis] := '12345.000N';
end;

const
  CProjCoordShowFormatId: array[TProjCoordShowFormat] of Integer = (
    0, 1, 2, 3
  );

function ProjCoordShowFormatToInteger(const AValue: TProjCoordShowFormat): Integer;
begin
  Result := Integer(AValue);
end;

function IntegerToProjCoordShowFormat(const AValue: Integer): TProjCoordShowFormat;
var
  I: TProjCoordShowFormat;
begin
  for I := Low(TProjCoordShowFormat) to High(TProjCoordShowFormat) do begin
    if CProjCoordShowFormatId[I] = AValue then begin
      Result := I;
      Exit;
    end;
  end;

  Result := Low(TProjCoordShowFormat);
end;

end.
