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

unit u_GeoTiffFunc;

interface

uses
  Types,
  Math;

type
  TGeoTiffFunc = record
    class function OverviewToZoom(
      const ABaseZoom: Byte;
      const AOverview: Integer;
      out AZoom: Byte
    ): Boolean; static;

    class function OverviewArrayToZoomArray(
      const ABaseZoom: Byte;
      const AOverviewArray: TIntegerDynArray
    ): TByteDynArray; static;

    class function ZoomArrayToOverviewArray(
      const ABaseZoom: Byte;
      const AZoomArray: TByteDynArray
    ): TIntegerDynArray; static;

    class function IsValidOverviewValue(
      const AValue: Integer
    ): Boolean; static; inline;
  end;

implementation

function IsPowerOfTwo(const AValue: Integer): Boolean; inline;
begin
  Result := (AValue > 0) and (AValue and (AValue - 1) = 0);
end;

{ TGeoTiffFunc }

class function TGeoTiffFunc.OverviewToZoom(
  const ABaseZoom: Byte;
  const AOverview: Integer;
  out AZoom: Byte
): Boolean;
var
  Z: Integer;
begin
  Z := Trunc(Log2(AOverview));
  if Z > ABaseZoom then begin
    Result := False;
  end else begin
    AZoom := ABaseZoom - Z;
    Result := True;
  end;
end;

class function TGeoTiffFunc.OverviewArrayToZoomArray(
  const ABaseZoom: Byte;
  const AOverviewArray: TIntegerDynArray
): TByteDynArray;
var
  I, J: Integer;
begin
  J := 0;
  SetLength(Result, Length(AOverviewArray));
  for I := 0 to Length(AOverviewArray) - 1 do begin
    if OverviewToZoom(ABaseZoom, AOverviewArray[I], Result[J]) then begin
      Inc(J);
    end;
  end;
  SetLength(Result, J);
end;

class function TGeoTiffFunc.ZoomArrayToOverviewArray(
  const ABaseZoom: Byte;
  const AZoomArray: TByteDynArray
): TIntegerDynArray;
var
  I: Integer;
begin
  SetLength(Result, Length(AZoomArray));
  for I := 0 to Length(AZoomArray) - 1 do begin
    Result[I] := 1 shl (ABaseZoom - AZoomArray[I]);
  end;
end;

class function TGeoTiffFunc.IsValidOverviewValue(
  const AValue: Integer
): Boolean;
begin
  Result := IsPowerOfTwo(AValue);
end;

end.
