{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_CoordFromStringParser;

interface

uses
  t_GeoTypes,
  t_CoordRepresentation,
  i_CoordFromStringParser,
  i_CoordRepresentationConfig,
  u_BaseInterfacedObject;

type
  TCoordFromStringParser = class(TBaseInterfacedObject, ICoordFromStringParser)
  private
    FConfig: ICoordRepresentationConfig;
  private
    function TryStrToCoord(
      const ALon: string;
      const ALat: string;
      out ACoord: TDoublePoint
    ): Boolean; overload;

    function TryStrToCoord(
      const AX: string;
      const AY: string;
      const AZone: Integer;
      const AIsNorth: Boolean;
      out ACoord: TDoublePoint
    ): Boolean; overload;
  public
    constructor Create(
      const AConfig: ICoordRepresentationConfig
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  Proj4,
  u_GeoToStrFunc;

function Edit2Digit(
  const Atext: string;
  lat: boolean;
  out res: Double
): boolean;
var
  i, delitel: integer;
  gms: double;
  VText: string;
  minus: boolean;
begin
  result := true;
  res := 0;
  VText := UpperCase(Atext);

  VText := StringReplace(VText, 'S', '-', [rfReplaceAll]);
  VText := StringReplace(VText, 'W', '-', [rfReplaceAll]);
  VText := StringReplace(VText, 'N', '+', [rfReplaceAll]);
  VText := StringReplace(VText, 'E', '+', [rfReplaceAll]);
  VText := StringReplace(VText, 'Ю', '-', [rfReplaceAll]);
  VText := StringReplace(VText, 'З', '-', [rfReplaceAll]);
  VText := StringReplace(VText, 'В', '+', [rfReplaceAll]);
  VText := StringReplace(VText, 'С', '+', [rfReplaceAll]);
  minus := false;
  if posEx('-', VText, 1) > 0 then begin
    minus := true;
  end;

  i := 1;
  while i <= length(VText) do begin
    if (not (AnsiChar(VText[i]) in ['0'..'9', '-', '+', '.', ',', ' '])) then begin
      VText[i] := ' ';
      dec(i);
    end;

    if ((i = 1) and (VText[i] = ' ')) or
      ((i = length(VText)) and (VText[i] = ' ')) or
      ((i < length(VText) - 1) and (VText[i] = ' ') and (VText[i + 1] = ' ')) or
      ((i > 1) and (VText[i] = ' ') and (not (AnsiChar(VText[i - 1]) in ['0'..'9']))) or
      ((i < length(VText) - 1) and (VText[i] = ',') and (VText[i + 1] = ' ')) then begin
      Delete(VText, i, 1);
      dec(i);
    end;
    inc(i);
  end;

  try
    res := 0;
    delitel := 1;
    repeat
      i := posEx(' ', VText, 1);
      if i = 0 then begin
        gms := str2r(VText);
      end else begin
        gms := str2r(copy(VText, 1, i - 1));
        Delete(VText, 1, i);
      end;
      if ((delitel > 1) and (abs(gms) > 60)) or
        ((delitel = 1) and (lat) and (abs(gms) > 90)) or
        ((delitel = 1) and (not lat) and (abs(gms) > 180)) then begin
        Result := false;
      end;
      if res < 0 then begin
        res := res - gms / delitel;
      end else begin
        res := res + gms / delitel;
      end;
      if minus and (res > 0) then begin
        res := -res;
      end;
      delitel := delitel * 60;
    until (i = 0) or (delitel > 3600) or (not result);
  except
    result := false;
  end;
end;

{ TCoordFromStringParser }

constructor TCoordFromStringParser.Create(
  const AConfig: ICoordRepresentationConfig
);
begin
  Assert(AConfig <> nil);
  inherited Create;
  FConfig := AConfig;
end;

function TCoordFromStringParser.TryStrToCoord(
  const ALon: string;
  const ALat: string;
  out ACoord: TDoublePoint
): Boolean;
var
  VCoord: TDoublePoint;
begin
  Result := Edit2Digit(ALon, False, VCoord.X);
  if not Result then begin
    Exit;
  end;

  Result := Edit2Digit(ALat, True, VCoord.Y);
  if not Result then begin
    Exit;
  end;

  Result := False;
  case FConfig.CoordSysType of
    cstWGS84: Result := True;
    cstSK42: Result := geodetic_sk42_to_wgs84(VCoord.X, VCoord.Y);
  else
    Assert(False);
  end;

  if Result then begin
    ACoord := VCoord;
  end;
end;

function TCoordFromStringParser.TryStrToCoord(
  const AX: string;
  const AY: string;
  const AZone: Integer;
  const AIsNorth: Boolean;
  out ACoord: TDoublePoint
): Boolean;
var
  X, Y: Double;
  VCoord: TDoublePoint;
begin
  Result := TryStrToFloat(AX, X);
  if not Result then begin
    Exit;
  end;

  Result := TryStrToFloat(AY, Y);
  if not Result then begin
    Exit;
  end;

  Result := False;
  case FConfig.CoordSysType of
    cstSK42GK: Result := gauss_kruger_to_wgs84(X, Y, AZone, AIsNorth, VCoord.X, VCoord.Y);
  else
    Assert(False);
  end;

  if Result then begin
    ACoord := VCoord;
  end;
end;

end.
