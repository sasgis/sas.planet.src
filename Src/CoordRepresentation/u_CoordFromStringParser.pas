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

unit u_CoordFromStringParser;

interface

uses
  Proj4.UTM,
  Proj4.GaussKruger,
  Proj4.Utils,
  t_GeoTypes,
  t_CoordRepresentation,
  i_CoordFromStringParser,
  i_CoordRepresentationConfig,
  u_CoordTransformer,
  u_BaseInterfacedObject;

type
  TCoordFromStringParser = class(TBaseInterfacedObject, ICoordFromStringParser)
  private
    FConfig: ICoordRepresentationConfig;
    FTransformer: TCoordTransformer;
  private
    { ICoordFromStringParser }
    function TryStrToCoord(
      const ALon: string;
      const ALat: string;
      out ACoord: TDoublePoint
    ): Boolean; overload;

    function TryStrToCoord(
      const AX: string;
      const AY: string;
      const AZone: string;
      out ACoord: TDoublePoint
    ): Boolean; overload;

    function TryStrToCoord(
      const AStr: string;
      out ACoord: TDoublePoint
    ): Boolean; overload;
  public
    constructor Create(
      const AConfig: ICoordRepresentationConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  StrUtils,
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
  VText := StringReplace(VText, 'Þ', '-', [rfReplaceAll]);
  VText := StringReplace(VText, 'Ç', '-', [rfReplaceAll]);
  VText := StringReplace(VText, 'Â', '+', [rfReplaceAll]);
  VText := StringReplace(VText, 'Ñ', '+', [rfReplaceAll]);
  minus := false;
  if posEx('-', VText, 1) > 0 then begin
    minus := true;
  end;

  i := 1;
  while i <= length(VText) do begin
    if not CharInSet(VText[i], ['0'..'9', '-', '+', '.', ',', ' ']) then begin
      VText[i] := ' ';
      dec(i);
    end;

    if ((i = 1) and (VText[i] = ' ')) or
      ((i = length(VText)) and (VText[i] = ' ')) or
      ((i < length(VText) - 1) and (VText[i] = ' ') and (VText[i + 1] = ' ')) or
      ((i > 1) and (VText[i] = ' ') and (not CharInSet(VText[i - 1], ['0'..'9']))) or
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
  FTransformer := TCoordTransformer.Create;
end;

destructor TCoordFromStringParser.Destroy;
begin
  FreeAndNil(FTransformer);
  inherited Destroy;
end;

function TCoordFromStringParser.TryStrToCoord(
  const ALon: string;
  const ALat: string;
  out ACoord: TDoublePoint
): Boolean;
var
  VCoordSysType: TCoordSysType;
begin
  Result := Edit2Digit(ALon, False, ACoord.X);
  if not Result then begin
    Exit;
  end;

  Result := Edit2Digit(ALat, True, ACoord.Y);
  if not Result then begin
    Exit;
  end;

  VCoordSysType := FConfig.CoordSysType;

  case VCoordSysType of
    cstWGS84: begin
      Result := True;
    end;

    cstSK42, cstGSK2011: begin
      Result := FTransformer[VCoordSysType].geog_to_wgs84(ACoord.X, ACoord.Y);
    end;
  else
    Result := False;
    Assert(False);
  end;
end;

function TCoordFromStringParser.TryStrToCoord(
  const AX: string;
  const AY: string;
  const AZone: string;
  out ACoord: TDoublePoint
): Boolean;

  procedure _GetZoneParts(out AZoneNumber: Integer; out AZoneLetter: Char);
  var
    P: PChar;
    I: Integer;
    VZone: string;
  begin
    VZone := StringReplace(UpperCase(AZone), ' ', '', [rfReplaceAll]);
    P := Pointer(VZone);

    I := 0;
    while P^ <> #0 do begin
      if AnsiChar(P^) in ['0'..'9'] then begin
        Inc(P);
        Inc(I);
      end else begin
        Break;
      end;
    end;

    if I > 0 then begin
      AZoneNumber := StrToInt( Copy(VZone, 1, I) );
    end else begin
      AZoneNumber := 0;
    end;

    AZoneLetter := P^;
  end;

  procedure _SwapXY(var X, Y: Double);
  var
    VTmp: Double;
  begin
    VTmp := X;
    X := Y;
    Y := VTmp;
  end;

var
  X, Y: Double;
  VUtm: TUtmCoord;
  VGauss: TGaussKrugerCoord;
  VZoneNumber: Integer;
  VZoneLetter: Char;
  VCoordSysType: TCoordSysType;
begin
  Result := False;

  X := str2r(AX);
  Y := str2r(AY);

  _GetZoneParts(VZoneNumber, VZoneLetter);

  VCoordSysType := FConfig.CoordSysType;

  case VCoordSysType of
    cstSK42GK, cstGSK2011GK: begin
      if not (VZoneNumber in [1..60]) or not (AnsiChar(VZoneLetter) in ['N', 'S']) then begin
        Exit;
      end;

      _SwapXY(X, Y);

      VGauss.Zone := VZoneNumber;
      VGauss.IsNorth := VZoneLetter = 'N';
      VGauss.X := X;
      VGauss.Y := Y;

      Result := FTransformer[VCoordSysType].proj_to_wgs84(VGauss, ACoord.X, ACoord.Y);
    end;

    cstUTM: begin
      if not (VZoneNumber in [0..60]) or not (AnsiChar(VZoneLetter) in ['N', 'S']) then begin
        Exit;
      end;

      if VZoneNumber = 0 then begin
        _SwapXY(X, Y);
      end;

      VUtm.Zone := VZoneNumber;
      VUtm.Band := #0;
      VUtm.IsNorth := VZoneLetter = 'N';
      VUtm.X := X;
      VUtm.Y := Y;

      Result := utm_to_wgs84(VUtm, ACoord.X, ACoord.Y);
    end;
  else
    Assert(False);
  end;
end;

function TCoordFromStringParser.TryStrToCoord(
  const AStr: string;
  out ACoord: TDoublePoint
): Boolean;
var
  VMgrs: TMgrsCoord;
begin
  case FConfig.CoordSysType of
    cstMGRS: begin
      Result :=
        str_to_mgrs(AStr, VMgrs) and
        mgrs_to_wgs84(VMgrs, ACoord.X, ACoord.Y);
    end;
  else
    Result := False;
    Assert(False);
  end;
end;

end.
