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

unit u_GpxFakeTimeGenerator;

interface

uses
  i_GeoCalc,
  i_GeometryLonLat,
  t_GeoTypes,
  u_BaseInterfacedObject;

type
  IGpxFakeTimeGenerator = interface
    ['{69CB8437-8624-447C-8F9A-268BCEA60EBE}']

    function GetItemTimeStamp(const AIndex: Integer): TDateTime;
    property TimeStamp[const AIndex: Integer]: TDateTime read GetItemTimeStamp;
  end;

  TGpxFakeTimeGenerator = class(TBaseInterfacedObject, IGpxFakeTimeGenerator)
  private
    FStart: TDateTime;
    FDelta: TDateTime;
    FTimes: array of TDateTime;

    FNowUtc: TDateTime;
    FGeoCalc: IGeoCalc;
    FLines: array of IGeometryLonLatSingleLine;

    FIsInitilized: Boolean;

    procedure LazyInit;
    function IsMetaAvailable: Boolean;
    procedure DoInitTimesByMeta(const APointsCount: Integer);
  private
    { IGpxFakeTimeGenerator }
    function GetItemTimeStamp(const AIndex: Integer): TDateTime;
  public
    constructor Create(
      const ANowUtc: TDateTime;
      const AGeoCalc: IGeoCalc;
      const ALine: IGeometryLonLatLine
    );
  end;

implementation

uses
  DateUtils,
  SysUtils;

const
  CDummySpeedKMH = 5; // dummy speed in km per hour
  CDummySpeedMS  = CDummySpeedKMH * 1000 / (60 * 60); // dummy speed in meters per second

{ TGpxFakeTimeGenerator }

constructor TGpxFakeTimeGenerator.Create(
  const ANowUtc: TDateTime;
  const AGeoCalc: IGeoCalc;
  const ALine: IGeometryLonLatLine
);
var
  I: Integer;
  VLine: IGeometryLonLatSingleLine;
  VLines: IGeometryLonLatMultiLine;
begin
  Assert(AGeoCalc <> nil);
  Assert(ALine <> nil);

  inherited Create;

  FNowUtc := ANowUtc;
  FGeoCalc := AGeoCalc;

  if Supports(ALine, IGeometryLonLatSingleLine, VLine) then begin
    SetLength(FLines, 1);
    FLines[0] := VLine;
  end else
  if Supports(ALine, IGeometryLonLatMultiLine, VLines) then begin
    SetLength(FLines, VLines.Count);
    for I := 0 to VLines.Count - 1 do begin
      FLines[I] := VLines.Item[I];
    end;
  end else begin
    raise Exception.Create('Unexpected GeometryLonLatLine type!');
  end;

  FIsInitilized := False;
end;

function TGpxFakeTimeGenerator.GetItemTimeStamp(
  const AIndex: Integer
): TDateTime;
begin
  if not FIsInitilized then begin
    LazyInit;
  end;

  if Length(FTimes) > 0 then begin
    Result := FTimes[AIndex];
  end else begin
    Result := FStart + FDelta * AIndex;
  end;
end;

procedure TGpxFakeTimeGenerator.LazyInit;
var
  I: Integer;
  VDist: Double;
  VCount: Integer;
begin
  Assert(not FIsInitilized);
  FIsInitilized := True;

  VDist := 0;
  VCount := 0;

  for I := 0 to Length(FLines) - 1 do begin
    VDist := VDist + FGeoCalc.CalcSingleLineLength(FLines[I]);
    Inc(VCount, FLines[I].Count);
  end;

  FStart := IncSecond(FNowUtc, -Round(VDist / CDummySpeedMS));
  FDelta := (FNowUtc - FStart) / VCount;

  if IsMetaAvailable then begin
    DoInitTimesByMeta(VCount);
  end;
end;

function TGpxFakeTimeGenerator.IsMetaAvailable: Boolean;
var
  I, J: Integer;
  VTimeStamp: PArrayOfDateTime;
begin
  Result := False;
  for I := 0 to Length(FLines) - 1 do begin
    if (FLines[I].Meta <> nil) and (FLines[I].Meta.TimeStamp <> nil) then begin
      VTimeStamp := FLines[I].Meta.TimeStamp;
      for J := 0 to FLines[I].Count - 1 do begin
        Result := VTimeStamp[J] <> 0;
        if Result then begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TGpxFakeTimeGenerator.DoInitTimesByMeta(const APointsCount: Integer);
var
  I, J: Integer;
  VCount: Integer;
  VDelta: TDateTime;
  VPrev: TDateTime;
  VNext: TDateTime;
  VPrevIndex: Integer;
  VNextIndex: Integer;
begin
  SetLength(FTimes, APointsCount);

  J := 0;
  for I := 0 to Length(FLines) - 1 do begin
    if (FLines[I].Meta <> nil) and (FLines[I].Meta.TimeStamp <> nil) then begin
      Move(FLines[I].Meta.TimeStamp[0], FTimes[J], FLines[I].Count * SizeOf(FTimes[0]));
    end;
    Inc(J, FLines[I].Count);
  end;

  I := 0;
  while I < Length(FTimes) do begin

    if FTimes[I] <> 0 then begin
      Inc(I);
      Continue;
    end;

    // find the next valid time
    VNext := 0;
    VNextIndex := 0;
    VPrevIndex := I;

    if I > 0 then begin
      VPrev := FTimes[I-1];
    end else begin
      VPrev := 0;
    end;

    for J := VPrevIndex + 1 to Length(FTimes) - 1 do begin
      if FTimes[J] <> 0 then begin
        VNext := FTimes[J];
        VNextIndex := J;
        Break;
      end;
    end;

    // fill in the gaps
    if VNext = 0 then begin
      // at the end
      Assert(VPrevIndex > 0);
      for J := VPrevIndex to Length(FTimes) - 1 do begin
        FTimes[J] := FTimes[J-1] + FDelta;
      end;
      Exit;
    end else begin
      if VPrevIndex > 0 then begin
        // in the middle
        VCount := VNextIndex - VPrevIndex;
        VDelta := (VNext - VPrev) / VCount;
        for J := VPrevIndex to VNextIndex - 1 do begin
          FTimes[J] := FTimes[J-1] + VDelta;
        end;
        Inc(I, VCount);
      end else begin
        // at the start
        for J := VNextIndex - 1 downto 0 do begin
          FTimes[J] := FTimes[J+1] - FDelta;
        end;
        Inc(I, VNextIndex);
      end;
    end;
  end;
end;

end.
