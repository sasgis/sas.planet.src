{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_SunCalcProvider;

interface

uses
  t_GeoTypes,
  i_SunCalcConfig,
  i_SunCalcProvider,
  i_SunCalcDataProvider,
  u_TimeZoneInfo,
  u_ConfigDataElementBase;

type
  TSunCalcProvider = class(TConfigDataElementBaseEmptySaveLoad, ISunCalcProvider)
  private
    FLocation: TDoublePoint;
    FDateTime: TDateTime;
    FTz: TSunCalcTzInfo;
    FIsTzValid: Boolean;
    FTimeZoneInfo: TTimeZoneInfo;
    FDataProviderChangeable: ISunCalcDataProviderChangeable;
  private
    { ISunCalcProvider }
    function GetLocation: TDoublePoint;
    procedure SetLocation(const AValue: TDoublePoint);

    function GetDateTime: TDateTime;
    procedure SetDateTime(const AValue: TDateTime);

    procedure SetDateTimeFromLocalTime(const ALocalTime: TDateTime);
    function GetLocalTimeFromDateTime: TDateTime;

    function GetTzInfo(
      const AUTCTime: TDateTime;
      out ATzInfo: TSunCalcTzInfo
    ): Boolean;

    function GetTzOffset(
      const AUTCTime: TDateTime;
      out ATzOffset: Extended
    ): Boolean;

    function GetDataProviderChangeable: ISunCalcDataProviderChangeable;

    procedure Reset;
  public
    constructor Create(const AConfig: ISunCalcConfig);
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,
  DateUtils,
  u_GeoFunc,
  u_SunCalcDataProviderChangeable;

{ TSunCalcProvider }

constructor TSunCalcProvider.Create(const AConfig: ISunCalcConfig);
begin
  inherited Create;
  FDataProviderChangeable := TSunCalcDataProviderChangeable.Create(AConfig);
  FTimeZoneInfo := TTimeZoneInfo.Create;
  Reset;
end;

destructor TSunCalcProvider.Destroy;
begin
  FreeAndNil(FTimeZoneInfo);
  inherited;
end;

procedure TSunCalcProvider.Reset;
begin
  LockWrite;
  try
    FLocation := CEmptyDoublePoint;
    FDateTime := 0;
    FIsTzValid := False;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

function TSunCalcProvider.GetDataProviderChangeable: ISunCalcDataProviderChangeable;
begin
  Result := FDataProviderChangeable;
end;

function TSunCalcProvider.GetDateTime: TDateTime;
begin
  LockRead;
  try
    Result := FDateTime;
  finally
    UnlockRead;
  end;
end;

function TSunCalcProvider.GetLocalTimeFromDateTime: TDateTime;
begin
  LockRead;
  try
    Result := TTimeZoneInfo.UTCToLocalTime(FDateTime);
  finally
    UnlockRead;
  end;
end;

function TSunCalcProvider.GetLocation: TDoublePoint;
begin
  LockRead;
  try
    Result := FLocation;
  finally
    UnlockRead;
  end;
end;

function TSunCalcProvider.GetTzInfo(
  const AUTCTime: TDateTime;
  out ATzInfo: TSunCalcTzInfo
): Boolean;
var
  VNeedUpdateTzInfo: Boolean;
  VTz: TTimeZoneRec;
begin
  Result := False;

  if not FTimeZoneInfo.Available then begin
    Exit;
  end;

  VNeedUpdateTzInfo := True;

  LockRead;
  try
    if FIsTzValid and SameDateTime(FTz.TzTime, AUTCTime) then begin
      ATzInfo := FTz;
      Result := ATzInfo.TzTime <> 0;
      VNeedUpdateTzInfo := False;
    end;
  finally
    UnlockRead;
  end;

  if VNeedUpdateTzInfo then begin
    LockWrite;
    try
      VTz := FTimeZoneInfo.GetTzInfo(FLocation, AUTCTime);

      FTz.TzName := VTz.TzName;
      FTz.TzTime := VTz.TzTime;
      FTz.TzOffset := VTz.TzOffset;

      ATzInfo := FTz;
      Result := ATzInfo.TzTime <> 0;

      FIsTzValid := True;
    finally
      UnlockWrite;
    end;
  end;
end;

function TSunCalcProvider.GetTzOffset(
  const AUTCTime: TDateTime;
  out ATzOffset: Extended
): Boolean;
var
  VNeedUpdateTzInfo: Boolean;
  VTz: TTimeZoneRec;
begin
  Result := False;

  if not FTimeZoneInfo.Available then begin
    Exit;
  end;

  VNeedUpdateTzInfo := True;

  LockRead;
  try
    if FIsTzValid and SameDate(FTz.TzTime, AUTCTime) then begin
      ATzOffset := FTz.TzOffset;
      Result := True;
      VNeedUpdateTzInfo := False;
    end;
  finally
    UnlockRead;
  end;

  if VNeedUpdateTzInfo then begin
    LockWrite;
    try
      VTz := FTimeZoneInfo.GetTzInfo(FLocation, AUTCTime);

      FTz.TzName := VTz.TzName;
      FTz.TzTime := VTz.TzTime;
      FTz.TzOffset := VTz.TzOffset;

      ATzOffset := FTz.TzOffset;
      Result := True;

      FIsTzValid := True;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TSunCalcProvider.SetDateTime(const AValue: TDateTime);
begin
  LockWrite;
  try
    if not SameDateTime(AValue, FDateTime) then begin
      FDateTime := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSunCalcProvider.SetDateTimeFromLocalTime(
  const ALocalTime: TDateTime
);
var
  VUTCTime: TDateTime;
begin
  VUTCTime := TTimeZoneInfo.LocalTimeToUTC(ALocalTime);
  Self.SetDateTime(VUTCTime);
end;

procedure TSunCalcProvider.SetLocation(const AValue: TDoublePoint);
begin
  LockWrite;
  try
    if not DoublePointsEqual(AValue, FLocation) then begin
      FLocation := AValue;
      FIsTzValid := False;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
