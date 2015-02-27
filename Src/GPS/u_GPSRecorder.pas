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

unit u_GPSRecorder;

interface

uses
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_Datum,
  i_PathConfig,
  i_GPS,
  i_GPSRecorder,
  u_ChangeableBase;

type
  TGPSRecorder = class(TChangeableWithSimpleLockBase, IGPSRecorder, IGPSRecorderInternal)
  private
    FDataFile: IPathConfig;
    FDatum: IDatum;
    FEmptyPosition: IGPSPosition;

    FOdometer1: Double;
    FOdometer2: Double;
    FDist: Double;
    FMaxSpeed: Double;
    FAvgSpeed: Double;
    FAvgSpeedTickCount: Double;

    FLastSpeed: Double;
    FLastAltitude: Double;
    FLastHeading: Double;
    FLastPosition: TDoublePoint;

    FCurrentPosition: IGPSPosition;
    FLastPositionOK: Boolean;
  private
    procedure Load;
    procedure Save;
  private
    procedure AddPoint(const APosition: IGPSPosition);
    procedure AddEmptyPoint;

    function GetOdometer1: Double;
    procedure ResetOdometer1;
    function GetOdometer2: Double;
    procedure ResetOdometer2;
    function GetDist: Double;
    procedure ResetDist;
    function GetMaxSpeed: Double;
    procedure ResetMaxSpeed;
    function GetAvgSpeed: Double;
    procedure ResetAvgSpeed;
    function GetLastSpeed: Double;
    function GetLastAltitude: Double;
    function GetLastHeading: Double;
    function GetLastPosition: TDoublePoint;
    function GetCurrentPosition: IGPSPosition;
  public
    constructor Create(
      const ADatum: IDatum;
      const ADataFile: IPathConfig;
      const AEmptyPosition: IGPSPosition
    );
  end;

implementation

uses
  Math,
  SysUtils,
  IniFiles,
  u_GeoFunc,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataWriteProviderByIniFile;

{ TGPSRecorder }

constructor TGPSRecorder.Create(
  const ADatum: IDatum;
  const ADataFile: IPathConfig;
  const AEmptyPosition: IGPSPosition
);
begin
  inherited Create;
  FDataFile := ADataFile;
  FEmptyPosition := AEmptyPosition;
  FDatum := ADatum;
  FLastPositionOK := False;
  FCurrentPosition := FEmptyPosition;
end;

procedure TGPSRecorder.Load;
var
  VFileName: string;
  VIniFile: TMemIniFile;
  VData: IConfigDataProvider;
  VSensorsData: IConfigDataProvider;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    VFileName := FDataFile.FullPath;
    if FileExists(VFileName) then begin
      try
        VIniFile := TMemIniFile.Create(VFileName);
        try
          VData := TConfigDataProviderByIniFile.CreateWithOwn(VIniFile);
          VIniFile := nil;
        finally
          VIniFile.Free;
        end;
        VSensorsData := VData.GetSubItem('GPS');
        if VSensorsData <> nil then begin
          FOdometer1 := VSensorsData.ReadFloat('Odometer1', FOdometer1);
          FOdometer2 := VSensorsData.ReadFloat('Odometer2', FOdometer2);
          VNeedNotify := True;
        end;
      except
        Assert(False, 'Exception on GPSRecorder read');
      end;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TGPSRecorder.Save;
var
  VFileName: string;
  VIniFile: TMemIniFile;
  VData: IConfigDataWriteProvider;
  VSensorsData: IConfigDataWriteProvider;
begin
  CS.BeginRead;
  try
    VFileName := FDataFile.FullPath;
    try
      VIniFile := TMemIniFile.Create(VFileName);
      try
        VData := TConfigDataWriteProviderByIniFile.CreateWithOwn(VIniFile);
        VIniFile := nil;
      finally
        VIniFile.Free;
      end;
      VSensorsData := VData.GetOrCreateSubItem('GPS');
      VSensorsData.WriteFloat('Odometer1', FOdometer1);
      VSensorsData.WriteFloat('Odometer2', FOdometer2);
    except
      Assert(False, 'Exception on GPSRecorder write');
    end;
  finally
    CS.EndRead;
  end;
end;

procedure TGPSRecorder.AddEmptyPoint;
var
  VPoint: TGPSTrackPoint;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VPoint.Point := CEmptyDoublePoint;
  VPoint.Speed := 0;
  VPoint.Time := NaN;
  CS.BeginWrite;
  try
    if (FLastPositionOK) then begin
      FLastPositionOK := False;
      VNeedNotify := True;
    end;
    FCurrentPosition := FEmptyPosition;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TGPSRecorder.AddPoint(const APosition: IGPSPosition);
var
  VAlfa: Double;
  VBeta: Double;
  VDistToPrev: Double;
  VPointPrev: TDoublePoint;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FLastPositionOK or APosition.PositionOK then begin
      VPointPrev := FLastPosition;

        // check new values
      if APosition.PositionOK then begin
        FLastPosition := APosition.LonLat;
        FLastAltitude := APosition.Altitude;
        FLastHeading := APosition.Heading;
        FLastSpeed := APosition.Speed_KMH;

          // allow calc max and avg speed even if no stats
          // check AllowCalcStats only for permanent (overall) stats

          // speed may be unavailable
        if APosition.SpeedOK then begin
            // max speen
          if (APosition.Speed_KMH > FMaxSpeed) then begin
            FMaxSpeed := APosition.Speed_KMH;
          end;
            // avg speed
          FAvgSpeedTickCount := FAvgSpeedTickCount + 1;
          VAlfa := 1 / FAvgSpeedTickCount;
          VBeta := 1 - VAlfa;
          FAvgSpeed := VAlfa * APosition.Speed_KMH + VBeta * FAvgSpeed;
        end;

          // if prev position available too - calc distance
          // no recalc if AllowCalcStats disabled
        if FLastPositionOK then begin
          VDistToPrev := FDatum.CalcDist(VPointPrev, FLastPosition);
          FDist := FDist + VDistToPrev;
          FOdometer1 := FOdometer1 + VDistToPrev;
          FOdometer2 := FOdometer2 + VDistToPrev;
        end;
      end;

      FLastPositionOK := APosition.PositionOK;
      VNeedNotify := True;
    end;
    FCurrentPosition := APosition;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

function TGPSRecorder.GetAvgSpeed: Double;
begin
  CS.BeginRead;
  try
    Result := FAvgSpeed;
  finally
    CS.EndRead;
  end;
end;

function TGPSRecorder.GetCurrentPosition: IGPSPosition;
begin
  CS.BeginRead;
  try
    Result := FCurrentPosition;
  finally
    CS.EndRead;
  end;
end;

function TGPSRecorder.GetDist: Double;
begin
  CS.BeginRead;
  try
    Result := FDist;
  finally
    CS.EndRead;
  end;
end;

function TGPSRecorder.GetLastAltitude: Double;
begin
  CS.BeginRead;
  try
    Result := FLastAltitude;
  finally
    CS.EndRead;
  end;
end;

function TGPSRecorder.GetLastHeading: Double;
begin
  CS.BeginRead;
  try
    Result := FLastHeading;
  finally
    CS.EndRead;
  end;
end;

function TGPSRecorder.GetLastPosition: TDoublePoint;
begin
  CS.BeginRead;
  try
    Result := FLastPosition;
  finally
    CS.EndRead;
  end;
end;

function TGPSRecorder.GetLastSpeed: Double;
begin
  CS.BeginRead;
  try
    Result := FLastSpeed;
  finally
    CS.EndRead;
  end;
end;

function TGPSRecorder.GetMaxSpeed: Double;
begin
  CS.BeginRead;
  try
    Result := FMaxSpeed;
  finally
    CS.EndRead;
  end;
end;

function TGPSRecorder.GetOdometer1: Double;
begin
  CS.BeginRead;
  try
    Result := FOdometer1;
  finally
    CS.EndRead;
  end;
end;

function TGPSRecorder.GetOdometer2: Double;
begin
  CS.BeginRead;
  try
    Result := FOdometer2;
  finally
    CS.EndRead;
  end;
end;

procedure TGPSRecorder.ResetAvgSpeed;
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FAvgSpeed <> 0 then begin
      FAvgSpeed := 0;
      FAvgSpeedTickCount := 0;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TGPSRecorder.ResetDist;
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FDist <> 0 then begin
      FDist := 0;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TGPSRecorder.ResetMaxSpeed;
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FMaxSpeed <> 0 then begin
      FMaxSpeed := 0;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TGPSRecorder.ResetOdometer1;
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FOdometer1 <> 0 then begin
      FOdometer1 := 0;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TGPSRecorder.ResetOdometer2;
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FOdometer2 <> 0 then begin
      FOdometer2 := 0;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
