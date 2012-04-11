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

unit u_SensorTextFromGPSRecorder;

interface

uses
  i_GPSRecorder,
  i_ValueToStringConverter,
  i_LanguageManager,
  i_Sensor,
  u_SensorBase;

type
  TSensorTextFromGPSRecorder = class(TSensorBase, ISensorText)
  private
    FGPSRecorder: IGPSRecorder;
    FValueConverterConfig: IValueToStringConverterConfig;
    FValueConverter: IValueToStringConverter;

    FLastValue: Double;
    procedure OnConverterConfigChange;
    procedure OnRecorderChanged;
  protected
    property GPSRecorder: IGPSRecorder read FGPSRecorder;
    property ValueConverter: IValueToStringConverter read FValueConverter;

    function ValueChanged(const AOld, ANew: Double): Boolean; virtual;

    function ValueToText(const AValue: Double): string; virtual; abstract;
    function GetValue: Double; virtual; abstract;
  protected
    function GetText: string;
  public
    constructor Create(
      const AGUID: TGUID;
      ACanReset: Boolean;
      const ALanguageManager: ILanguageManager;
      const AGPSRecorder: IGPSRecorder;
      const AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

  TSensorTimeFromGPSRecorder = class(TSensorTextFromGPSRecorder)
  protected
    function ValueChanged(const AOld, ANew: Double): Boolean; override;
    function ValueToText(const AValue: Double): string; override;
  end;

  TSensorSimpleTextFromGPSRecorder = class(TSensorTextFromGPSRecorder)
  protected
    function ValueChanged(const AOld, ANew: Double): Boolean; override;
    function GetValue: Double; override;
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener;

{ TSensorTextFromGPSRecorder }

constructor TSensorTextFromGPSRecorder.Create(
  const AGUID: TGUID;
  ACanReset: Boolean;
  const ALanguageManager: ILanguageManager;
  const AGPSRecorder: IGPSRecorder;
  const AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(AGUID, ACanReset, ISensorText, ALanguageManager);
  FGPSRecorder := AGPSRecorder;
  FValueConverterConfig := AValueConverterConfig;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnRecorderChanged),
    FGPSRecorder.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConverterConfigChange),
    FValueConverterConfig.GetChangeNotifier
  );

  OnConverterConfigChange;
  OnRecorderChanged;
end;

function TSensorTextFromGPSRecorder.GetText: string;
var
  VValue: Double;
begin
  LockRead;
  try
    VValue := FLastValue;
    Result := ValueToText(VValue);
  finally
    UnlockRead;
  end;
end;

procedure TSensorTextFromGPSRecorder.OnConverterConfigChange;
begin
  LockWrite;
  try
    FValueConverter := FValueConverterConfig.GetStatic;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TSensorTextFromGPSRecorder.OnRecorderChanged;
var
  VValue: Double;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := GetValue;
  LockWrite;
  try
    if ValueChanged(FLastValue, VValue) then begin
      FLastValue := VValue;
      VNeedNotify := True;
    end;
  finally
    UnlockWrite;
  end;
  if VNeedNotify then begin
    NotifyDataUpdate;
  end;
end;

function TSensorTextFromGPSRecorder.ValueChanged(const AOld, ANew: Double): Boolean;
begin
  Result := (Abs(AOld - ANew) > 0.001);
end;

{ TSensorSimpleTextFromGPSRecorder }

function TSensorSimpleTextFromGPSRecorder.GetValue: Double;
begin
  Result := 0;
end;

function TSensorSimpleTextFromGPSRecorder.ValueChanged(const AOld, ANew: Double): Boolean;
begin
  Result := TRUE;
end;

{ TSensorTimeFromGPSRecorder }

function TSensorTimeFromGPSRecorder.ValueChanged(const AOld, ANew: Double): Boolean;
begin
  Result:=TRUE;
end;

function TSensorTimeFromGPSRecorder.ValueToText(const AValue: Double): string;
begin
  if (0=AValue) then
    Result:=''
  else
    Result := FormatDateTime('hh:nn:ss',AValue);
end;

end.
