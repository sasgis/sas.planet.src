{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_GPSDeviceParamsFrame;

interface

uses
  Windows,
  Classes,
  StdCtrls,
  t_GPSModuleByCOMPortSettings,
  i_LanguageManager,
  i_GpsSystem,
  i_GPSConfig,
  i_GPSModuleByCOMPortSettings,
  u_CommonFormAndFrameParents;

const
  CCancelModeCaption = 'X';

type
  TGetConnectionTimeoutSec = function: DWORD of object;

  TGPSDeviceParamsFrame = class(TCommonFrameParent)
  protected
    FGPSConfig: IGPSConfig;
    FOrigin: TGPSOrigin;
    FGetConnectionTimeoutSec: TGetConnectionTimeoutSec;
    FOnBusyChanged: TNotifyEvent;
    function GetIsGpsOn(const AState: TGpsConnectionState): Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AGPSConfig: IGPSConfig;
      const AOrigin: TGPSOrigin;
      const AGetConnectionTimeoutSec: TGetConnectionTimeoutSec
    ); reintroduce;
  public
    procedure Init; virtual; abstract;
    procedure ApplyChanges; virtual; abstract;
    procedure UpdateControlsState(const AState: TGpsConnectionState); virtual; abstract;
    function IsBusy: Boolean; virtual; abstract;
    function ValidateBeforeConnect: Boolean; virtual;
  public
    property OnBusyChanged: TNotifyEvent read FOnBusyChanged write FOnBusyChanged;
  end;
  TFrame = class(TGPSDeviceParamsFrame);

function IsActiveConnectionState(const AState: TGpsConnectionState): Boolean; inline;

procedure SwitchToCancelMode(
  AButton: TButton;
  const ACancelHint: string;
  out AOldCaption: string;
  out AOldHint: string
);

procedure RestoreFromCancelMode(
  AButton: TButton;
  const AOldCaption: string;
  const AOldHint: string
);

implementation

function IsActiveConnectionState(const AState: TGpsConnectionState): Boolean; inline;
begin
  Result := AState in [gcsConnecting, gcsConnected, gcsConnectedNoData, gcsDisconnecting];
end;

procedure SwitchToCancelMode(
  AButton: TButton;
  const ACancelHint: string;
  out AOldCaption: string;
  out AOldHint: string
);
begin
  AOldCaption := AButton.Caption;
  AOldHint := AButton.Hint;
  AButton.Caption := CCancelModeCaption;
  AButton.Hint := ACancelHint;
end;

procedure RestoreFromCancelMode(
  AButton: TButton;
  const AOldCaption: string;
  const AOldHint: string
);
begin
  AButton.Caption := AOldCaption;
  AButton.Hint := AOldHint;
end;

{ TGPSDeviceParamsFrame }

constructor TGPSDeviceParamsFrame.Create(
  const ALanguageManager: ILanguageManager;
  const AGPSConfig: IGPSConfig;
  const AOrigin: TGPSOrigin;
  const AGetConnectionTimeoutSec: TGetConnectionTimeoutSec
);
begin
  Assert(AGPSConfig <> nil);
  inherited Create(ALanguageManager);
  FGPSConfig := AGPSConfig;
  FOrigin := AOrigin;
  FGetConnectionTimeoutSec := AGetConnectionTimeoutSec;
end;

function TGPSDeviceParamsFrame.GetIsGpsOn(const AState: TGpsConnectionState): Boolean;
begin
  Result := IsActiveConnectionState(AState) or FGPSConfig.GPSEnabled;
end;

function TGPSDeviceParamsFrame.ValidateBeforeConnect: Boolean;
begin
  Result := True;
end;

end.
