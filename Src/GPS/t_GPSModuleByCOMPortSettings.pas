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

unit t_GPSModuleByCOMPortSettings;

interface

uses
  Windows;

type
  // do not change the order of the existing items - the value is saved to the config by its ordinal
  TGPSOrigin = (gpsoNMEA, gpsoGarmin, gpsoFlyOnTrack, gpsoLocationAPI, gpsoTCPNMEA, gpsoUDPNMEA);

  TGPSComSettings = record
    Port: DWORD;
    BaudRate: DWORD;
    AutodetectCOMOnConnect: Boolean;
    AutodetectCOMFlags: DWORD;
    class operator Equal(const A, B: TGPSComSettings): Boolean; inline;
    class operator NotEqual(const A, B: TGPSComSettings): Boolean; inline;
  end;

  TGPSNetworkSettings = record
    ServerAddress: string;
    class operator Equal(const A, B: TGPSNetworkSettings): Boolean; inline;
    class operator NotEqual(const A, B: TGPSNetworkSettings): Boolean; inline;
  end;

  TGPSReplaySettings = record
    UseTrackList: Boolean;
    TrackFile: string;
    PlaybackSpeedMultiplier: DWORD;
    LoopPlayback: Boolean;
    class operator Equal(const A, B: TGPSReplaySettings): Boolean; inline;
    class operator NotEqual(const A, B: TGPSReplaySettings): Boolean; inline;
  end;

implementation

{ TGPSComSettings }

class operator TGPSComSettings.Equal(const A, B: TGPSComSettings): Boolean;
begin
  Result :=
    (A.Port = B.Port) and
    (A.BaudRate = B.BaudRate) and
    (A.AutodetectCOMOnConnect = B.AutodetectCOMOnConnect) and
    (A.AutodetectCOMFlags = B.AutodetectCOMFlags);
end;

class operator TGPSComSettings.NotEqual(const A, B: TGPSComSettings): Boolean;
begin
  Result := not (A = B);
end;

{ TGPSNetworkSettings }

class operator TGPSNetworkSettings.Equal(const A, B: TGPSNetworkSettings): Boolean;
begin
  Result :=
    (A.ServerAddress = B.ServerAddress);
end;

class operator TGPSNetworkSettings.NotEqual(const A, B: TGPSNetworkSettings): Boolean;
begin
  Result := not (A = B);
end;

{ TGPSReplaySettings }

class operator TGPSReplaySettings.Equal(const A, B: TGPSReplaySettings): Boolean;
begin
  Result :=
    (A.UseTrackList = B.UseTrackList) and
    (A.TrackFile = B.TrackFile) and
    (A.PlaybackSpeedMultiplier = B.PlaybackSpeedMultiplier) and
    (A.LoopPlayback = B.LoopPlayback);
end;

class operator TGPSReplaySettings.NotEqual(const A, B: TGPSReplaySettings): Boolean;
begin
  Result := not (A = B);
end;

end.
