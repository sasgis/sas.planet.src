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

unit fr_GPSDeviceReplay;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Dialogs,
  StdCtrls,
  Spin,
  ExtCtrls,
  Math,
  gnugettext,
  t_GPSModuleByCOMPortSettings,
  i_LanguageManager,
  i_GpsSystem,
  i_GPSConfig,
  i_GPSModuleByCOMPortSettings,
  u_ResStrings,
  u_Dialogs,
  u_GPSDeviceParamsFrame;

type
  TfrGPSDeviceReplay = class(TFrame)
    chkUseReplayTrackFile: TCheckBox;
    lblTrackFile: TLabel;
    pnlTrackFileRow: TPanel;
    edtTrackFile: TEdit;
    btnBrowseTrackFile: TButton;
    flwpnlPlaybackSpeed: TFlowPanel;
    lblPlaybackSpeed: TLabel;
    sePlaybackSpeed: TSpinEdit;
    chkLoopPlayback: TCheckBox;
    procedure btnBrowseTrackFileClick(Sender: TObject);
    procedure OnUseReplayTrackFileClick(Sender: TObject);
  private
    FFileDialog: TOpenDialog;
  public
    procedure Init; override;
    procedure ApplyChanges; override;
    procedure UpdateControlsState(const AState: TGpsConnectionState); override;
    function IsBusy: Boolean; override;
    function ValidateBeforeConnect: Boolean; override;
  end;

implementation

uses
  u_CommonFormAndFrameParents;

const
  CReplayTrackFileFilter = 'Track files|*.plt;*.gpx;*.nmea;*.garmin;*.locationapi|All files (*.*)|*.*';
  CReplaySpeedMinMultiplier = 1;
  CReplaySpeedMaxMultiplier = 10000;

{$R *.dfm}

{ TfrGPSDeviceReplay }

procedure TfrGPSDeviceReplay.Init;
var
  VSettings: TGPSReplaySettings;
begin
  FGPSConfig.LockRead;
  try
    VSettings := FGPSConfig.ModuleConfig.ReplaySettings;
  finally
    FGPSConfig.UnlockRead;
  end;

  chkUseReplayTrackFile.Checked := VSettings.UseTrackList;
  edtTrackFile.Text := VSettings.TrackFile;
  sePlaybackSpeed.Value := EnsureRange(VSettings.PlaybackSpeedMultiplier, CReplaySpeedMinMultiplier, CReplaySpeedMaxMultiplier);
  chkLoopPlayback.Checked := VSettings.LoopPlayback;
end;

procedure TfrGPSDeviceReplay.ApplyChanges;
var
  VSettings: TGPSReplaySettings;
begin
  FGPSConfig.LockRead;
  try
    VSettings := FGPSConfig.ModuleConfig.ReplaySettings;
  finally
    FGPSConfig.UnlockRead;
  end;

  VSettings.UseTrackList := chkUseReplayTrackFile.Checked;
  VSettings.TrackFile := Trim(edtTrackFile.Text);
  VSettings.PlaybackSpeedMultiplier := sePlaybackSpeed.Value;
  VSettings.LoopPlayback := chkLoopPlayback.Checked;

  FGPSConfig.LockWrite;
  try
    FGPSConfig.ModuleConfig.ReplaySettings := VSettings;
  finally
    FGPSConfig.UnlockWrite;
  end;
end;

procedure TfrGPSDeviceReplay.UpdateControlsState(const AState: TGpsConnectionState);
var
  VGpsOn: Boolean;
  VTrackFileEnabled: Boolean;
begin
  VGpsOn := GetIsGpsOn(AState);

  chkUseReplayTrackFile.Enabled := not VGpsOn;
  chkLoopPlayback.Enabled := not VGpsOn;
  SetControlEnabled(flwpnlPlaybackSpeed, not VGpsOn);

  VTrackFileEnabled := (not VGpsOn) and (not chkUseReplayTrackFile.Checked);
  lblTrackFile.Enabled := VTrackFileEnabled;
  SetControlEnabled(pnlTrackFileRow, VTrackFileEnabled);
end;

function TfrGPSDeviceReplay.IsBusy: Boolean;
begin
  Result := False;
end;

function TfrGPSDeviceReplay.ValidateBeforeConnect: Boolean;
var
  VTrackFile: string;
  VTrackList: TStringList;
begin
  if chkUseReplayTrackFile.Checked then begin
    Result := False;
    VTrackList := TStringList.Create;
    try
      if FileExists(CReplayTrackListFileName) then begin
        try
          VTrackList.LoadFromFile(CReplayTrackListFileName);
          Result := (Length(Trim(VTrackList.Text)) > 0);
        except
          Result := False;
        end;
      end;
    finally
      VTrackList.Free;
    end;
    if not Result then begin
      ShowErrorMessage(Format(SAS_MSG_ReplayTrackListIsEmpty, [CReplayTrackListFileName]));
    end;
    Exit;
  end;

  VTrackFile := Trim(edtTrackFile.Text);
  if VTrackFile = '' then begin
    ShowErrorMessage(_('Select a track file to replay'));
    Result := False;
    Exit;
  end;
  Result := FileExists(VTrackFile);
  if not Result then begin
    ShowErrorMessage(Format(SAS_MSG_ReplayTrackFileNotFound, [VTrackFile]));
  end;
end;

procedure TfrGPSDeviceReplay.btnBrowseTrackFileClick(Sender: TObject);
var
  VTrackFile: string;
begin
  if FFileDialog = nil then begin
    FFileDialog := TOpenDialog.Create(Self);
    FFileDialog.Options := [ofFileMustExist, ofEnableSizing];
    FFileDialog.Filter := CReplayTrackFileFilter;
  end;
  VTrackFile := Trim(edtTrackFile.Text);
  if VTrackFile <> '' then begin
    FFileDialog.InitialDir := ExtractFileDir(VTrackFile);
  end else begin
    FFileDialog.InitialDir := '';
  end;
  FFileDialog.FileName := '';
  if FFileDialog.Execute then begin
    edtTrackFile.Text := FFileDialog.FileName;
  end;
end;

procedure TfrGPSDeviceReplay.OnUseReplayTrackFileClick(Sender: TObject);
begin
  lblTrackFile.Enabled := not chkUseReplayTrackFile.Checked;
  SetControlEnabled(pnlTrackFileRow, not chkUseReplayTrackFile.Checked);
end;

end.
