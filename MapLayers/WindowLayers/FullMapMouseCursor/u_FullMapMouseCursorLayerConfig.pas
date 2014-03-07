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

unit u_FullMapMouseCursorLayerConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_FullMapMouseCursorLayerConfig,
  u_ConfigDataElementBase;

type
  TFullMapMouseCursorLayerConfig = class(TConfigDataElementBase, IFullMapMouseCursorLayerConfig)
  private
    FLineColor: TColor32;
    FEnabled: Boolean;
    FShowAlways: Boolean;
  private
    function GetLineColor: TColor32;
    procedure SetLineColor(AValue: TColor32);

    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);

    function GetShowAlways: Boolean;
    procedure SetShowAlways(AValue: Boolean);
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;
{ TFullMapMouseCursorLayerConfig }

constructor TFullMapMouseCursorLayerConfig.Create;
begin
  inherited Create;
  FLineColor := clWhite32;
  FEnabled := False;
  FShowAlways := False;
end;

procedure TFullMapMouseCursorLayerConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FLineColor := ReadColor32(AConfigData, 'LineColor', FLineColor);
    FEnabled := AConfigData.ReadBool('Enabled', FEnabled);
    FShowAlways := AConfigData.ReadBool('ShowAlways', FShowAlways);

    SetChanged;
  end;
end;

procedure TFullMapMouseCursorLayerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  WriteColor32(AConfigData, 'LineColor', FLineColor);
  AConfigData.WriteBool('Enabled', FEnabled);
  AConfigData.WriteBool('ShowAlways', FShowAlways);
end;

function TFullMapMouseCursorLayerConfig.GetEnabled: Boolean;
begin
  LockRead;
  try
    Result := FEnabled;
  finally
    UnlockRead;
  end;
end;

function TFullMapMouseCursorLayerConfig.GetLineColor: TColor32;
begin
  LockRead;
  try
    Result := FLineColor;
  finally
    UnlockRead;
  end;
end;

function TFullMapMouseCursorLayerConfig.GetShowAlways: Boolean;
begin
  LockRead;
  try
    Result := FShowAlways;
  finally
    UnlockRead;
  end;
end;

procedure TFullMapMouseCursorLayerConfig.SetEnabled(AValue: Boolean);
begin
  LockWrite;
  try
    if FEnabled <> AValue then begin
      FEnabled := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFullMapMouseCursorLayerConfig.SetLineColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FLineColor <> AValue then begin
      FLineColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFullMapMouseCursorLayerConfig.SetShowAlways(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowAlways <> AValue then begin
      FShowAlways := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
