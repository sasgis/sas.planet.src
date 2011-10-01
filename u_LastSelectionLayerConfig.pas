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

unit u_LastSelectionLayerConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LastSelectionLayerConfig,
  u_ConfigDataElementBase;

type
  TLastSelectionLayerConfig = class(TConfigDataElementBase, ILastSelectionLayerConfig)
  private
    FVisible: Boolean;
    FLineWidth: Integer;
    FLineColor: TColor32;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetLineWidth: Integer;
    procedure SetLineWidth(AValue: Integer);

    function GetLineColor: TColor32;
    procedure SetLineColor(AValue: TColor32);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TLastSelectionLayerConfig }

constructor TLastSelectionLayerConfig.Create;
begin
  inherited;
  FVisible := True;
  FLineColor := SetAlpha(clBlack32, 210);
  FLineWidth := 2;
end;

procedure TLastSelectionLayerConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    FLineColor := ReadColor32(AConfigData, 'LineColor', FLineColor);
    FLineWidth := AConfigData.ReadInteger('LineWidth', FLineWidth);
    SetChanged;
  end;
end;

procedure TLastSelectionLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
  WriteColor32(AConfigData, 'LineColor', FLineColor);
  AConfigData.WriteInteger('LineWidth', FLineWidth);
end;

function TLastSelectionLayerConfig.GetLineColor: TColor32;
begin
  LockRead;
  try
    Result := FLineColor;
  finally
    UnlockRead;
  end;
end;

function TLastSelectionLayerConfig.GetLineWidth: Integer;
begin
  LockRead;
  try
    Result := FLineWidth;
  finally
    UnlockRead;
  end;
end;

function TLastSelectionLayerConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TLastSelectionLayerConfig.SetLineColor(AValue: TColor32);
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

procedure TLastSelectionLayerConfig.SetLineWidth(AValue: Integer);
begin
  LockWrite;
  try
    if FLineWidth <> AValue then begin
      FLineWidth := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLastSelectionLayerConfig.SetVisible(AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
