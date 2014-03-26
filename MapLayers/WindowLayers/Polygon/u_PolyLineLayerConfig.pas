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

unit u_PolyLineLayerConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_PolyLineLayerConfig,
  u_ConfigDataElementBase;

type
  TLineLayerConfig = class(TConfigDataElementBase, ILineLayerConfig)
  private
    FLineColor: TColor32;
    FLineWidth: integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetLineColor: TColor32;
    procedure SetLineColor(AValue: TColor32);

    function GetLineWidth: integer;
    procedure SetLineWidth(AValue: integer);
  public
    constructor Create;
  end;


implementation

uses
  GR32,
  u_ConfigProviderHelpers;

{ TLineLayerConfig }

constructor TLineLayerConfig.Create;
begin
  inherited Create;
  FLineColor := SetAlpha(ClRed32, 150);
  FLineWidth := 3;
end;

procedure TLineLayerConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FLineColor := ReadColor32(AConfigData, 'LineColor', FLineColor);
    FLineWidth := AConfigData.ReadInteger('LineWidth', FLineWidth);

    SetChanged;
  end;
end;

procedure TLineLayerConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  WriteColor32(AConfigData, 'LineColor', FLineColor);
  AConfigData.WriteInteger('LineWidth', FLineWidth);
end;

function TLineLayerConfig.GetLineColor: TColor32;
begin
  LockRead;
  try
    Result := FLineColor;
  finally
    UnlockRead;
  end;
end;

function TLineLayerConfig.GetLineWidth: integer;
begin
  LockRead;
  try
    Result := FLineWidth;
  finally
    UnlockRead;
  end;
end;

procedure TLineLayerConfig.SetLineColor(AValue: TColor32);
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

procedure TLineLayerConfig.SetLineWidth(AValue: integer);
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

end.
