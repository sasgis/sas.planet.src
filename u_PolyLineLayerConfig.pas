{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_PolyLineLayerConfig;

interface

uses
  GR32,
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
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetLineColor: TColor32;
    procedure SetLineColor(AValue: TColor32);

    function GetLineWidth: integer;
    procedure SetLineWidth(AValue: integer);
  public
    constructor Create;
  end;


  TPointsSetLayerConfig = class(TConfigDataElementBase, IPointsSetLayerConfig)
  private
    FPointFillColor: TColor32;
    FPointRectColor: TColor32;
    FPointFirstColor: TColor32;
    FPointActiveColor: TColor32;
    FPointSize: integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetPointFillColor: TColor32;
    procedure SetPointFillColor(AValue: TColor32);

    function GetPointRectColor: TColor32;
    procedure SetPointRectColor(AValue: TColor32);

    function GetPointFirstColor: TColor32;
    procedure SetPointFirstColor(AValue: TColor32);

    function GetPointActiveColor: TColor32;
    procedure SetPointActiveColor(AValue: TColor32);

    function GetPointSize: integer;
    procedure SetPointSize(AValue: integer);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TPolyLineLayerConfig }

constructor TPointsSetLayerConfig.Create;
begin
  inherited;
  FPointFillColor := SetAlpha(clYellow32, 150);
  FPointRectColor := SetAlpha(ClRed32, 150);
  FPointFirstColor := SetAlpha(ClGreen32, 255);
  FPointActiveColor := SetAlpha(ClRed32, 255);
  FPointSize := 8;
end;

procedure TPointsSetLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FPointFillColor := ReadColor32(AConfigData, 'PointFillColor', FPointFillColor);
    FPointRectColor := ReadColor32(AConfigData, 'PointRectColor', FPointRectColor);
    FPointFirstColor := ReadColor32(AConfigData, 'PointFirstColor', FPointFirstColor);
    FPointActiveColor := ReadColor32(AConfigData, 'PointActiveColor', FPointActiveColor);
    FPointSize := AConfigData.ReadInteger('PointSize', FPointSize);

    SetChanged;
  end;
end;

procedure TPointsSetLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  WriteColor32(AConfigData, 'PointFillColor', FPointFillColor);
  WriteColor32(AConfigData, 'PointRectColor', FPointRectColor);
  WriteColor32(AConfigData, 'PointFirstColor', FPointFirstColor);
  WriteColor32(AConfigData, 'PointActiveColor', FPointActiveColor);
  AConfigData.WriteInteger('PointSize', FPointSize);
end;

function TPointsSetLayerConfig.GetPointActiveColor: TColor32;
begin
  LockRead;
  try
    Result := FPointActiveColor;
  finally
    UnlockRead;
  end;
end;

function TPointsSetLayerConfig.GetPointFillColor: TColor32;
begin
  LockRead;
  try
    Result := FPointFillColor;
  finally
    UnlockRead;
  end;
end;

function TPointsSetLayerConfig.GetPointFirstColor: TColor32;
begin
  LockRead;
  try
    Result := FPointFirstColor;
  finally
    UnlockRead;
  end;
end;

function TPointsSetLayerConfig.GetPointRectColor: TColor32;
begin
  LockRead;
  try
    Result := FPointRectColor;
  finally
    UnlockRead;
  end;
end;

function TPointsSetLayerConfig.GetPointSize: integer;
begin
  LockRead;
  try
    Result := FPointSize;
  finally
    UnlockRead;
  end;
end;

procedure TPointsSetLayerConfig.SetPointActiveColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FPointActiveColor <> AValue then begin
      FPointActiveColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPointsSetLayerConfig.SetPointFillColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FPointFillColor <> AValue then begin
      FPointFillColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPointsSetLayerConfig.SetPointFirstColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FPointFirstColor <> AValue then begin
      FPointFirstColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPointsSetLayerConfig.SetPointRectColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FPointRectColor <> AValue then begin
      FPointRectColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPointsSetLayerConfig.SetPointSize(AValue: integer);
begin
  LockWrite;
  try
    if FPointSize <> AValue then begin
      FPointSize := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TLineLayerConfig }

constructor TLineLayerConfig.Create;
begin
  inherited;
  FLineColor := SetAlpha(ClRed32, 150);
  FLineWidth := 3;
end;

procedure TLineLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FLineColor := ReadColor32(AConfigData, 'LineColor', FLineColor);
    FLineWidth := AConfigData.ReadInteger('LineWidth', FLineWidth);

    SetChanged;
  end;
end;

procedure TLineLayerConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
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
