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

unit u_SelectionPolylineLayerConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_SelectionPolylineLayerConfig,
  u_PolylineLayerConfig;

type
  TSelectionPolylineLayerConfig = class(TPolylineLayerConfig, ISelectionPolylineLayerConfig)
  private
    FShadowPolygonColor: TColor32;
    FPolygoneRadius: Double;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetRadius: Double;
    procedure SetRadius(AValue: Double);
    function GetShadowPolygonColor: TColor32;
    procedure SetShadowPolygonColor(AValue: TColor32);
  public
    constructor Create;
  end;

implementation

{ TSelectionPolylineLayerConfig }

constructor TSelectionPolylineLayerConfig.Create;
begin
  inherited;
  LockWrite;
  try
    SetLineColor(SetAlpha(clBlue32, 180));

    SetPointFillColor(SetAlpha(clYellow32, 150));
    SetPointRectColor(SetAlpha(ClRed32, 150));
    SetPointFirstColor(SetAlpha(ClGreen32, 255));
    SetPointActiveColor(SetAlpha(ClRed32, 255));

    SetShadowPolygonColor(SetAlpha(clBlack32,150));

    FPolygoneRadius:=100;
  finally
    UnlockWrite;
  end;
end;

procedure TSelectionPolylineLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FPolygoneRadius := AConfigData.ReadFloat('PolygoneRadius', FPolygoneRadius);
    SetChanged;
  end;
end;

procedure TSelectionPolylineLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteFloat('PolygoneRadius', FPolygoneRadius);
end;

function TSelectionPolylineLayerConfig.GetRadius: Double;
begin
  LockRead;
  try
    Result := FPolygoneRadius;
  finally
    UnlockRead;
  end;
end;

function TSelectionPolylineLayerConfig.GetShadowPolygonColor: TColor32;
begin
  LockRead;
  try
    Result := FShadowPolygonColor;
  finally
    UnlockRead;
  end;
end;

procedure TSelectionPolylineLayerConfig.SetRadius(AValue: Double);
begin
  LockWrite;
  try
    if FPolygoneRadius <> AValue then begin
      FPolygoneRadius := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSelectionPolylineLayerConfig.SetShadowPolygonColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FShadowPolygonColor <> AValue then begin
      FShadowPolygonColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;


end.
