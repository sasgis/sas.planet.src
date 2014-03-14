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

unit u_BaseGridConfig;

interface

uses
  GR32,
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_LocalCoordConverter,
  i_MapLayerGridsConfig,
  u_ConfigDataElementBase;

type
  TBaseGridConfig = class(TConfigDataElementBase, IBaseGridConfig)
  private
    FVisible: Boolean;
    FGridColor: TColor32;
    FShowText: Boolean;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetPointStickToGrid(
      const ALocalConverter: ILocalCoordConverter;
      const ASourceLonLat: TDoublePoint
    ): TDoublePoint; virtual; abstract;
    function GetRectStickToGrid(
      const ALocalConverter: ILocalCoordConverter;
      const ASourceRect: TDoubleRect
    ): TDoubleRect; virtual; abstract;

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetGridColor: TColor32;
    procedure SetGridColor(AValue: TColor32);

    function GetShowText: Boolean;
    procedure SetShowText(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TBaseGridConfig }

constructor TBaseGridConfig.Create;
begin
  inherited Create;
  FVisible := False;
  FShowText := True;
  FGridColor := SetAlpha(clWhite32, 200);
end;

procedure TBaseGridConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    FShowText := AConfigData.ReadBool('ShowText', FShowText);
    FGridColor := ReadColor32(AConfigData, 'GridColor', FGridColor);
    SetChanged;
  end;
end;

procedure TBaseGridConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
  AConfigData.WriteBool('ShowText', FShowText);
  WriteColor32(AConfigData, 'GridColor', FGridColor);
end;

function TBaseGridConfig.GetGridColor: TColor32;
begin
  LockRead;
  try
    Result := FGridColor;
  finally
    UnlockRead;
  end;
end;

function TBaseGridConfig.GetShowText: Boolean;
begin
  LockRead;
  try
    Result := FShowText;
  finally
    UnlockRead;
  end;
end;

function TBaseGridConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TBaseGridConfig.SetGridColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FGridColor <> AValue then begin
      FGridColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TBaseGridConfig.SetShowText(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowText <> AValue then begin
      FShowText := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TBaseGridConfig.SetVisible(AValue: Boolean);
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
