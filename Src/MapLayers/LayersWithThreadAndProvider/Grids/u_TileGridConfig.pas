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

unit u_TileGridConfig;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapLayerGridsConfig,
  u_BaseGridConfig;

type
  TTileGridConfig = class(TBaseGridConfig, ITileGridConfig)
  private
    FUseRelativeZoom: Boolean;
    FZoom: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetUseRelativeZoom: Boolean;
    procedure SetUseRelativeZoom(AValue: Boolean);

    function GetZoom: Integer;
    procedure SetZoom(AValue: Integer);
  public
    constructor Create;
  end;

implementation

uses
  Types,
  Math,
  u_GeoFunc;

{ TTileGridConfig }

constructor TTileGridConfig.Create;
begin
  inherited;
  FUseRelativeZoom := True;
  FZoom := 0;
end;

procedure TTileGridConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUseRelativeZoom := AConfigData.ReadBool('UseRelativeZoom', FUseRelativeZoom);
    FZoom := AConfigData.ReadInteger('Zoom', FZoom);
    SetChanged;
  end;
end;

procedure TTileGridConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UseRelativeZoom', FUseRelativeZoom);
  AConfigData.WriteInteger('Zoom', FZoom);
end;

function TTileGridConfig.GetUseRelativeZoom: Boolean;
begin
  LockRead;
  try
    Result := FUseRelativeZoom;
  finally
    UnlockRead;
  end;
end;

function TTileGridConfig.GetZoom: Integer;
begin
  LockRead;
  try
    Result := FZoom;
  finally
    UnlockRead;
  end;
end;

procedure TTileGridConfig.SetUseRelativeZoom(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseRelativeZoom <> AValue then begin
      FUseRelativeZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTileGridConfig.SetZoom(AValue: Integer);
begin
  LockWrite;
  try
    if FZoom <> AValue then begin
      FZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
