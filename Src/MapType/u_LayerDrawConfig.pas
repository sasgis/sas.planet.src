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

unit u_LayerDrawConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ZmpInfo,
  i_LayerDrawConfig,
  u_ConfigDataElementBase;

type
  TLayerDrawConfig = class(TConfigDataElementBase, ILayerDrawConfig)
  private
    FLayerZOrderDef: Integer;
    FLayerZOrder: Integer;
  private
    function GetLayerZOrder: Integer;
    procedure SetLayerZOrder(AValue: Integer);
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create(
      const ADefConfig: IZmpInfo
    );
  end;

implementation

{ TLayerDrawConfig }

constructor TLayerDrawConfig.Create(const ADefConfig: IZmpInfo);
begin
  inherited Create;
  FLayerZOrderDef := ADefConfig.LayerZOrder;
  FLayerZOrder := FLayerZOrderDef;
end;

procedure TLayerDrawConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FLayerZOrder := AConfigData.ReadInteger('LayerZOrder', FLayerZOrder);
    SetChanged;
  end;
end;

procedure TLayerDrawConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  if FLayerZOrder <> FLayerZOrderDef then begin
    AConfigData.WriteInteger('LayerZOrder', FLayerZOrder);
  end else begin
    AConfigData.DeleteValue('LayerZOrder');
  end;
end;

function TLayerDrawConfig.GetLayerZOrder: Integer;
begin
  LockRead;
  try
    Result := FLayerZOrder;
  finally
    UnlockRead;
  end;
end;

procedure TLayerDrawConfig.SetLayerZOrder(AValue: Integer);
begin
  LockWrite;
  try
    if FLayerZOrder <> AValue then begin
      FLayerZOrder := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
