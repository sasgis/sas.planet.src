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

unit u_ZmpConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ZmpConfig,
  u_ConfigDataElementComplexBase;

type
  TZmpConfig = class(TConfigDataElementComplexBase, IZmpConfig)
  private
    FMaxConnectToServerCount: Cardinal;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMaxConnectToServerCount: Cardinal;
    procedure SetMaxConnectToServerCount(AValue: Cardinal);
  public
    constructor Create;
  end;

implementation

{ TZmpConfig }

constructor TZmpConfig.Create;
begin
  inherited;
  FMaxConnectToServerCount := 4;
end;

procedure TZmpConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FMaxConnectToServerCount := AConfigData.ReadInteger('MaxConnectToServerCount', FMaxConnectToServerCount);
    SetChanged;
  end;
end;

procedure TZmpConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('MaxConnectToServerCount', FMaxConnectToServerCount);
end;

function TZmpConfig.GetMaxConnectToServerCount: Cardinal;
begin
  LockRead;
  try
    Result := FMaxConnectToServerCount;
  finally
    UnlockRead;
  end;
end;

procedure TZmpConfig.SetMaxConnectToServerCount(AValue: Cardinal);
begin
  LockWrite;
  try
    if FMaxConnectToServerCount <> AValue then begin
      FMaxConnectToServerCount := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
