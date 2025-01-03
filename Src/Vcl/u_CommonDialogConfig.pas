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

unit u_CommonDialogConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase,
  i_CommonDialogConfig;

type
  TCommonDialogConfig = class(TConfigDataElementBase, ICommonDialogConfig)
  private
    FInitialDir: string;
    FFilterIndex: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { ICommonDialogConfig }
    function GetInitialDir: string;
    procedure SetInitialDir(const AValue: string);
    function GetFilterIndex: Integer;
    procedure SetFilterIndex(const AValue: Integer);
  public
    constructor Create;
  end;


implementation

{ TCommonDialogConfig }

constructor TCommonDialogConfig.Create;
begin
  inherited Create;
  FInitialDir := '';
  FFilterIndex := 1;
end;

procedure TCommonDialogConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FInitialDir := AConfigData.ReadString('InitialDir', FInitialDir);
    FFilterIndex := AConfigData.ReadInteger('FilterIndex', FFilterIndex);
    SetChanged;
  end;
end;

procedure TCommonDialogConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    AConfigData.WriteString('InitialDir', FInitialDir);
    AConfigData.WriteInteger('FilterIndex', FFilterIndex);
  end;
end;

function TCommonDialogConfig.GetFilterIndex: Integer;
begin
  LockRead;
  try
    Result := FFilterIndex;
  finally
    UnlockRead;
  end;
end;

function TCommonDialogConfig.GetInitialDir: string;
begin
  LockRead;
  try
    Result := FInitialDir;
  finally
    UnlockRead;
  end;
end;

procedure TCommonDialogConfig.SetFilterIndex(const AValue: Integer);
begin
  LockWrite;
  try
    if AValue <> FFilterIndex then begin
      FFilterIndex := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TCommonDialogConfig.SetInitialDir(const AValue: string);
begin
  LockWrite;
  try
    if AValue <> FInitialDir then begin
      FInitialDir := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
