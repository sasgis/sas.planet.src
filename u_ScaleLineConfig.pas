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

unit u_ScaleLineConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ScaleLineConfig,
  u_ConfigDataElementBase;

type
  TScaleLineConfig = class(TConfigDataElementBase, IScaleLineConfig)
  private
    FVisible: Boolean;
    FBottomMargin: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetBottomMargin: Integer;
    procedure SetBottomMargin(AValue: Integer);
  public
    constructor Create;
  end;

implementation

{ TScaleLineConfig }

constructor TScaleLineConfig.Create;
begin
  inherited;
  FVisible := True;
  FBottomMargin := 0;
end;

procedure TScaleLineConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    SetChanged;
  end;
end;

procedure TScaleLineConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
end;

function TScaleLineConfig.GetBottomMargin: Integer;
begin
  LockRead;
  try
    Result := FBottomMargin;
  finally
    UnlockRead;
  end;
end;

function TScaleLineConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TScaleLineConfig.SetBottomMargin(AValue: Integer);
begin
  LockWrite;
  try
    if FBottomMargin <> AValue then begin
      FBottomMargin := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TScaleLineConfig.SetVisible(AValue: Boolean);
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
