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

unit u_MarksGUIConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarksGUIConfig,
  u_ConfigDataElementBase;

type
  TMarksGUIConfig = class(TConfigDataElementBase, IMarksGUIConfig)
  private
    FIsAddTypeToCaption: Boolean;
    FIsAddTimeToDescription: Boolean;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetIsAddTypeToCaption: Boolean;
    procedure SetIsAddTypeToCaption(AValue: Boolean);

    function GetIsAddTimeToDescription: Boolean;
    procedure SetIsAddTimeToDescription(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TMarksGUIConfig }

constructor TMarksGUIConfig.Create;
begin
  inherited Create;
  FIsAddTypeToCaption := True;
  FIsAddTimeToDescription := True;
end;

procedure TMarksGUIConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsAddTypeToCaption := AConfigData.ReadBool('IsAddTypeToCaption', FIsAddTypeToCaption);
    FIsAddTimeToDescription := AConfigData.ReadBool('IsAddTimeToDescription', FIsAddTimeToDescription);
    SetChanged;
  end;
end;

procedure TMarksGUIConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('IsAddTypeToCaption', FIsAddTypeToCaption);
  AConfigData.WriteBool('IsAddTimeToDescription', FIsAddTimeToDescription);
end;

function TMarksGUIConfig.GetIsAddTimeToDescription: Boolean;
begin
  LockRead;
  try
    Result := FIsAddTimeToDescription;
  finally
    UnlockRead;
  end;
end;

function TMarksGUIConfig.GetIsAddTypeToCaption: Boolean;
begin
  LockRead;
  try
    Result := FIsAddTypeToCaption;
  finally
    UnlockRead;
  end;
end;

procedure TMarksGUIConfig.SetIsAddTimeToDescription(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsAddTimeToDescription <> AValue then begin
      FIsAddTimeToDescription := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarksGUIConfig.SetIsAddTypeToCaption(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsAddTypeToCaption <> AValue then begin
      FIsAddTypeToCaption := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
