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

unit u_InternalDebugConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_InternalDebugConfig,
  u_ConfigDataElementBase;

type
  TInternalDebugConfig = class(TConfigDataElementBase, IInternalDebugConfig)
  private
    FIsShowDebugInfo: Boolean;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetIsShowDebugInfo: Boolean;
    procedure SetIsShowDebugInfo(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TInternalDebugConfig }

constructor TInternalDebugConfig.Create;
begin
  inherited Create;
  {$IFDEF DEBUG}
  FIsShowDebugInfo := True;
  {$ELSE}
  FIsShowDebugInfo := False;
  {$ENDIF}
end;

procedure TInternalDebugConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsShowDebugInfo := AConfigData.ReadBool('ShowDebugInfo', FIsShowDebugInfo);
    SetChanged;
  end;
end;

procedure TInternalDebugConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
end;

function TInternalDebugConfig.GetIsShowDebugInfo: Boolean;
begin
  LockRead;
  try
    Result := FIsShowDebugInfo;
  finally
    UnlockRead;
  end;
end;

procedure TInternalDebugConfig.SetIsShowDebugInfo(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsShowDebugInfo <> AValue then begin
      FIsShowDebugInfo := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
