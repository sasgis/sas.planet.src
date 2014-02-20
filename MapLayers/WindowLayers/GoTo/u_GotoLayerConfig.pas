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

unit u_GotoLayerConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GotoLayerConfig,
  u_ConfigDataElementBase;

type
  TGotoLayerConfig = class(TConfigDataElementBase, IGotoLayerConfig)
  private
    FShowTickCount: Cardinal;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetShowTickCount: Cardinal;
    procedure SetShowTickCount(AValue: Cardinal);
  public
    constructor Create;
  end;

implementation

{ TGotoLayerConfig }

constructor TGotoLayerConfig.Create;
begin
  inherited Create;
  FShowTickCount := 20000;
end;

procedure TGotoLayerConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FShowTickCount := AConfigData.ReadInteger('ShowTickCount', FShowTickCount);
    SetChanged;
  end;
end;

procedure TGotoLayerConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('ShowTickCount', FShowTickCount);
end;

function TGotoLayerConfig.GetShowTickCount: Cardinal;
begin
  LockRead;
  try
    Result := FShowTickCount;
  finally
    UnlockRead;
  end;
end;

procedure TGotoLayerConfig.SetShowTickCount(AValue: Cardinal);
begin
  LockWrite;
  try
    if FShowTickCount <> AValue then begin
      FShowTickCount := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
