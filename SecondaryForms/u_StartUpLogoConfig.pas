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

unit u_StartUpLogoConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_StartUpLogoConfig,
  u_ConfigDataElementBase;

type
  TStartUpLogoConfig = class(TConfigDataElementBase, IStartUpLogoConfig)
  private
    FIsShowLogo: Boolean;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetIsShowLogo: Boolean;
    procedure SetIsShowLogo(AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TGlobalAppConfig }

constructor TStartUpLogoConfig.Create;
begin
  inherited Create;
  FIsShowLogo := True;
end;

procedure TStartUpLogoConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsShowLogo := AConfigData.ReadBool('ShowLogo', FIsShowLogo);
    SetChanged;
  end;
end;

procedure TStartUpLogoConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('ShowLogo', FIsShowLogo);
end;

function TStartUpLogoConfig.GetIsShowLogo: Boolean;
begin
  LockRead;
  try
    Result := FIsShowLogo;
  finally
    UnlockRead;
  end;
end;

procedure TStartUpLogoConfig.SetIsShowLogo(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsShowLogo <> AValue then begin
      FIsShowLogo := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
