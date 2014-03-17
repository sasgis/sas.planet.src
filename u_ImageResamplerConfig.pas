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

unit u_ImageResamplerConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ImageResamplerConfig,
  u_ConfigDataElementBase;

type
  TImageResamplerConfig = class(TConfigDataElementBase, IImageResamplerConfig)
  private
    FDefGUID: TGUID;
    FActiveGUID: TGUID;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetDefaultGUID: TGUID;
    function GetActiveGUID: TGUID;
    procedure SetActiveGUID(const AValue: TGUID);
  public
    constructor Create(
      const ADefGUID: TGUID
    );
  end;

implementation

uses
  SysUtils,
  u_ConfigProviderHelpers;

{ TMainFormMainConfig }

constructor TImageResamplerConfig.Create(
  const ADefGUID: TGUID
);
begin
  inherited Create;
  FActiveGUID := ADefGUID;
end;

procedure TImageResamplerConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetActiveGUID(ReadGUID(AConfigData, 'ResamplingType', FActiveGUID));
  end;
end;

procedure TImageResamplerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  WriteGUID(AConfigData, 'ResamplingType', FActiveGUID);
end;

function TImageResamplerConfig.GetActiveGUID: TGUID;
begin
  LockRead;
  try
    Result := FActiveGUID;
  finally
    UnlockRead;
  end;
end;

function TImageResamplerConfig.GetDefaultGUID: TGUID;
begin
  Result := FDefGUID;
end;

procedure TImageResamplerConfig.SetActiveGUID(const AValue: TGUID);
begin
  LockWrite;
  try
    if not IsEqualGUID(FActiveGUID, AValue) then begin
      FActiveGUID := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
