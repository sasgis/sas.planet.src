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

unit u_MapAbilitiesConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapAbilitiesConfig,
  u_ConfigDataElementBase;

type
  TMapAbilitiesConfig = class(TConfigDataElementWithStaticBase, IMapAbilitiesConfig)
  private
    FDefConfig: IMapAbilitiesConfigStatic;

    FIsShowOnSmMap: Boolean;
    FUseDownload: Boolean;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetIsShowOnSmMap: Boolean;
    procedure SetIsShowOnSmMap(AValue: Boolean);

    function GetUseDownload: Boolean;
    procedure SetUseDownload(AValue: Boolean);

    function GetStatic: IMapAbilitiesConfigStatic;
  public
    constructor Create(
      const ADefConfig: IMapAbilitiesConfigStatic
    );
  end;

implementation

uses
  u_MapAbilitiesConfigStatic;

{ TMapAbilitiesConfig }

constructor TMapAbilitiesConfig.Create(
  const ADefConfig: IMapAbilitiesConfigStatic
);
begin
  inherited Create;
  FDefConfig := ADefConfig;

  FIsShowOnSmMap := FDefConfig.IsShowOnSmMap;
  FUseDownload := FDefConfig.UseDownload;
end;

function TMapAbilitiesConfig.CreateStatic: IInterface;
var
  VStatic: IMapAbilitiesConfigStatic;
begin
  VStatic :=
    TMapAbilitiesConfigStatic.Create(
      FIsShowOnSmMap,
      FUseDownload
    );
  Result := VStatic;
end;

procedure TMapAbilitiesConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetIsShowOnSmMap(AConfigData.ReadBool('CanShowOnSmMap', FIsShowOnSmMap));
    SetUseDownload(AConfigData.ReadBool('UseDwn', FUseDownload));
    SetChanged;
  end;
end;

procedure TMapAbilitiesConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  if FIsShowOnSmMap <> FDefConfig.IsShowOnSmMap then begin
    AConfigData.WriteBool('CanShowOnSmMap', FIsShowOnSmMap);
  end else begin
    AConfigData.DeleteValue('CanShowOnSmMap');
  end;
  if FUseDownload <> FDefConfig.UseDownload then begin
    AConfigData.WriteBool('UseDwn', FUseDownload);
  end else begin
    AConfigData.DeleteValue('UseDwn');
  end;
end;

function TMapAbilitiesConfig.GetIsShowOnSmMap: Boolean;
begin
  LockRead;
  try
    Result := FIsShowOnSmMap;
  finally
    UnlockRead;
  end;
end;

function TMapAbilitiesConfig.GetUseDownload: Boolean;
begin
  LockRead;
  try
    Result := FUseDownload;
  finally
    UnlockRead;
  end;
end;

function TMapAbilitiesConfig.GetStatic: IMapAbilitiesConfigStatic;
begin
  Result := IMapAbilitiesConfigStatic(GetStaticInternal);
end;

procedure TMapAbilitiesConfig.SetIsShowOnSmMap(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsShowOnSmMap <> AValue then begin
      FIsShowOnSmMap := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapAbilitiesConfig.SetUseDownload(AValue: Boolean);
var
  VValue: Boolean;
begin
  LockWrite;
  try
    VValue := FDefConfig.UseDownload and AValue;
    if FUseDownload <> VValue then begin
      FUseDownload := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
