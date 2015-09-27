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

unit u_MapVersionRequestConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_Listener,
  i_MapVersionInfo,
  i_MapVersionFactory,
  i_MapVersionRequest,
  i_MapVersionRequestConfig,
  u_ConfigDataElementBase;

type
  TMapVersionRequestConfig = class(TConfigDataElementWithStaticBase, IMapVersionRequestConfig)
  private
    FVersionDef: string;
    FVersion: string;
    FShowPrevVersion: Boolean;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetVersion: string;
    procedure SetVersion(const AValue: string);

    function GetShowPrevVersion: Boolean;
    procedure SetShowPrevVersion(const AValue: Boolean);

    function GetStatic: IMapVersionRequestConfigStatic;
  public
    constructor Create(
      const AVersion: string
    );
  end;


implementation

uses
  u_ListenerByEvent,
  u_MapVersionRequest,
  u_BaseInterfacedObject;

type
  TMapVersionRequestConfigStatic = class(TBaseInterfacedObject, IMapVersionRequestConfigStatic)
  private
    FVersion: string;
    FShowPrevVersion: Boolean;
  private
    function GetVersion: string;
    function GetShowPrevVersion: Boolean;
  public
    constructor Create(
      const AVersion: string;
      const AShowPrevVersion: Boolean
    );
  end;

{ TMapVersionRequestConfigStatic }

constructor TMapVersionRequestConfigStatic.Create(
  const AVersion: string;
  const AShowPrevVersion: Boolean
);
begin
  inherited Create;
  FVersion := AVersion;
  FShowPrevVersion := AShowPrevVersion;
end;

function TMapVersionRequestConfigStatic.GetShowPrevVersion: Boolean;
begin
  Result := FShowPrevVersion;
end;

function TMapVersionRequestConfigStatic.GetVersion: string;
begin
  Result := FVersion;
end;

{ TMapVersionRequestConfig }

constructor TMapVersionRequestConfig.Create(
  const AVersion: string
);
begin
  inherited Create;
  FVersionDef := AVersion;
  FVersion := AVersion;
  FShowPrevVersion := True;
end;

function TMapVersionRequestConfig.CreateStatic: IInterface;
begin
  Result := IMapVersionRequestConfigStatic(TMapVersionRequestConfigStatic.Create(FVersion, FShowPrevVersion));
end;

procedure TMapVersionRequestConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVersion := AConfigData.ReadString('Version', FVersion);
    FShowPrevVersion := AConfigData.ReadBool('ShowPrevVersion', FShowPrevVersion);
    SetChanged;
  end;
end;

procedure TMapVersionRequestConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  VStoreString: string;
begin
  inherited;
  VStoreString := FVersion;
  if VStoreString <> FVersionDef then begin
    AConfigData.WriteString('Version', VStoreString);
  end else begin
    AConfigData.DeleteValue('Version');
  end;
  AConfigData.WriteBool('ShowPrevVersion', FShowPrevVersion);
end;

function TMapVersionRequestConfig.GetShowPrevVersion: Boolean;
begin
  LockRead;
  try
    Result := FShowPrevVersion;
  finally
    UnlockRead;
  end;
end;

function TMapVersionRequestConfig.GetStatic: IMapVersionRequestConfigStatic;
begin
  Result := IMapVersionRequestConfigStatic(GetStaticInternal);
end;

function TMapVersionRequestConfig.GetVersion: string;
begin
  LockRead;
  try
    Result := FVersion;
  finally
    UnlockRead;
  end;
end;

procedure TMapVersionRequestConfig.SetShowPrevVersion(const AValue: Boolean);
begin
  LockWrite;
  try
    if FShowPrevVersion <> AValue then begin
      FShowPrevVersion := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapVersionRequestConfig.SetVersion(const AValue: string);
begin
  LockWrite;
  try
    if FVersion <> AValue then begin
      FVersion := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
