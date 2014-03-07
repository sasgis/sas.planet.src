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

unit u_ProxyConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ProxySettings,
  u_ConfigDataElementBase,
  u_BaseInterfacedObject;

type
  TProxyConfigStatic = class(TBaseInterfacedObject, IProxyConfigStatic)
  private
    FUseIESettings: Boolean;
    FUseProxy: Boolean;
    FHost: AnsiString;
    FUseLogin: boolean;
    FLogin: AnsiString;
    FPassword: AnsiString;
  private
    function GetUseIESettings: Boolean;
    function GetUseProxy: Boolean;
    function GetHost: AnsiString;
    function GetUseLogin: boolean;
    function GetLogin: AnsiString;
    function GetPassword: AnsiString;
  public
    constructor Create(
      AUseIESettings: Boolean;
      AUseProxy: Boolean;
      const AHost: AnsiString;
      AUseLogin: boolean;
      const ALogin: AnsiString;
      const APassword: AnsiString
    );
  end;

  TProxyConfig = class(TConfigDataElementWithStaticBase, IProxyConfig)
  private
    FUseIESettings: Boolean;
    FUseProxy: Boolean;
    FHost: AnsiString;
    FUseLogin: boolean;
    FLogin: AnsiString;
    FPassword: AnsiString;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetUseIESettings: Boolean; safecall;
    procedure SetUseIESettings(AValue: Boolean);

    function GetUseProxy: Boolean; safecall;
    procedure SetUseProxy(AValue: Boolean);

    function GetHost: AnsiString; safecall;
    procedure SetHost(const AValue: AnsiString);

    function GetUseLogin: boolean; safecall;
    procedure SetUseLogin(AValue: Boolean);

    function GetLogin: AnsiString; safecall;
    procedure SetLogin(const AValue: AnsiString);

    function GetPassword: AnsiString; safecall;
    procedure SetPassword(const AValue: AnsiString);

    function GetStatic: IProxyConfigStatic;
  public
    constructor Create;
  end;

implementation

{ TProxyConfig }

constructor TProxyConfig.Create;
begin
  inherited Create;
  FUseIESettings := True;
  FUseProxy := False;
  FUseLogin := False;
  FHost := '';
  FLogin := '';
  FPassword := '';
end;

function TProxyConfig.CreateStatic: IInterface;
var
  VStatic: IProxyConfigStatic;
begin
  VStatic :=
    TProxyConfigStatic.Create(
      FUseIESettings,
      FUseProxy,
      FHost,
      FUseLogin,
      FLogin,
      FPassword
    );
  Result := VStatic;
end;

procedure TProxyConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUseIESettings := AConfigData.ReadBool('UseIEProxySettings', FUseIESettings);
    FUseProxy := AConfigData.ReadBool('UseProxy', FUseProxy);
    FHost := AConfigData.ReadAnsiString('Host', FHost);
    FUseLogin := AConfigData.ReadBool('UseAuth', FUseLogin);
    FLogin := AConfigData.ReadAnsiString('Login', FLogin);
    FPassword := AConfigData.ReadAnsiString('Password', FPassword);
    SetChanged;
  end;
end;

procedure TProxyConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UseIEProxySettings', FUseIESettings);
  AConfigData.WriteBool('UseProxy', FUseProxy);
  AConfigData.WriteAnsiString('Host', FHost);
  AConfigData.WriteBool('UseAuth', FUseLogin);
  AConfigData.WriteAnsiString('Login', FLogin);
  AConfigData.WriteAnsiString('Password', FPassword);
end;

function TProxyConfig.GetHost: AnsiString;
begin
  LockRead;
  try
    Result := FHost;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetLogin: AnsiString;
begin
  LockRead;
  try
    Result := FLogin;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetPassword: AnsiString;
begin
  LockRead;
  try
    Result := FPassword;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetStatic: IProxyConfigStatic;
begin
  Result := IProxyConfigStatic(GetStaticInternal);
end;

function TProxyConfig.GetUseIESettings: Boolean;
begin
  LockRead;
  try
    Result := FUseIESettings;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetUseLogin: boolean;
begin
  LockRead;
  try
    Result := FUseLogin;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetUseProxy: Boolean;
begin
  LockRead;
  try
    Result := FUseProxy;
  finally
    UnlockRead;
  end;
end;

procedure TProxyConfig.SetHost(const AValue: AnsiString);
begin
  LockWrite;
  try
    if FHost <> AValue then begin
      FHost := AValue;
      if not FUseIESettings and FUseProxy then begin
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TProxyConfig.SetLogin(const AValue: AnsiString);
begin
  LockWrite;
  try
    if FLogin <> AValue then begin
      FLogin := AValue;
      if not FUseIESettings and FUseProxy and FUseLogin then begin
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TProxyConfig.SetPassword(const AValue: AnsiString);
begin
  LockWrite;
  try
    if FPassword <> AValue then begin
      FPassword := AValue;
      if not FUseIESettings and FUseProxy and FUseLogin then begin
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TProxyConfig.SetUseIESettings(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseIESettings <> AValue then begin
      FUseIESettings := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;

end;

procedure TProxyConfig.SetUseLogin(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseLogin <> AValue then begin
      FUseLogin := AValue;
      if not FUseIESettings and FUseProxy then begin
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TProxyConfig.SetUseProxy(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseProxy <> AValue then begin
      FUseProxy := AValue;
      if not FUseIESettings then begin
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TProxyConfigStatic }

constructor TProxyConfigStatic.Create(
  AUseIESettings, AUseProxy: Boolean;
  const AHost: AnsiString;
  AUseLogin: boolean;
  const ALogin, APassword: AnsiString
);
begin
  inherited Create;
  FUseIESettings := AUseIESettings;
  FUseProxy := AUseProxy;
  FHost := AHost;
  FUseLogin := AUseLogin;
  FLogin := ALogin;
  FPassword := APassword;
end;

function TProxyConfigStatic.GetHost: AnsiString;
begin
  Result := FHost;
end;

function TProxyConfigStatic.GetLogin: AnsiString;
begin
  Result := FLogin;
end;

function TProxyConfigStatic.GetPassword: AnsiString;
begin
  Result := FPassword;
end;

function TProxyConfigStatic.GetUseIESettings: Boolean;
begin
  Result := FUseIESettings;
end;

function TProxyConfigStatic.GetUseLogin: boolean;
begin
  Result := FUseLogin;
end;

function TProxyConfigStatic.GetUseProxy: Boolean;
begin
  Result := FUseProxy;
end;

end.
