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
    FLogin: string;
    FPassword: string;
    FProxyType: TProxyServerType;
  private
    function GetUseIESettings: Boolean;
    function GetUseProxy: Boolean;
    function GetHost: AnsiString;
    function GetUseLogin: boolean;
    function GetLogin: string;
    function GetPassword: string;
    function GetProxyType: TProxyServerType;
  public
    constructor Create(
      const AUseIESettings: Boolean;
      const AUseProxy: Boolean;
      const AHost: AnsiString;
      const AUseLogin: boolean;
      const ALogin: string;
      const APassword: string;
      const AProxyType: TProxyServerType
    );
  end;

  TProxyConfig = class(TConfigDataElementWithStaticBase, IProxyConfig)
  private
    FUseIESettings: Boolean;
    FUseProxy: Boolean;
    FProxyType: TProxyServerType;
    FHost: AnsiString;
    FUseLogin: boolean;
    FLogin: string;
    FPassword: string;
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

    function GetProxyType: TProxyServerType;
    procedure SetProxyType(const AValue: TProxyServerType);

    function GetHost: AnsiString; safecall;
    procedure SetHost(const AValue: AnsiString);

    function GetUseLogin: boolean; safecall;
    procedure SetUseLogin(AValue: Boolean);

    function GetLogin: string; safecall;
    procedure SetLogin(const AValue: string);

    function GetPassword: string; safecall;
    procedure SetPassword(const AValue: string);

    function GetStatic: IProxyConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils;

{ TProxyConfig }

constructor TProxyConfig.Create;
begin
  inherited Create;
  FUseIESettings := True;
  FUseProxy := False;
  FUseLogin := False;
  FHost := '127.0.0.1:8080';
  FLogin := '';
  FPassword := '';
  FProxyType := ptHttp;
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
      FPassword,
      FProxyType
    );
  Result := VStatic;
end;

procedure TProxyConfig.DoReadConfig(const AConfigData: IConfigDataProvider);

  procedure SetProxyType(const AValue: Integer);
  const
    cKnownTypes: set of TProxyServerType = [
      ptHttp, ptHttps, ptSocks4, ptSocks4a, ptSocks5, ptSocks5h
    ];
  begin
    if TProxyServerType(AValue) in cKnownTypes then begin
      FProxyType := TProxyServerType(AValue);
    end else begin
      Assert(False, Format('Unexpected ProxyType value: %d', [AValue]));
    end;
  end;

  function StrToProxyAddress(const AStr: AnsiString): AnsiString;
  var
    I: Integer;
    VStr: string;
  begin
    VStr := Trim(string(AStr));

    I := Pos('=', VStr);
    if I > 0 then begin
      Delete(VStr, 1, I);
    end;

    I := Pos('://', VStr);
    if I > 0 then begin
      Delete(VStr, 1, I+2);
    end;

    Result := AnsiString(VStr);
  end;

begin
  inherited;
  if AConfigData <> nil then begin
    FUseIESettings := AConfigData.ReadBool('UseIEProxySettings', FUseIESettings);
    FUseProxy := AConfigData.ReadBool('UseProxy', FUseProxy);
    FHost := StrToProxyAddress(AConfigData.ReadAnsiString('Host', FHost));
    FUseLogin := AConfigData.ReadBool('UseAuth', FUseLogin);
    FLogin := AConfigData.ReadString('Login', FLogin);
    FPassword := AConfigData.ReadString('Password', FPassword);
    SetProxyType( AConfigData.ReadInteger('ProxyType', Integer(FProxyType)) );
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
  AConfigData.WriteString('Login', FLogin);
  AConfigData.WriteString('Password', FPassword);
  AConfigData.WriteInteger('ProxyType', Integer(FProxyType));
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

function TProxyConfig.GetLogin: string;
begin
  LockRead;
  try
    Result := FLogin;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetPassword: string;
begin
  LockRead;
  try
    Result := FPassword;
  finally
    UnlockRead;
  end;
end;

function TProxyConfig.GetProxyType: TProxyServerType;
begin
  LockRead;
  try
    Result := FProxyType;
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

procedure TProxyConfig.SetLogin(const AValue: string);
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

procedure TProxyConfig.SetPassword(const AValue: string);
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

procedure TProxyConfig.SetProxyType(const AValue: TProxyServerType);
begin
  LockWrite;
  try
    if FProxyType <> AValue then begin
      FProxyType := AValue;
      SetChanged;
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
  const AUseIESettings: Boolean;
  const AUseProxy: Boolean;
  const AHost: AnsiString;
  const AUseLogin: Boolean;
  const ALogin, APassword: string;
  const AProxyType: TProxyServerType
);
begin
  inherited Create;
  FUseIESettings := AUseIESettings;
  FUseProxy := AUseProxy;
  FHost := AHost;
  FUseLogin := AUseLogin;
  FLogin := ALogin;
  FPassword := APassword;
  FProxyType := AProxyType;
end;

function TProxyConfigStatic.GetHost: AnsiString;
begin
  Result := FHost;
end;

function TProxyConfigStatic.GetLogin: string;
begin
  Result := FLogin;
end;

function TProxyConfigStatic.GetPassword: string;
begin
  Result := FPassword;
end;

function TProxyConfigStatic.GetProxyType: TProxyServerType;
begin
  Result := FProxyType;
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
