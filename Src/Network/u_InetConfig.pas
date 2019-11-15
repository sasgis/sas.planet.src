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

unit u_InetConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_WinInetConfig,
  i_ProxySettings,
  i_InetConfig,
  u_ConfigDataElementComplexBase;

type
  TInetConfig = class(TConfigDataElementComplexWithStaticBase, IInetConfig)
  private
    FUserAgentString: AnsiString;
    FTimeOut: Cardinal;
    FProxyConfig: IProxyConfig;
    FWinInetConfig: IWinInetConfig;
    FSleepOnResetConnection: Cardinal;
    FDownloadTryCount: Integer;
    FNetworkEngineType: TNetworkEngineType;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetWinInetConfig: IWinInetConfig;

    function GetProxyConfig: IProxyConfig;

    function GetUserAgentString: AnsiString;
    procedure SetUserAgentString(const AValue: AnsiString);

    function GetTimeOut: Cardinal;
    procedure SetTimeOut(AValue: Cardinal);

    function GetSleepOnResetConnection: Cardinal;
    procedure SetSleepOnResetConnection(AValue: Cardinal);

    function GetDownloadTryCount: Integer;
    procedure SetDownloadTryCount(AValue: Integer);

    function GetNetworkEngineType: TNetworkEngineType;
    procedure SetNetworkEngineType(const AValue: TNetworkEngineType);

    function GetStatic: IInetConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  c_InetConfig,
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_InetConfigStatic,
  u_WinInetConfig,
  u_ProxyConfig;

{ TInetConfig }

constructor TInetConfig.Create;
begin
  inherited Create;
  FUserAgentString := cUserAgent;
  FTimeOut := 40000;
  FSleepOnResetConnection := 30000;
  FDownloadTryCount := 2;
  FNetworkEngineType := neWinInet;

  FProxyConfig := TProxyConfig.Create;
  Add(FProxyConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Proxy'));

  FWinInetConfig := TWinInetConfig.Create;
  Add(FWinInetConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('WinInet'));
end;

function TInetConfig.CreateStatic: IInterface;
var
  VStatic: IInetConfigStatic;
begin
  VStatic :=
    TInetConfigStatic.Create(
      FWinInetConfig.GetStatic,
      FProxyConfig.GetStatic,
      FUserAgentString,
      FTimeOut,
      FSleepOnResetConnection,
      FDownloadTryCount,
      FNetworkEngineType
    );
  Result := VStatic;
end;

procedure TInetConfig.DoReadConfig(const AConfigData: IConfigDataProvider);

  procedure SetNetworkEngineType(const ATypeId: Integer);
  begin
    if TNetworkEngineType(ATypeId) in [neWinInet, neCurl] then begin
      FNetworkEngineType := TNetworkEngineType(ATypeId);
    end else begin
      Assert(False, Format('Unexpected NetworkEngineType Id: %d', [ATypeId]));
    end;
  end;

begin
  inherited;
  if AConfigData <> nil then begin
    FUserAgentString := AConfigData.ReadAnsiString('UserAgentString', FUserAgentString);
    FTimeOut := AConfigData.ReadInteger('TimeOut', FTimeOut);
    SetDownloadTryCount(AConfigData.ReadInteger('DownloadTryCount', FDownloadTryCount));
    FSleepOnResetConnection := AConfigData.ReadInteger('SleepOnResetConnection', FSleepOnResetConnection);
    SetNetworkEngineType(
      AConfigData.ReadInteger('NetworkEngine', Integer(FNetworkEngineType))
    );
    SetChanged;
  end;
end;

procedure TInetConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteAnsiString('UserAgentString', FUserAgentString);
  AConfigData.WriteInteger('TimeOut', FTimeOut);
  AConfigData.WriteInteger('DownloadTryCount', FDownloadTryCount);
  AConfigData.WriteInteger('SleepOnResetConnection', FSleepOnResetConnection);
  AConfigData.WriteInteger('NetworkEngine', Integer(FNetworkEngineType));
end;

function TInetConfig.GetDownloadTryCount: Integer;
begin
  LockRead;
  try
    Result := FDownloadTryCount;
  finally
    UnlockRead;
  end;
end;

function TInetConfig.GetNetworkEngineType: TNetworkEngineType;
begin
  LockRead;
  try
    Result := FNetworkEngineType
  finally
    UnlockRead;
  end;
end;

function TInetConfig.GetWinInetConfig: IWinInetConfig;
begin
  Result := FWinInetConfig;
end;

function TInetConfig.GetProxyConfig: IProxyConfig;
begin
  Result := FProxyConfig;
end;

function TInetConfig.GetSleepOnResetConnection: Cardinal;
begin
  LockRead;
  try
    Result := FSleepOnResetConnection;
  finally
    UnlockRead;
  end;
end;

function TInetConfig.GetStatic: IInetConfigStatic;
begin
  Result := IInetConfigStatic(GetStaticInternal);
end;

function TInetConfig.GetTimeOut: Cardinal;
begin
  LockRead;
  try
    Result := FTimeOut;
  finally
    UnlockRead;
  end;
end;

function TInetConfig.GetUserAgentString: AnsiString;
begin
  LockRead;
  try
    Result := FUserAgentString;
  finally
    UnlockRead;
  end;
end;

procedure TInetConfig.SetDownloadTryCount(AValue: Integer);
begin
  if (AValue > 0) and (AValue < 100) then begin
    LockWrite;
    try
      if FDownloadTryCount <> AValue then begin
        FDownloadTryCount := AValue;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TInetConfig.SetNetworkEngineType(const AValue: TNetworkEngineType);
begin
  LockWrite;
  try
    if FNetworkEngineType <> AValue then begin
      FNetworkEngineType := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TInetConfig.SetSleepOnResetConnection(AValue: Cardinal);
begin
  LockWrite;
  try
    if FSleepOnResetConnection <> AValue then begin
      FSleepOnResetConnection := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TInetConfig.SetTimeOut(AValue: Cardinal);
begin
  LockWrite;
  try
    if FTimeOut <> AValue then begin
      FTimeOut := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TInetConfig.SetUserAgentString(const AValue: AnsiString);
begin
  LockWrite;
  try
    if FUserAgentString <> AValue then begin
      FUserAgentString := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
