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

unit u_ZmpConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ZmpConfig,
  u_ConfigDataElementComplexBase;

type
  TZmpConfig = class(TConfigDataElementComplexBase, IZmpConfig)
  private
    FMaxConnectToServerCount: Cardinal;
    FUseMemCache: Boolean;
    FMemCacheCapacity: Integer;
    FMemCacheTTL: Cardinal;
    FMemCacheClearStrategy: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMaxConnectToServerCount: Cardinal;
    procedure SetMaxConnectToServerCount(const AValue: Cardinal);

    function GetUseMemCache: Boolean;
    procedure SetUseMemCache(const AValue: Boolean);

    function GetMemCacheCapacity: Integer;
    procedure SetMemCacheCapacity(const AValue: Integer);

    function GetMemCacheTTL: Cardinal;
    procedure SetMemCacheTTL(const AValue: Cardinal);

    function GetMemCacheClearStrategy: Integer;
    procedure SetMemCacheClearStrategy(const AValue: Integer);
  public
    constructor Create;
  end;

implementation

{ TZmpConfig }

constructor TZmpConfig.Create;
begin
  inherited Create;
  FMaxConnectToServerCount := 4;
  FUseMemCache := True;
  FMemCacheCapacity := 100;
  FMemCacheTTL := 60000; // ms
  FMemCacheClearStrategy := 1; // csByYongest
end;

procedure TZmpConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FMaxConnectToServerCount := AConfigData.ReadInteger('MaxConnectToServerCount', FMaxConnectToServerCount);
    FUseMemCache := AConfigData.ReadBool('UseMemCache', FUseMemCache);
    FMemCacheCapacity := AConfigData.ReadInteger('MemCacheCapacity', FMemCacheCapacity);
    FMemCacheTTL := AConfigData.ReadInteger('MemCacheTTL', FMemCacheTTL);
    FMemCacheClearStrategy := AConfigData.ReadInteger('MemCacheClearStrategy', FMemCacheClearStrategy);
    SetChanged;
  end;
end;

procedure TZmpConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('MaxConnectToServerCount', FMaxConnectToServerCount);
  AConfigData.WriteBool('UseMemCache', FUseMemCache);
  AConfigData.WriteInteger('MemCacheCapacity', FMemCacheCapacity);
  AConfigData.WriteInteger('MemCacheTTL', FMemCacheTTL);
  AConfigData.WriteInteger('MemCacheClearStrategy', FMemCacheClearStrategy);
end;

function TZmpConfig.GetMaxConnectToServerCount: Cardinal;
begin
  LockRead;
  try
    Result := FMaxConnectToServerCount;
  finally
    UnlockRead;
  end;
end;

function TZmpConfig.GetUseMemCache: Boolean;
begin
  LockRead;
  try
    Result := FUseMemCache;
  finally
    UnlockRead;
  end;
end;

function TZmpConfig.GetMemCacheCapacity: Integer;
begin
  LockRead;
  try
    Result := FMemCacheCapacity;
  finally
    UnlockRead;
  end;
end;

function TZmpConfig.GetMemCacheTTL: Cardinal;
begin
  LockRead;
  try
    Result := FMemCacheTTL;
  finally
    UnlockRead;
  end;
end;

function TZmpConfig.GetMemCacheClearStrategy: Integer;
begin
  LockRead;
  try
    Result := FMemCacheClearStrategy;
  finally
    UnlockRead;
  end;
end;

procedure TZmpConfig.SetMaxConnectToServerCount(const AValue: Cardinal);
begin
  LockWrite;
  try
    if FMaxConnectToServerCount <> AValue then begin
      FMaxConnectToServerCount := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TZmpConfig.SetUseMemCache(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUseMemCache <> AValue then begin
      FUseMemCache := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TZmpConfig.SetMemCacheCapacity(const AValue: Integer);
begin
  LockWrite;
  try
    if FMemCacheCapacity <> AValue then begin
      FMemCacheCapacity := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TZmpConfig.SetMemCacheTTL(const AValue: Cardinal);
begin
  LockWrite;
  try
    if FMemCacheTTL <> AValue then begin
      FMemCacheTTL := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TZmpConfig.SetMemCacheClearStrategy(const AValue: Integer);
begin
  LockWrite;
  try
    if FMemCacheClearStrategy <> AValue then begin
      FMemCacheClearStrategy := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
