{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_WinInetConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_WinInetConfig,
  u_ConfigDataElementBase,
  u_BaseInterfacedObject;

type
  TWinInetConfigStatic = class(TBaseInterfacedObject, IWinInetConfigStatic)
  private
    FMaxConnsPerServer: TConnsPerServerRec;
    FMaxConnsPerProxy: TConnsPerServerRec;
  private
    function GetMaxConnsPerServer: TConnsPerServerRec;
    function GetMaxConnsPerProxy: TConnsPerServerRec;
  public
    constructor Create(
      const AMaxConnsPerServer: TConnsPerServerRec;
      const AMaxConnsPerProxy: TConnsPerServerRec
    );
  end;

  TWinInetConfig = class(TConfigDataElementWithStaticBase, IWinInetConfig)
  private
    FMaxConnsPerServer: TConnsPerServerRec;
    FMaxConnsPerProxy: TConnsPerServerRec;
    procedure _CheckValue(var ARec: TConnsPerServerRec);
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMaxConnsPerServer: TConnsPerServerRec;
    procedure SetMaxConnsPerServer (const AValue: TConnsPerServerRec);

    function GetMaxConnsPerProxy: TConnsPerServerRec;
    procedure SetMaxConnsPerProxy(const AValue: TConnsPerServerRec);

    function GetStatic: IWinInetConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  WinInetFix;

{ TWinInetConfig }

constructor TWinInetConfig.Create;

  procedure _SetMinMax(var ARec: TConnsPerServerRec);
  begin
    ARec.Min := 2;
    ARec.Max := 128;
  end;

begin
  inherited Create;

  FMaxConnsPerServer.IsAvailable := True;
  FMaxConnsPerServer.Value := 4;
  FMaxConnsPerServer.Def := 2;
  _SetMinMax(FMaxConnsPerServer);

  FMaxConnsPerProxy.IsAvailable := IsMaxConnsPerProxyAvailable;
  FMaxConnsPerProxy.Value := 4;
  FMaxConnsPerProxy.Def := 4;
  _SetMinMax(FMaxConnsPerProxy);
end;

procedure TWinInetConfig._CheckValue(var ARec: TConnsPerServerRec);
begin
  if ARec.Value < ARec.Min then begin
    ARec.Value := ARec.Min;
  end else
  if ARec.Value > ARec.Max then begin
    ARec.Value := ARec.Max;
  end;
end;

function TWinInetConfig.CreateStatic: IInterface;
var
  VStatic: IWinInetConfigStatic;
begin
  VStatic :=
    TWinInetConfigStatic.Create(
      FMaxConnsPerServer,
      FMaxConnsPerProxy
    );
  Result := VStatic;
end;

procedure TWinInetConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FMaxConnsPerServer.Value := AConfigData.ReadInteger('MaxConnsPerServer', FMaxConnsPerServer.Value);
    _CheckValue(FMaxConnsPerServer);

    FMaxConnsPerProxy.Value := AConfigData.ReadInteger('MaxConnsPerProxy', FMaxConnsPerProxy.Value);
    _CheckValue(FMaxConnsPerProxy);

    SetChanged;
  end;
end;

procedure TWinInetConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('MaxConnsPerServer', FMaxConnsPerServer.Value);
  AConfigData.WriteInteger('MaxConnsPerProxy', FMaxConnsPerProxy.Value);
end;

function TWinInetConfig.GetMaxConnsPerServer: TConnsPerServerRec;
begin
  LockRead;
  try
    Result := FMaxConnsPerServer;
  finally
    UnlockRead;
  end;
end;

function TWinInetConfig.GetMaxConnsPerProxy: TConnsPerServerRec;
begin
  LockRead;
  try
    Result := FMaxConnsPerProxy;
  finally
    UnlockRead;
  end;
end;

function TWinInetConfig.GetStatic: IWinInetConfigStatic;
begin
  Result := IWinInetConfigStatic(GetStaticInternal);
end;

procedure TWinInetConfig.SetMaxConnsPerServer(const AValue: TConnsPerServerRec);
begin
  LockWrite;
  try
    if FMaxConnsPerServer <> AValue then begin
      FMaxConnsPerServer := AValue;
      _CheckValue(FMaxConnsPerServer);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TWinInetConfig.SetMaxConnsPerProxy(const AValue: TConnsPerServerRec);
begin
  LockWrite;
  try
    if FMaxConnsPerProxy <> AValue then begin
      FMaxConnsPerProxy := AValue;
      _CheckValue(FMaxConnsPerProxy);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TWinInetConfigStatic }

constructor TWinInetConfigStatic.Create(
  const AMaxConnsPerServer: TConnsPerServerRec;
  const AMaxConnsPerProxy: TConnsPerServerRec
);
begin
  inherited Create;
  FMaxConnsPerServer := AMaxConnsPerServer;
  FMaxConnsPerProxy := AMaxConnsPerProxy;
end;

function TWinInetConfigStatic.GetMaxConnsPerServer: TConnsPerServerRec;
begin
  Result := FMaxConnsPerServer;
end;

function TWinInetConfigStatic.GetMaxConnsPerProxy: TConnsPerServerRec;
begin
  Result := FMaxConnsPerProxy;
end;

end.
