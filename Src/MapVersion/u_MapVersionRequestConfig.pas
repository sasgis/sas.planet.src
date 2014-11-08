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
    FDefConfig: IMapVersionInfo;
    FShowPrevVersion: Boolean;
    FVersionFactory: IMapVersionFactoryChangeable;
    FVersion: IMapVersionInfo;
    FFactoryListener: IListener;
    procedure OnFactoryChange;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetVersionFactory: IMapVersionFactoryChangeable;

    function GetVersion: IMapVersionInfo;
    procedure SetVersion(const AValue: IMapVersionInfo);

    function GetShowPrevVersion: Boolean;
    procedure SetShowPrevVersion(const AValue: Boolean);

    function GetStatic: IMapVersionRequest;
  public
    constructor Create(
      const ADefConfig: IMapVersionInfo;
      const AMapVersionFactory: IMapVersionFactoryChangeable
    );
    destructor Destroy; override;
  end;


implementation

uses
  u_ListenerByEvent,
  u_MapVersionRequest;

{ TMapVersionRequestConfig }

constructor TMapVersionRequestConfig.Create(
  const ADefConfig: IMapVersionInfo;
  const AMapVersionFactory: IMapVersionFactoryChangeable
);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FShowPrevVersion := True;
  FVersion := ADefConfig;
  FVersionFactory := AMapVersionFactory;

  FFactoryListener := TNotifyNoMmgEventListener.Create(Self.OnFactoryChange);
  FVersionFactory.ChangeNotifier.Add(FFactoryListener);
  OnFactoryChange;
end;

function TMapVersionRequestConfig.CreateStatic: IInterface;
begin
  Result := IMapVersionRequest(TMapVersionRequest.Create(FVersion, FShowPrevVersion));
end;

destructor TMapVersionRequestConfig.Destroy;
begin
  if Assigned(FVersionFactory) and Assigned(FFactoryListener) then begin
    FVersionFactory.ChangeNotifier.Remove(FFactoryListener);
    FVersionFactory := nil;
    FFactoryListener := nil;
  end;
  inherited;
end;

procedure TMapVersionRequestConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VStoreString: string;
  VShowPrevVersion: Boolean;
begin
  inherited;
  if AConfigData <> nil then begin
    VStoreString := AConfigData.ReadString('Version', FVersion.StoreString);
    SetVersion(FVersionFactory.GetStatic.CreateByStoreString(VStoreString));
    VShowPrevVersion := AConfigData.ReadBool('ShowPrevVersion', FShowPrevVersion);
    SetShowPrevVersion(VShowPrevVersion);
  end;
end;

procedure TMapVersionRequestConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  VStoreString: string;
begin
  inherited;
  VStoreString := FVersion.StoreString;
  if VStoreString <> FDefConfig.StoreString then begin
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

function TMapVersionRequestConfig.GetStatic: IMapVersionRequest;
begin
  Result := IMapVersionRequest(GetStaticInternal);
end;

function TMapVersionRequestConfig.GetVersion: IMapVersionInfo;
begin
  LockRead;
  try
    Result := FVersion;
  finally
    UnlockRead;
  end;
end;

function TMapVersionRequestConfig.GetVersionFactory: IMapVersionFactoryChangeable;
begin
  LockRead;
  try
    Result := FVersionFactory;
  finally
    UnlockRead;
  end;
end;

procedure TMapVersionRequestConfig.OnFactoryChange;
begin

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

procedure TMapVersionRequestConfig.SetVersion(const AValue: IMapVersionInfo);
var
  VValue: IMapVersionInfo;
begin
  LockWrite;
  try
    VValue := FVersionFactory.GetStatic.CreateByMapVersion(AValue);
    if not FVersion.IsSame(VValue) then begin
      FVersion := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
