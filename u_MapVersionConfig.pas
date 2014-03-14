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

unit u_MapVersionConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_Listener,
  i_MapVersionInfo,
  i_MapVersionFactory,
  i_MapVersionConfig,
  u_ConfigDataElementBase;

type
  TMapVersionConfig = class(TConfigDataElementBase, IMapVersionConfig)
  private
    FDefConfig: IMapVersionInfo;
    FVersionFactory: IMapVersionFactoryChangeable;
    FVersion: IMapVersionInfo;
    FFactoryListener: IListener;
    procedure OnFactoryChange;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetVersionFactory: IMapVersionFactoryChangeable;

    function GetVersion: IMapVersionInfo;
    procedure SetVersion(const AValue: IMapVersionInfo);
  public
    constructor Create(
      const ADefConfig: IMapVersionInfo;
      const AMapVersionFactory: IMapVersionFactoryChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent;

{ TMapVersionConfig }

constructor TMapVersionConfig.Create(
  const ADefConfig: IMapVersionInfo;
  const AMapVersionFactory: IMapVersionFactoryChangeable
);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FVersionFactory := AMapVersionFactory;
  FVersion := FDefConfig;
  FFactoryListener := TNotifyNoMmgEventListener.Create(Self.OnFactoryChange);
  FVersionFactory.ChangeNotifier.Add(FFactoryListener);
  OnFactoryChange;
end;

destructor TMapVersionConfig.Destroy;
begin
  if Assigned(FVersionFactory) and Assigned(FFactoryListener) then begin
    FVersionFactory.ChangeNotifier.Remove(FFactoryListener);
    FVersionFactory := nil;
    FFactoryListener := nil;
  end;
  inherited;
end;

procedure TMapVersionConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VStoreString: string;
begin
  inherited;
  if AConfigData <> nil then begin
    VStoreString := AConfigData.ReadString('Version', FVersion.StoreString);
    SetVersion(FVersionFactory.GetStatic.CreateByStoreString(VStoreString));
  end;
end;

procedure TMapVersionConfig.DoWriteConfig(
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
end;

function TMapVersionConfig.GetVersion: IMapVersionInfo;
begin
  LockRead;
  try
    Result := FVersion;
  finally
    UnlockRead;
  end;
end;

function TMapVersionConfig.GetVersionFactory: IMapVersionFactoryChangeable;
begin
  LockRead;
  try
    Result := FVersionFactory;
  finally
    UnlockRead;
  end;
end;

procedure TMapVersionConfig.OnFactoryChange;
begin
  LockWrite;
  try
    SetVersion(FVersion);
  finally
    UnlockWrite;
  end;
end;

procedure TMapVersionConfig.SetVersion(const AValue: IMapVersionInfo);
var
  VValue: IMapVersionInfo;
begin
  LockWrite;
  try
    if FVersion <> AValue then begin
      VValue := FVersionFactory.GetStatic.CreateByMapVersion(AValue);
      if not FVersion.IsSame(VValue) then begin
        FVersion := VValue;
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
