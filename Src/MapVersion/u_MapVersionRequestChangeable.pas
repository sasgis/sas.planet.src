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

unit u_MapVersionRequestChangeable;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_Listener,
  i_MapVersionInfo,
  i_MapVersionFactory,
  i_MapVersionRequest,
  i_MapVersionRequestConfig,
  i_MapVersionRequestChangeable,
  u_ChangeableBase;

type
  TMapVersionRequestChangeable = class(TChangeableWithSimpleLockBase, IMapVersionRequestChangeable)
  private
    FConfig: IMapVersionRequestConfig;
    FVersionFactory: IMapVersionFactoryChangeable;

    FFactoryListener: IListener;
    FConfigListener: IListener;

    FStatic: IMapVersionRequest;
    procedure OnFactoryChange;
    procedure OnConfigChange;
  private
    function GetStatic: IMapVersionRequest;
  public
    constructor Create(
      const AConfig: IMapVersionRequestConfig;
      const AMapVersionFactory: IMapVersionFactoryChangeable
    );
    destructor Destroy; override;
  end;


implementation

uses
  u_ListenerByEvent,
  u_MapVersionRequest;

{ TMapVersionRequestChangeable }

constructor TMapVersionRequestChangeable.Create(
  const AConfig: IMapVersionRequestConfig;
  const AMapVersionFactory: IMapVersionFactoryChangeable
);
begin
  inherited Create;
  FConfig := AConfig;
  FVersionFactory := AMapVersionFactory;

  FFactoryListener := TNotifyNoMmgEventListener.Create(Self.OnFactoryChange);
  FVersionFactory.ChangeNotifier.Add(FFactoryListener);

  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FConfigListener);

  OnFactoryChange;
end;

destructor TMapVersionRequestChangeable.Destroy;
begin
  if Assigned(FVersionFactory) and Assigned(FFactoryListener) then begin
    FVersionFactory.ChangeNotifier.Remove(FFactoryListener);
    FVersionFactory := nil;
    FFactoryListener := nil;
  end;
  if Assigned(FConfig) and Assigned(FConfigListener) then begin
    FConfig.ChangeNotifier.Remove(FConfigListener);
    FConfig := nil;
    FConfigListener := nil;
  end;
  inherited;
end;

function TMapVersionRequestChangeable.GetStatic: IMapVersionRequest;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TMapVersionRequestChangeable.OnConfigChange;
var
  VConfig: IMapVersionRequestConfigStatic;
  VVersionFactory: IMapVersionFactory;
  VVersion: IMapVersionInfo;
begin
  CS.BeginWrite;
  try
    VConfig := FConfig.GetStatic;
    VVersionFactory := FVersionFactory.GetStatic;
    VVersion := VVersionFactory.CreateByStoreString(VConfig.Version);
    FStatic := TMapVersionRequest.Create(VVersion, VConfig.ShowPrevVersion);
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

procedure TMapVersionRequestChangeable.OnFactoryChange;
var
  VConfig: IMapVersionRequestConfigStatic;
  VVersionFactory: IMapVersionFactory;
  VVersion: IMapVersionInfo;
begin
  CS.BeginWrite;
  try
    VConfig := FConfig.GetStatic;
    VVersionFactory := FVersionFactory.GetStatic;
    VVersion := VVersionFactory.CreateByStoreString(VConfig.Version);
    FStatic := TMapVersionRequest.Create(VVersion, VConfig.ShowPrevVersion);
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

end.
