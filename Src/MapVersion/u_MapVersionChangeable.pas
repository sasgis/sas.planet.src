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

unit u_MapVersionChangeable;

interface

uses
  i_Listener,
  i_MapVersionInfo,
  i_MapVersionFactory,
  i_MapVersionChangeable,
  i_MapVersionConfig,
  u_ChangeableBase;

type
  TMapVersionChangeable = class(TChangeableWithSimpleLockBase, IMapVersionChangeable)
  private
    FConfig: IMapVersionConfig;
    FVersionFactory: IMapVersionFactoryChangeable;

    FFactoryListener: IListener;
    FConfigListener: IListener;

    FStatic: IMapVersionInfo;
    procedure OnFactoryChange;
    procedure OnConfigChange;
  private
    function GetStatic: IMapVersionInfo;
  public
    constructor Create(
      const AConfig: IMapVersionConfig;
      const AMapVersionFactory: IMapVersionFactoryChangeable
    );
    destructor Destroy; override;
  end;


implementation

uses
  u_ListenerByEvent;

{ TMapVersionChangeable }

constructor TMapVersionChangeable.Create(
  const AConfig: IMapVersionConfig;
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

destructor TMapVersionChangeable.Destroy;
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

function TMapVersionChangeable.GetStatic: IMapVersionInfo;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TMapVersionChangeable.OnConfigChange;
var
  VVersionFactory: IMapVersionFactory;
  VVersion: IMapVersionInfo;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    VVersionFactory := FVersionFactory.GetStatic;
    VVersion := VVersionFactory.CreateByStoreString(FConfig.Version);
    if not VVersion.IsSame(FStatic) then begin
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TMapVersionChangeable.OnFactoryChange;
var
  VVersionFactory: IMapVersionFactory;
  VVersion: IMapVersionInfo;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    VVersionFactory := FVersionFactory.GetStatic;
    VVersion := VVersionFactory.CreateByStoreString(FConfig.Version);
    if not VVersion.IsSame(FStatic) then begin
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
