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

unit u_MapTypeChangeableByConfig;

interface

uses
  SysUtils,
  i_Notifier,
  i_Listener,
  i_MapType,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  u_ChangeableBase;

type
  TMapTypeChangeableByConfig = class(TChangeableBase, IMapTypeChangeable)
  private
    FConfig: IActiveMapConfig;
    FMapsSet: IMapTypeSet;

    FListener: IListener;

    FStatic: IMapType;
    FStaticCS: IReadWriteSync;

    procedure OnConfigChange;
  private
    function GetStatic: IMapType;
  public
    constructor Create(
      const AConfig: IActiveMapConfig;
      const AMapsSet: IMapTypeSet
    );
    destructor Destroy; override;
  end;

implementation

uses
  c_ZeroGUID,
  u_ListenerByEvent,
  u_Synchronizer;

{ TMapTypeChangeableByConfig }

constructor TMapTypeChangeableByConfig.Create(
  const AConfig: IActiveMapConfig;
  const AMapsSet: IMapTypeSet
);
begin
  Assert(Assigned(AConfig));
  Assert(Assigned(AMapsSet));
  inherited Create(GSync.SyncVariable.Make(ClassName + 'Notifiers'));
  FConfig := AConfig;
  FMapsSet := AMapsSet;

  FStaticCS := GSync.SyncVariable.Make(ClassName);
  FListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FConfig.ChangeNotifier.Add(FListener);
  OnConfigChange;
end;

destructor TMapTypeChangeableByConfig.Destroy;
begin
  if Assigned(FConfig) and Assigned(FListener) then begin
    FConfig.ChangeNotifier.Remove(FListener);
    FListener := nil;
    FConfig := nil;
  end;
  inherited;
end;

function TMapTypeChangeableByConfig.GetStatic: IMapType;
begin
  FStaticCS.BeginRead;
  try
    Result := FStatic;
  finally
    FStaticCS.EndRead;
  end;
end;

procedure TMapTypeChangeableByConfig.OnConfigChange;
var
  VGUID: TGUID;
  VMapType: IMapType;
  VChanged: Boolean;
begin
  VGUID := FConfig.MainMapGUID;
  Assert(not IsEqualGUID(VGUID, CGUID_Zero));
  VChanged := False;
  FStaticCS.BeginWrite;
  try
    if not Assigned(FStatic) or not IsEqualGUID(VGUID, FStatic.GUID) then begin
      VMapType := FMapsSet.GetMapTypeByGUID(VGUID);
      Assert(Assigned(VMapType));
      FStatic := VMapType;
      VChanged := True;
    end;
  finally
    FStaticCS.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

end.
