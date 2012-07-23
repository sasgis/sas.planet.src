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

unit u_ActiveMapConfig;

interface

uses
  Windows,
  SysUtils,
  i_Notifier,
  i_Listener,
  i_GUIDSet,
  i_MapTypes,
  i_ActiveMapsConfig,
  u_ConfigDataElementBase;

type
  TActiveMapConfig = class(TConfigDataElementBaseEmptySaveLoad, IActiveMap, IMapTypeChangeable)
  private
    FSelectedGUID: TGUID;
    FMapsSet: IMapTypeSet;
    FSingeMapsList: IGUIDInterfaceSet;
    FStatic: IMapType;
  private
    FMainMapChangeNotyfier: INotifier;
    FMainMapListener: IListener;
    procedure OnMainMapChange(const AGUID: TGUID);
  private
    function GetSelectedGUID: TGUID;
    function GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
    function GetMapsSet: IMapTypeSet;
    function GetStatic: IMapType;
  public
    constructor Create(
      const AMainMapChangeNotyfier: INotifier;
      const ASingeMapsList: IGUIDInterfaceSet;
      const AMapsSet: IMapTypeSet
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_NotifyWithGUIDEvent;

{ TActiveMapConfigNew }

constructor TActiveMapConfig.Create(
  const AMainMapChangeNotyfier: INotifier;
  const ASingeMapsList: IGUIDInterfaceSet;
  const AMapsSet: IMapTypeSet
);
var
  i: Cardinal;
begin
  inherited Create;
  FMapsSet := AMapsSet;
  FSingeMapsList := ASingeMapsList;
  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  FMainMapListener := TNotifyWithGUIDEventListener.Create(Self.OnMainMapChange);
  FMainMapChangeNotyfier.Add(FMainMapListener);
  if FMapsSet.GetIterator.Next(1, FSelectedGUID, i) <> S_OK then begin
    raise Exception.Create('Empty maps list');
  end;
  FStatic := FMapsSet.GetMapTypeByGUID(FSelectedGUID);
end;

destructor TActiveMapConfig.Destroy;
begin
  FMainMapChangeNotyfier.Remove(FMainMapListener);
  FMainMapListener := nil;
  FMainMapChangeNotyfier := nil;
  FMapsSet := nil;
  FSingeMapsList := nil;
  inherited;
end;

function TActiveMapConfig.GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
begin
  Result := nil;
  if FMapsSet.GetMapTypeByGUID(AMapGUID) <> nil then begin
    Result := IActiveMapSingle(FSingeMapsList.GetByGUID(AMapGUID));
  end;
end;

function TActiveMapConfig.GetMapsSet: IMapTypeSet;
begin
  Result := FMapsSet;
end;

function TActiveMapConfig.GetSelectedGUID: TGUID;
begin
  LockRead;
  try
    Result := FSelectedGUID;
  finally
    UnlockRead;
  end;
end;

function TActiveMapConfig.GetStatic: IMapType;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

procedure TActiveMapConfig.OnMainMapChange(const AGUID: TGUID);
begin
  LockWrite;
  try
    if not IsEqualGUID(FSelectedGUID, AGUID) then begin
      FSelectedGUID := AGUID;
      FStatic := FMapsSet.GetMapTypeByGUID(FSelectedGUID);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
