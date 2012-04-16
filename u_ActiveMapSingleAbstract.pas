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

unit u_ActiveMapSingleAbstract;

interface

uses
  i_JclNotify,
  i_MapTypes,
  i_ActiveMapsConfig,
  u_ConfigDataElementBase;

type
  TActiveMapSingleAbstract = class(TConfigDataElementBaseEmptySaveLoad, IActiveMapSingle)
  private
    FMapType: IMapType;
    FMapGUID: TGUID;
    FIsActive: Boolean;
  protected
    procedure SetIsActive(AValue: Boolean);
    property MapGUID: TGUID read FMapGUID;
  protected
    function GetMapType: IMapType;
    function GetIsActive: Boolean;
  public
    constructor Create(const AMapType: IMapType);
    destructor Destroy; override;
  end;

type
  TActiveMapSingleMainMap = class(TActiveMapSingleAbstract)
  private
    FMainMapChangeNotyfier: IJclNotifier;
    FMainMapListener: IJclListener;
    procedure OnMainMapChange(const AGUID: TGUID);
  public
    constructor Create(
      const AMapType: IMapType;
      const AMainMapChangeNotyfier: IJclNotifier
    );
    destructor Destroy; override;
  end;

type
  TActiveMapSingleLayer = class(TActiveMapSingleAbstract)
  private
    FLayerSetSelectNotyfier: IJclNotifier;
    FLayerSetUnselectNotyfier: IJclNotifier;
    FLayerSetSelectListener: IJclListener;
    FLayerSetUnselectListener: IJclListener;
    procedure OnLayerSetSelectChange(const AGUID: TGUID);
    procedure OnLayerSetUnselectChange(const AGUID: TGUID);
  public
    constructor Create(
      const AMapType: IMapType;
      const ALayerSetSelectNotyfier: IJclNotifier;
      const ALayerSetUnselectNotyfier: IJclNotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  u_NotifyWithGUIDEvent;

{ TActiveMapSingleAbstract }

constructor TActiveMapSingleAbstract.Create(const AMapType: IMapType);
begin
  inherited Create;
  FMapType := AMapType;
  if FMapType <> nil then begin
    FMapGUID := FMapType.GUID;
  end else begin
    FMapGUID := CGUID_Zero;
  end;
end;

destructor TActiveMapSingleAbstract.Destroy;
begin
  FMapType := nil;
  inherited;
end;

function TActiveMapSingleAbstract.GetIsActive: Boolean;
begin
  LockRead;
  try
    Result := FIsActive;
  finally
    UnlockRead;
  end;
end;

function TActiveMapSingleAbstract.GetMapType: IMapType;
begin
  Result := FMapType;
end;

procedure TActiveMapSingleAbstract.SetIsActive(AValue: Boolean);
begin
  LockWrite;
  try
    if AValue <> FIsActive then begin
      FIsActive := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

{ TActiveMapSingleMainMap }

constructor TActiveMapSingleMainMap.Create(
  const AMapType: IMapType;
  const AMainMapChangeNotyfier: IJclNotifier
);
begin
  inherited Create(AMapType);
  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  FMainMapListener := TNotifyWithGUIDEventListener.Create(Self.OnMainMapChange);
  FMainMapChangeNotyfier.Add(FMainMapListener);
end;

destructor TActiveMapSingleMainMap.Destroy;
begin
  FMainMapChangeNotyfier.Remove(FMainMapListener);
  FMainMapListener := nil;
  FMainMapChangeNotyfier := nil;
  inherited;
end;

procedure TActiveMapSingleMainMap.OnMainMapChange(const AGUID: TGUID);
begin
  SetIsActive(IsEqualGUID(FMapGUID, AGUID));
end;

{ TActiveMapSingleLayer }

constructor TActiveMapSingleLayer.Create(
  const AMapType: IMapType;
  const ALayerSetSelectNotyfier, ALayerSetUnselectNotyfier: IJclNotifier
);
begin
  inherited Create(AMapType);
  FLayerSetSelectNotyfier := ALayerSetSelectNotyfier;
  FLayerSetSelectListener := TNotifyWithGUIDEventListener.Create(Self.OnLayerSetSelectChange);
  FLayerSetSelectNotyfier.Add(FLayerSetSelectListener);

  FLayerSetUnselectNotyfier := ALayerSetUnselectNotyfier;
  FLayerSetUnselectListener := TNotifyWithGUIDEventListener.Create(Self.OnLayerSetUnselectChange);
  FLayerSetUnselectNotyfier.Add(FLayerSetUnselectListener);
end;

destructor TActiveMapSingleLayer.Destroy;
begin
  FLayerSetSelectNotyfier.Remove(FLayerSetSelectListener);
  FLayerSetSelectListener := nil;
  FLayerSetSelectNotyfier := nil;

  FLayerSetUnselectNotyfier.Remove(FLayerSetUnselectListener);
  FLayerSetUnselectListener := nil;
  FLayerSetUnselectNotyfier := nil;

  inherited;
end;

procedure TActiveMapSingleLayer.OnLayerSetSelectChange(const AGUID: TGUID);
begin
  if IsEqualGUID(MapGUID, AGUID) then begin
    SetIsActive(True);
  end;
end;

procedure TActiveMapSingleLayer.OnLayerSetUnselectChange(const AGUID: TGUID);
begin
  if IsEqualGUID(MapGUID, AGUID) then begin
    SetIsActive(False);
  end;
end;

end.
