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

unit u_FillingMapMapsConfig;

interface

uses
  i_Notify,
  i_ActiveMapsConfig,
  i_MapTypes,
  i_FillingMapLayerConfig,
  u_MainActiveMap;

type
  TFillingMapMapsConfig = class(TMainActiveMap, IFillingMapMapsConfig)
  private
    FActualMap: IMapType;
    FSelectedMapChangeListener: IListener;
    FMainMapsConfig: IMainMapsConfig;
    FMainMapChangeListener: IListener;
    function CreateMapsSet: IMapTypeSet;
    procedure OnMainMapChange;
    procedure OnSelectedChange(const AGUID: TGUID);
    procedure SetActualMap(const AValue: IMapType);
  protected
    function GetActualMap: IMapType;
  public
    constructor Create(const AMapsConfig: IMainMapsConfig);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  c_ZeroGUID,
  u_NotifyEventListener,
  u_NotifyWithGUIDEvent,
  u_MapTypeBasic,
  u_MapTypeSet;

{ TFillingMapMapsConfig }

constructor TFillingMapMapsConfig.Create(const AMapsConfig: IMainMapsConfig);
begin
  FMainMapsConfig := AMapsConfig;
  inherited Create(CreateMapsSet);

  FMainMapChangeListener := TNotifyNoMmgEventListener.Create(Self.OnMainMapChange);
  FMainMapsConfig.GetActiveMap.GetChangeNotifier.Add(FMainMapChangeListener);

  FSelectedMapChangeListener := TNotifyWithGUIDEventListener.Create(Self.OnSelectedChange);
  MainMapChangeNotyfier.Add(FSelectedMapChangeListener);

  SetActualMap(FMainMapsConfig.GetSelectedMapType);
end;

destructor TFillingMapMapsConfig.Destroy;
begin
  FMainMapsConfig.GetActiveMap.GetChangeNotifier.Remove(FMainMapChangeListener);
  FMainMapChangeListener := nil;

  MainMapChangeNotyfier.Remove(FSelectedMapChangeListener);
  FSelectedMapChangeListener := nil;

  FMainMapsConfig := nil;
  inherited;
end;

function TFillingMapMapsConfig.CreateMapsSet: IMapTypeSet;
var
  VSourceSet: IMapTypeSet;
  VMap: IMapType;
  VList: TMapTypeSet;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VSourceSet := FMainMapsConfig.GetAllActiveMapsSet.GetMapsSet;
  VList := TMapTypeSet.Create(True);
  Result := VList;
  VList.Add(TMapTypeBasic.Create(nil));
  VEnun := VSourceSet.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMap := VSourceSet.GetMapTypeByGUID(VGUID);
    VList.Add(VMap);
  end;
end;

function TFillingMapMapsConfig.GetActualMap: IMapType;
begin
  LockRead;
  try
    Result := FActualMap;
  finally
    UnlockRead;
  end;
end;

procedure TFillingMapMapsConfig.OnMainMapChange;
var
  VGUID: TGUID;
begin
  VGUID := GetActiveMap.GetSelectedGUID;
  if IsEqualGUID(VGUID, CGUID_Zero) then begin
    SetActualMap(FMainMapsConfig.GetSelectedMapType);
  end;
end;

procedure TFillingMapMapsConfig.OnSelectedChange(const AGUID: TGUID);
begin
  if IsEqualGUID(AGUID, CGUID_Zero) then begin
    SetActualMap(FMainMapsConfig.GetSelectedMapType);
  end else begin
    SetActualMap(GetActiveMap.GetMapsSet.GetMapTypeByGUID(AGUID));
  end;
end;

procedure TFillingMapMapsConfig.SetActualMap(const AValue: IMapType);
begin
  LockWrite;
  try
    if FActualMap <> AValue then begin
      FActualMap := AValue;
      inherited SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.

