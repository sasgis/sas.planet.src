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

unit u_MapTypeChangeableWithDefault;

interface

uses
  SysUtils,
  i_MapType,
  i_MapTypeSet,
  i_Listener,
  i_ActiveMapsConfig,
  u_ChangeableBase;

type
  TMapTypeChangeableWithDefault = class(TChangeableWithSimpleLockBase, IMapTypeChangeable)
  private
    FMapsSet: IMapTypeSet;
    FParentMap: IMapTypeChangeable;
    FConfig: IActiveMapConfig;

    FStatic: IMapType;

    FListener: IListener;
    procedure OnChange;
  private
    function GetStatic: IMapType;
  public
    constructor Create(
      const AMapsSet: IMapTypeSet;
      const AParentMap: IMapTypeChangeable;
      const AConfig: IActiveMapConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  c_ZeroGUID,
  u_ListenerByEvent;

{ TMapTypeChangeableWithDefault }

constructor TMapTypeChangeableWithDefault.Create(
  const AMapsSet: IMapTypeSet;
  const AParentMap: IMapTypeChangeable;
  const AConfig: IActiveMapConfig
);
begin
  Assert(Assigned(AMapsSet));
  Assert(Assigned(AParentMap));
  Assert(Assigned(AConfig));
  inherited Create;
  FMapsSet := AMapsSet;
  FParentMap := AParentMap;
  FConfig := AConfig;

  FListener := TNotifyNoMmgEventListener.Create(Self.OnChange);
  FConfig.ChangeNotifier.Add(FListener);
  FParentMap.ChangeNotifier.Add(FListener);
  OnChange;
end;

destructor TMapTypeChangeableWithDefault.Destroy;
begin
  if Assigned(FConfig) and Assigned(FListener) then begin
    FConfig.ChangeNotifier.Remove(FListener);
    FConfig := nil;
  end;
  if Assigned(FParentMap) and Assigned(FListener) then begin
    FParentMap.ChangeNotifier.Remove(FListener);
    FParentMap := nil;
  end;
  FListener := nil;

  inherited;
end;

function TMapTypeChangeableWithDefault.GetStatic: IMapType;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TMapTypeChangeableWithDefault.OnChange;
var
  VGUID: TGUID;
  VMapType: IMapType;
  VChanged: Boolean;
begin
  VGUID := FConfig.MainMapGUID;
  if IsEqualGUID(VGUID, CGUID_Zero) then begin
    VMapType := FParentMap.GetStatic;
    Assert(Assigned(VMapType));
    VGUID := VMapType.GUID;
  end else begin
    VMapType := nil;
  end;

  VChanged := False;
  CS.BeginWrite;
  try
    if not Assigned(FStatic) or not IsEqualGUID(VGUID, FStatic.GUID) then begin
      if not Assigned(VMapType) then begin
        VMapType := FMapsSet.GetMapTypeByGUID(VGUID);
        Assert(Assigned(VMapType));
      end;
      FStatic := VMapType;
      VChanged := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

end.
