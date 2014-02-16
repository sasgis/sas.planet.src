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
  i_MapTypes,
  i_MapTypeSet,
  u_ConfigDataElementBase;

type
  TMapTypeChangeableByNotifier = class(TConfigDataElementBaseEmptySaveLoad, IMapTypeChangeable)
  private
    FIsAllowNil: Boolean;
    FMapsSet: IMapTypeSet;
    FStatic: IMapType;
  private
    FMainMapChangeNotyfier: INotifier;
    FMainMapListener: IListener;
    procedure OnMainMapChange(const AGUID: TGUID);
  private
    function GetStatic: IMapType;
  public
    constructor Create(
      const AIsAllowNil: Boolean;
      const AMainMapChangeNotyfier: INotifier;
      const AMapsSet: IMapTypeSet
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  c_ZeroGUID,
  u_NotifyWithGUIDEvent;

{ TActiveMapConfigNew }

constructor TMapTypeChangeableByNotifier.Create(
  const AIsAllowNil: Boolean;
  const AMainMapChangeNotyfier: INotifier;
  const AMapsSet: IMapTypeSet
);
var
  i: Cardinal;
  VGUID: TGUID;
begin
  Assert(AMainMapChangeNotyfier <> nil);
  Assert(AMapsSet <> nil);
  inherited Create;
  FIsAllowNil := AIsAllowNil;
  FMapsSet := AMapsSet;
  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  FMainMapListener := TNotifyWithGUIDEventListener.Create(Self.OnMainMapChange);
  FMainMapChangeNotyfier.Add(FMainMapListener);
  if FIsAllowNil then begin
    FStatic := nil;
  end else begin
    if FMapsSet.GetIterator.Next(1, VGUID, i) <> S_OK then begin
      raise Exception.Create('Empty maps list');
    end;
    FStatic := FMapsSet.GetMapTypeByGUID(VGUID);
  end;
end;

destructor TMapTypeChangeableByNotifier.Destroy;
begin
  if Assigned(FMainMapChangeNotyfier) and Assigned(FMainMapListener) then begin
    FMainMapChangeNotyfier.Remove(FMainMapListener);
    FMainMapListener := nil;
    FMainMapChangeNotyfier := nil;
  end;
  FMapsSet := nil;
  inherited;
end;

function TMapTypeChangeableByNotifier.GetStatic: IMapType;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

procedure TMapTypeChangeableByNotifier.OnMainMapChange(const AGUID: TGUID);
var
  VGUID: TGUID;
  VMapType: IMapType;
begin
  LockWrite;
  try
    VGUID := CGUID_Zero;
    if Assigned(FStatic) then begin
      VGUID := FStatic.GUID;
    end;
    if not IsEqualGUID(VGUID, AGUID) then begin
      if IsEqualGUID(AGUID, CGUID_Zero) then begin
        FStatic := nil;
        SetChanged;
      end else begin
        VMapType := FMapsSet.GetMapTypeByGUID(AGUID);
        FStatic := VMapType;
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
