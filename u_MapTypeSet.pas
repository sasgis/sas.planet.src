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

unit u_MapTypeSet;

interface

uses
  ActiveX,
  i_GUIDSet,
  i_MapTypes,
  u_BaseInterfacedObject;

type
  TMapTypeSet = class(TBaseInterfacedObject, IMapTypeSet)
  private
    FList: IGUIDInterfaceSet;
    function IsEqual(const AValue: IMapTypeSet): Boolean;
    function GetMapTypeByGUID(const AGUID: TGUID): IMapType;
    function GetIterator: IEnumGUID;
    function GetCount: Integer;
  public
    procedure Add(const AMap: IMapType);
    constructor Create(AAllowNil: Boolean);
  end;

implementation

uses
  u_GUIDInterfaceSet,
  c_ZeroGUID;

{ TMapTypeList }

procedure TMapTypeSet.Add(const AMap: IMapType);
var
  VGUID: TGUID;
begin
  if AMap <> nil then begin
    VGUID := AMap.GUID;
  end else begin
    VGUID := CGUID_Zero;
  end;
  FList.Add(VGUID, AMap);
end;

constructor TMapTypeSet.Create(AAllowNil: Boolean);
begin
  inherited Create;
  FList := TGUIDInterfaceSet.Create(AAllowNil);
end;

function TMapTypeSet.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMapTypeSet.GetIterator: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

function TMapTypeSet.GetMapTypeByGUID(const AGUID: TGUID): IMapType;
begin
  Result := FList.GetByGUID(AGUID) as IMapType;
end;

function TMapTypeSet.IsEqual(const AValue: IMapTypeSet): Boolean;
var
  VEnum: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  if AValue = nil then begin
    Result := False;
    Exit;
  end;
  if AValue = IMapTypeSet(Self) then begin
    Result := True;
    Exit;
  end;
  if AValue.GetCount <> FList.Count then begin
    Result := False;
    Exit;
  end;
  VEnum := AValue.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    if not FList.IsExists(VGUID) then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

end.
