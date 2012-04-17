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
  i_MapTypes;

type
  TMapTypeSet = class(TInterfacedObject, IMapTypeSet)
  private
    FList: IGUIDInterfaceSet;
    function GetMapTypeByGUID(const AGUID: TGUID): IMapType;
    function GetIterator: IEnumGUID;
    function GetCount: Integer;
  public
    procedure Add(const AMap: IMapType);
    constructor Create(AAllowNil: Boolean);
    destructor Destroy; override;
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

destructor TMapTypeSet.Destroy;
begin
  FList := nil;
  inherited;
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
  Result := Flist.GetByGUID(AGUID) as IMapType;
end;

end.
