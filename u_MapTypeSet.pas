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
  TMapTypeSetBuilder = class(TBaseInterfacedObject, IMapTypeSetBuilder)
  private
    FAllowNil: Boolean;
    FList: IGUIDInterfaceSet;
  private
    function GetCount: Integer;
    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);
    procedure Add(const AItem: IMapType);
    procedure Clear;
    function MakeCopy: IMapTypeSet;
    function MakeAndClear: IMapTypeSet;
  public
    constructor Create(AAllowNil: Boolean);
  end;

implementation

uses
  u_GUIDInterfaceSet,
  c_ZeroGUID;

{ TMapTypeList }

type
  TMapTypeSet = class(TBaseInterfacedObject, IMapTypeSet)
  private
    FList: IGUIDInterfaceSet;
    function IsEqual(const AValue: IMapTypeSet): Boolean;
    function GetMapTypeByGUID(const AGUID: TGUID): IMapType;
    function GetIterator: IEnumGUID;
    function GetMapTypeIterator: IEnumUnknown;
    function GetItem(AIndex: Integer): IMapType;
    function GetCount: Integer;
  public
    constructor Create(const AList: IGUIDInterfaceSet);
  end;

constructor TMapTypeSet.Create(const AList: IGUIDInterfaceSet);
begin
  Assert(Assigned(AList));
  inherited Create;
  FList := AList;
end;

function TMapTypeSet.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TMapTypeSet.GetItem(AIndex: Integer): IMapType;
begin
  Result := FList.Items[AIndex] as IMapType;
end;

function TMapTypeSet.GetIterator: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

function TMapTypeSet.GetMapTypeByGUID(const AGUID: TGUID): IMapType;
begin
  Result := FList.GetByGUID(AGUID) as IMapType;
end;

function TMapTypeSet.GetMapTypeIterator: IEnumUnknown;
begin
  Result := FList.GetEnumUnknown;
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

{ TMapTypeSetBuilder }

constructor TMapTypeSetBuilder.Create(AAllowNil: Boolean);
begin
  inherited Create;
  FAllowNil := AAllowNil;
end;

procedure TMapTypeSetBuilder.Add(const AItem: IMapType);
var
  VGUID: TGUID;
begin
  if not Assigned(FList) then begin
    FList := TGUIDInterfaceSet.Create(FAllowNil);
  end;

  if AItem <> nil then begin
    VGUID := AItem.GUID;
  end else begin
    VGUID := CGUID_Zero;
  end;
  FList.Add(VGUID, AItem);
end;

procedure TMapTypeSetBuilder.Clear;
begin
  if Assigned(FList) then begin
    FList.Clear;
  end;
end;

function TMapTypeSetBuilder.GetCapacity: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Capacity;
  end else begin
    Result := 0;
  end;
end;

function TMapTypeSetBuilder.GetCount: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Count;
  end else begin
    Result := 0;
  end;
end;

function TMapTypeSetBuilder.MakeAndClear: IMapTypeSet;
begin
  if not Assigned(FList) then begin
    FList := TGUIDInterfaceSet.Create(FAllowNil);
  end;
  Result := TMapTypeSet.Create(FList);
  FList := nil;
end;

function TMapTypeSetBuilder.MakeCopy: IMapTypeSet;
var
  VList: IGUIDInterfaceSet;
  i: Integer;
  VMap: IMapType;
begin
  VList := TGUIDInterfaceSet.Create(FAllowNil);
  if Assigned(FList) then begin
    for i := 0 to FList.Count - 1 do begin
      VMap := FList.Items[i] as IMapType;
      if Assigned(VMap) then begin
        VList.Add(VMap.GUID, VMap);
      end else begin
        VList.Add(CGUID_Zero, nil);
      end;
    end;
  end;
end;

procedure TMapTypeSetBuilder.SetCapacity(ANewCapacity: Integer);
begin
  if ANewCapacity > 0 then begin
    if not Assigned(FList) then begin
      FList := TGUIDInterfaceSet.Create(FAllowNil);
    end;
    FList.Capacity := ANewCapacity;
  end else begin
    if Assigned(FList) then begin
      FList.Capacity := ANewCapacity;
    end;
  end;
end;

end.
