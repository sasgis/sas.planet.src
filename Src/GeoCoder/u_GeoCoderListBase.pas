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

unit u_GeoCoderListBase;

interface

uses
  SysUtils,
  i_GeoCoderList,
  i_InterfaceListStatic,
  u_BaseInterfacedObject;

type
  TGeoCoderListStatic = class(TBaseInterfacedObject, IGeoCoderListStatic)
  private
    FList: IInterfaceListStatic;
  private
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): IGeoCoderListEntity;
    function GetIndexByGUID(const AGUID: TGUID): Integer;
  public
    constructor Create(
      const AList: IInterfaceListStatic
    );
  end;

implementation

{ TGeoCoderListStatic }

constructor TGeoCoderListStatic.Create(const AList: IInterfaceListStatic);
begin
  Assert(Assigned(AList));
  Assert(AList.Count > 0);
  inherited Create;
  FList := AList;
end;

function TGeoCoderListStatic.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGeoCoderListStatic.GetIndexByGUID(const AGUID: TGUID): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FList.Count - 1 do begin
    if IsEqualGUID(IGeoCoderListEntity(FList.Items[i]).GUID, AGUID) then begin
      Result := i;
      Break;
    end;
  end;
end;

function TGeoCoderListStatic.GetItem(
  const AIndex: Integer
): IGeoCoderListEntity;
begin
  Result := IGeoCoderListEntity(FList.Items[AIndex]);
end;

end.
