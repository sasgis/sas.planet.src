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

unit u_MapVersionListStatic;

interface

uses
  i_InterfaceListStatic,
  i_MapVersionInfo,
  i_MapVersionListStatic,
  u_BaseInterfacedObject;

type
  TMapVersionListStatic = class(TBaseInterfacedObject, IMapVersionListStatic)
  private
    FList: IInterfaceListStatic;
    FSorted: Boolean;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): IMapVersionInfo;
    function GetSorted: Boolean;
  public
    constructor Create(
      const AList: IInterfaceListStatic;
      const ASorted: Boolean = False
    );
  end;

implementation

{ TMapVersionListStatic }

constructor TMapVersionListStatic.Create(
  const AList: IInterfaceListStatic;
  const ASorted: Boolean
);
begin
  inherited Create;
  FList := AList;
  FSorted := ASorted;
end;

function TMapVersionListStatic.GetCount: Integer;
begin
  if not Assigned(FList) then begin
    Result := 0;
  end else begin
    Result := FList.Count;
  end;
end;

function TMapVersionListStatic.GetItem(AIndex: Integer): IMapVersionInfo;
begin
  if not Assigned(FList) then begin
    Result := nil;
  end else begin
    Result := IMapVersionInfo(FList.Items[AIndex]);
  end;
end;

function TMapVersionListStatic.GetSorted: Boolean;
begin
  Result := FSorted;
end;

end.
