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

unit u_GeoCodeResult;

interface

uses
  ActiveX,
  i_InterfaceListStatic,
  i_GeoCoder,
  u_BaseInterfacedObject;

type
  TGeoCodeResult = class(TBaseInterfacedObject, IGeoCodeResult)
  private
    FSearchText: string;
    FMessage: string;
    FResultCode: Integer;
    FList: IInterfaceListStatic;
    function GetSearchText: string;
    function GetResultCode: Integer;
    function GetMessage: string;
    function GetPlacemarks: IEnumUnknown;
    function GetPlacemarksCount: integer;
  public
    constructor Create(
      const ASearchText: string;
      AResultCode: integer;
      const AMessage: string;
      const AList: IInterfaceListStatic
    );
  end;

implementation

uses
  u_EnumUnknown;

{ TGeoCodeResult }

constructor TGeoCodeResult.Create(
  const ASearchText: string;
  AResultCode: integer;
  const AMessage: string;
  const AList: IInterfaceListStatic
);
begin
  inherited Create;
  FSearchText := ASearchText;
  FList := AList;
  FMessage := AMessage;
  FResultCode := AResultCode;
end;

function TGeoCodeResult.GetMessage: string;
begin
  Result := FMessage;
end;

function TGeoCodeResult.GetPlacemarks: IEnumUnknown;
begin
  if Assigned(FList) then begin
    Result := TEnumUnknownByStatic.Create(FList);
  end else begin
    Result := nil;
  end;
end;

function TGeoCodeResult.GetPlacemarksCount: integer;
begin
  if Assigned(FList) then begin
    Result := FList.Count;
  end else begin
    Result := 0;
  end;
end;

function TGeoCodeResult.GetResultCode: Integer;
begin
  Result := FResultCode;
end;

function TGeoCodeResult.GetSearchText: string;
begin
  Result := FSearchText;
end;

end.
 