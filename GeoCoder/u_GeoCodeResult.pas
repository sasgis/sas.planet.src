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
    FSearchText: WideString;
    FMessage: WideString;
    FResultCode: Integer;
    FList: IInterfaceListStatic;
    function GetSearchText: WideString; safecall;
    function GetResultCode: Integer; safecall;
    function GetMessage: WideString; safecall;
    function GetPlacemarks: IEnumUnknown; safecall;
    function GetPlacemarksCount: integer; safecall;
  public
    constructor Create(
      const ASearchText: WideString;
      AResultCode: integer;
      const AMessage: WideString;
      const AList: IInterfaceListStatic
    );
  end;

implementation

uses
  u_EnumUnknown;

{ TGeoCodeResult }

constructor TGeoCodeResult.Create(
  const ASearchText: WideString;
  AResultCode: integer;
  const AMessage: WideString;
  const AList: IInterfaceListStatic
);
begin
  inherited Create;
  FSearchText := ASearchText;
  FList := AList;
  FMessage := AMessage;
  FResultCode := AResultCode;
end;

function TGeoCodeResult.GetMessage: WideString;
begin
  Result := FMessage;
end;

function TGeoCodeResult.GetPlacemarks: IEnumUnknown;
begin
  Result := TEnumUnknownByStatic.Create(FList);
end;

function TGeoCodeResult.GetPlacemarksCount: integer;
begin
  Result := FList.Count;
end;

function TGeoCodeResult.GetResultCode: Integer;
begin
  Result := FResultCode;
end;

function TGeoCodeResult.GetSearchText: WideString;
begin
  Result := FSearchText;
end;

end.
 