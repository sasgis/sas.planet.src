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

unit u_GeoCoderLocalBasic;

interface

uses
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_GeoCoder,
  i_LocalCoordConverter,
  u_BaseInterfacedObject;

type
  TGeoCoderLocalBasic = class(TBaseInterfacedObject, IGeoCoder)
  private
    FPlacemarkFactory: IGeoCodePlacemarkFactory;
  protected
    function DoSearch(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; virtual; abstract;
    property PlacemarkFactory: IGeoCodePlacemarkFactory read FPlacemarkFactory;
  private
    function GetLocations(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: string;
      const ALocalConverter: ILocalCoordConverter
    ): IGeoCodeResult;
  public
    constructor Create(
      const APlacemarkFactory: IGeoCodePlacemarkFactory
    );
  end;

implementation

uses
  u_InterfaceListSimple,
  u_SortFunc,
  u_GeoCodeResult;

{ TGeoCoderLocalBasic }

procedure SortIt(
  const AList: IInterfaceListSimple;
  const ALocalConverter: ILocalCoordConverter
);
var
  i: integer;
  VMark: IGeoCodePlacemark;
  VDistArr: array of Double;
begin
  SetLength(VDistArr, AList.Count);
  for i := 0 to AList.GetCount-1 do begin
    VMark := IGeoCodePlacemark(AList.Items[i]);
    VDistArr[i] := ALocalConverter.GetGeoConverter.Datum.CalcDist(ALocalConverter.GetCenterLonLat, VMark.GetPoint);
  end;
  SortInterfaceListByDoubleMeasure(AList, VDistArr);
end;

constructor TGeoCoderLocalBasic.Create(
  const APlacemarkFactory: IGeoCodePlacemarkFactory
);
begin
  inherited Create;
  FPlacemarkFactory := APlacemarkFactory;
end;

function TGeoCoderLocalBasic.GetLocations(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IGeoCodeResult;
var
  VList: IInterfaceListSimple;
  VResultCode: Integer;
begin
  VResultCode := 200;
  VList := nil;
  Result := nil;
  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Exit;
  end;
  VList :=
   DoSearch(
   ACancelNotifier,
   AOperationID,
   ASearch,
   ALocalConverter
   );
  if VList = nil then begin
    VList := TInterfaceListSimple.Create;
  end;

  if VList.GetCount>1 then SortIt(VList ,ALocalConverter);

  Result := TGeoCodeResult.Create(ASearch, VResultCode,'', VList.MakeStaticAndClear);
end;


end.
