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
  Classes,
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_GeoCoder,
  i_LocalCoordConverter,
  u_BaseInterfacedObject;

type
  TGeoCoderLocalBasic = class(TBaseInterfacedObject, IGeoCoder)
  protected
    function DoSearch(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; virtual; abstract;
  private
    function GetLocations(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IGeoCodeResult; safecall;
  end;
implementation

uses
  u_InterfaceListSimple,
  u_GeoCodeResult;

{ TGeoCoderLocalBasic }

procedure QuickSort(
  var AList: IInterfaceListSimple;
  var ADist: array of Double;
  L, R: Integer
  );
var
  I, J: Integer;
  P: Double;
  TD: Double;
begin
  repeat
    I := L;
    J := R;
    P := ADist[(L + R) shr 1];
    repeat
      while ADist[I] < P do begin
        Inc(I);
      end;
      while ADist[J] > P do begin
        Dec(J);
      end;
      if I <= J then begin
        TD := ADist[I];
        ADist[I] := ADist[J];
        ADist[J] := TD;
        AList.Exchange(I,J);
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then begin
      QuickSort(AList, ADist, L, J);
    end;
    L := I;
  until I >= R;
end;

procedure SortIt(
  var AList: IInterfaceListSimple;
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
  QuickSort(AList, VDistArr, 0, AList.GetCount-1);
end;

function TGeoCoderLocalBasic.GetLocations(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASearch: WideString;
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
