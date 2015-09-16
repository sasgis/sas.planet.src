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

unit u_GeoCoderLocalBasic;

interface

uses
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_GeoCoder,
  i_LocalCoordConverter,
  i_VectorItemSubset,
  i_VectorItemSubsetBuilder,
  u_BaseInterfacedObject;

type
  TGeoCoderLocalBasic = class(TBaseInterfacedObject, IGeoCoder)
  private
    FPlacemarkFactory: IGeoCodePlacemarkFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    function BuildSortedSubset(
      const AList: IInterfaceListSimple;
      const ALocalConverter: ILocalCoordConverter
    ): IVectorItemSubset;
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
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory
    );
  end;

implementation

uses
  i_VectorDataItemSimple,
  i_Datum,
  u_SortFunc,
  u_GeoCodeResult;

{ TGeoCoderLocalBasic }

constructor TGeoCoderLocalBasic.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory
);
begin
  inherited Create;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FPlacemarkFactory := APlacemarkFactory;
end;

function TGeoCoderLocalBasic.BuildSortedSubset(
  const AList: IInterfaceListSimple;
  const ALocalConverter: ILocalCoordConverter
): IVectorItemSubset;
var
  i: integer;
  VMark: IVectorDataItem;
  VDatum: IDatum;
  VDistArr: array of Double;
  VSubsetBuilder: IVectorItemSubsetBuilder;
begin
  Result := nil;
  if Assigned(AList) then begin
    if AList.Count > 1 then begin
      VDatum := ALocalConverter.Projection.ProjectionType.Datum;
      SetLength(VDistArr, AList.Count);
      for i := 0 to AList.GetCount - 1 do begin
        VMark := IVectorDataItem(AList.Items[i]);
        VDistArr[i] := VDatum.CalcDist(ALocalConverter.GetCenterLonLat, VMark.Geometry.Bounds.CalcRectCenter);
      end;
      SortInterfaceListByDoubleMeasure(AList, VDistArr);
    end;
    VSubsetBuilder := FVectorItemSubsetBuilderFactory.Build;
    for i := 0 to AList.GetCount - 1 do begin
      VMark := IVectorDataItem(AList.Items[i]);
      VSubsetBuilder.Add(VMark);
    end;
    Result := VSubsetBuilder.MakeStaticAndClear;
  end;
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
  VSubset: IVectorItemSubset;
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
  VSubset := BuildSortedSubset(VList, ALocalConverter);
  Result := TGeoCodeResult.Create(ASearch, VResultCode, '', VSubset);
end;


end.
