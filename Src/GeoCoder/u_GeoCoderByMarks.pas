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

unit u_GeoCoderByMarks;

interface

uses
  i_InterfaceListSimple,
  i_MarkDb,
  i_GeoCoder,
  i_NotifierOperation,
  i_VectorItemSubsetBuilder,
  i_LocalCoordConverter,
  u_GeoCoderLocalBasic;

type
  TGeoCoderByMarks = class(TGeoCoderLocalBasic)
  private
    FMarksDb: IMarkDb;
  protected
    function DoSearch(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AMarksDb: IMarkDb
    );
  end;

implementation

uses
  i_VectorItemSubset,
  u_InterfaceListSimple;

{ TGeoCoderByMarks }

constructor TGeoCoderByMarks.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AMarksDb: IMarkDb
);
begin
  inherited Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory);
  FMarksDb := AMarksDb;
end;

function TGeoCoderByMarks.DoSearch(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  I: Integer;
  VList: IInterfaceListSimple;
  VVectorItems: IVectorItemSubset;
begin
  VList := TInterfaceListSimple.Create;
  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Exit;
  end;
  VVectorItems := FMarksDb.FindMarks(ASearch, 100, True, True);

  if VVectorItems <> nil then begin
    for I := 0 to VVectorItems.Count - 1 do begin
      VList.Add(VVectorItems.Items[i]);
    end;
  end;

  Result := VList;
end;

end.
