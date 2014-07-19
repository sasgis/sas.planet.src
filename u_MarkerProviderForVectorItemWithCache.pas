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

unit u_MarkerProviderForVectorItemWithCache;

interface

uses
  t_Hash,
  i_HashFunction,
  i_VectorDataItemSimple,
  i_MarkerDrawable,
  i_MarksDrawConfig,
  i_MarkerProviderForVectorItem,
  i_HashInterfaceCache,
  u_BaseInterfacedObject;

type
  TMarkerProviderForVectorItemWithCache = class(TBaseInterfacedObject, IMarkerProviderForVectorItem)
  private
    FHashFunction: IHashFunction;
    FProvider: IMarkerProviderForVectorItem;
    FCache: IHashInterfaceCache;
  private
    function GetMarker(
      const AConfig: ICaptionDrawConfigStatic;
      const AItem: IVectorDataItem
    ): IMarkerDrawable;
  private
    function CreateByKey(
      const AKey: THashValue;
      const AData: Pointer
    ): IInterface;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const AProvider: IMarkerProviderForVectorItem
    );
  end;

implementation

uses
  u_HashInterfaceCache2Q;

type
  PDataRecord = ^TDataRecord;
  TDataRecord = record
    Config: ICaptionDrawConfigStatic;
    Item: IVectorDataItem;
  end;

{ TMarkerProviderForVectorItemWithCache }

constructor TMarkerProviderForVectorItemWithCache.Create(
  const AHashFunction: IHashFunction;
  const AProvider: IMarkerProviderForVectorItem
);
begin
  Assert(AProvider <> nil);
  inherited Create;
  FHashFunction := AHashFunction;
  FProvider := AProvider;
  FCache :=
    THashInterfaceCache2Q.Create(
      Self.CreateByKey,
      14,  // 2^14 elements in hash-table
      1000,
      4000,
      1000
    );
end;

function TMarkerProviderForVectorItemWithCache.CreateByKey(
  const AKey: THashValue;
  const AData: Pointer
): IInterface;
var
  VData: PDataRecord;
begin
  VData := PDataRecord(AData);
  Result := FProvider.GetMarker(VData^.Config, VData^.Item);
end;

function TMarkerProviderForVectorItemWithCache.GetMarker(
  const AConfig: ICaptionDrawConfigStatic;
  const AItem: IVectorDataItem
): IMarkerDrawable;
var
  VHash: THashValue;
  VData: TDataRecord;
begin
  Assert(Assigned(AItem));
  VHash := AItem.Hash;
  if Assigned(AConfig) then begin
    FHashFunction.UpdateHashByHash(VHash, AConfig.Hash);
  end;
  VData.Config :=  AConfig;
  VData.Item := AItem;
  Result := IMarkerDrawable(FCache.GetOrCreateItem(VHash, @VData));
end;

end.
