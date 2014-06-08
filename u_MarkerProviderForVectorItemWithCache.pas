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
  u_HashCacheWithQueuesAbstract;

type
  TMarkerProviderForVectorItemWithCache = class(THashCacheWithQueuesAbstract, IMarkerProviderForVectorItem)
  private
    FHashFunction: IHashFunction;
    FProvider: IMarkerProviderForVectorItem;
  private
    function GetMarker(
      const AConfig: ICaptionDrawConfigStatic;
      const AItem: IVectorDataItem
    ): IMarkerDrawable;
  protected
    function CreateByKey(
      const AKey: THashValue;
      AData: Pointer
    ): IInterface; override;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const AProvider: IMarkerProviderForVectorItem
    );
  end;

implementation

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
  inherited Create(14, 1000, 4000, 1000); // 2^14 elements in hash-table
  FHashFunction := AHashFunction;
  FProvider := AProvider;
end;

function TMarkerProviderForVectorItemWithCache.CreateByKey(
  const AKey: THashValue;
  AData: Pointer
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
  Result := IMarkerDrawable(GetOrCreateItem(VHash, @VData));
end;

end.
