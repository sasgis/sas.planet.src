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

unit u_VectorItemSubsetBuilder;

interface

uses
  t_Hash,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_VectorItemSubsetBuilder,
  i_HashFunction,
  i_InterfaceListSimple,
  u_BaseInterfacedObject;

type
  TVectorItemSubsetBuilder = class(TBaseInterfacedObject, IVectorItemSubsetBuilder)
  private
    FHashFunction: IHashFunction;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FList: IInterfaceListSimple;
    FHash: THashValue;
  private
    procedure Clear;
    function Add(const AItem: IVectorDataItem): Integer;

    function GetItem(AIndex: Integer): IVectorDataItem;
    function GetCapacity: Integer;
    procedure SetCapacity(ANewCapacity: Integer);
    function GetCount: Integer;

    procedure RemoveDuplicates;

    function MakeStaticAndClear: IVectorItemSubset;
    function MakeStaticCopy: IVectorItemSubset;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AHashFunction: IHashFunction
    );
  end;

  TVectorItemSubsetBuilderFactory = class(TBaseInterfacedObject, IVectorItemSubsetBuilderFactory)
  private
    FHashFunction: IHashFunction;
  private
    function Build: IVectorItemSubsetBuilder;
  public
    constructor Create(const AHashFunction: IHashFunction);
  end;

implementation

uses
  u_InterfaceListSimple,
  u_SortFunc,
  u_VectorItemSubset;

{ TVectorItemSubsetBuilder }

constructor TVectorItemSubsetBuilder.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AHashFunction: IHashFunction
);
begin
  inherited Create;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FHashFunction := AHashFunction;
end;

function TVectorItemSubsetBuilder.Add(
  const AItem: IVectorDataItem): Integer;
begin
  Result := -1;
  Assert(Assigned(AItem));
  if Assigned(AItem) then begin
    if not Assigned(FList) then begin
      FList := TInterfaceListSimple.Create;
    end;
    Result := FList.Add(AItem);
    if FList.Count = 1 then begin
      FHash := AItem.Hash;
    end else begin
      FHashFunction.UpdateHashByHash(FHash, AItem.Hash);
    end;
  end;
end;

procedure TVectorItemSubsetBuilder.Clear;
begin
  if Assigned(FList) then begin
    FList.Clear;
  end;
end;

function TVectorItemSubsetBuilder.GetCapacity: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Capacity;
  end else begin
    Result := 0;
  end;
end;

function TVectorItemSubsetBuilder.GetCount: Integer;
begin
  if Assigned(FList) then begin
    Result := FList.Count;
  end else begin
    Result := 0;
  end;
end;

function TVectorItemSubsetBuilder.GetItem(
  AIndex: Integer
): IVectorDataItem;
begin
  Result := IVectorDataItem(FList[AIndex]);
end;

function TVectorItemSubsetBuilder.MakeStaticAndClear: IVectorItemSubset;
begin
  Result := nil;
  if Assigned(FList) then begin
    if FList.Count > 0 then begin
      Result :=
        TVectorItemSubset.Create(
          FHash,
          FVectorItemSubsetBuilderFactory,
          FList.MakeStaticAndClear
        );
    end;
  end;
end;

function TVectorItemSubsetBuilder.MakeStaticCopy: IVectorItemSubset;
begin
  Result := nil;
  if Assigned(FList) then begin
    if FList.Count > 0 then begin
      Result :=
        TVectorItemSubset.Create(
          FHash,
          FVectorItemSubsetBuilderFactory,
          FList.MakeStaticCopy
        );
    end;
  end;
end;

function CompareVectorItems(const Item1, Item2: IInterface): Integer;
var
  VHash1, VHash2: THashValue;
begin
  VHash1 := IVectorDataItem(Item1).Hash;
  VHash2 := IVectorDataItem(Item2).Hash;
  if VHash1 = VHash2 then begin
    Result := 0;
  end else if VHash1 < VHash2 then begin
    Result := 1;
  end else begin
    Result := -1;
  end;
end;

procedure TVectorItemSubsetBuilder.RemoveDuplicates;
var
  i: Integer;
  VPrevIndex: Integer;
  VItemCurr: IVectorDataItem;
  VItemPrev: IVectorDataItem;
  VHash: THashValue;
begin
  if Assigned(FList) then begin
    if FList.Count > 1 then begin
      SortInterfaceListByCompareFunction(FList, CompareVectorItems);
      VPrevIndex := 0;
      VItemPrev := IVectorDataItem(FList.Items[0]);
      VHash := VItemPrev.Hash;
      for i := 1 to FList.Count - 1 do begin
        VItemCurr := IVectorDataItem(FList.Items[i]);
        if not VItemPrev.IsEqual(VItemCurr) then begin
          FHashFunction.UpdateHashByHash(VHash, VItemCurr.Hash);
          VItemPrev := VItemCurr;
          Inc(VPrevIndex);
          if VPrevIndex <> i then begin
            FList.Items[VPrevIndex] := VItemCurr;
          end;
        end;
      end;
      FList.Count := VPrevIndex + 1;
      FHash := VHash;
    end;
  end;
end;

procedure TVectorItemSubsetBuilder.SetCapacity(ANewCapacity: Integer);
begin
  if (not Assigned(FList)) and (ANewCapacity > 0) then begin
    FList := TInterfaceListSimple.Create;
    FList.Capacity := ANewCapacity;
  end else if Assigned(FList) then begin
    FList.Capacity := ANewCapacity;
  end;
end;

{ TVectorItemSubsetBuilderFactory }

constructor TVectorItemSubsetBuilderFactory.Create(
  const AHashFunction: IHashFunction);
begin
  inherited Create;
  FHashFunction := AHashFunction;
end;

function TVectorItemSubsetBuilderFactory.Build: IVectorItemSubsetBuilder;
begin
  Result := TVectorItemSubsetBuilder.Create(Self, FHashFunction);
end;

end.
