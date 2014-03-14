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

unit u_VectorItemSubset;

interface

uses
  ActiveX,
  t_Hash,
  t_GeoTypes,
  i_VectorDataItemSimple,
  i_InterfaceListStatic,
  i_VectorItemSubsetBuilder,
  i_VectorItemSubset,
  u_BaseInterfacedObject;

type
  TVectorItemSubset = class(TBaseInterfacedObject, IVectorItemSubset)
  private
    FHash: THashValue;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FList: IInterfaceListStatic;
  private
    function GetSubsetByLonLatRect(const ARect: TDoubleRect): IVectorItemSubset;
    function GetEnum: IEnumUnknown;
    function IsEmpty: Boolean;
    function IsEqual(const ASubset: IVectorItemSubset): Boolean;

    function GetCount: Integer;
    function GetItem(AIndex: Integer): IVectorDataItemSimple;
    function GetHash: THashValue;
  public
    constructor Create(
      const AHash: THashValue;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AList: IInterfaceListStatic
    );
  end;

implementation

uses
  u_EnumUnknown;

{ TVectorItemSubset }

constructor TVectorItemSubset.Create(
  const AHash: THashValue;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AList: IInterfaceListStatic
);
begin
  inherited Create;
  FHash := AHash;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FList := AList;
end;

function TVectorItemSubset.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TVectorItemSubset.GetEnum: IEnumUnknown;
begin
  Result := TEnumUnknownByStatic.Create(FList);
end;

function TVectorItemSubset.GetHash: THashValue;
begin
  Result := FHash;
end;

function TVectorItemSubset.GetItem(AIndex: Integer): IVectorDataItemSimple;
begin
  Result := IVectorDataItemSimple(FList.Items[AIndex]);
end;

function TVectorItemSubset.GetSubsetByLonLatRect(
  const ARect: TDoubleRect): IVectorItemSubset;
var
  VNewList: IVectorItemSubsetBuilder;
  i: Integer;
  VItem: IVectorDataItemSimple;
begin
  VNewList := FVectorItemSubsetBuilderFactory.Build;
    for i := 0 to FList.Count - 1 do begin
      VItem := IVectorDataItemSimple(FList.Items[i]);
      if VItem.Geometry.Bounds.IsIntersecWithRect(ARect) then begin
        VNewList.Add(VItem);
      end;
    end;
  Result := VNewList.MakeStaticAndClear;
end;

function TVectorItemSubset.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

function TVectorItemSubset.IsEqual(const ASubset: IVectorItemSubset): Boolean;
var
  i: Integer;
begin
  if not Assigned(ASubset) then begin
    Result := False;
  end else if ASubset = IVectorItemSubset(Self) then begin
    Result := True;
  end else if FList.Count <> ASubset.Count then begin
    Result := False;
  end else if (FHash <> 0) and (ASubset.Hash <>0) and (FHash <> ASubset.Hash)  then begin
    Result := False;
  end else begin
    Result := True;
    for i := 0 to FList.Count - 1 do begin
      if not IVectorDataItemSimple(FList.Items[i]).IsEqual(ASubset.Items[i]) then begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

end.
