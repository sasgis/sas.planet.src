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

unit u_MapVersionFactorySimpleString;

interface

uses
  t_Hash,
  i_HashFunction,
  i_MapVersionInfo,
  i_MapVersionFactory,
  i_HashInterfaceCache,
  u_BaseInterfacedObject;

type
  IMapVersionFactorySimpleInternal = interface
    ['{AF11A02B-BB29-47FE-A2DC-FC72568827C5}']
  end;

  TMapVersionFactorySimpleString = class(TBaseInterfacedObject, IMapVersionFactory, IMapVersionFactorySimpleInternal)
  private
    FHashFunction: IHashFunction;
    FCache: IHashInterfaceCache;
  private
    function CreateByKey(
      const AKey: THashValue;
      const AData: Pointer
    ): IInterface;
  private
    { IMapVersionFactory }
    function CreateByStoreString(const AValue: string): IMapVersionInfo;
    function CreateByMapVersion(const AValue: IMapVersionInfo): IMapVersionInfo;
    function IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  SysUtils,
  u_HashInterfaceCache2Q,
  u_MapVersionInfo,
  u_Synchronizer;

{ TMapVersionFactorySimpleString }

constructor TMapVersionFactorySimpleString.Create(
  const AHashFunction: IHashFunction);
begin
  Assert(Assigned(AHashFunction));
  inherited Create;
  FHashFunction := AHashFunction;
  FCache :=
    THashInterfaceCache2Q.Create(
      GSync.SyncVariable.Make(Self.ClassName),
      Self.CreateByKey,
      10,  // 2^10 elements in hash-table
      256,
      512,
      256
    );
end;

function TMapVersionFactorySimpleString.CreateByKey(
  const AKey: THashValue;
  const AData: Pointer
): IInterface;
var
  VResult: IMapVersionInfo;
begin
  VResult := TMapVersionInfo.Create(AKey, string(AData));
  Result := VResult;
end;

function TMapVersionFactorySimpleString.CreateByMapVersion(
  const AValue: IMapVersionInfo
): IMapVersionInfo;
begin
  if AValue <> nil then begin
    Result := CreateByStoreString(AValue.StoreString);
  end else begin
    Result := CreateByStoreString('');
  end;
end;

function TMapVersionFactorySimpleString.CreateByStoreString(
  const AValue: string
): IMapVersionInfo;
var
  VHash: THashValue;
begin
  VHash := FHashFunction.CalcHashByString(AValue);
  Result := IMapVersionInfo(FCache.GetOrCreateItem(VHash, Pointer(AValue)));
end;

function TMapVersionFactorySimpleString.IsSameFactoryClass(const AMapVersionFactory: IMapVersionFactory): Boolean;
begin
  if (nil = AMapVersionFactory) then begin
    Result := False;
  end else begin
    Result := Supports(AMapVersionFactory, IMapVersionFactorySimpleInternal);
  end;
end;

end.
