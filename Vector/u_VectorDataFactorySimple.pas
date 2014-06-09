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

unit u_VectorDataFactorySimple;

interface

uses
  t_Hash,
  i_HashFunction,
  i_Appearance,
  i_HtmlToHintTextConverter,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  i_VectorDataFactory,
  u_BaseInterfacedObject;

type
  TVectorDataItemMainInfoFactory = class(TBaseInterfacedObject, IVectorDataItemMainInfoFactory)
  private
    FHashFunction: IHashFunction;
    FHintConverter: IHtmlToHintTextConverter;
  private
    function BuildMainInfo(
      const AIdData: Pointer;
      const AName: string;
      const ADesc: string
    ): IVectorDataItemMainInfo;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const AHintConverter: IHtmlToHintTextConverter
    );
  end;

  TVectorDataFactorySimple = class(TBaseInterfacedObject, IVectorDataFactory)
  private
    FHashFunction: IHashFunction;
  private
    function BuildItem(
      const AMainInfo: IVectorDataItemMainInfo;
      const AAppearance: IAppearance;
      const AGeometry: IGeometryLonLat
    ): IVectorDataItem;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  SysUtils,
  u_VectorDataItemBase;

{ TVectorDataFactorySimple }

constructor TVectorDataFactorySimple.Create(
  const AHashFunction: IHashFunction
);
begin
  Assert(Assigned(AHashFunction));
  inherited Create;
  FHashFunction := AHashFunction;
end;

function TVectorDataFactorySimple.BuildItem(
  const AMainInfo: IVectorDataItemMainInfo;
  const AAppearance: IAppearance;
  const AGeometry: IGeometryLonLat
): IVectorDataItem;
var
  VHash: THashValue;
begin
  Assert(Assigned(AGeometry));
  Assert(Assigned(AMainInfo));
  VHash := AGeometry.Hash;
  FHashFunction.UpdateHashByHash(VHash, AMainInfo.Hash);
  if Assigned(AAppearance) then begin
    FHashFunction.UpdateHashByHash(VHash, AAppearance.Hash);
  end;
  Result :=
    TVectorDataItem.Create(
      VHash,
      AAppearance,
      AMainInfo,
      AGeometry
    );
end;

{ TVectorDataItemMainInfoFactory }

constructor TVectorDataItemMainInfoFactory.Create(
  const AHashFunction: IHashFunction;
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  Assert(Assigned(AHashFunction));
  Assert(Assigned(AHintConverter));
  inherited Create;
  FHashFunction := AHashFunction;
  FHintConverter := AHintConverter;
end;

function TVectorDataItemMainInfoFactory.BuildMainInfo(
  const AIdData: Pointer;
  const AName, ADesc: string
): IVectorDataItemMainInfo;
var
  VHash: THashValue;
begin
  VHash := FHashFunction.CalcHashByString(AName);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  Result :=
    TVectorDataItemMainInfo.Create(
      VHash,
      FHintConverter,
      AName,
      ADesc
    );
end;

end.
