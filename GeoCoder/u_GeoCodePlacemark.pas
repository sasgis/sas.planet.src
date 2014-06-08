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

unit u_GeoCodePlacemark;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_GeometryLonLat,
  i_HashFunction,
  i_VectorDataItemSimple,
  i_GeometryLonLatFactory,
  i_GeoCoder,
  u_BaseInterfacedObject;

type
  TGeoCodePlacemarkInfo = class(TBaseInterfacedObject, IVectorDataItemMainInfo, IGeoCodePlacemarkInfo)
  private
    FHash: THashValue;
    FAddress: string;
    FDesc: string;
    FFullDesc: string;
    FAccuracy: Integer;
  private
    function GetHash: THashValue;
    function GetName: string;
    function GetDesc: string;
    function IsEqual(const AItem: IVectorDataItemMainInfo): Boolean;
    function GetHintText: string;
    function GetInfoHTML: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
    function GetAccuracy: Integer;
  public
    constructor Create(
      const AHash: THashValue;
      const AAddress: string;
      const ADesc: string;
      const AFullDesc: string;
      const AAccuracy: Integer
    );
  end;

  TGeoCodePlacemarkFactory = class(TBaseInterfacedObject, IGeoCodePlacemarkFactory)
  private
    FGeometryFactory: IGeometryLonLatFactory;
    FHashFunction: IHashFunction;
  private
    function Build(
      const APoint: TDoublePoint;
      const AAddress: string;
      const ADesc: string;
      const AFullDesc: string;
      const AAccuracy: Integer
    ): IVectorDataItem;
  public
    constructor Create(
      const AGeometryFactory: IGeometryLonLatFactory;
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  SysUtils,
  u_VectorDataItemBase;

{ TGeoCodePlacemark }

constructor TGeoCodePlacemarkInfo.Create(
  const AHash: THashValue;
  const AAddress: string;
  const ADesc: string;
  const AFullDesc: string;
  const AAccuracy: Integer
);
begin
  inherited Create;
  FHash := AHash;
  FAddress := AAddress;
  FDesc := ADesc;
  FFullDesc := AFullDesc;
  FAccuracy := AAccuracy;
end;

function TGeoCodePlacemarkInfo.GetAccuracy: Integer;
begin
  Result := FAccuracy;
end;

function TGeoCodePlacemarkInfo.GetName: string;
begin
  Result := FAddress;
end;

function TGeoCodePlacemarkInfo.GetDesc: string;
begin
  Result := FDesc;
end;

function TGeoCodePlacemarkInfo.GetHash: THashValue;
begin
  Result := FHash;
end;

function TGeoCodePlacemarkInfo.GetHintText: string;
begin
  Result := FDesc;
end;

function TGeoCodePlacemarkInfo.GetInfoCaption: string;
begin
  Result := FAddress;
end;

function TGeoCodePlacemarkInfo.GetInfoHTML: string;
begin
  Result := FFullDesc;
end;

function TGeoCodePlacemarkInfo.GetInfoUrl: string;
begin
  Result := '';
end;

function TGeoCodePlacemarkInfo.IsEqual(const AItem: IVectorDataItemMainInfo): Boolean;
var
  VGeoCodePlacemark: IGeoCodePlacemarkInfo;
begin
  if not Assigned(AItem) then begin
    Result := False;
    Exit;
  end;
  if AItem = IVectorDataItemMainInfo(Self) then begin
    Result := True;
    Exit;
  end;
  if (FHash <> 0) and (AItem.Hash <> 0) and (FHash <> AItem.Hash) then begin
    Result := False;
    Exit;
  end;
  if FAddress <> AItem.Name then begin
    Result := False;
    Exit;
  end;
  if FDesc <> AItem.Desc then begin
    Result := False;
    Exit;
  end;
  if not Supports(AItem, IGeoCodePlacemarkInfo, VGeoCodePlacemark) then begin
    Result := False;
    Exit;
  end;
  if FAccuracy <> VGeoCodePlacemark.GetAccuracy then begin
    Result := False;
    Exit;
  end;

  if FFullDesc <> VGeoCodePlacemark.GetInfoHTML then begin
    Result := False;
    Exit;
  end;

  Result := True;
end;

{ TGeoCodePlacemarkFactory }

constructor TGeoCodePlacemarkFactory.Create(
  const AGeometryFactory: IGeometryLonLatFactory;
  const AHashFunction: IHashFunction
);
begin
  inherited Create;
  FGeometryFactory := AGeometryFactory;
  FHashFunction := AHashFunction;
end;

function TGeoCodePlacemarkFactory.Build(
  const APoint: TDoublePoint;
  const AAddress, ADesc, AFullDesc: string;
  const AAccuracy: Integer
): IVectorDataItem;
var
  VHash: THashValue;
  VPoint: IGeometryLonLatPoint;
  VMainInfo: IVectorDataItemMainInfo;
begin
  VPoint := FGeometryFactory.CreateLonLatPoint(APoint);

  VHash := FHashFunction.CalcHashByString(AAddress);
  FHashFunction.UpdateHashByString(VHash, ADesc);
  FHashFunction.UpdateHashByString(VHash, AFullDesc);
  VMainInfo :=
    TGeoCodePlacemarkInfo.Create(
      VHash,
      AAddress,
      ADesc,
      AFullDesc,
      AAccuracy
    );

  VHash := VPoint.Hash;
  FHashFunction.UpdateHashByHash(VHash, VMainInfo.Hash);
  Result :=
    TVectorDataItemBase.Create(
      VHash,
      nil,
      VMainInfo,
      VPoint
    );
end;

end.
