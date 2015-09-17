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

unit u_CoordConverterFactorySimple;

interface

uses
  i_DatumFactory,
  i_HashFunction,
  i_ProjectionType,
  i_ProjectionSet,
  i_ConfigDataProvider,
  i_ProjectionTypeFactory,
  i_ProjectionSetFactory,
  u_BaseInterfacedObject;

type
  TCoordConverterFactorySimple = class(TBaseInterfacedObject, IProjectionSetFactory)
  private
    FHashFunction: IHashFunction;
    FProjectionTypeFactory: IProjectionTypeFactory;

    FProjectionSetGoogle: IProjectionSet;
    FProjectionSetYandex: IProjectionSet;
    FProjectionSetLonLat: IProjectionSet;
    FProjectionSetEPSG53004: IProjectionSet;
    function CreateProjectionSet(const AProjectionType: IProjectionType): IProjectionSet;
  private
    function GetProjectionSetByConfig(const AConfig: IConfigDataProvider): IProjectionSet;
    function GetProjectionSetByCode(
      AProjectionEPSG: Integer;
      ATileSplitCode: Integer
    ): IProjectionSet;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const ADatumFactory: IDatumFactory
    );
  end;

implementation

uses
  t_Hash,
  c_CoordConverter,
  i_InterfaceListSimple,
  i_ProjectionInfo,
  u_InterfaceListSimple,
  u_ProjectionBasic256x256,
  u_ProjectionSetSimple,
  u_ProjectionTypeFactorySimple;

{ TCoordConverterFactorySimple }

constructor TCoordConverterFactorySimple.Create(
  const AHashFunction: IHashFunction;
  const ADatumFactory: IDatumFactory
);
var
  VProjectionType: IProjectionType;
begin
  inherited Create;
  FHashFunction := AHashFunction;
  FProjectionTypeFactory := TProjectionTypeFactorySimple.Create(AHashFunction, ADatumFactory);

  VProjectionType := FProjectionTypeFactory.GetByCode(CGoogleProjectionEPSG);
  FProjectionSetGoogle := CreateProjectionSet(VProjectionType);

  VProjectionType := FProjectionTypeFactory.GetByCode(53004);
  FProjectionSetEPSG53004 := CreateProjectionSet(VProjectionType);

  VProjectionType := FProjectionTypeFactory.GetByCode(CYandexProjectionEPSG);
  FProjectionSetYandex := CreateProjectionSet(VProjectionType);

  VProjectionType := FProjectionTypeFactory.GetByCode(CGELonLatProjectionEPSG);
  FProjectionSetLonLat := CreateProjectionSet(VProjectionType);
end;

function TCoordConverterFactorySimple.CreateProjectionSet(
  const AProjectionType: IProjectionType
): IProjectionSet;
var
  VZooms: IInterfaceListSimple;
  i: Integer;
  VHash: THashValue;
  VProjection: IProjection;
begin
  VZooms := TInterfaceListSimple.Create;
  VZooms.Capacity := 24;

  for i := 0 to 24 - 1 do begin
    VHash := AProjectionType.Hash;
    FHashFunction.UpdateHashByInteger(VHash, i);
    VProjection := TProjectionBasic256x256.Create(VHash, AProjectionType, i);
    VZooms.Add(VProjection);
  end;
  Result :=
    TProjectionSetSimple.Create(
      AProjectionType.Hash,
      VZooms.MakeStaticAndClear
    );
end;

function TCoordConverterFactorySimple.GetProjectionSetByCode(
  AProjectionEPSG, ATileSplitCode: Integer
): IProjectionSet;
var
  VProjectionType: IProjectionType;
begin
  Assert(ATileSplitCode = CTileSplitQuadrate256x256);
  VProjectionType := FProjectionTypeFactory.GetByCode(AProjectionEPSG);
  case VProjectionType.ProjectionEPSG of
    CGoogleProjectionEPSG: begin
      Result := FProjectionSetGoogle;
    end;
    CYandexProjectionEPSG: begin
      Result := FProjectionSetYandex;
    end;
    53004: begin
      Result := FProjectionSetEPSG53004;
    end;
    CGELonLatProjectionEPSG: begin
      Result := FProjectionSetLonLat;
    end;
  else begin
    Result := CreateProjectionSet(VProjectionType);
  end;
  end;
end;

function TCoordConverterFactorySimple.GetProjectionSetByConfig(
  const AConfig: IConfigDataProvider
): IProjectionSet;
var
  VProjectionType: IProjectionType;
begin
  VProjectionType := FProjectionTypeFactory.GetByConfig(AConfig);
  case VProjectionType.ProjectionEPSG of
    CGoogleProjectionEPSG: begin
      Result := FProjectionSetGoogle;
    end;
    CYandexProjectionEPSG: begin
      Result := FProjectionSetYandex;
    end;
    53004: begin
      Result := FProjectionSetEPSG53004;
    end;
    CGELonLatProjectionEPSG: begin
      Result := FProjectionSetLonLat;
    end;
  else begin
    Result := CreateProjectionSet(VProjectionType);
  end;
  end;
end;

end.
