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
  i_Datum,
  i_HashFunction,
  i_CoordConverter,
  i_ConfigDataProvider,
  i_CoordConverterFactory,
  u_BaseInterfacedObject;

type
  TCoordConverterFactorySimple = class(TBaseInterfacedObject, ICoordConverterFactory)
  private
    FHashFunction: IHashFunction;
    FDatumFactory: IDatumFactory;

    FGoogle: ICoordConverter;
    FYandex: ICoordConverter;
    FLonLat: ICoordConverter;
    FEPSG53004: ICoordConverter;
  private
    function GetCoordConverterByConfig(const AConfig: IConfigDataProvider): ICoordConverter;
    function GetCoordConverterByCode(
      AProjectionEPSG: Integer;
      ATileSplitCode: Integer
    ): ICoordConverter;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const ADatumFactory: IDatumFactory
   );
  end;

implementation

uses
  SysUtils,
  t_Hash,
  t_GeoTypes,
  c_CoordConverter,
  u_CoordConverterMercatorOnSphere,
  u_CoordConverterMercatorOnEllipsoid,
  u_CoordConverterSimpleLonLat,
  u_ResStrings;

{ TCoordConverterFactorySimple }

constructor TCoordConverterFactorySimple.Create(
  const AHashFunction: IHashFunction;
  const ADatumFactory: IDatumFactory
);
var
  VHash: THashValue;
  VDatum: IDatum;
begin
  inherited Create;
  FHashFunction := AHashFunction;
  FDatumFactory := ADatumFactory;

  VHash := FHashFunction.CalcHashByInteger(1);
  VDatum := FDatumFactory.GetByCode(CGoogleDatumEPSG);
  FHashFunction.UpdateHashByHash(VHash, VDatum.Hash);
  FHashFunction.UpdateHashByInteger(VHash, CGoogleProjectionEPSG);
  FGoogle := TCoordConverterMercatorOnSphere.Create(VHash, VDatum, CGoogleProjectionEPSG);

  VHash := FHashFunction.CalcHashByInteger(1);
  VDatum := FDatumFactory.GetByCode(53004);
  FHashFunction.UpdateHashByHash(VHash, VDatum.Hash);
  FHashFunction.UpdateHashByInteger(VHash, 53004);
  FEPSG53004 := TCoordConverterMercatorOnSphere.Create(VHash, VDatum, 53004);

  VHash := FHashFunction.CalcHashByInteger(2);
  VDatum := FDatumFactory.GetByCode(CYandexDatumEPSG);
  FHashFunction.UpdateHashByHash(VHash, VDatum.Hash);
  FHashFunction.UpdateHashByInteger(VHash, CYandexProjectionEPSG);
  FYandex := TCoordConverterMercatorOnEllipsoid.Create(VHash, VDatum, CYandexProjectionEPSG);

  VHash := FHashFunction.CalcHashByInteger(3);
  VDatum := FDatumFactory.GetByCode(CYandexDatumEPSG);
  FHashFunction.UpdateHashByHash(VHash, VDatum.Hash);
  FHashFunction.UpdateHashByInteger(VHash, CGELonLatProjectionEPSG);
  FLonLat := TCoordConverterSimpleLonLat.Create(VHash, VDatum, CGELonLatProjectionEPSG);
end;

function TCoordConverterFactorySimple.GetCoordConverterByCode(
  AProjectionEPSG, ATileSplitCode: Integer
): ICoordConverter;
begin
  Result := nil;
  if ATileSplitCode = CTileSplitQuadrate256x256 then begin
    case AProjectionEPSG of
      CGoogleProjectionEPSG: begin
        Result := FGoogle;
      end;
      53004: begin
        Result := FEPSG53004;
      end;
      CYandexProjectionEPSG: begin
        Result := FYandex;
      end;
      CGELonLatProjectionEPSG: begin
        Result := FLonLat;
      end;
    else begin
      raise Exception.CreateFmt(SAS_ERR_MapProjectionUnexpectedType, [IntToStr(AProjectionEPSG)]);
    end;
    end;
  end else begin
    raise Exception.Create('Неизвестный тип разделения карты на тайлы');
  end;
end;

function TCoordConverterFactorySimple.GetCoordConverterByConfig(
  const AConfig: IConfigDataProvider
): ICoordConverter;
var
  VProjection: byte;
  VRadiusA: Double;
  VRadiusB: Double;
  VEPSG: Integer;
  VTileSplitCode: Integer;
  VDatum: IDatum;
  VHash: THashValue;
begin
  Result := nil;
  VTileSplitCode := CTileSplitQuadrate256x256;
  VEPSG := 0;
  VProjection := 1;
  VRadiusA := 6378137;
  VRadiusB := VRadiusA;

  if AConfig <> nil then begin
    VEPSG := AConfig.ReadInteger('EPSG', VEPSG);
    VProjection := AConfig.ReadInteger('projection', VProjection);
    VRadiusA := AConfig.ReadFloat('sradiusa', VRadiusA);
    VRadiusB := AConfig.ReadFloat('sradiusb', VRadiusA);
  end;

  if VEPSG = 0 then begin
    case VProjection of
      1: begin
        if Abs(VRadiusA - 6378137) < 1 then begin
          VEPSG := CGoogleProjectionEPSG;
        end else if Abs(VRadiusA - 6371000) < 1 then begin
          VEPSG := 53004;
        end;
      end;
      2: begin
        if (Abs(VRadiusA - 6378137) < 1) and (Abs(VRadiusB - 6356752) < 1) then begin
          VEPSG := CYandexProjectionEPSG;
        end;
      end;
      3: begin
        if (Abs(VRadiusA - 6378137) < 1) and (Abs(VRadiusB - 6356752) < 1) then begin
          VEPSG := CGELonLatProjectionEPSG;
        end;
      end else begin
      raise Exception.CreateFmt(SAS_ERR_MapProjectionUnexpectedType, [IntToStr(VProjection)]);
    end;
    end;
  end;

  if VEPSG <> 0 then begin
    try
      Result := GetCoordConverterByCode(VEPSG, VTileSplitCode);
    except
      Result := nil;
    end;
  end;

  if Result = nil then begin
    case VProjection of
      1: begin
        VDatum := FDatumFactory.GetByRadius(VRadiusA, VRadiusA);
        if VDatum.EPSG = CGoogleDatumEPSG then begin
          Result := FGoogle;
        end else if VDatum.EPSG = 53004  then begin
          Result := FEPSG53004;
        end else begin
          VHash := FHashFunction.CalcHashByInteger(1);
          FHashFunction.UpdateHashByHash(VHash, VDatum.Hash);
          FHashFunction.UpdateHashByInteger(VHash, 0);
          Result := TCoordConverterMercatorOnSphere.Create(VHash, VDatum, 0);
        end;
      end;
      2: begin
        VDatum := FDatumFactory.GetByRadius(VRadiusA, VRadiusB);
        if VDatum.EPSG = CYandexDatumEPSG then begin
          Result := FYandex;
        end else begin
          VHash := FHashFunction.CalcHashByInteger(2);
          FHashFunction.UpdateHashByHash(VHash, VDatum.Hash);
          FHashFunction.UpdateHashByInteger(VHash, 0);
          Result := TCoordConverterMercatorOnEllipsoid.Create(VHash, VDatum, 0);
        end;
      end;
      3: begin
        VDatum := FDatumFactory.GetByRadius(VRadiusA, VRadiusB);
        if VDatum.EPSG = CYandexDatumEPSG then begin
          Result := FLonLat;
        end else begin
          VHash := FHashFunction.CalcHashByInteger(3);
          FHashFunction.UpdateHashByHash(VHash, VDatum.Hash);
          FHashFunction.UpdateHashByInteger(VHash, 0);
          Result := TCoordConverterSimpleLonLat.Create(VHash, VDatum, 0);
        end;
      end;
    else begin
      raise Exception.CreateFmt(SAS_ERR_MapProjectionUnexpectedType, [IntToStr(VProjection)]);
    end;
    end;
  end;
end;

end.
