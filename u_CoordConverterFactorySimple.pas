{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_CoordConverterFactorySimple;

interface

uses
  i_Datum,
  i_CoordConverter,
  i_ConfigDataProvider,
  i_CoordConverterFactory,
  u_BaseInterfacedObject;

type
  TCoordConverterFactorySimple = class(TBaseInterfacedObject, IDatumFactory, ICoordConverterFactory)
  private
    FDatumGoogle: IDatum;
    FDatumYandex: IDatum;
    FDatum53004: IDatum;

    FGoogle: ICoordConverter;
    FYandex: ICoordConverter;
    FLonLat: ICoordConverter;
    FEPSG53004: ICoordConverter;
  private
    function GetByCode(ADatumEPSG: Integer): IDatum;
    function GetByRadius(const ARadiusA, ARadiusB: Double): IDatum;
  private
    function GetCoordConverterByConfig(const AConfig: IConfigDataProvider): ICoordConverter;
    function GetCoordConverterByCode(
      AProjectionEPSG: Integer;
      ATileSplitCode: Integer
    ): ICoordConverter;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  c_CoordConverter,
  u_Datum,
  u_CoordConverterMercatorOnSphere,
  u_CoordConverterMercatorOnEllipsoid,
  u_CoordConverterSimpleLonLat,
  u_ResStrings;

{ TCoordConverterFactorySimple }

constructor TCoordConverterFactorySimple.Create;
var
  VRadiusA: Double;
  VRadiusB: Double;
begin
  inherited Create;

  VRadiusA := 6378137;
  FDatumGoogle := TDatum.Create(CGoogleDatumEPSG, VRadiusA, VRadiusA);

  VRadiusA := 6378137;
  VRadiusB := 6356752;
  FDatumYandex := TDatum.Create(CYandexDatumEPSG, VRadiusA, VRadiusB);

  VRadiusA := 6371000;
  FDatum53004 := TDatum.Create(53004, VRadiusA, VRadiusA);

  FGoogle := TCoordConverterMercatorOnSphere.Create(FDatumGoogle, CGoogleProjectionEPSG, CELL_UNITS_METERS);

  FEPSG53004 := TCoordConverterMercatorOnSphere.Create(FDatum53004, 53004, CELL_UNITS_METERS);

  FYandex := TCoordConverterMercatorOnEllipsoid.Create(FDatumYandex, CYandexProjectionEPSG, CELL_UNITS_METERS);

  FLonLat := TCoordConverterSimpleLonLat.Create(FDatumYandex, CGELonLatProjectionEPSG, CELL_UNITS_DEGREES);
end;

function TCoordConverterFactorySimple.GetByCode(ADatumEPSG: Integer): IDatum;
begin
  Result := nil;
  case ADatumEPSG of
    CGoogleDatumEPSG: begin
      Result := FDatumGoogle;
    end;
    CYandexDatumEPSG: begin
      Result := FDatumYandex;
    end;
    53004: begin
      Result := FDatum53004;
    end;
  end;
end;

function TCoordConverterFactorySimple.GetByRadius(
  const ARadiusA, ARadiusB: Double
): IDatum;
var
  VEPSG: Integer;
begin
  VEPSG := 0;
  if (Abs(ARadiusA - 6378137) < 1) and (Abs(ARadiusB - 6378137) < 1) then begin
    VEPSG := CGoogleDatumEPSG;
  end;
  if (Abs(ARadiusA - 6378137) < 1) and (Abs(ARadiusB - 6356752) < 1) then begin
    VEPSG := CYandexDatumEPSG;
  end;
  if VEPSG > 0 then begin
    Result := GetByCode(VEPSG);
  end else begin
    Result := TDatum.Create(0, ARadiusA, ARadiusB);
  end;
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
        VDatum := GetByRadius(VRadiusA, VRadiusA);
        if VDatum.EPSG = CGoogleDatumEPSG then begin
          Result := FGoogle;
        end else if VDatum.EPSG = 53004  then begin
          Result := FEPSG53004;
        end else begin
          Result := TCoordConverterMercatorOnSphere.Create(VDatum, 0, CELL_UNITS_UNKNOWN);
        end;
      end;
      2: begin
        VDatum := GetByRadius(VRadiusA, VRadiusB);
        if VDatum.EPSG = CYandexDatumEPSG then begin
          Result := FYandex;
        end else begin
          Result := TCoordConverterMercatorOnEllipsoid.Create(VDatum, 0, CELL_UNITS_UNKNOWN);
        end;
      end;
      3: begin
        VDatum := GetByRadius(VRadiusA, VRadiusB);
        if VDatum.EPSG = CYandexDatumEPSG then begin
          Result := FLonLat;
        end else begin
          Result := TCoordConverterSimpleLonLat.Create(VDatum, 0, CELL_UNITS_UNKNOWN);
        end;
      end;
    else begin
      raise Exception.CreateFmt(SAS_ERR_MapProjectionUnexpectedType, [IntToStr(VProjection)]);
    end;
    end;
  end;
end;

end.
