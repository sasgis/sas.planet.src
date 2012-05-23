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
  i_proj4,
  i_CoordConverter,
  i_ProjectionInfo,
  i_ConfigDataProvider,
  i_CoordConverterFactory;

type
  TCoordConverterFactorySimple = class(TInterfacedObject, ICoordConverterFactory, IProjectionInfoFactory)
  private
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
  private
    function GetByConverterAndZoom(
      const AGeoConverter: ICoordConverter;
      AZoom: Byte
    ): IProjectionInfo;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  c_CoordConverter,
  u_CoordConverterMercatorOnSphere,
  u_CoordConverterMercatorOnEllipsoid,
  u_CoordConverterSimpleLonLat,
  u_ProjectionInfo,
  u_ResStrings;

{ TCoordConverterFactorySimple }

constructor TCoordConverterFactorySimple.Create;
var
  VRadiusA: Double;
  VRadiusB: Double;
  VProj4Info: TProj4Info;
begin
  inherited Create;

  // predefined values without proj4
  Proj4InfoClear(VProj4Info);

  VRadiusA := 6378137;
  FGoogle := TCoordConverterMercatorOnSphere.Create(VRadiusA, VProj4Info);

  VRadiusA := 6371000;
  FEPSG53004 := TCoordConverterMercatorOnSphere.Create(VRadiusA, VProj4Info);

  VRadiusA := 6378137;
  VRadiusB := 6356752;
  FYandex := TCoordConverterMercatorOnEllipsoid.Create(VRadiusA, VRadiusB, VProj4Info);

  VRadiusA := 6378137;
  VRadiusB := 6356752;
  FLonLat := TCoordConverterSimpleLonLat.Create(VRadiusA, VRadiusB, VProj4Info);
end;

function TCoordConverterFactorySimple.GetByConverterAndZoom(
  const AGeoConverter: ICoordConverter;
  AZoom: Byte
): IProjectionInfo;
begin
  Result := TProjectionInfo.Create(AGeoConverter, AZoom);
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
  VProj4Info: TProj4Info;
begin
  Result := nil;
  VTileSplitCode := CTileSplitQuadrate256x256;
  VEPSG := 0;
  VProjection := 1;
  VRadiusA := 6378137;
  VRadiusB := VRadiusA;
  Proj4InfoClear(VProj4Info);
  
  if AConfig <> nil then begin
    VEPSG := AConfig.ReadInteger('EPSG', VEPSG);
    VProjection := AConfig.ReadInteger('projection', VProjection);
    VRadiusA := AConfig.ReadFloat('sradiusa', VRadiusA);
    VRadiusB := AConfig.ReadFloat('sradiusb', VRadiusA);
    // read proj4 params
    VProj4Info.Proj4Args := AConfig.ReadString('Proj4Args', '');
    VProj4Info.Proj4Path := AConfig.ReadString('Proj4Path', '');
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
      // if no proj4 - try to use predefined
      if Proj4InfoIsEmpty(VProj4Info) then
        Result := GetCoordConverterByCode(VEPSG, VTileSplitCode);
    except
      Result := nil;
    end;
  end;

  if Result = nil then begin
    case VProjection of
      1: begin
        Result := TCoordConverterMercatorOnSphere.Create(VRadiusA, VProj4Info);
      end;
      2: begin
        Result := TCoordConverterMercatorOnEllipsoid.Create(VRadiusA, VRadiusB, VProj4Info);
      end;
      3: begin
        Result := TCoordConverterSimpleLonLat.Create(VRadiusA, VRadiusB, VProj4Info);
      end;
    else begin
      raise Exception.CreateFmt(SAS_ERR_MapProjectionUnexpectedType, [IntToStr(VProjection)]);
    end;
    end;
  end;
end;

end.
