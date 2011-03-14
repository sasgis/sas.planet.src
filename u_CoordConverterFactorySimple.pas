unit u_CoordConverterFactorySimple;

interface

uses
  i_ICoordConverter,
  i_IConfigDataProvider,
  i_ICoordConverterFactory;

type
  TCoordConverterFactorySimple = class(TInterfacedObject, ICoordConverterFactory)
  protected
    function GetCoordConverterByConfig(AConfig: IConfigDataProvider): ICoordConverter;
    function GetCoordConverterByCode(AProjectionEPSG: Integer; ATileSplitCode: Integer): ICoordConverter;
  end;

implementation

uses
  SysUtils,
  c_CoordConverter,
  u_CoordConverterMercatorOnSphere,
  u_CoordConverterMercatorOnEllipsoid,
  u_CoordConverterSimpleLonLat,
  UResStrings;

{ TCoordConverterFactorySimple }

function TCoordConverterFactorySimple.GetCoordConverterByCode(AProjectionEPSG,
  ATileSplitCode: Integer): ICoordConverter;
var
  VRadiusA: Double;
  VRadiusB: Double;
begin
  if ATileSplitCode = CTileSplitQuadrate256x256 then begin
    case AProjectionEPSG of
      CGoogleProjectionEPSG: begin
        VRadiusA := 6378137;
        Result := TCoordConverterMercatorOnSphere.Create(VRadiusA);
      end;
      53004: begin
        VRadiusA := 6371000;
        Result := TCoordConverterMercatorOnSphere.Create(VRadiusA);
      end;
      CYandexProjectionEPSG: begin
        VRadiusA := 6378137;
        VRadiusB := 6356752;
        Result := TCoordConverterMercatorOnEllipsoid.Create(VRadiusA, VRadiusB);
      end;
      CGELonLatProjectionEPSG: begin
        VRadiusA := 6378137;
        VRadiusB := 6356752;
        Result := TCoordConverterSimpleLonLat.Create(VRadiusA, VRadiusB);
      end;
      else
        raise Exception.CreateFmt(SAS_ERR_MapProjectionUnexpectedType, [IntToStr(AProjectionEPSG)]);
    end;
  end else begin
    raise Exception.Create('Неизвестный тип разделения карты на тайлы');
  end;
end;

function TCoordConverterFactorySimple.GetCoordConverterByConfig(
  AConfig: IConfigDataProvider): ICoordConverter;
var
  VProjection: byte;
  VRadiusA: Double;
  VRadiusB: Double;
  VEPSG: Integer;
  VTileSplitCode: Integer;
begin
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
  if VEPSG <> 0 then begin
    try
      Result := GetCoordConverterByCode(VEPSG, VTileSplitCode);
    except
      Result := nil;
    end;
  end;
  if Result = nil then begin
    case VProjection of
      1: Result := TCoordConverterMercatorOnSphere.Create(VRadiusA);
      2: Result := TCoordConverterMercatorOnEllipsoid.Create(VRadiusA, VRadiusB);
      3: Result := TCoordConverterSimpleLonLat.Create(VRadiusA, VRadiusB);
      else
        raise Exception.CreateFmt(SAS_ERR_MapProjectionUnexpectedType, [IntToStr(VProjection)]);
    end;
  end;
end;

end.
