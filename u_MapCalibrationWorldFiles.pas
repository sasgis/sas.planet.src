unit u_MapCalibrationWorldFiles;

interface

uses
  Types,
  i_CoordConverter,
  i_IMapCalibration;

type
  TMapCalibrationWorldFiles = class(TInterfacedObject, IMapCalibration)
  private
    procedure SavePrjFile(AFileName: WideString; AConverter: ICoordConverter);
    procedure SaveAuxXmlFile(AFileName: WideString; AConverter: ICoordConverter);
  public
    // Имя для вывода в листбоксе для выбора при экспорте.
    function GetName: WideString; safecall;
    // Более детальное описание привязки
    function GetDescription: WideString; safecall;
    // Генерирует привязку для склеенной карты.
    procedure SaveCalibrationInfo(AFileName: WideString; xy1, xy2: TPoint; Azoom: byte; AConverter: ICoordConverter); safecall;
  end;

implementation

uses
  Classes,
  SysUtils,
  Ugeofun,
  t_GeoTypes,
  u_GeoToStr;

{ TMapCalibrationWorldFiles }

function TMapCalibrationWorldFiles.GetDescription: WideString;
begin
  Result := 'Привязка при помощи World файла и файлов с описанием проекции';
end;

function TMapCalibrationWorldFiles.GetName: WideString;
begin
  Result := '.w';
end;

procedure TMapCalibrationWorldFiles.SaveAuxXmlFile(AFileName: WideString;
  AConverter: ICoordConverter);
var
  AuxXmkfile: TMemoryStream;
  str: UTF8String;
  VprojInfo: String;
begin
  AuxXmkfile := TMemoryStream.create;
  str := AnsiToUtf8('<PAMDataset>' + #13#10 + '<SRS>');
  VprojInfo := GetProj(AConverter);
  str := str + AnsiToUtf8(VprojInfo);
  str := str + AnsiToUtf8('</SRS>' + #13#10 + '<Metadata>' + #13#10 + '<MDI key="PyramidResamplingType">NEAREST</MDI>' + #13#10 + '</Metadata>' + #13#10 + '</PAMDataset>');
  AuxXmkfile.Write(str[1], length(str));
  AuxXmkfile.SaveToFile(AFileName + '.aux.xml');
  AuxXmkfile.Free;
end;

procedure TMapCalibrationWorldFiles.SaveCalibrationInfo(
  AFileName: WideString; xy1, xy2: TPoint; Azoom: byte;
  AConverter: ICoordConverter);
var
  f: TextFile;
  ll1, ll2: TDoublePoint;
  CellX, CellY, OrigX, OrigY: Double;
begin
  ll1 := AConverter.PixelPos2LonLat(xy1, Azoom);
  ll2 := AConverter.PixelPos2LonLat(xy2, Azoom);
  CalculateWFileParams(ll1, ll2, xy2.X - xy1.X, xy2.Y - xy1.Y, AConverter, CellX, CellY, OrigX, OrigY);
  assignfile(f, AFileName + 'w');
  rewrite(f);
  writeln(f, R2StrPoint(CellX));
  writeln(f, '0');
  writeln(f, '0');
  writeln(f, R2StrPoint(CellY));
  writeln(f, R2StrPoint(OrigX));
  writeln(f, R2StrPoint(OrigY));
  closefile(f);
  SavePrjFile(AFileName, AConverter);
  SaveAuxXmlFile(AFileName, AConverter);
end;

procedure TMapCalibrationWorldFiles.SavePrjFile(AFileName: WideString;
  AConverter: ICoordConverter);
var
  f: TextFile;
  VprojInfo: String;
begin
  assignfile(f, ChangeFileExt(AFileName, '.prj'));
  rewrite(f);
  VprojInfo := GetProj(AConverter);
  writeln(f, VprojInfo);
  closefile(f);
end;

end.
