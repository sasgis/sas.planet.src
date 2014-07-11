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

unit u_VectorItemTreeImporterJpegWithExif;

interface

uses
  Classes,
  i_VectorItemSubsetBuilder,
  i_VectorDataFactory,
  i_ValueToStringConverter,
  i_VectorItemTreeImporter,
  i_GeometryLonLatFactory,
  i_VectorItemTree,
  i_PathConfig,
  u_BaseInterfacedObject;

type
  TVectorItemTreeImporterJpegWithExif = class(TBaseInterfacedObject, IVectorItemTreeImporter)
  private
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FVectorDataFactory: IVectorDataFactory;
    FMediaDataPath: IPathConfig;
    FValueToStringConverter: IValueToStringConverterChangeable;
  private
    function ProcessImport(
      const AFileName: string
    ): IVectorItemTree;
  public
    constructor Create(
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AMediaDataPath: IPathConfig;
      const AValueToStringConverter: IValueToStringConverterChangeable
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  CCR.Exif,
  CCR.Exif.IPTC,
  t_GeoTypes,
  c_InternalBrowser,
  i_VectorItemSubset,
  i_VectorDataItemSimple,
  u_VectorItemTree,
  u_GeoFunc;

{ TVectorItemTreeImporterJpegWithExif }

constructor TVectorItemTreeImporterJpegWithExif.Create(
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AMediaDataPath: IPathConfig;
  const AValueToStringConverter: IValueToStringConverterChangeable
);
begin
  inherited Create;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FVectorDataFactory := AVectorDataFactory;
  FMediaDataPath := AMediaDataPath;
  FValueToStringConverter := AValueToStringConverter
end;

function TVectorItemTreeImporterJpegWithExif.ProcessImport(
  const AFileName: string
): IVectorItemTree;
const
  br = '<br>' + #$0D#$0A;

  function _ListToString(const AList: TStrings; const AComma: string): string;
  var
    I: Integer;
  begin
    Result := '';
    if Assigned(AList) and (AList.Count > 0) then begin
      for I := 0 to AList.Count - 1 do begin
        if AList.Strings[I] <> '' then begin
          if Result <> '' then Result := Result + AComma;
          Result := Result + AList.Strings[I];
        end;
      end;
    end;
  end;

var
  VDesc: string;
  VTmpStr: string;
  VImgName: string;
  VAltitude: string;
  VExAltitude: Extended;
  VKeys: TStrings;
  VPoint: TDoublePoint;
  VJpeg: TJPEGImageEx;
  VExifData: TExifData;
  VIPTCData: TIPTCData;
  VGPSLatitude: TGPSLatitude;
  VGPSLongitude: TGPSLongitude;
  VGPSAltitude: TExifFraction;
  VItem: IVectorDataItem;
  VList: IVectorItemSubsetBuilder;
  VVectorData: IVectorItemSubset;
  VValueToStringConverter: IValueToStringConverter;
begin
  Assert(FileExists(AFileName));

  Result := nil;

  VValueToStringConverter := FValueToStringConverter.GetStatic;

  VDesc := '';
  VPoint := CEmptyDoublePoint;

  VJpeg := TJPEGImageEx.Create;
  try
    VJpeg.LoadFromFile(AFileName);

    if VJpeg.Empty then Exit;    

    VExifData := VJpeg.ExifData;

    if VExifData.Empty then Exit;

    VGPSLatitude := VExifData.GPSLatitude;
    if VGPSLatitude.Degrees.MissingOrInvalid then Exit;
    VPoint.Y := VGPSLatitude.Degrees.Quotient;
    VPoint.Y := VPoint.Y + VGPSLatitude.Minutes.Quotient / 60;
    VPoint.Y := VPoint.Y + VGPSLatitude.Seconds.Quotient / 3600;
    if VGPSLatitude.Direction = ltSouth then VPoint.Y := -VPoint.Y;

    VGPSLongitude := VExifData.GPSLongitude;
    if VGPSLongitude.Degrees.MissingOrInvalid then Exit;
    VPoint.X := VGPSLongitude.Degrees.Quotient;
    VPoint.X := VPoint.X + VGPSLongitude.Minutes.Quotient / 60;
    VPoint.X := VPoint.X + VGPSLongitude.Seconds.Quotient / 3600;
    if VGPSLongitude.Direction = lnWest then VPoint.X := -VPoint.X;

    if PointIsEmpty(VPoint) then Exit;

    VGPSAltitude := VExifData.GPSAltitude;
    if VGPSAltitude.MissingOrInvalid or (VGPSAltitude.Quotient = 0) then
      VAltitude := ''
    else begin
      VExAltitude := VGPSAltitude.Quotient;
      if VExifData.GPSAltitudeRef = alBelowSeaLevel then
        VExAltitude := -VExAltitude ;
      VAltitude := FloatToStrF(VExAltitude, ffFixed, 10, 2);
    end;

    VIPTCData := VJpeg.IPTCData;
    if not VIPTCData.Empty then begin
      VKeys := TStringList.Create;
      try
        VIPTCData.GetKeyWords(VKeys);

        VTmpStr := _ListToString(VKeys, '; ');
        if VTmpStr <> '' then
          VDesc := VDesc + 'Tags: '+ VTmpStr + br;

        VKeys.Clear;

        VKeys.Add(VIPTCData.CountryName);
        VKeys.Add(VIPTCData.ProvinceOrState);
        VKeys.Add(VIPTCData.City);
        VKeys.Add(VIPTCData.SubLocation);

        VTmpStr := _ListToString(VKeys, ', ');
        if VTmpStr <> '' then
          VDesc := VDesc + 'Location: '+ VTmpStr + br + br;
      finally
        VKeys.Free;
      end;
    end;

    VDesc := VDesc + 'GPS Coordinates: [ '+VValueToStringConverter.LonLatConvert(VPoint)+' ]' + br;

    if VAltitude <> '' then
      VDesc := VDesc + 'GPS Elevation: ' + VAltitude + br;

    if not VExifData.GPSVersion.MissingOrInvalid then
      VDesc := VDesc + 'GPS Version:' + VExifData.GPSVersion.AsString + br;

    if VExifData.CameraMake <> '' then
      VDesc := VDesc + 'Camera: '+ VExifData.CameraMake + ' '+VExifData.CameraModel + br;

    if not VExifData.DateTimeOriginal.MissingOrInvalid then begin
      VDesc := VDesc + 'Date: '+ VExifData.DateTimeOriginal.AsString + br;
    end else if not VExifData.DateTime.MissingOrInvalid then begin
      VDesc := VDesc + 'Date: '+ VExifData.DateTime.AsString + br;
    end;

    if VExifData.Keywords <> '' then
      VDesc := VDesc + 'Windows Tags: '+ VExifData.Keywords + br;

    if VExifData.Author <> '' then
      VDesc := VDesc + 'Author: '+ VExifData.Author + br;

    if StartsText(FMediaDataPath.FullPath, AFileName) then begin
      VImgName := StringReplace(
        AFileName,
        IncludeTrailingPathDelimiter(FMediaDataPath.FullPath),
        CMediaDataInternalURL,
        [rfIgnoreCase]
      );
      VImgName := StringReplace(VImgName, '\', '/', [rfReplaceAll]);
    end else begin
      VImgName := AFileName;
    end;

    if VJpeg.Width < VJpeg.Height then begin
      VTmpStr := 'height';
    end else begin
      VTmpStr := 'width';
    end;

    VDesc := VDesc + '<img ' + VTmpStr + '=600 src="' + VImgName + '">';
  finally
    VJpeg.Free;
  end;

  VItem := FVectorDataFactory.BuildItem(
    FVectorDataItemMainInfoFactory.BuildMainInfo(nil, ExtractFileName(AFileName), VDesc),
    nil,
    FVectorGeometryLonLatFactory.CreateLonLatPoint(VPoint)
  );

  if VItem <> nil then begin
    VList := FVectorItemSubsetBuilderFactory.Build;
    VList.Add(VItem);
    VVectorData := VList.MakeStaticAndClear;
    Result := TVectorItemTree.Create('', VVectorData, nil);
  end;
end;

end.
