{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit u_ImportJpegWithExif;

interface

uses
  Classes,
  i_InterfaceListStatic,
  i_VectorDataFactory,
  i_ValueToStringConverter,
  i_ImportConfig,
  i_ImportFile,
  i_MarkSystem,
  u_BaseInterfacedObject;

type
  TImportJpegWithExif = class(TBaseInterfacedObject, IImportFile)
  private
    FVectorDataFactory: IVectorDataFactory;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
  private
    function ProcessImport(
      const AMarksSystem: IMarkSystem;
      const AFileName: string;
      const AConfig: IImportConfig
    ): IInterfaceListStatic;
  public
    constructor Create(
      const AVectorDataFactory: IVectorDataFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  CCR.Exif,
  SysUtils,
  t_GeoTypes,
  i_VectorItemSubset,
  i_VectorDataItemSimple,
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  CCR.Exif.IPTC,
  u_VectorDataItemSubset;

{ TImportJpegWithExif }

constructor TImportJpegWithExif.Create(
  const AVectorDataFactory: IVectorDataFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create;
  FVectorDataFactory := AVectorDataFactory;
  FValueToStringConverterConfig := AValueToStringConverterConfig
end;

function TImportJpegWithExif.ProcessImport(
  const AMarksSystem: IMarkSystem;
  const AFileName: string;
  const AConfig: IImportConfig
): IInterfaceListStatic;
var
  VPoint: TDoublePoint;
  VExifData: TExifData;
  VGPSLatitude: TGPSLatitude;
  VGPSLongitude: TGPSLongitude;
  VGPSAltitude: TExifFraction;
  VDesc: string;
  VItem: IVectorDataItemSimple;
  VValueToStringConverter: IValueToStringConverter;
  VAltitude: string;
  VExAltitude: Extended;
  VTmpStr: string;
  i: Integer;
  VTitle: string;
  Vkeys: TStrings;
  VIPTCData: TIPTCData;
  VList: IInterfaceListSimple;
  VVectorData: IVectorItemSubset;
  VFormattedDateTime : string;
begin
  Result := nil;
  if not FileExists(AFileName) then Exit;
  VDEsc := '';
  VValueToStringConverter := FValueToStringConverterConfig.GetStatic;
  VExifData := TExifData.Create;
  try
    VExifData.LoadFromGraphic(AFileName);
    if VExifData.Empty then Exit;

    VGPSLatitude := VExifData.GPSLatitude;
    if VGPSLatitude.Degrees.MissingOrInvalid then Exit;
    VPoint.Y := VGPSLatitude.Degrees.Quotient;
    VPoint.Y := VPoint.Y + VGPSLatitude.Minutes.Quotient/60;
    VPoint.Y := VPoint.Y + VGPSLatitude.Seconds.Quotient/3600;
    if VGPSLatitude.Direction = ltSouth then VPoint.Y := -VPoint.Y;

    VGPSLongitude := VExifData.GPSLongitude;
    if VGPSLongitude.Degrees.MissingOrInvalid then Exit;
    VPoint.X := VGPSLongitude.Degrees.Quotient;
    VPoint.X := VPoint.X + VGPSLongitude.Minutes.Quotient/60;
    VPoint.X := VPoint.X + VGPSLongitude.Seconds.Quotient/3600;
    if VGPSLongitude.Direction = lnWest then VPoint.X := -VPoint.X;

    VGPSAltitude := VExifData.GPSAltitude;
    if VGPSAltitude.MissingOrInvalid then VAltitude := '' else
    begin
      VExAltitude := VGPSAltitude.Quotient;
      if VExifData.GPSAltitudeRef = alBelowSeaLevel then VExAltitude := -VExAltitude ;
      VAltitude := FloatToStrF(VExAltitude, ffFixed, 10, 2);
    end;

    VIPTCData := TIPTCData.Create;
    try
      VIPTCData.LoadFromGraphic(AFileName);
      Vkeys := TStringList.Create;
      try
        VIPTCData.GetKeyWords(Vkeys);
        VTmpStr := '';
        if Assigned(Vkeys) and (Vkeys.Count > 0) then begin
          for i := 0 to Vkeys.Count - 1 do begin
           if Vkeys.Strings[i]<>'' then
             VTmpStr := VTmpStr + Vkeys.Strings[i] + '; ';
          end;
          VDEsc := VDEsc + 'Tags: '+ VTmpStr + '<br>' + #$0D#$0A;
        end;
        VTmpStr := VIPTCData.CountryName + ', ' + VIPTCData.ProvinceOrState + ', ' + VIPTCData.City + ', ' +
          VIPTCData.SubLocation + '<br>' + #$0D#$0A;
        //Esli sodergit nekoe kolichestvo razumnoi informacii
        if Length(VTmpStr)>20 then VDEsc := VDEsc + 'Location: '+ VTmpStr + '<br>' + #$0D#$0A;
        VTmpStr := '';
      finally
        VKeys.Free;
      end;
    finally
      VIPTCData.Free;
    end;

    VDEsc := VDEsc + 'Coordinates: [ '+VValueToStringConverter.LonLatConvert(VPoint)+' ]<br>' + #$0D#$0A;
    if VAltitude <> '' then
      VDEsc := VDEsc + 'Elevation: ' + VAltitude + '<br>' + #$0D#$0A;
    if not VExifData.GPSVersion.MissingOrInvalid then
      VDEsc := VDEsc + 'GPS Version:' + VExifData.GPSVersion.AsString + '<br>' + #$0D#$0A;
    if VExifData.CameraMake <> '' then
      VDEsc := VDEsc + 'Camera: '+ VExifData.CameraMake + ' '+VExifData.CameraModel + '<br>' + #$0D#$0A;
    if not VExifData.DateTimeOriginal.MissingOrInvalid then begin
      VDEsc := VDEsc + 'Date: '+ VExifData.DateTimeOriginal.AsString + '<br>' + #$0D#$0A;
    end else if not VExifData.DateTime.MissingOrInvalid then begin
      VDEsc := VDEsc + 'Date: '+ VExifData.DateTime.AsString + '<br>' + #$0D#$0A;
    end;

    if VExifData.Keywords<>'' then
      VDEsc := VDEsc + 'Windows Tags: '+ VExifData.Keywords + '<br>' + #$0D#$0A;
    if VExifData.Author<>'' then
      VDEsc := VDEsc + 'Author: '+ VExifData.Author + '<br>' + #$0D#$0A;

    VDEsc := VDEsc + '<img width=600 src="' + AFileName + '">';

    VTitle := ExtractFileName(AFileName);
    if not VExifData.DateTimeOriginal.MissingOrInvalid then begin
      DateTimeToString(VFormattedDateTime, 'yyyy.mm.dd-hh:mm:ss', VExifData.DateTimeOriginal.Value);
      VTitle := VFormattedDateTime;
    end else if not VExifData.DateTime.MissingOrInvalid then begin
      DateTimeToString(VFormattedDateTime, 'yyyy.mm.dd-hh:mm:ss', VExifData.DateTime.Value);
      VTitle := VFormattedDateTime;
    end;

  finally
    VExifData.Free;
  end;

  VItem := FVectorDataFactory.BuildPoint(
    nil,
    VTitle,
    VDesc,
    VPoint
  );

  if VItem <> nil then begin
    VList := TInterfaceListSimple.Create;
    VList.Add(VItem);
    VVectorData := TVectorItemSubset.Create(VList.MakeStaticAndClear);
    Result := AMarksSystem.ImportItemsList(VVectorData, AConfig, ExtractFileName(AFileName));
  end;
end;

end.
