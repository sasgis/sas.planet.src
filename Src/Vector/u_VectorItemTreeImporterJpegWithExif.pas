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
  i_NotifierOperation,
  i_VectorItemSubsetBuilder,
  i_VectorDataFactory,
  i_ValueToStringConverter,
  i_VectorItemTreeImporter,
  i_GeometryLonLatFactory,
  i_VectorItemTree,
  i_PathConfig,
  i_HashFunction,
  i_ContentTypeManager,
  i_Appearance,
  i_AppearanceOfVectorItem,
  i_AppearanceOfMarkFactory,
  u_BaseInterfacedObject;

type
  TVectorItemTreeImporterJpegWithExif = class(TBaseInterfacedObject, IVectorItemTreeImporter)
  private
    FHashFunction: IHashFunction;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FVectorDataFactory: IVectorDataFactory;
    FAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    FMediaDataPath: IPathConfig;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FContentTypeManager: IContentTypeManager;
  private
    function ProcessImport(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      var AConfig: IInterface
    ): IVectorItemTree;
  public
    constructor Create(
      const AHashFunction: IHashFunction;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMediaDataPath: IPathConfig;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AContentTypeManager: IContentTypeManager
    );
  end;

implementation

uses
  Math,
  SysUtils,
  StrUtils,
  Jpeg,
  CCR.Exif,
  CCR.Exif.IPTC,
  t_GeoTypes,
  c_InternalBrowser,
  i_MarkPicture,
  i_VectorItemSubset,
  i_VectorDataItemSimple,
  i_ContentTypeInfo,
  i_BitmapTileSaveLoad,
  i_ImportConfig,
  i_JpegWithExifImportConfig,
  u_MarkPictureSimple,
  u_VectorItemTree,
  u_GeoFunc;

const
  cThumbnailFolderName = '.thumbnail';

{ TVectorItemTreeImporterJpegWithExif }

constructor TVectorItemTreeImporterJpegWithExif.Create(
  const AHashFunction: IHashFunction;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMediaDataPath: IPathConfig;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AContentTypeManager: IContentTypeManager
);
begin
  inherited Create;
  FHashFunction := AHashFunction;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FVectorDataFactory := AVectorDataFactory;
  FAppearanceOfMarkFactory := AAppearanceOfMarkFactory;
  FMediaDataPath := AMediaDataPath;
  FValueToStringConverter := AValueToStringConverter;
  FContentTypeManager := AContentTypeManager;
end;

function TVectorItemTreeImporterJpegWithExif.ProcessImport(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  var AConfig: IInterface
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
          if Result <> '' then begin
            Result := Result + AComma;
          end;
          Result := Result + AList.Strings[I];
        end;
      end;
    end;
  end;

  function _GetInternalFileName(const AFullFileName: string): string;
  begin
    if StartsText(FMediaDataPath.FullPath, AFullFileName) then begin
      Result := StringReplace(
        AFullFileName,
        IncludeTrailingPathDelimiter(FMediaDataPath.FullPath),
        CMediaDataInternalURL,
        [rfIgnoreCase]
      );
      Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
    end else begin
      Result := AFullFileName;
    end;
  end;

  function _GetLoaderByExt(const AExt: string): IBitmapTileLoader;
  var
    VContentType: IContentTypeInfoBasic;
    VContentTypeBitmap: IContentTypeInfoBitmap;
  begin
    VContentType := FContentTypeManager.GetInfoByExt(AExt);
    if VContentType <> nil then begin
      if Supports(VContentType, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
        Result := VContentTypeBitmap.GetLoader;
      end;
    end;
  end;

var
  VDesc: string;
  VTmpStr: string;
  VImgName: string;
  VPicFullName: string;
  VPicShortName: string;
  VAltitude: string;
  VThumbnailPath: string;
  VResLimit: string;
  VExAltitude: Extended;
  VKeys: TStrings;
  VPoint: TDoublePoint;
  VJpeg: TJPEGImageEx;
  VThumbnail: TJPEGImage;
  VExifData: TExifData;
  VIPTCData: TIPTCData;
  VGPSLatitude: TGPSLatitude;
  VGPSLongitude: TGPSLongitude;
  VGPSAltitude: TExifFraction;
  VItem: IVectorDataItem;
  VList: IVectorItemSubsetBuilder;
  VVectorData: IVectorItemSubset;
  VValueToStringConverter: IValueToStringConverter;
  VConfig: IJpegWithExifImportConfig;
  VIcon: IAppearancePointIcon;
  VCaption: IAppearancePointCaption;
  VPointParams: IImportPointParams;
  VAppearance: IAppearance;
  VMark: IMarkPicture;
begin
  Assert(FileExists(AFileName));

  Result := nil;

  VValueToStringConverter := FValueToStringConverter.GetStatic;

  if not Supports(AConfig, IJpegWithExifImportConfig, VConfig) then begin
    VConfig := nil;
  end;

  VDesc := '';
  VResLimit := '600';
  VPoint := CEmptyDoublePoint;

  VJpeg := TJPEGImageEx.Create;
  try
    VJpeg.LoadFromFile(AFileName);

    if VJpeg.Empty then begin
      Exit;
    end;

    VExifData := VJpeg.ExifData;

    if VExifData.Empty then begin
      Exit;
    end;

    VGPSLatitude := VExifData.GPSLatitude;
    if VGPSLatitude.Degrees.MissingOrInvalid then begin
      Exit;
    end;
    VPoint.Y := VGPSLatitude.Degrees.Quotient;
    VPoint.Y := VPoint.Y + VGPSLatitude.Minutes.Quotient / 60;
    VPoint.Y := VPoint.Y + VGPSLatitude.Seconds.Quotient / 3600;
    if VGPSLatitude.Direction = ltSouth then begin
      VPoint.Y := -VPoint.Y;
    end;

    VGPSLongitude := VExifData.GPSLongitude;
    if VGPSLongitude.Degrees.MissingOrInvalid then begin
      Exit;
    end;
    VPoint.X := VGPSLongitude.Degrees.Quotient;
    VPoint.X := VPoint.X + VGPSLongitude.Minutes.Quotient / 60;
    VPoint.X := VPoint.X + VGPSLongitude.Seconds.Quotient / 3600;
    if VGPSLongitude.Direction = lnWest then begin
      VPoint.X := -VPoint.X;
    end;

    if PointIsEmpty(VPoint) then begin
      Exit;
    end;

    VGPSAltitude := VExifData.GPSAltitude;
    if VGPSAltitude.MissingOrInvalid or (VGPSAltitude.Quotient = 0) then begin
      VAltitude := '';
    end else begin
      VExAltitude := VGPSAltitude.Quotient;
      if VExifData.GPSAltitudeRef = alBelowSeaLevel then begin
        VExAltitude := -VExAltitude;
      end;
      VAltitude := FloatToStrF(VExAltitude, ffFixed, 10, 2);
    end;

    VIPTCData := VJpeg.IPTCData;
    if not VIPTCData.Empty then begin
      VKeys := TStringList.Create;
      try
        VIPTCData.GetKeyWords(VKeys);

        VTmpStr := _ListToString(VKeys, '; ');
        if VTmpStr <> '' then begin
          VDesc := VDesc + 'Tags: ' + VTmpStr + br;
        end;

        VKeys.Clear;

        VKeys.Add(VIPTCData.CountryName);
        VKeys.Add(VIPTCData.ProvinceOrState);
        VKeys.Add(VIPTCData.City);
        VKeys.Add(VIPTCData.SubLocation);

        VTmpStr := _ListToString(VKeys, ', ');
        if VTmpStr <> '' then begin
          VDesc := VDesc + 'Location: ' + VTmpStr + br + br;
        end;
      finally
        VKeys.Free;
      end;
    end;

    VDesc := VDesc + 'GPS Coordinates: [ ' + VValueToStringConverter.LonLatConvert(VPoint) + ' ]' + br;

    if VAltitude <> '' then begin
      VDesc := VDesc + 'GPS Elevation: ' + VAltitude + br;
    end;

    if not VExifData.GPSVersion.MissingOrInvalid then begin
      VDesc := VDesc + 'GPS Version:' + VExifData.GPSVersion.AsString + br;
    end;

    if VExifData.CameraMake <> '' then begin
      VDesc := VDesc + 'Camera: ' + VExifData.CameraMake + ' ' + VExifData.CameraModel + br;
    end;

    if not VExifData.DateTimeOriginal.MissingOrInvalid then begin
      VDesc := VDesc + 'Date: ' + VExifData.DateTimeOriginal.AsString + br;
    end else if not VExifData.DateTime.MissingOrInvalid then begin
      VDesc := VDesc + 'Date: ' + VExifData.DateTime.AsString + br;
    end;

    if VExifData.Keywords <> '' then begin
      VDesc := VDesc + 'Windows Tags: ' + VExifData.Keywords + br;
    end;

    if VExifData.Author <> '' then begin
      VDesc := VDesc + 'Author: ' + VExifData.Author + br;
    end;

    VImgName := _GetInternalFileName(AFileName);

    if Assigned(VConfig) and VConfig.UseThumbnailAsIcon then begin

      VResLimit := IntToStr(VConfig.ResolutionLimit);

      VPointParams := VConfig.PointParams;

      if Assigned(VPointParams) then begin
        VCaption := VPointParams.CaptionAppearance;
        VIcon := VPointParams.IconAppearance;
      end;

      if Assigned(VCaption) and Assigned(VIcon) then begin
        VThumbnail := VExifData.Thumbnail;
        if not Assigned(VThumbnail) or VThumbnail.Empty then begin
          VJpeg.CreateThumbnail;
          VThumbnail := VExifData.Thumbnail;
        end;
        if Assigned(VThumbnail) and not VThumbnail.Empty then begin
          VThumbnailPath :=
            IncludeTrailingPathDelimiter(ExtractFilePath(AFileName)) +
            cThumbnailFolderName + PathDelim;

          if not DirectoryExists(VThumbnailPath) then begin
            if not ForceDirectories(VThumbnailPath) then begin
              RaiseLastOSError;
            end;
          end;

          VPicFullName := VThumbnailPath + ExtractFileName(AFileName);

          VThumbnail.SaveToFile(VPicFullName);

          VPicShortName := _GetInternalFileName(VPicFullName);

          VMark := TMarkPictureSimple.Create(
            FHashFunction.CalcHashByString(VPicFullName),
            VPicFullName,
            VPicShortName,
            _GetLoaderByExt(ExtractFileExt(AFileName))
          );

          VAppearance :=
            FAppearanceOfMarkFactory.CreatePointAppearance(
              VCaption.TextColor,
              VCaption.TextBgColor,
              VCaption.FontSize,
              VPicShortName,
              VMark,
              VIcon.MarkerSize
            );
        end else begin
          Assert(False, 'Can''t create Thumbnail for image: ' + AFileName);
        end;
      end else begin
        Assert(False, 'Appearance not assigned!');
      end;
    end;

    if VJpeg.Width < VJpeg.Height then begin
      VTmpStr := 'height';
    end else begin
      VTmpStr := 'width';
    end;

    VDesc := VDesc + '<img ' + VTmpStr + '=' + VResLimit + ' src="' + VImgName + '">';
  finally
    VJpeg.Free;
  end;

  VItem := FVectorDataFactory.BuildItem(
    FVectorDataItemMainInfoFactory.BuildMainInfo(nil, ExtractFileName(AFileName), VDesc),
    VAppearance,
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
