{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_TileStorageImporterFunc;

interface

uses
  i_ConfigDataProvider,
  i_ArchiveReadWriteFactory,
  t_TileStorageImporter;

function MakeZmpMapConfig(
  const AGuid: string;
  const AFileInfo: TTileStorageImporterFileInfo;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
): IConfigDataProvider;

implementation

uses
  Classes,
  SysUtils,
  Math,
  i_ArchiveReadWrite,
  i_BinaryData,
  u_BinaryData,
  u_StrFunc,
  u_GeoToStrFunc,
  u_ConfigDataProviderByZip;

function GetMapCenterStr(const AGotoInfo: TTileStorageImporterGotoInfo): string;
begin
  if AGotoInfo.Status = gtsLonLat then begin
    Result :=
      RoundEx(AGotoInfo.LonLat.X, 6) + ',' +
      RoundEx(AGotoInfo.LonLat.Y, 6) + ',' +
      IntToStr(AGotoInfo.Zoom);
  end else begin
    Result := '';
  end;
end;

function MakeZmpMapConfig(
  const AGuid: string;
  const AFileInfo: TTileStorageImporterFileInfo;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
): IConfigDataProvider;
const
  CParamsTxtFmt =
    '[PARAMS]'         + #13#10 +
    'GUID=%s'          + #13#10 +
    'asLayer=%s'       + #13#10 +
    'Name=%s'          + #13#10 +
    'ParentSubMenu=%s' + #13#10 +
    'NameInCache=%s'   + #13#10 +
    'ContentType=%s'   + #13#10 +
    'Ext=%s'           + #13#10 +
    'Epsg=%d'          + #13#10 +
    'CacheType=%d'     + #13#10 +
    'UseDwn=0'         + #13#10 +
    'MapCenter=%s'     + #13#10 +
    'IsReadOnly=1'     + #13#10;
var
  VBytes: TBytes;
  VParamsTxt: string;
  VBinaryData: IBinaryData;
  VZipStream: TStream;
  VZip: IArchiveType;
  VZipWriter: IArchiveWriter;
  VZipReader: IArchiveReader;
begin
  VZip := AArchiveReadWriteFactory.Zip;

  VZipStream := TMemoryStream.Create;
  try
    VParamsTxt := Format(CParamsTxtFmt, [AGuid, AFileInfo.FIsLayer.ToString,
      AFileInfo.FMapName, AFileInfo.FParentSubMenu, AFileInfo.FNameInCache,
      AFileInfo.FContentType, AFileInfo.FExt, AFileInfo.FProjectionEpsg,
      AFileInfo.FCacheTypeCode, GetMapCenterStr(AFileInfo.FGotoInfo)]
    );

    VBytes := StringToUtf8WithBOM(VParamsTxt);
    VBinaryData := TBinaryData.Create(Length(VBytes), VBytes);

    VZipWriter :=  VZip.WriterFactory.BuildByStream(VZipStream);
    VZipWriter.AddFile(VBinaryData, 'params.txt', Now);
    VZipWriter := nil;

    VZipStream.Position := 0;
    VZipReader := VZip.ReaderFactory.BuildByStreamWithOwn(VZipStream);
    Result := TConfigDataProviderByArchive.Create('', VZipReader);

    VZipStream := nil;
  finally
    VZipStream.Free;
  end;
end;

end.
