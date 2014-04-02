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

unit u_ContentConverterKmz2Kml;

interface

uses
  Classes,
  i_BinaryData,
  i_ContentTypeInfo,
  i_ArchiveReadWriteFactory,
  u_ContentConverterBase;

type
  TContentConverterKmz2Kml = class(TContentConverterBase)
  private
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
  protected
    function Convert(const AData: IBinaryData): IBinaryData; override;
  public
    constructor Create(
      const ASource: IContentTypeInfoBasic;
      const ATarget: IContentTypeInfoBasic;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory
    );
  end;

implementation

uses
  i_ArchiveReadWrite,
  u_StreamReadOnlyByBinaryData;

{ TContentConverterKmz2Kml }

constructor TContentConverterKmz2Kml.Create(
  const ASource: IContentTypeInfoBasic;
  const ATarget: IContentTypeInfoBasic;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
begin
  inherited Create(ASource, ATarget);
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

function TContentConverterKmz2Kml.Convert(const AData: IBinaryData): IBinaryData;
var
  VZip: IArchiveReader;
  VFileName: string;
  VMemStream: TCustomMemoryStream;
begin
  VMemStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    VZip := FArchiveReadWriteFactory.CreateZipReaderByStream(VMemStream);
    Result := VZip.GetItemByIndex(0, VFileName);
  finally
    VMemStream.Free;
  end;
end;

end.
