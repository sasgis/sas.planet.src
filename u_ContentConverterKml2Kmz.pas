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

unit u_ContentConverterKml2Kmz;

interface

uses
  Classes,
  i_BinaryData,
  i_ContentTypeInfo,
  i_ArchiveReadWriteFactory,
  u_ContentConverterBase;

type
  TContentConverterKml2Kmz = class(TContentConverterBase)
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
  SysUtils,
  i_ArchiveReadWrite,
  u_BinaryDataByMemStream;

{ TContentConverterKmz2Kml }

constructor TContentConverterKml2Kmz.Create(
  const ASource: IContentTypeInfoBasic;
  const ATarget: IContentTypeInfoBasic;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
begin
  inherited Create(ASource, ATarget);
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
end;

function TContentConverterKml2Kmz.Convert(const AData: IBinaryData): IBinaryData;
var
  VZip: IArchiveWriter;
  VResultStream: TMemoryStream;
begin
  VResultStream := TMemoryStream.Create;
  try
    VZip := FArchiveReadWriteFactory.CreateZipWriterByStream(VResultStream);
    VZip.AddFile(AData, 'doc.kml', Now);
    Result := TBinaryDataByMemStream.CreateWithOwn(VResultStream);
    VResultStream := nil;
  finally
    VResultStream.Free;
  end;
end;

end.
