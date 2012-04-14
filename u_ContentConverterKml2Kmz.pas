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
  u_ContentConverterBase;

type
  TContentConverterKml2Kmz = class(TContentConverterBase)
  protected
    function Convert(const AData: IBinaryData): IBinaryData; override;
  end;

implementation

uses
  KAZip,
  u_BinaryDataByMemStream,
  u_StreamReadOnlyByBinaryData;

{ TContentConverterKmz2Kml }

function TContentConverterKml2Kmz.Convert(const AData: IBinaryData): IBinaryData;
var
  VMemStream: TCustomMemoryStream;
  VZip:TKAZip;
  VResultStream: TMemoryStream;
begin
  VMemStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    VZip:=TKAZip.Create(nil);
    try
      VResultStream := TMemoryStream.Create;
      try
        VZip.CreateZip(VResultStream);
        VZip.Open(VResultStream);
        VZip.CompressionType := ctNormal;
        VZip.AddStream('doc.kml', VMemStream);
        Result := TBinaryDataByMemStream.CreateWithOwn(VResultStream);
        VResultStream:= nil;
      finally
        VResultStream.Free;
      end;
    finally
      VZip.Free;
    end;
  finally
    VMemStream.Free;
  end;
end;

end.
