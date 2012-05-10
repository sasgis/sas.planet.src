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

unit u_ContentConverterKmz2Kml;

interface

uses
  Classes,
  i_BinaryData,
  u_ContentConverterBase;

type
  TContentConverterKmz2Kml = class(TContentConverterBase)
  protected
    function Convert(const AData: IBinaryData): IBinaryData; override;
  end;

implementation

uses
  KAZip,
  u_BinaryDataByMemStream,
  u_StreamReadOnlyByBinaryData;

{ TContentConverterKmz2Kml }

function TContentConverterKmz2Kml.Convert(const AData: IBinaryData): IBinaryData;
var
  UnZip: TKAZip;
  VMemStream: TCustomMemoryStream;
  VResultStream: TMemoryStream;
begin
  VMemStream := TStreamReadOnlyByBinaryData.Create(AData);
  try
    UnZip := TKAZip.Create(nil);
    try
      UnZip.Open(VMemStream);
      VResultStream := TMemoryStream.Create;
      try
        UnZip.Entries.Items[0].ExtractToStream(VResultStream);
        Result := TBinaryDataByMemStream.CreateWithOwn(VResultStream);
        VResultStream := nil;
      finally
        VResultStream.Free;
      end;
    finally
      UnZip.Free;
    end;
  finally
    VMemStream.Free;
  end;
end;

end.
