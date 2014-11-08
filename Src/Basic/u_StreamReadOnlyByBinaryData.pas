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

unit u_StreamReadOnlyByBinaryData;

interface

uses
  Classes,
  i_BinaryData;

type
  TStreamReadOnlyByBinaryData = class(TCustomMemoryStream)
  private
    FData: IBinaryData;
  public
    procedure SetSize(NewSize: Longint); override;
    function Write(
      const Buffer;
      Count: Longint
    ): Longint; override;
  public
    constructor Create(const AData: IBinaryData);
  end;

implementation

{ TStreamReadOnlyByBinaryData }

constructor TStreamReadOnlyByBinaryData.Create(const AData: IBinaryData);
begin
  Assert(AData <> nil);
  inherited Create;
  FData := AData;
  SetPointer(FData.Buffer, FData.Size);
end;

procedure TStreamReadOnlyByBinaryData.SetSize(NewSize: Integer);
begin
  inherited;
  if NewSize <> FData.Size then begin
    raise EWriteError.Create('Read only stream');
  end;
end;

function TStreamReadOnlyByBinaryData.Write(
  const Buffer;
  Count: Integer
): Longint;
begin
  raise EWriteError.Create('Read only stream');
end;

end.
