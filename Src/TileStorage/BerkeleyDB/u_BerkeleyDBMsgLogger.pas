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

unit u_BerkeleyDBMsgLogger;

interface

uses
  Classes,
  SysUtils,
  SyncObjs;

type
  TBerkeleyDBMsgLogger = class(TObject)
  private
    FMsgCS: TCriticalSection;
    FMsgFileName: string;
    FMsgFileStream: TFileStream;
    FFormatSettings: TFormatSettings;
  public
    procedure SaveVerbMsg(const AMsg: string);
  public
    constructor Create(const AMsgFileName: string);
    destructor Destroy; override;
  end;

implementation

constructor TBerkeleyDBMsgLogger.Create(const AMsgFileName: string);
begin
  inherited Create;
  FFormatSettings.DateSeparator := '-';
  FFormatSettings.TimeSeparator := ':';
  FFormatSettings.DecimalSeparator := '.';
  FMsgFileStream := nil;
  FMsgCS := TCriticalSection.Create;
  FMsgFileName := AMsgFileName;
end;

destructor TBerkeleyDBMsgLogger.Destroy;
begin
  FreeAndNil(FMsgFileStream);
  FreeAndNil(FMsgCS);
  inherited;
end;

procedure TBerkeleyDBMsgLogger.SaveVerbMsg(const AMsg: string);
var
  VMsg: string;
begin
  FMsgCS.Acquire;
  try
    if not Assigned(FMsgFileStream) then begin
      if not FileExists(FMsgFileName) then begin
        FMsgFileStream := TFileStream.Create(FMsgFileName, fmCreate);
        FMsgFileStream.Free;
      end;
      FMsgFileStream := TFileStream.Create(FMsgFileName, fmOpenReadWrite or fmShareDenyNone);
    end;

    VMsg := FormatDateTime('dd-mm-yyyy hh:nn:ss.zzzz', Now, FFormatSettings) + #09 + AMsg + #13#10;

    FMsgFileStream.Position := FMsgFileStream.Size;
    FMsgFileStream.Write(PChar(VMsg)^, Length(VMsg) * SizeOf(Char));
  finally
    FMsgCS.Release;
  end;
end;

end.
