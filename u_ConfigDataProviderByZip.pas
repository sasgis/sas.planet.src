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

unit u_ConfigDataProviderByZip;

interface

uses
  Classes,
  i_StringListStatic,
  i_BinaryData,
  i_ArchiveReadWrite,
  i_ArchiveReadWriteFactory,
  i_ConfigDataProvider;

type
  TConfigDataProviderByZip = class(TInterfacedObject, IConfigDataProvider)
  private
    FSourceFileName: string;
    FZip: IArchiveReader;
  private
    function GetSubItem(const AIdent: string): IConfigDataProvider;
    function ReadBinary(const AIdent: string): IBinaryData;
    function ReadAnsiString(
      const AIdent: string;
      const ADefault: AnsiString
    ): AnsiString;
    function ReadString(
      const AIdent: string;
      const ADefault: string
    ): string;
    function ReadInteger(
      const AIdent: string;
      const ADefault: Longint
    ): Longint;
    function ReadBool(
      const AIdent: string;
      const ADefault: Boolean
    ): Boolean;
    function ReadDate(
      const AIdent: string;
      const ADefault: TDateTime
    ): TDateTime;
    function ReadDateTime(
      const AIdent: string;
      const ADefault: TDateTime
    ): TDateTime;
    function ReadFloat(
      const AIdent: string;
      const ADefault: Double
    ): Double;
    function ReadTime(
      const AIdent: string;
      const ADefault: TDateTime
    ): TDateTime;

    function ReadSubItemsList: IStringListStatic;
    function ReadValuesList: IStringListStatic;
  public
    constructor Create(
      const AFileName: string;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  IniFiles,
  u_ResStrings,
  u_StringListStatic,
  u_StreamReadOnlyByBinaryData,
  u_ConfigDataProviderByIniFile;

{ TConfigDataProviderByZip }

constructor TConfigDataProviderByZip.Create(
  const AFileName: string;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
begin
  inherited Create;
  FSourceFileName := AFileName;
  if AFileName = '' then begin
    raise Exception.Create(SAS_ERR_EmptyZMPFileName);
  end;
  if not FileExists(AFileName) then begin
    raise Exception.CreateFmt(SAS_ERR_FileNotFoundFmt, [AFileName]);
  end;
  FZip := AArchiveReadWriteFactory.CreateZipReaderByName(AFileName);
end;

destructor TConfigDataProviderByZip.Destroy;
begin
  FZip := nil;
  inherited Destroy;
end;

function TConfigDataProviderByZip.GetSubItem(
  const AIdent: string): IConfigDataProvider;
var
  VExt: string;
  VIniFile: TMemIniFile;
  VIniStrings: TStringList;
  VIniStream: TStream;
  VData: IBinaryData;
begin
  Result := nil;
  VExt := UpperCase(ExtractFileExt(AIdent));
  if (VExt = '.INI') or (VExt = '.TXT') then begin
    VData := FZip.GetItemByName(AIdent);
    if VData <> nil then begin
      VIniFile := TMemIniFile.Create('');
      VIniStream := TStreamReadOnlyByBinaryData.Create(VData);
      try
        VIniStream.Position := 0;
        VIniStrings := TStringList.Create;
        try
          VIniStrings.LoadFromStream(VIniStream);
          VIniFile.SetStrings(VIniStrings);
          Result := TConfigDataProviderByIniFile.Create(VIniFile);
        finally
          VIniStrings.Free;
        end;
      finally
        VIniStream.Free;
      end;
    end;
  end;
end;

function TConfigDataProviderByZip.ReadBinary(
  const AIdent: string): IBinaryData;
begin
  Result := FZip.GetItemByName(AIdent);
end;

function TConfigDataProviderByZip.ReadBool(
  const AIdent: string;
  const ADefault: Boolean
): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderByZip.ReadDate(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByZip.ReadDateTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByZip.ReadFloat(
  const AIdent: string;
  const ADefault: Double
): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderByZip.ReadInteger(
  const AIdent: string;
  const ADefault: Integer
): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderByZip.ReadAnsiString(
  const AIdent: string;
  const ADefault: AnsiString
): AnsiString;
var
  VExt: string;
  VStream: TStream;
  VData: IBinaryData;
begin
  Result := '';
  if AIdent = '::FileName' then begin
    Result := AnsiString(FSourceFileName);
  end else begin
    VExt := UpperCase(ExtractFileExt(AIdent));
    if (VExt = '.INI') or (VExt = '.HTML') or (VExt = '.TXT') then begin
      VData := FZip.GetItemByName(AIdent);
      if VData <> nil then begin
        VStream := TStreamReadOnlyByBinaryData.Create(VData);
        try
          SetLength(Result, VStream.Size);
          VStream.Position := 0;
          VStream.ReadBuffer(Result[1], VStream.Size);
        finally
          VStream.Free;
        end;
      end;
    end else begin
      Result := ADefault;
    end;
  end;
end;

function TConfigDataProviderByZip.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := '';
  if AIdent = '::FileName' then begin
    Result := FSourceFileName;
  end else begin
    Result := ReadAnsiString(AIdent, ADefault);
  end;
end;

function TConfigDataProviderByZip.ReadSubItemsList: IStringListStatic;
var
  VList: TStringList;
  I: Integer;
  VExt: string;
  VFileName: string;
begin
  VList := TStringList.Create;
  try
    for I := 0 to FZip.GetItemsCount - 1 do begin
      VFileName := FZip.GetItemNameByIndex(I);
      VExt := UpperCase(ExtractFileExt(VFileName));
      if (VExt = '.INI') or (VExt = '.TXT') then begin
        VList.Add(VFileName);
      end;
    end;
    Result := TStringListStatic.CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

function TConfigDataProviderByZip.ReadTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByZip.ReadValuesList: IStringListStatic;
var
  VList: TStringList;
  i: Integer;
  VExt: string;
  VFileName: string;
begin
  VList := TStringList.Create;
  try
    for i := 0 to FZip.GetItemsCount - 1 do begin
      VFileName := FZip.GetItemNameByIndex(i);
      VExt := UpperCase(ExtractFileExt(VFileName));
      if (VExt <> '.INI') or (VExt = '.HTML') or (VExt = '.TXT') then begin
        VList.Add(VFileName);
      end;
    end;
    Result := TStringListStatic.CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

end.
