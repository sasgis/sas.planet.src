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

unit u_ConfigDataProviderByZip;

interface

uses
  Classes,
  i_StringListStatic,
  i_BinaryData,
  i_ArchiveReadWrite,
  i_ConfigDataProvider,
  u_BaseInterfacedObject;

type
  TConfigDataProviderByArchive = class(TBaseInterfacedObject, IConfigDataProvider)
  private
    FSourceFileName: string;
    FArchive: IArchiveReader;
    FSubFolder: string;
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
      const AArchive: IArchiveReader;
      const ASubFolder: string = ''
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  IniFiles,
  u_StringListStatic,
  u_StreamReadOnlyByBinaryData,
  Encodings,
  u_ConfigDataProviderByIniFile;

{ TConfigDataProviderByZip }

constructor TConfigDataProviderByArchive.Create(
  const AFileName: string;
  const AArchive: IArchiveReader;
  const ASubFolder: string
);
begin
  Assert(AArchive <> nil);
  inherited Create;
  FSourceFileName := AFileName;
  FArchive := AArchive;
  FSubFolder := ASubFolder;
  if FSubFolder <> '' then begin
    FSubFolder := IncludeTrailingPathDelimiter(FSubFolder);
  end;
end;

destructor TConfigDataProviderByArchive.Destroy;
begin
  FArchive := nil;
  inherited;
end;

function TConfigDataProviderByArchive.GetSubItem(
  const AIdent: string): IConfigDataProvider;
var
  VExt: string;
  VIniFile: TMemIniFile;
  VIniStrings: TStringList;
  VIniStream: TStream;
  VData: IBinaryData;
  i: Integer;
  VSubFolder: string;
begin
  Result := nil;
  VExt := UpperCase(ExtractFileExt(AIdent));
  if (VExt = '.INI') or (VExt = '.TXT') then begin
    VData := FArchive.GetItemByName(FSubFolder + AIdent);
    if VData <> nil then begin
      VIniStream := TStreamReadOnlyByBinaryData.Create(VData);
      try
        VIniStream.Position := 0;
        VIniStrings := TStringList.Create;
        try
          LoadStringsFromStream(VIniStrings, VIniStream);
          VIniFile := TMemIniFile.Create('');
          try
            VIniFile.SetStrings(VIniStrings);
            Result := TConfigDataProviderByIniFile.CreateWithOwn(VIniFile);
            VIniFile := nil;
          finally
            VIniFile.Free;
          end;
        finally
          VIniStrings.Free;
        end;
      finally
        VIniStream.Free;
      end;
    end;
  end else begin
    VSubFolder := IncludeTrailingPathDelimiter(FSubFolder + AIdent);
    for i := 0 to FArchive.GetItemsCount - 1 do begin
      if CompareText(FArchive.GetItemNameByIndex(i), VSubFolder) = 0 then begin
        Result := TConfigDataProviderByArchive.Create(FSourceFileName + PathDelim + AIdent, FArchive, VSubFolder);
        Break;
      end;
    end;
  end;
end;

function TConfigDataProviderByArchive.ReadBinary(
  const AIdent: string): IBinaryData;
begin
  Result := FArchive.GetItemByName(FSubFolder + AIdent);
end;

function TConfigDataProviderByArchive.ReadBool(
  const AIdent: string;
  const ADefault: Boolean
): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderByArchive.ReadDate(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByArchive.ReadDateTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByArchive.ReadFloat(
  const AIdent: string;
  const ADefault: Double
): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderByArchive.ReadInteger(
  const AIdent: string;
  const ADefault: Integer
): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderByArchive.ReadAnsiString(
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
      VData := FArchive.GetItemByName(FSubFolder + AIdent);
      if VData <> nil then begin
        VStream := TStreamReadOnlyByBinaryData.Create(VData);
        try
          Result := StreamToString(VStream);
        finally
          VStream.Free;
        end;
      end;
    end else begin
      Result := ADefault;
    end;
  end;
end;

function TConfigDataProviderByArchive.ReadString(
  const AIdent, ADefault: string
): string;
var
  VExt: string;
  VStream: TStream;
  VData: IBinaryData;
begin
  Result := '';
  if AIdent = '::FileName' then begin
    Result := FSourceFileName;
  end else begin
    VExt := UpperCase(ExtractFileExt(AIdent));
    if (VExt = '.INI') or (VExt = '.HTML') or (VExt = '.TXT') then begin
      VData := FArchive.GetItemByName(FSubFolder + AIdent);
      if VData <> nil then begin
        VStream := TStreamReadOnlyByBinaryData.Create(VData);
        try
          Result := StreamToText(VStream);
        finally
          VStream.Free;
        end;
      end;
    end else begin
      Result := ADefault;
    end;
  end;
end;

function TConfigDataProviderByArchive.ReadSubItemsList: IStringListStatic;
var
  VList: TStringList;
  I: Integer;
  VExt: string;
  VFullFileName: string;
  VFileName: string;
begin
  VList := TStringList.Create;
  try
    for I := 0 to FArchive.GetItemsCount - 1 do begin
      VFullFileName := FArchive.GetItemNameByIndex(I);
      if CompareText(ExtractFilePath(ExcludeTrailingPathDelimiter(VFullFileName)), FSubFolder) = 0 then begin
        VFileName := ExtractFileName(VFullFileName);
        VExt := UpperCase(ExtractFileExt(VFileName));
        if (VExt = '.INI') or (VExt = '.TXT') then begin
          VList.Add(VFileName);
        end;
      end;
    end;
    Result := TStringListStatic.CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

function TConfigDataProviderByArchive.ReadTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByArchive.ReadValuesList: IStringListStatic;
var
  VList: TStringList;
  i: Integer;
  VFullFileName: string;
  VFileName: string;
begin
  VList := TStringList.Create;
  try
    for i := 0 to FArchive.GetItemsCount - 1 do begin
      VFullFileName := FArchive.GetItemNameByIndex(i);
      if CompareText(ExtractFilePath(ExcludeTrailingPathDelimiter(VFullFileName)), FSubFolder) = 0 then begin
        VFileName := ExtractFileName(VFullFileName);
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
