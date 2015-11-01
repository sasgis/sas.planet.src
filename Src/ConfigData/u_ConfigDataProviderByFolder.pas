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

unit u_ConfigDataProviderByFolder;

interface

uses
  Classes,
  i_StringListStatic,
  i_BinaryData,
  i_ConfigDataProvider,
  u_BaseInterfacedObject;

type
  TConfigDataProviderByFolder = class(TBaseInterfacedObject, IConfigDataProvider)
  private
    FSourceFolderName: string;
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
    constructor Create(const AFolderName: string);
  end;

implementation

uses
  SysUtils,
  IniFiles,
  u_StringListStatic,
  u_BinaryDataByMemStream,
  Encodings,
  u_ConfigDataProviderByIniFile;

{ TConfigDataProviderByFolder }

constructor TConfigDataProviderByFolder.Create(const AFolderName: string);
begin
  inherited Create;
  FSourceFolderName := AFolderName;
end;

function TConfigDataProviderByFolder.GetSubItem(
  const AIdent: string): IConfigDataProvider;
var
  VExt: string;
  VFullName: string;
  VIniFile: TMemIniFile;
  VIniStrings: TStringList;
  VIniStream: TMemoryStream;
begin
  Result := nil;
  VFullName := IncludeTrailingPathDelimiter(FSourceFolderName) + AIdent;
  if DirectoryExists(VFullName) then begin
    Result := TConfigDataProviderByFolder.Create(VFullName);
  end else begin
    VExt := UpperCase(ExtractFileExt(AIdent));
    if (VExt = '.INI') or (VExt = '.TXT') then begin
      if FileExists(VFullName) then begin
        VIniStream := TMemoryStream.Create;
        try
          VIniStream.LoadFromFile(VFullName);
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
    end;
  end;
end;

function TConfigDataProviderByFolder.ReadBinary(const AIdent: string): IBinaryData;
var
  VMemStream: TMemoryStream;
  VFileName: string;
begin
  Result := nil;
  VFileName := IncludeTrailingPathDelimiter(FSourceFolderName) + AIdent;
  if FileExists(VFileName) then begin
    VMemStream := TMemoryStream.Create;
    try
      VMemStream.LoadFromFile(VFileName);
      Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
      VMemStream := nil;
    finally
      VMemStream.Free;
    end;
  end;
end;

function TConfigDataProviderByFolder.ReadBool(
  const AIdent: string;
  const ADefault: Boolean
): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderByFolder.ReadDate(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByFolder.ReadDateTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByFolder.ReadFloat(
  const AIdent: string;
  const ADefault: Double
): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderByFolder.ReadInteger(
  const AIdent: string;
  const ADefault: Integer
): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderByFolder.ReadAnsiString(
  const AIdent: string;
  const ADefault: AnsiString
): AnsiString;
var
  VExt: string;
  VFileName: string;
begin
  Result := '';
  if AIdent = '::FileName' then begin
    Result := AnsiString(FSourceFolderName);
  end else begin
    VExt := UpperCase(ExtractFileExt(AIdent));
    if (VExt = '.INI') or (VExt = '.HTML') or (VExt = '.TXT') then begin
      VFileName := IncludeTrailingPathDelimiter(FSourceFolderName) + AIdent;
      if FileExists(VFileName) then begin
        Result := FileToString(VFileName);
      end else begin
        Result := ADefault;
      end;
    end else begin
      Result := ADefault;
    end;
  end;
end;

function TConfigDataProviderByFolder.ReadString(
  const AIdent, ADefault: string
): string;
var
  VExt: string;
  VFileName: string;
begin
  Result := '';
  if AIdent = '::FileName' then begin
    Result := FSourceFolderName;
  end else begin
    VExt := UpperCase(ExtractFileExt(AIdent));
    if (VExt = '.INI') or (VExt = '.HTML') or (VExt = '.TXT') then begin
      VFileName := IncludeTrailingPathDelimiter(FSourceFolderName) + AIdent;
      if FileExists(VFileName) then begin
        Result := FileToText(VFileName);
      end else begin
        Result := ADefault;
      end;
    end else begin
      Result := ADefault;
    end;
  end;
end;

function TConfigDataProviderByFolder.ReadSubItemsList: IStringListStatic;
var
  VList: TStringList;
  VExt: string;
  VFolder: string;
  SearchRec: TSearchRec;
begin
  VList := TStringList.Create;
  try
    VFolder := IncludeTrailingPathDelimiter(FSourceFolderName);
    if FindFirst(VFolder + '*', faAnyFile, SearchRec) = 0 then begin
      repeat
        if (SearchRec.Attr and faDirectory) = faDirectory then begin
          continue;
        end;
        VExt := UpperCase(ExtractFileExt(SearchRec.Name));
        if (VExt = '.INI') or (VExt = '.TXT') then begin
          VList.Add(SearchRec.Name);
        end;
      until FindNext(SearchRec) <> 0;
    end;
    Result := TStringListStatic.CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

function TConfigDataProviderByFolder.ReadTime(
  const AIdent: string;
  const ADefault: TDateTime
): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByFolder.ReadValuesList: IStringListStatic;
var
  VList: TStringList;
  VExt: string;
  VFolder: string;
  SearchRec: TSearchRec;
begin
  VList := TStringList.Create;
  try
    VFolder := IncludeTrailingPathDelimiter(FSourceFolderName);
    if FindFirst(VFolder + '*', faAnyFile, SearchRec) = 0 then begin
      repeat
        if (SearchRec.Attr and faDirectory) = faDirectory then begin
          continue;
        end;
        VExt := UpperCase(ExtractFileExt(SearchRec.Name));
        if (VExt <> '.INI') or (VExt = '.HTML') or (VExt = '.TXT') then begin
          VList.Add(SearchRec.Name);
        end;
      until FindNext(SearchRec) <> 0;
    end;
    Result := TStringListStatic.CreateWithOwn(VList);
    VList := nil;
  finally
    VList.Free;
  end;
end;

end.
