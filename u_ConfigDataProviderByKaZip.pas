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

unit u_ConfigDataProviderByKaZip;

interface

uses
  Classes,
  KAZip,
  i_StringListStatic,
  i_BinaryData,
  i_ConfigDataProvider;

type
  TConfigDataProviderByKaZip = class(TInterfacedObject, IConfigDataProvider)
  private
    FSourceFileName: string;
    FUnZip: TKAZip;
  protected
    function GetSubItem(const AIdent: string): IConfigDataProvider;
    function ReadBinary(const AIdent: string): IBinaryData;
    function ReadString(const AIdent: string; const ADefault: string): string;
    function ReadInteger(const AIdent: string; const ADefault: Longint): Longint;
    function ReadBool(const AIdent: string; const ADefault: Boolean): Boolean;
    function ReadDate(const AIdent: string; const ADefault: TDateTime): TDateTime;
    function ReadDateTime(const AIdent: string; const ADefault: TDateTime): TDateTime;
    function ReadFloat(const AIdent: string; const ADefault: Double): Double;
    function ReadTime(const AIdent: string; const ADefault: TDateTime): TDateTime; 

    function ReadSubItemsList: IStringListStatic;
    function ReadValuesList: IStringListStatic;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  IniFiles,
  u_ResStrings,
  u_StringListStatic,
  u_BinaryDataByMemStream,
  u_ConfigDataProviderByIniFile;

{ TConfigDataProviderByKaZip }

constructor TConfigDataProviderByKaZip.Create(const AFileName: string);
begin
  FSourceFileName := AFileName;
  if AFileName = '' then begin
    raise Exception.Create(SAS_ERR_EmptyZMPFileName);
  end;
  if not FileExists(AFileName) then begin
    raise Exception.CreateFmt(SAS_ERR_FileNotFoundFmt, [AFileName]);
  end;
  FUnZip := TKAZip.Create(nil);
  FUnZip.Open(AFileName);
end;

destructor TConfigDataProviderByKaZip.Destroy;
begin
  FreeAndNil(FUnZip);
  inherited;
end;

function TConfigDataProviderByKaZip.GetSubItem(
  const AIdent: string): IConfigDataProvider;
var
  VExt: string;
  VIniFile: TMemIniFile;
  VIniStrings: TStringList;
  VIniStream: TMemoryStream;
  VIndex: Integer;
begin
  Result := nil;
  VExt := UpperCase(ExtractFileExt(AIdent));
  if (VExt = '.INI') or (VExt = '.TXT') then begin
    VIndex := FUnZip.Entries.IndexOf(AIdent);
    if VIndex >= 0 then begin
      VIniFile := TMemIniFile.Create('');
      VIniStream := TMemoryStream.Create;
      try
        FUnZip.Entries.Items[VIndex].ExtractToStream(VIniStream);
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

function TConfigDataProviderByKaZip.ReadBinary(
  const AIdent: string): IBinaryData;
var
  VIndex: Integer;
  VMemStream: TMemoryStream;
begin
  Result := nil;
  VIndex := FUnZip.Entries.IndexOf(AIdent);
  if VIndex >= 0 then begin
    VMemStream := TMemoryStream.Create;
    try
      FUnZip.Entries.Items[VIndex].ExtractToStream(VMemStream);
      Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
      VMemStream := nil;
    finally
      VMemStream.Free;
    end;
  end;
end;

function TConfigDataProviderByKaZip.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderByKaZip.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByKaZip.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByKaZip.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderByKaZip.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderByKaZip.ReadString(const AIdent,
  ADefault: string): string;
var
  VExt: string;
  VStream: TMemoryStream;
  VIndex: Integer;
begin
  Result := '';
  if AIdent = '::FileName' then begin
    Result := FSourceFileName;
  end else begin
    VExt := UpperCase(ExtractFileExt(AIdent));
    if (VExt = '.INI') or (VExt = '.HTML') or (VExt = '.TXT') then begin
      VIndex := FUnZip.Entries.IndexOf(AIdent);
      if VIndex >= 0 then begin
        VStream := TMemoryStream.Create;
        try
          FUnZip.Entries.Items[VIndex].ExtractToStream(VStream);
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

function TConfigDataProviderByKaZip.ReadSubItemsList: IStringListStatic;
var
  VList: TStringList;
  i: Integer;
  VExt: string;
  VFileName: string;
begin
  VList := TStringList.Create;
  try
    for i := 0 to FUnZip.Entries.Count - 1 do begin
      VFileName := FUnZip.Entries.Items[i].FileName;
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

function TConfigDataProviderByKaZip.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByKaZip.ReadValuesList: IStringListStatic;
var
  VList: TStringList;
  i: Integer;
  VExt: string;
  VFileName: string;
begin
  VList := TStringList.Create;
  try
    for i := 0 to FUnZip.Entries.Count - 1 do begin
      VFileName := FUnZip.Entries.Items[i].FileName;
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
