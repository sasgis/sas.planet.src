{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_FileSystemTools;

interface

uses
  Windows,
  i_TileInfoBasic;

type
  TFileSystemTools = record
    class function GetTileNotFoundExt: string; inline; static;

    class procedure CreateDirIfNotExists(const APath: string); static;

    class procedure UpdateTileInfoByFile(
      const AIsTneFile: Boolean;
      const AIsLoadData: Boolean;
      const AFileName: string;
      out ATileInfo: TTileInfo
    ); static;

    class function GetFileDate(
      const AInfo: WIN32_FILE_ATTRIBUTE_DATA
    ): TDateTime; static;

    class procedure SetFileDate(
      const AHandle: THandle;
      const ADate: TDateTime
    ); inline; static;
  end;

implementation

uses
  Classes,
  SysUtils,
  u_BinaryDataByMemStream;

{ TFileSystemTools }

class function TFileSystemTools.GetTileNotFoundExt: string;
begin
  Result := '.tne';
end;

class procedure TFileSystemTools.CreateDirIfNotExists(const APath: string);
var
  I: Integer;
  VPath: string;
begin
  I := LastDelimiter(PathDelim, APath);
  VPath := Copy(APath, 1, I);
  if not DirectoryExists(VPath) then begin
    if not ForceDirectories(VPath) then begin
      RaiseLastOSError;
    end;
  end;
end;

class procedure TFileSystemTools.UpdateTileInfoByFile(
  const AIsTneFile: Boolean;
  const AIsLoadData: Boolean;
  const AFileName: string;
  out ATileInfo: TTileInfo
);
var
  VInfo: WIN32_FILE_ATTRIBUTE_DATA;
  VMemStream: TMemoryStream;
begin
  if GetFileAttributesEx(PChar(AFileName), GetFileExInfoStandard, @VInfo) <> FALSE then begin
    if AIsTneFile then begin
      ATileInfo.FInfoType := titTneExists;
      ATileInfo.FLoadDate := TFileSystemTools.GetFileDate(VInfo);
      ATileInfo.FData := nil;
      ATileInfo.FSize := 0;
    end else begin
      ATileInfo.FInfoType := titExists;
      ATileInfo.FLoadDate := TFileSystemTools.GetFileDate(VInfo);
      if AIsLoadData then begin
        VMemStream := TMemoryStream.Create;
        try
          VMemStream.LoadFromFile(AFileName);
          ATileInfo.FData := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
          VMemStream := nil;
        finally
          VMemStream.Free;
        end;
        ATileInfo.FSize := ATileInfo.FData.Size;
      end else begin
        ATileInfo.FData := nil;
        ATileInfo.FSize := VInfo.nFileSizeLow;
      end;
    end;
  end else begin
    ATileInfo.FInfoType := titNotExists;
    ATileInfo.FLoadDate := 0;
    ATileInfo.FData := nil;
    ATileInfo.FSize := 0;
  end;
end;

class function TFileSystemTools.GetFileDate(
  const AInfo: WIN32_FILE_ATTRIBUTE_DATA
): TDateTime;
var
  VSysTime: TSystemTime;
  VFileTimePtr: PFileTime;
begin
  Result := 0;
  VFileTimePtr := nil;

  if
    (AInfo.ftLastWriteTime.dwLowDateTime <> 0) and
    (AInfo.ftLastWriteTime.dwHighDateTime <> 0) then
  begin
    // last modified time (if exists)
    VFileTimePtr := @(AInfo.ftLastWriteTime);
  end else
  if
    (AInfo.ftCreationTime.dwLowDateTime <> 0) and
    (AInfo.ftCreationTime.dwHighDateTime <> 0) then
  begin
    // ..or created time (if exists)
    VFileTimePtr := @(AInfo.ftCreationTime);
  end;

  if VFileTimePtr <> nil then begin
    if FileTimeToSystemTime(VFileTimePtr^, VSysTime) then begin
      {$IF CompilerVersion >= 33}
      if not TrySystemTimeToDateTime(VSysTime, Result) then begin
        Result := 0;
      end;
      {$ELSE}
      try
        Result := SystemTimeToDateTime(VSysTime);
      except
        //
      end;
      {$IFEND}
    end;
  end;
end;

class procedure TFileSystemTools.SetFileDate(
  const AHandle: THandle;
  const ADate: TDateTime
);
begin
  {$WARN SYMBOL_PLATFORM OFF}
  FileSetDate( // 'FileSetDate' is specific to a platform
    AHandle,
    DateTimeToFileDate(ADate)
  );
  {$WARN SYMBOL_PLATFORM ON}
end;

end.
