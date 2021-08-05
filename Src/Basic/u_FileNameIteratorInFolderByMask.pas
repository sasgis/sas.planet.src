{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_FileNameIteratorInFolderByMask;

interface

uses
  Windows,
  i_FileNameIterator;

type
  TFileNameIteratorInFolderByMask = class(TInterfacedObject, IFileNameIterator)
  private
    FRootFolderName: string;
    FFolderNameFromRoot: string;
    FFileMask: string;
    FFilesOnly: Boolean;
    FValidFindData: Boolean;
    FFindHandle: THandle;
    FFindFileData: TWIN32FindData;
  protected
    function IsNeedProcess(AFindFileData: TWIN32FindData): Boolean; virtual;
  protected
    function GetRootFolderName: string;
    function Next(var AFileName: string): Boolean;
    procedure Reset;
  public
    constructor Create(
      const ARootFolderName: string;
      const AFolderNameFromRoot: string;
      const AFileMask: string;
      const AFilesOnly: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TFileNameIteratorInFolderByMask }

constructor TFileNameIteratorInFolderByMask.Create(
  const ARootFolderName: string;
  const AFolderNameFromRoot: string;
  const AFileMask: string;
  const AFilesOnly: Boolean
);
begin
  inherited Create;
  FRootFolderName := ARootFolderName;
  FFolderNameFromRoot := AFolderNameFromRoot;
  if FFolderNameFromRoot <> '' then begin
    FFolderNameFromRoot := FFolderNameFromRoot + PathDelim;
  end;
  FFileMask := AFileMask;
  FFindHandle := INVALID_HANDLE_VALUE;
  FFilesOnly := AFilesOnly;
  Reset;
end;

destructor TFileNameIteratorInFolderByMask.Destroy;
begin
  if not (FFindHandle = INVALID_HANDLE_VALUE) then begin
    Windows.FindClose(FFindHandle);
  end;
  inherited;
end;

function TFileNameIteratorInFolderByMask.GetRootFolderName: string;
begin
  Result := FRootFolderName;
end;

function TFileNameIteratorInFolderByMask.IsNeedProcess(
  AFindFileData: TWIN32FindData): Boolean;
begin
  if FFilesOnly then begin
    Result := (AFindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0;
  end else begin
    Result := (CompareStr(AFindFileData.cFileName, '.') <> 0) and
      (CompareStr(AFindFileData.cFileName, '..') <> 0);
  end;
end;

function TFileNameIteratorInFolderByMask.Next(
  var AFileName: string): Boolean;
begin
  Result := False;
  AFileName := '';
  if FValidFindData then begin
    repeat
      if IsNeedProcess(FFindFileData) then begin
        AFileName := FFolderNameFromRoot + FFindFileData.cFileName;
        Result := True;
      end;
      FValidFindData := Windows.FindNextFile(FFindHandle, FFindFileData);
    until not FValidFindData or Result;
  end;
end;

procedure TFileNameIteratorInFolderByMask.Reset;
var
  VCurrFullFilesMask: string;
begin
  if not (FFindHandle = INVALID_HANDLE_VALUE) then begin
    Windows.FindClose(FFindHandle);
  end;
  VCurrFullFilesMask := FRootFolderName + FFolderNameFromRoot + FFileMask;
  FFindHandle := Windows.FindFirstFile(PChar(VCurrFullFilesMask), FFindFileData);
  FValidFindData := not (FFindHandle = INVALID_HANDLE_VALUE);
end;

end.
