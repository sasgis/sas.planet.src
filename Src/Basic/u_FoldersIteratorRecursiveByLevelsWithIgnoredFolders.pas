{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_FoldersIteratorRecursiveByLevelsWithIgnoredFolders;

interface

uses
  Windows,
  Classes,
  i_FileNameIterator,
  u_FoldersIteratorRecursiveByLevels;

type
  TFoldersIteratorRecursiveByLevelsWithIgnoredFolders = class(TFoldersIteratorRecursiveByLevels)
  private
    FIgnoredFoldersMasksList: TStrings;
  protected
    function IsNeedFolderProcess(const AParentFolderNameFromRoot, AFolderName: string): Boolean; override;
  public
    constructor Create(
      const ARootFolderName: string;
      const AFolderNameFromRoot: string;
      AMaxFolderDepth: integer;
      AIgnoredFoldersMasksList: TStrings
    );
    destructor Destroy; override;
  end;

  TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory = class(TInterfacedObject, IFileNameIteratorFactory)
  private
    FMaxFolderDepth: integer;
    FIgnoredFoldersMasksList: TStrings;
  protected
    function CreateIterator(
      const ARootFolderName: string;
      const AFolderNameFromRoot: string
    ): IFileNameIterator;
  public
    constructor Create(
      AMaxFolderDepth: integer;
      AIgnoredFoldersMasksList: TStrings
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{$IFDef UNICODE}
function PathMatchSpec(pszFile, pszSpec: PChar): BOOL; stdcall; external 'shlwapi.dll' name 'PathMatchSpecW';
{$ELSE}
function PathMatchSpec(pszFile, pszSpec: PChar): BOOL; stdcall; external 'shlwapi.dll' name 'PathMatchSpecA';
{$ENDIF}

{ TFoldersIteratorRecursiveByLevelsWithIgnoredFolders }

constructor TFoldersIteratorRecursiveByLevelsWithIgnoredFolders.Create(
  const ARootFolderName: string;
  const AFolderNameFromRoot: string;
  AMaxFolderDepth: integer;
  AIgnoredFoldersMasksList: TStrings
);
begin
  inherited Create(
    ARootFolderName,
    AFolderNameFromRoot,
    AMaxFolderDepth
  );
  FIgnoredFoldersMasksList := TStringList.Create;
  FIgnoredFoldersMasksList.Assign(AIgnoredFoldersMasksList);
end;

destructor TFoldersIteratorRecursiveByLevelsWithIgnoredFolders.Destroy;
begin
  FreeAndNil(FIgnoredFoldersMasksList);
  inherited;
end;

function TFoldersIteratorRecursiveByLevelsWithIgnoredFolders.IsNeedFolderProcess(
  const AParentFolderNameFromRoot, AFolderName: string
): Boolean;
var
  i: Integer;
  VFolderName: string;
  VMask: string;
begin
  Result := inherited IsNeedFolderProcess(AParentFolderNameFromRoot, AFolderName);
  if Result then begin
    VFolderName := AFolderName;
    for i := 0 to FIgnoredFoldersMasksList.Count - 1 do begin
      VMask := FIgnoredFoldersMasksList.Strings[i];
      if PathMatchSpec(PChar(VFolderName), PChar(VMask)) then begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

{ TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory }

constructor TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory.Create(
  AMaxFolderDepth: integer;
  AIgnoredFoldersMasksList: TStrings
);
begin
  inherited Create;
  FMaxFolderDepth := AMaxFolderDepth;
  FIgnoredFoldersMasksList := TStringList.Create;
  FIgnoredFoldersMasksList.Assign(AIgnoredFoldersMasksList);
end;

destructor TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory.Destroy;
begin
  FreeAndNil(FIgnoredFoldersMasksList);
  inherited;
end;

function TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory.CreateIterator(
  const ARootFolderName, AFolderNameFromRoot: string
): IFileNameIterator;
begin
  Result := TFoldersIteratorRecursiveByLevelsWithIgnoredFolders.Create(
    ARootFolderName,
    AFolderNameFromRoot,
    FMaxFolderDepth,
    FIgnoredFoldersMasksList
  );
end;

end.
