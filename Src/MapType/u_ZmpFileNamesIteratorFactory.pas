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

unit u_ZmpFileNamesIteratorFactory;

interface

uses
  i_FileNameIterator,
  u_BaseInterfacedObject;

type
  TZmpFileNamesIteratorFactory = class(TBaseInterfacedObject, IFileNameIteratorFactory)
  private
    FFactory: IFileNameIteratorFactory;
  private
    function CreateIterator(
      const ARootFolderName: string;
      const AFolderNameFromRoot: string
    ): IFileNameIterator;
  public
    constructor Create;
  end;

implementation

uses
  Classes,
  SysUtils,
  u_FoldersIteratorRecursiveByLevelsWithIgnoredFolders,
  u_FileNameIteratorInFolderByMaskList,
  u_FileNameIteratorFolderWithSubfolders;

{ TZmpFileNamesIteratorFactory }

constructor TZmpFileNamesIteratorFactory.Create;
var
  VIgnoredFodlerMasks: TStringList;
  VProcessFileMasks: TStringList;
  VFoldersIteratorFactory: IFileNameIteratorFactory;
  VFilesInFolderIteratorFactory: IFileNameIteratorFactory;
begin
  inherited Create;
  VIgnoredFodlerMasks := TStringList.Create;
  VProcessFileMasks := TStringList.Create;
  try
    VProcessFileMasks.Add('*.zmp');
    VIgnoredFodlerMasks.Add('*.zmp');
    VIgnoredFodlerMasks.Add('.*');
    VFoldersIteratorFactory :=
      TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory.Create(
        6,
        VIgnoredFodlerMasks
      );
    VFilesInFolderIteratorFactory :=
      TFileNameIteratorInFolderByMaskListFactory.Create(
        VProcessFileMasks,
        False
      );
    FFactory :=
      TFileNameIteratorFolderWithSubfoldersFactory.Create(
        VFoldersIteratorFactory,
        VFilesInFolderIteratorFactory
      );
  finally
    FreeAndNil(VIgnoredFodlerMasks);
    FreeAndNil(VProcessFileMasks);
  end;
end;

function TZmpFileNamesIteratorFactory.CreateIterator(
  const ARootFolderName, AFolderNameFromRoot: string
): IFileNameIterator;
begin
  Result := FFactory.CreateIterator(ARootFolderName, AFolderNameFromRoot);
end;

end.
