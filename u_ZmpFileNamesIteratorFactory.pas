{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ZmpFileNamesIteratorFactory;

interface

uses
  u_WideStrings,
  i_FileNameIterator;

type
  TZmpFileNamesIteratorFactory = class(TInterfacedObject, IFileNameIteratorFactory)
  private
    FFactory: IFileNameIteratorFactory;
  protected
    function  CreateIterator(
      ARootFolderName: WideString;
      AFolderNameFromRoot: WideString
    ): IFileNameIterator;
  public
    constructor Create();
    destructor Destroy; override;
  end;
implementation

uses
  SysUtils,
  u_FoldersIteratorRecursiveByLevelsWithIgnoredFolders,
  u_FileNameIteratorInFolderByMaskList,
  u_FileNameIteratorFolderWithSubfolders;

{ TZmpFileNamesIteratorFactory }

constructor TZmpFileNamesIteratorFactory.Create;
var
  VIgnoredFodlerMasks: TWideStringList;
  VProcessFileMasks: TWideStringList;
  VFoldersIteratorFactory: IFileNameIteratorFactory;
  VFilesInFolderIteratorFactory: IFileNameIteratorFactory;
begin
  VIgnoredFodlerMasks := TWideStringList.Create;
  VProcessFileMasks := TWideStringList.Create;
  try
    VProcessFileMasks.Add('*.zmp');
    VIgnoredFodlerMasks.Add('*.zmp');
    VIgnoredFodlerMasks.Add('.*');
    VFoldersIteratorFactory :=
      TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory.Create(
        6,
        VIgnoredFodlerMasks
      );
    try
      VFilesInFolderIteratorFactory :=
        TFileNameIteratorInFolderByMaskListFactory.Create(
          VProcessFileMasks,
          False
        );
      try
        FFactory :=
          TFileNameIteratorFolderWithSubfoldersFactory.Create(
            VFoldersIteratorFactory,
            VFilesInFolderIteratorFactory
          );
      finally
        VFilesInFolderIteratorFactory := nil;
      end;
    finally
      VFoldersIteratorFactory := nil;
    end;
  finally
    FreeAndNil(VIgnoredFodlerMasks);
    FreeAndNil(VProcessFileMasks);
  end;
end;

function TZmpFileNamesIteratorFactory.CreateIterator(ARootFolderName,
  AFolderNameFromRoot: WideString): IFileNameIterator;
begin
  Result := FFactory.CreateIterator(ARootFolderName, AFolderNameFromRoot);
end;

destructor TZmpFileNamesIteratorFactory.Destroy;
begin
  FFactory := nil;
  inherited;
end;

end.
