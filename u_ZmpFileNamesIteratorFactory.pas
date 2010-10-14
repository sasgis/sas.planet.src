unit u_ZmpFileNamesIteratorFactory;

interface

uses
  u_WideStrings,
  i_IFileNameIterator;

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
