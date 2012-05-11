unit u_FoldersIteratorRecursiveByLevelsWithIgnoredFolders;

interface

uses
  Windows,
  WideStrings,
  i_FileNameIterator,
  u_FoldersIteratorRecursiveByLevels;

type
  TFoldersIteratorRecursiveByLevelsWithIgnoredFolders = class(TFoldersIteratorRecursiveByLevels)
  private
    FIgnoredFoldersMasksList: TWideStrings;
  protected
    function IsNeedFolderProcess(const AParentFolderNameFromRoot, AFolderName: WideString): Boolean; override;
  public
    constructor Create(
      const ARootFolderName: WideString;
      const AFolderNameFromRoot: WideString;
      AMaxFolderDepth: integer;
      AIgnoredFoldersMasksList: TWideStrings
    );
    destructor Destroy; override;
  end;

  TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory = class(TInterfacedObject, IFileNameIteratorFactory)
  private
    FMaxFolderDepth: integer;
    FIgnoredFoldersMasksList: TWideStrings;
  protected
    function CreateIterator(
      const ARootFolderName: WideString;
      const AFolderNameFromRoot: WideString
    ): IFileNameIterator;
  public
    constructor Create(
      AMaxFolderDepth: integer;
      AIgnoredFoldersMasksList: TWideStrings
    );
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils;

function PathMatchSpecW(pszFile, pszSpec: PWideChar): BOOL; stdcall; external 'shlwapi.dll' name 'PathMatchSpecW';

{ TFoldersIteratorRecursiveByLevelsWithIgnoredFolders }

constructor TFoldersIteratorRecursiveByLevelsWithIgnoredFolders.Create(
  const ARootFolderName: WideString;
  const AFolderNameFromRoot: WideString;
  AMaxFolderDepth: integer;
  AIgnoredFoldersMasksList: TWideStrings
);
begin
  inherited Create(
    ARootFolderName,
    AFolderNameFromRoot,
    AMaxFolderDepth
  );
  FIgnoredFoldersMasksList := TWideStringList.Create;
  FIgnoredFoldersMasksList.Assign(AIgnoredFoldersMasksList);
end;

destructor TFoldersIteratorRecursiveByLevelsWithIgnoredFolders.Destroy;
begin
  FreeAndNil(FIgnoredFoldersMasksList);
  inherited;
end;

function TFoldersIteratorRecursiveByLevelsWithIgnoredFolders.IsNeedFolderProcess(
  const AParentFolderNameFromRoot, AFolderName: WideString
): Boolean;
var
  i: Integer;
  VMask: WideString;
begin
  Result := inherited IsNeedFolderProcess(AParentFolderNameFromRoot, AFolderName);
  if Result then begin
    for i := 0 to FIgnoredFoldersMasksList.Count - 1 do begin
      VMask := FIgnoredFoldersMasksList.Strings[i];
      if PathMatchSpecW(PWideChar(AFolderName), PWideChar(VMask)) then begin
        Result := False;
        Break;
      end;
    end;
  end;
end;

{ TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory }

constructor TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory.Create(
  AMaxFolderDepth: integer;
  AIgnoredFoldersMasksList: TWideStrings
);
begin
  FMaxFolderDepth := AMaxFolderDepth;
  FIgnoredFoldersMasksList := TWideStringList.Create;
  FIgnoredFoldersMasksList.Assign(AIgnoredFoldersMasksList);
end;

destructor TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory.Destroy;
begin
  FreeAndNil(FIgnoredFoldersMasksList);
  inherited;
end;

function TFoldersIteratorRecursiveByLevelsWithIgnoredFoldersFactory.CreateIterator(
  const ARootFolderName, AFolderNameFromRoot: WideString
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
