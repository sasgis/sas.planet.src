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

function PathMatchSpecW(pszFile, pszSpec: PWideChar): BOOL; stdcall; external 'shlwapi.dll' name 'PathMatchSpecW';

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
  VFolderName: WideString;
  VMask: WideString;
begin
  Result := inherited IsNeedFolderProcess(AParentFolderNameFromRoot, AFolderName);
  if Result then begin
    VFolderName := AFolderName;
    for i := 0 to FIgnoredFoldersMasksList.Count - 1 do begin
      VMask := FIgnoredFoldersMasksList.Strings[i];
      if PathMatchSpecW(PWideChar(VFolderName), PWideChar(VMask)) then begin
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
