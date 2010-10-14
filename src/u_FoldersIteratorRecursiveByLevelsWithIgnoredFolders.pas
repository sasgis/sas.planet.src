unit u_FoldersIteratorRecursiveByLevelsWithIgnoredFolders;

interface

uses
  Windows,
  u_WideStrings,
  i_IFileNameIterator,
  u_FoldersIteratorRecursiveByLevels;

type
  TFoldersIteratorRecursiveByLevelsWithIgnoredFolders = class(TFoldersIteratorRecursiveByLevels)
  private
    FIgnoredFoldersMasksList: TWideStrings;
  protected
    function IsNeedFolderProcess(AParentFolderNameFromRoot, AFolderName: WideString): Boolean; override;
  public
    constructor Create(
      ARootFolderName: WideString;
      AFolderNameFromRoot: WideString;
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
  ARootFolderName: WideString;
  AFolderNameFromRoot: WideString;
  AMaxFolderDepth: integer;
  AIgnoredFoldersMasksList: TWideStrings);
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
  AParentFolderNameFromRoot, AFolderName: WideString): Boolean;
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

end.
