unit u_FileNameIteratorFolderWithSubfoldersWithIgnoredFolders;

interface

uses
  Windows,
  u_WideStrings,
  i_IFileNameIterator,
  u_FileNameIteratorFolderWithSubfolders;

type
  TFileNameIteratorFolderWithSubfoldersWithIgnoredFolders = class(TFileNameIteratorFolderWithSubfolders)
  private
    FIgnoredFoldersMasksList: TWideStrings;
  protected
    function IsNeedFolderProcess(AParentFolderNameFromRoot, AFolderName: WideString): Boolean; override;
  public
    constructor Create(
      ARootFolderName: WideString;
      AFolderNameFromRoot: WideString;
      AFolderIteratorFactory: IFileNameIteratorFactory;
      AFilesInFolderIteratorFactory: IFileNameIteratorFactory;
      AIgnoredFoldersMasksList: TWideStrings
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

function PathMatchSpecW(pszFile, pszSpec: PWideChar): BOOL; stdcall; external 'shlwapi.dll' name 'PathMatchSpecW';

{ TFileNameIteratorFolderWithSubfoldersWithIgnoredFolders }

constructor TFileNameIteratorFolderWithSubfoldersWithIgnoredFolders.Create(
  ARootFolderName, AFolderNameFromRoot: WideString; AFolderIteratorFactory,
  AFilesInFolderIteratorFactory: IFileNameIteratorFactory;
  AIgnoredFoldersMasksList: TWideStrings);
begin
  inherited Create(
    ARootFolderName,
    AFolderNameFromRoot,
    AFolderIteratorFactory,
    AFilesInFolderIteratorFactory
  );
  FIgnoredFoldersMasksList := TWideStringList.Create;
  FIgnoredFoldersMasksList.Assign(AIgnoredFoldersMasksList);
end;

destructor TFileNameIteratorFolderWithSubfoldersWithIgnoredFolders.Destroy;
begin
  FreeAndNil(FIgnoredFoldersMasksList);
  inherited;
end;

function TFileNameIteratorFolderWithSubfoldersWithIgnoredFolders.IsNeedFolderProcess(
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
