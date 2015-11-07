unit u_FoldersIteratorRecursiveByLevels;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_FileNameIterator;

type
  TFoldersIteratorRecursiveByLevels = class(TInterfacedObject, IFileNameIterator)
  private
    FRootFolderName: string;
    FFolderNameFromRoot: string;
    FFolderNamesList: TStringList;
    FMaxFolderDepth: integer;
    procedure ProcessAddSubFolders(
      const AFolderNameFromRoot: string;
      ADepth: Integer
    );
  protected
    function IsNeedFolderProcess(const AParentFolderNameFromRoot, AFolderName: string): Boolean; virtual;
  protected
    function GetRootFolderName: string;
    function Next(var AFileName: string): Boolean;
    procedure Reset;
  public
    constructor Create(
      const ARootFolderName: string;
      const AFolderNameFromRoot: string;
      AMaxFolderDepth: integer
    );
    destructor Destroy; override;
  end;

  TFoldersIteratorRecursiveByLevelsFactory = class(TInterfacedObject, IFileNameIteratorFactory)
  private
    FMaxFolderDepth: integer;
  protected
    function CreateIterator(
      const ARootFolderName: string;
      const AFolderNameFromRoot: string
    ): IFileNameIterator;
  public
    constructor Create(AMaxFolderDepth: integer);
  end;

implementation

{ TFoldersIteratorRecursiveByLevels }

constructor TFoldersIteratorRecursiveByLevels.Create(
  const ARootFolderName: string;
  const AFolderNameFromRoot: string;
  AMaxFolderDepth: integer
);
begin
  inherited Create;
  if ARootFolderName <> '' then begin
    FRootFolderName := IncludeTrailingPathDelimiter(ARootFolderName);
  end;
  FFolderNameFromRoot := AFolderNameFromRoot;
  FMaxFolderDepth := AMaxFolderDepth;
  FFolderNamesList := TStringList.Create;
  FFolderNamesList.AddObject(FFolderNameFromRoot, TObject(0));
end;

destructor TFoldersIteratorRecursiveByLevels.Destroy;
begin
  FreeAndNil(FFolderNamesList);
  inherited;
end;

function TFoldersIteratorRecursiveByLevels.GetRootFolderName: string;
begin
  Result := FRootFolderName;
end;

function TFoldersIteratorRecursiveByLevels.IsNeedFolderProcess(
  const AParentFolderNameFromRoot, AFolderName: string
): Boolean;
begin
  Result := (CompareStr(AFolderName, '.') <> 0) and
    (CompareStr(AFolderName, '..') <> 0);
end;

function TFoldersIteratorRecursiveByLevels.Next(
  var AFileName: string): Boolean;
begin
  AFileName := '';
  Result := False;
  if FFolderNamesList.Count > 0 then begin
    Result := True;
    AFileName := FFolderNamesList.Strings[0];
    ProcessAddSubFolders(AFileName, Integer(FFolderNamesList.Objects[0]));
    FFolderNamesList.Delete(0);
  end;
end;

procedure TFoldersIteratorRecursiveByLevels.ProcessAddSubFolders(
  const AFolderNameFromRoot: string;
  ADepth: Integer
);
var
  VFindFileData: TWIN32FindData;
  VhFind: THandle;
  VCurrFullFilesMask: string;
  VPathFromRootNew: string;
  VFolderNameFromRoot: string;
begin
  if ADepth < FMaxFolderDepth then begin
    if AFolderNameFromRoot <> '' then begin
      VFolderNameFromRoot := AFolderNameFromRoot + PathDelim;
    end;
    VCurrFullFilesMask := FRootFolderName + VFolderNameFromRoot + '*';
    VhFind := THandle(Windows.FindFirstFileEx(PChar(VCurrFullFilesMask),
      FindExInfoStandard, @VFindFileData, FindExSearchLimitToDirectories, nil, 0));
    if not (VhFind = INVALID_HANDLE_VALUE) then begin
      try
        repeat
          if (VFindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then begin
            if IsNeedFolderProcess(VFolderNameFromRoot, VFindFileData.cFileName) then begin
              VPathFromRootNew := VFolderNameFromRoot + VFindFileData.cFileName;
              FFolderNamesList.AddObject(VPathFromRootNew, TObject(ADepth + 1));
            end;
          end;
        until not Windows.FindNextFile(VhFind, VFindFileData);
      finally
        Windows.FindClose(VhFind);
      end;
    end;
  end;
end;

procedure TFoldersIteratorRecursiveByLevels.Reset;
begin
  FFolderNamesList.Clear;
  FFolderNamesList.AddObject(FFolderNameFromRoot, TObject(0));
end;

{ TFoldersIteratorRecursiveByLevelsFactory }

constructor TFoldersIteratorRecursiveByLevelsFactory.Create(
  AMaxFolderDepth: integer);
begin
  inherited Create;
  FMaxFolderDepth := AMaxFolderDepth;
end;

function TFoldersIteratorRecursiveByLevelsFactory.CreateIterator(
  const ARootFolderName, AFolderNameFromRoot: string
): IFileNameIterator;
begin
  Result := TFoldersIteratorRecursiveByLevels.Create(
    ARootFolderName,
    AFolderNameFromRoot,
    FMaxFolderDepth
  );
end;

end.
