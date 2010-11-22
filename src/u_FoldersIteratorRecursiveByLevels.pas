unit u_FoldersIteratorRecursiveByLevels;

interface

uses
  Windows,
  Classes,
  SysUtils,
  u_WideStrings,
  i_IFileNameIterator;

type
  TFoldersIteratorRecursiveByLevels = class(TInterfacedObject, IFileNameIterator)
  private
    FRootFolderName: WideString;
    FFolderNameFromRoot: WideString;
    FFolderNamesList: TWideStringList;
    FMaxFolderDepth: integer;
    procedure ProcessAddSubFolders(AFolderNameFromRoot: WideString; ADepth: Integer);
  protected
    function IsNeedFolderProcess(AParentFolderNameFromRoot, AFolderName: WideString): Boolean; virtual;
  protected
    function GetRootFolderName: WideString;
    function Next(var AFileName: WideString): Boolean;
    procedure Reset;
  public
    constructor Create(
      ARootFolderName: WideString;
      AFolderNameFromRoot: WideString;
      AMaxFolderDepth: integer
    );
    destructor Destroy; override;
  end;

  TFoldersIteratorRecursiveByLevelsFactory = class(TInterfacedObject, IFileNameIteratorFactory)
  private
    FMaxFolderDepth: integer;
  protected
    function  CreateIterator(
      ARootFolderName: WideString;
      AFolderNameFromRoot: WideString
    ): IFileNameIterator;
  public
    constructor Create(AMaxFolderDepth: integer);
  end;

implementation

{ TFoldersIteratorRecursiveByLevels }

constructor TFoldersIteratorRecursiveByLevels.Create(
  ARootFolderName: WideString;
  AFolderNameFromRoot: WideString;
  AMaxFolderDepth: integer);
begin
  if ARootFolderName <> '' then begin
    FRootFolderName := IncludeTrailingPathDelimiter(ARootFolderName);
  end;
  FFolderNameFromRoot := AFolderNameFromRoot;
  FMaxFolderDepth := AMaxFolderDepth;
  FFolderNamesList := TWideStringList.Create;
  FFolderNamesList.AddObject(FFolderNameFromRoot, TObject(0));
end;

destructor TFoldersIteratorRecursiveByLevels.Destroy;
begin
  FreeAndNil(FFolderNamesList);
  inherited;
end;

function TFoldersIteratorRecursiveByLevels.GetRootFolderName: WideString;
begin
  Result := FRootFolderName;
end;

function TFoldersIteratorRecursiveByLevels.IsNeedFolderProcess(
  AParentFolderNameFromRoot, AFolderName: WideString): Boolean;
begin
  Result := (WideCompareStr(AFolderName, '.') <> 0) and
    (WideCompareStr(AFolderName, '..') <> 0);
end;

function TFoldersIteratorRecursiveByLevels.Next(
  var AFileName: WideString): Boolean;
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
  AFolderNameFromRoot: WideString; ADepth: Integer);
var
  VFindFileData: TWIN32FindDataW;
  VhFind: THandle;
  VCurrFullFilesMask: WideString;
  VPathFromRootNew: WideString;
  VFolderNameFromRoot: WideString;
begin
  if ADepth < FMaxFolderDepth then begin
    if AFolderNameFromRoot <> '' then begin
      VFolderNameFromRoot := AFolderNameFromRoot + PathDelim;
    end;
    VCurrFullFilesMask := FRootFolderName + VFolderNameFromRoot + '*';
    VhFind := THandle(Windows.FindFirstFileExW(PWideChar(VCurrFullFilesMask),
      FindExInfoStandard, @VFindFileData, FindExSearchLimitToDirectories, nil, 0));
    if not(VhFind = INVALID_HANDLE_VALUE) then begin
      try
        repeat
          if (VFindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then begin
            if IsNeedFolderProcess(VFolderNameFromRoot, VFindFileData.cFileName) then begin
              VPathFromRootNew := VFolderNameFromRoot + VFindFileData.cFileName;
              FFolderNamesList.AddObject(VPathFromRootNew, TObject(ADepth + 1));
            end;
          end;
        until not Windows.FindNextFileW(VhFind, VFindFileData);
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
  FMaxFolderDepth := AMaxFolderDepth;
end;

function TFoldersIteratorRecursiveByLevelsFactory.CreateIterator(
  ARootFolderName, AFolderNameFromRoot: WideString): IFileNameIterator;
begin
  Result := TFoldersIteratorRecursiveByLevels.Create(
    ARootFolderName,
    AFolderNameFromRoot,
    FMaxFolderDepth
  );
end;

end.
