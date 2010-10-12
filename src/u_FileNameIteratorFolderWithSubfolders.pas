unit u_FileNameIteratorFolderWithSubfolders;

interface

uses
  Windows,
  Classes,
  SysUtils,
  u_WideStrings,
  i_IFileNameIterator;

type
  TFileNameIteratorFolderWithSubfolders = class(TInterfacedObject, IFileNameIterator)
  private
    FRootFolderName: WideString;
    FFolderIteratorFactory: IFileNameIteratorFactory;
    FCurrentIterator: IFileNameIterator;
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
      AMaxFolderDepth: integer;
      AFolderIteratorFactory: IFileNameIteratorFactory
    );
    destructor Destroy; override;
  end;


implementation

{ TFileNameIteratorFolderWithSubfolders }

constructor TFileNameIteratorFolderWithSubfolders.Create(ARootFolderName: WideString;
  AMaxFolderDepth: integer;
  AFolderIteratorFactory: IFileNameIteratorFactory);
begin
  FRootFolderName := ARootFolderName;
  FFolderIteratorFactory := AFolderIteratorFactory;
  FFolderNamesList := TWideStringList.Create;
  FMaxFolderDepth := AMaxFolderDepth;
  FFolderNamesList.AddObject('', TObject(0));
end;

destructor TFileNameIteratorFolderWithSubfolders.Destroy;
begin
  FCurrentIterator := nil;
  FreeAndNil(FFolderNamesList);
  FFolderIteratorFactory := nil;
  inherited;
end;

function TFileNameIteratorFolderWithSubfolders.GetRootFolderName: WideString;
begin
  Result := FRootFolderName;
end;

function TFileNameIteratorFolderWithSubfolders.IsNeedFolderProcess(
  AParentFolderNameFromRoot, AFolderName: WideString): Boolean;
begin
  Result := (WideCompareStr(AFolderName, '.') <> 0) and
    (WideCompareStr(AFolderName, '..') <> 0);
end;

function TFileNameIteratorFolderWithSubfolders.Next(
  var AFileName: WideString): Boolean;
var
  VFolderName: WideString;
begin
  AFileName := '';
  Result := False;
  if FFolderNamesList.Count > 0 then begin
    repeat
      if FCurrentIterator <> nil then begin
        Result := FCurrentIterator.Next(AFileName);
        if Result then begin
          Break;
        end else begin
          FCurrentIterator := nil;
          FFolderNamesList.Delete(0);
        end;
      end else begin
        VFolderName := FFolderNamesList.Strings[0];
        ProcessAddSubFolders(VFolderName, Integer(FFolderNamesList.Objects[0]));
        FCurrentIterator := FFolderIteratorFactory.CreateIterator(FRootFolderName, VFolderName);
      end;
    until not (FFolderNamesList.Count > 0);
  end;
end;

procedure TFileNameIteratorFolderWithSubfolders.ProcessAddSubFolders(
  AFolderNameFromRoot: WideString; ADepth: Integer);
var
  VFindFileData: TWIN32FindDataW;
  VhFind: THandle;
  VCurrFullFilesMask: WideString;
  VPathFromRootNew: WideString;
begin
  if ADepth < FMaxFolderDepth then begin
    VCurrFullFilesMask := FRootFolderName + AFolderNameFromRoot + '*';
    VhFind := THandle(Windows.FindFirstFileExW(PWideChar(VCurrFullFilesMask),
      FindExInfoStandard, @VFindFileData, FindExSearchLimitToDirectories, nil, 0));
    if not(VhFind = INVALID_HANDLE_VALUE) then begin
      try
        repeat
          if (VFindFileData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then begin
            if IsNeedFolderProcess(AFolderNameFromRoot, VFindFileData.cFileName) then begin
              VPathFromRootNew := AFolderNameFromRoot + VFindFileData.cFileName + '\';
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

procedure TFileNameIteratorFolderWithSubfolders.Reset;
begin
  FFolderNamesList.Clear;
  FFolderNamesList.AddObject('', TObject(0));
  FCurrentIterator := nil;
end;

end.
