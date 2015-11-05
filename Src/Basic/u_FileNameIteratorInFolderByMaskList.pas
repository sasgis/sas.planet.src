unit u_FileNameIteratorInFolderByMaskList;

interface

uses
  SysUtils,
  Classes,
  i_FileNameIterator;

type
  TFileNameIteratorInFolderByMaskList = class(TInterfacedObject, IFileNameIterator)
  private
    FRootFolderName: string;
    FFolderNameFromRoot: string;
    FFileMasksList: TStrings;
    FFilesOnly: Boolean;
    FCurrentMaskIndex: Integer;
    FCurrentFolderIterator: IFileNameIterator;
  protected
    function GetRootFolderName: string;
    function Next(var AFileName: string): Boolean;
    procedure Reset;
  public
    constructor Create(
      const ARootFolderName: string;
      const AFolderNameFromRoot: string;
      AFileMasksList: TStrings;
      AFilesOnly: Boolean
    );
    destructor Destroy; override;
  end;

  TFileNameIteratorInFolderByMaskListFactory = class(TInterfacedObject, IFileNameIteratorFactory)
  private
    FFileMasksList: TStrings;
    FFilesOnly: Boolean;
    function CreateIterator(
      const ARootFolderName: string;
      const AFolderNameFromRoot: string
    ): IFileNameIterator;
  public
    constructor Create(
      AFileMasksList: TStrings;
      AFilesOnly: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_FileNameIteratorInFolderByMask;

{ TFileNameIteratorInFolderByMaskList }

constructor TFileNameIteratorInFolderByMaskList.Create(
  const ARootFolderName, AFolderNameFromRoot: string;
  AFileMasksList: TStrings;
  AFilesOnly: Boolean
);
begin
  inherited Create;
  FRootFolderName := ARootFolderName;
  FFolderNameFromRoot := AFolderNameFromRoot;
  FFilesOnly := AFilesOnly;
  FFileMasksList := TStringList.Create;
  FFileMasksList.Assign(AFileMasksList);
  FCurrentMaskIndex := -1;
end;

destructor TFileNameIteratorInFolderByMaskList.Destroy;
begin
  FCurrentFolderIterator := nil;
  FreeAndNil(FFileMasksList);
  inherited;
end;

function TFileNameIteratorInFolderByMaskList.GetRootFolderName: string;
begin
  Result := FRootFolderName;
end;

function TFileNameIteratorInFolderByMaskList.Next(
  var AFileName: string): Boolean;
begin
  AFileName := '';
  Result := False;
  if FCurrentMaskIndex < FFileMasksList.Count then begin
    repeat
      if FCurrentFolderIterator <> nil then begin
        Result := FCurrentFolderIterator.Next(AFileName);
        if Result then begin
          Break;
        end else begin
          FCurrentFolderIterator := nil;
        end;
      end else begin
        Inc(FCurrentMaskIndex);
        if FCurrentMaskIndex < FFileMasksList.Count then begin
          FCurrentFolderIterator := TFileNameIteratorInFolderByMask.Create(
            FRootFolderName,
            FFolderNameFromRoot,
            FFileMasksList.Strings[FCurrentMaskIndex],
            FFilesOnly
          );
        end;
      end;
    until not (FCurrentMaskIndex < FFileMasksList.Count);
  end;
end;

procedure TFileNameIteratorInFolderByMaskList.Reset;
begin
  FCurrentMaskIndex := -1;
  FCurrentFolderIterator := nil;
end;

{ TFileNameIteratorInFolderByMaskListFactory }

constructor TFileNameIteratorInFolderByMaskListFactory.Create(
  AFileMasksList: TStrings;
  AFilesOnly: Boolean
);
begin
  inherited Create;
  FFilesOnly := AFilesOnly;
  FFileMasksList := TStringList.Create;
  FFileMasksList.Assign(AFileMasksList);
end;

function TFileNameIteratorInFolderByMaskListFactory.CreateIterator(
  const ARootFolderName, AFolderNameFromRoot: string
): IFileNameIterator;
begin
  if FFileMasksList.Count = 1 then begin
    Result := TFileNameIteratorInFolderByMask.Create(ARootFolderName, AFolderNameFromRoot, FFileMasksList.Strings[0], FFilesOnly);
  end else begin
    Result := TFileNameIteratorInFolderByMaskList.Create(ARootFolderName, AFolderNameFromRoot, FFileMasksList, FFilesOnly);
  end;
end;

destructor TFileNameIteratorInFolderByMaskListFactory.Destroy;
begin
  FreeAndNil(FFileMasksList);
  inherited;
end;

end.
