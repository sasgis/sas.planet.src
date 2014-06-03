unit u_FileNameIteratorFolderWithSubfolders;

interface

uses
  i_FileNameIterator;

type
  TFileNameIteratorFolderWithSubfolders = class(TInterfacedObject, IFileNameIterator)
  private
    FFilesInFolderIteratorFactory: IFileNameIteratorFactory;
    FFoldersIterator: IFileNameIterator;
    FCurrentIterator: IFileNameIterator;
    FEOI: Boolean;
  protected
    function IsNeedFolderProcess(const AParentFolderNameFromRoot, AFolderName: WideString): Boolean; virtual;
  protected
    function GetRootFolderName: WideString;
    function Next(var AFileName: WideString): Boolean;
    procedure Reset;
  public
    constructor Create(
      const ARootFolderName: WideString;
      const AFolderNameFromRoot: WideString;
      const AFolderIteratorFactory: IFileNameIteratorFactory;
      const AFilesInFolderIteratorFactory: IFileNameIteratorFactory
    );
    destructor Destroy; override;
  end;

  TFileNameIteratorFolderWithSubfoldersFactory = class(TInterfacedObject, IFileNameIteratorFactory)
  private
    FFolderIteratorFactory: IFileNameIteratorFactory;
    FFilesInFolderIteratorFactory: IFileNameIteratorFactory;
  protected
    function CreateIterator(
      const ARootFolderName: WideString;
      const AFolderNameFromRoot: WideString
    ): IFileNameIterator;
  public
    constructor Create(
      const AFolderIteratorFactory: IFileNameIteratorFactory;
      const AFilesInFolderIteratorFactory: IFileNameIteratorFactory
    );
  end;

implementation

{ TFileNameIteratorFolderWithSubfolders }

constructor TFileNameIteratorFolderWithSubfolders.Create(
  const ARootFolderName: WideString;
  const AFolderNameFromRoot: WideString;
  const AFolderIteratorFactory: IFileNameIteratorFactory;
  const AFilesInFolderIteratorFactory: IFileNameIteratorFactory
);
begin
  FFilesInFolderIteratorFactory := AFilesInFolderIteratorFactory;
  FFoldersIterator := AFolderIteratorFactory.CreateIterator(ARootFolderName, AFolderNameFromRoot);
  FCurrentIterator := nil;
  FEOI := False;
end;

destructor TFileNameIteratorFolderWithSubfolders.Destroy;
begin
  FCurrentIterator := nil;
  FFoldersIterator := nil;
  FFilesInFolderIteratorFactory := nil;
  inherited;
end;

function TFileNameIteratorFolderWithSubfolders.GetRootFolderName: WideString;
begin
  Result := FFoldersIterator.GetRootFolderName;
end;

function TFileNameIteratorFolderWithSubfolders.IsNeedFolderProcess(
  const AParentFolderNameFromRoot, AFolderName: WideString
): Boolean;
begin
  Result := True;
end;

function TFileNameIteratorFolderWithSubfolders.Next(
  var AFileName: WideString): Boolean;
var
  VFolderName: WideString;
begin
  AFileName := '';
  Result := False;
  if not FEOI then begin
    repeat
      if FCurrentIterator <> nil then begin
        Result := FCurrentIterator.Next(AFileName);
        if Result then begin
          Break;
        end else begin
          FCurrentIterator := nil;
        end;
      end else begin
        if FFoldersIterator.Next(VFolderName) then begin
          FCurrentIterator := FFilesInFolderIteratorFactory.CreateIterator(
            FFoldersIterator.GetRootFolderName,
            VFolderName
          );
        end else begin
          FEOI := True;
        end;
      end;
    until FEOI;
  end;
end;

procedure TFileNameIteratorFolderWithSubfolders.Reset;
begin
  FFoldersIterator.Reset;
  FCurrentIterator := nil;
  FEOI := False;
end;

{ TFileNameIteratorFolderWithSubfoldersFactory }

constructor TFileNameIteratorFolderWithSubfoldersFactory.Create(
  const AFolderIteratorFactory, AFilesInFolderIteratorFactory: IFileNameIteratorFactory
);
begin
  FFolderIteratorFactory := AFolderIteratorFactory;
  FFilesInFolderIteratorFactory := AFilesInFolderIteratorFactory;
end;

function TFileNameIteratorFolderWithSubfoldersFactory.CreateIterator(
  const ARootFolderName, AFolderNameFromRoot: WideString
): IFileNameIterator;
begin
  Result := TFileNameIteratorFolderWithSubfolders.Create(
    ARootFolderName,
    AFolderNameFromRoot,
    FFolderIteratorFactory,
    FFilesInFolderIteratorFactory
  );
end;

end.
