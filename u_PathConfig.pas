unit u_PathConfig;

interface

uses
  Windows,
  i_JclNotify,
  i_ConfigDataElement,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_PathConfig,
  u_ConfigDataElementBase;

type
  TPathConfig = class(TConfigDataElementBase, IPathConfig)
  private
    FStoreIdent: string;
    FDefaultPath: string;
    FBasePathConfig: IPathConfig;
    FPath: string;
    FIsRelative: Boolean;
    FFullPath: string;

    FBasePathListener: IJclListener;
    procedure OnBasePathChange;
    procedure _UpdateFullPath;
    function IsRelativePath(APath: string): Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  private
    function GetDefaultPath: string;
    function GetBasePathConfig: IPathConfig;
    function GetPath: string;
    procedure SetPath(AValue: string);
    function GetIsRelative: Boolean;
    function GetFullPath: string;
  public
    constructor Create(
      AStoreIdent: string;
      ADefaultPath: string;
      ABasePathConfig: IPathConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  ShLwApi,
  u_NotifyEventListener;

{ TPathConfig }

constructor TPathConfig.Create(AStoreIdent, ADefaultPath: string;
  ABasePathConfig: IPathConfig);
begin
  inherited Create;
  FStoreIdent := AStoreIdent;
  FDefaultPath := ADefaultPath;
  FBasePathConfig := ABasePathConfig;

  if FBasePathConfig <> nil then begin
    FBasePathListener := TNotifyNoMmgEventListener.Create(Self.OnBasePathChange);
  end;

  FPath := FDefaultPath;
  FIsRelative := IsRelativePath(FPath);
  if FIsRelative then begin
    if FBasePathConfig <> nil then begin
      FBasePathConfig.ChangeNotifier.Add(FBasePathListener);
    end;
  end;
  _UpdateFullPath;
end;

destructor TPathConfig.Destroy;
begin
  if FBasePathConfig <> nil then begin
    FBasePathConfig.ChangeNotifier.Remove(FBasePathListener);
    FBasePathListener := nil;
    FBasePathConfig := nil;
  end;
  inherited;
end;

procedure TPathConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    if FStoreIdent <> '' then begin
      SetPath(AConfigData.ReadString(FStoreIdent, FPath));
    end;
  end;
end;

procedure TPathConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  if FStoreIdent <> '' then begin
    AConfigData.WriteString(FStoreIdent, FPath);
  end;
end;

function TPathConfig.GetBasePathConfig: IPathConfig;
begin
  Result := FBasePathConfig;
end;

function TPathConfig.GetDefaultPath: string;
begin
  Result := FDefaultPath;
end;

function TPathConfig.GetFullPath: string;
begin
  LockRead;
  try
    Result := FFullPath;
  finally
    UnlockRead;
  end;
end;

function TPathConfig.GetIsRelative: Boolean;
begin
  LockRead;
  try
    Result := FIsRelative;
  finally
    UnlockRead;
  end;
end;

function TPathConfig.GetPath: string;
begin
  LockRead;
  try
    Result := FPath;
  finally
    UnlockRead;
  end;
end;

function TPathConfig.IsRelativePath(APath: string): Boolean;
begin
  Result := PathIsRelative(PChar(APath));
end;

procedure TPathConfig.OnBasePathChange;
begin
  LockWrite;
  try
    _UpdateFullPath;
  finally
    UnlockWrite;
  end;
end;

procedure TPathConfig.SetPath(AValue: string);
var
  VNewIsRelative: Boolean;
  VOldIsRelative: Boolean;
begin
  VNewIsRelative := IsRelativePath(AValue);
  LockWrite;
  try
    VOldIsRelative := FIsRelative;
    if VOldIsRelative and not VNewIsRelative then begin
      if FBasePathConfig <> nil then begin
        FBasePathConfig.ChangeNotifier.Remove(FBasePathListener);
      end;
    end;
    FIsRelative := VNewIsRelative;
    if FPath <> AValue then begin
      FPath := AValue;
      _UpdateFullPath;
    end;
    
    if not VOldIsRelative and VNewIsRelative then begin
      if FBasePathConfig <> nil then begin
        FBasePathConfig.ChangeNotifier.Add(FBasePathListener);
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPathConfig._UpdateFullPath;
  function RelativeToAbsolutePath(const ABasePath, ARelativePath: string): string;
  begin
    SetLength(Result, MAX_PATH);
    PathCombine(@Result[1], PChar(ABasePath), PChar(ARelativePath));
    SetLength(Result, StrLen(@Result[1]));
  end;
var
  VBasePath: string;
  VPath: string;
  VFullPath: string;
begin
  if FIsRelative then begin
    VPath := FPath;
    if VPath = '' then begin
      VPath := '.';
    end;
    if FBasePathConfig <> nil then begin
      VBasePath := IncludeTrailingPathDelimiter(FBasePathConfig.FullPath);
      VFullPath := RelativeToAbsolutePath(VBasePath, VPath);
    end else begin
      VFullPath := ExpandFileName(VPath);
    end;
  end else begin
    VFullPath := FPath;
  end;
  if FFullPath <> VFullPath then begin
    FFullPath := VFullPath;
    SetChanged;
  end;
end;

end.
