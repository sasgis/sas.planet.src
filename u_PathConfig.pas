unit u_PathConfig;

interface

uses
  Windows,
  i_Notifier,
  i_Listener,
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

    FBasePathListener: IListener;
    procedure OnBasePathChange;
    procedure _UpdateFullPath;
    function IsRelativePath(const APath: string): Boolean;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetDefaultPath: string;
    function GetBasePathConfig: IPathConfig;
    procedure SetBasePathConfig(const AValue: IPathConfig);
    function GetPath: string;
    procedure SetPath(const AValue: string);
    function GetIsRelative: Boolean;
    function GetFullPath: string;
  public
    constructor Create(
      const AStoreIdent: string;
      const ADefaultPath: string;
      const ABasePathConfig: IPathConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  ShLwApi,
  u_ListenerByEvent;

{ TPathConfig }

constructor TPathConfig.Create(
  const AStoreIdent, ADefaultPath: string;
  const ABasePathConfig: IPathConfig
);
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
  if Assigned(FBasePathConfig) and Assigned(FBasePathListener) then begin
    FBasePathConfig.ChangeNotifier.Remove(FBasePathListener);
    FBasePathListener := nil;
    FBasePathConfig := nil;
  end;
  inherited;
end;

procedure TPathConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    if FStoreIdent <> '' then begin
      SetPath(AConfigData.ReadString(FStoreIdent, FPath));
    end;
  end;
end;

procedure TPathConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  if FStoreIdent <> '' then begin
    AConfigData.WriteString(FStoreIdent, FPath);
  end;
end;

function TPathConfig.GetBasePathConfig: IPathConfig;
begin
  LockRead;
  try
    Result := FBasePathConfig;
  finally
    UnlockRead;
  end;
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

function TPathConfig.IsRelativePath(const APath: string): Boolean;
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

procedure TPathConfig.SetBasePathConfig(const AValue: IPathConfig);
begin
  LockWrite;
  try
    if FIsRelative then begin
      if FBasePathConfig <> nil then begin
        FBasePathConfig.ChangeNotifier.Remove(FBasePathListener);
      end;
    end;
    FBasePathConfig := AValue;
    _UpdateFullPath;
    if FIsRelative then begin
      if FBasePathConfig <> nil then begin
        if FBasePathListener = nil then begin
          FBasePathListener := TNotifyNoMmgEventListener.Create(Self.OnBasePathChange);
        end;
        FBasePathConfig.ChangeNotifier.Add(FBasePathListener);
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPathConfig.SetPath(const AValue: string);
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
    SetLength(Result, StrLen(PChar(Result)));
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
