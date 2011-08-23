unit u_MapTypeGUIConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapTypeGUIConfig,
  u_ConfigDataElementBase;

type
  TMapTypeGUIConfig = class(TConfigDataElementBase, IMapTypeGUIConfig)
  private
    FDefConfig: IMapTypeGUIConfigStatic;
    FName: string;
    FSortIndex: Integer;
    FSeparator: Boolean;
    FParentSubMenu: string;
    FEnabled: Boolean;
    FStatic: IMapTypeGUIConfigStatic;
    function CreateStatic: IMapTypeGUIConfigStatic;
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetName: string;
    procedure SetName(const AValue: string);

    function GetSortIndex: Integer;
    procedure SetSortIndex(const AValue: Integer);

    function GetSeparator: Boolean;
    procedure SetSeparator(const AValue: Boolean);

    function GetParentSubMenu: string;
    procedure SetParentSubMenu(const AValue: string);

    function GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);

    function GetStatic: IMapTypeGUIConfigStatic;
  public
    constructor Create(ADefConfig: IMapTypeGUIConfigStatic);
  end;

implementation

uses
  u_MapTypeGUIConfigStatic;

{ TMapTypeGUIConfig }

constructor TMapTypeGUIConfig.Create(ADefConfig: IMapTypeGUIConfigStatic);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FName := FDefConfig.Name;
  FSortIndex := FDefConfig.SortIndex;
  FSeparator := FDefConfig.Separator;
  FParentSubMenu := FDefConfig.ParentSubMenu;
  FEnabled := FDefConfig.Enabled;
end;

function TMapTypeGUIConfig.CreateStatic: IMapTypeGUIConfigStatic;
begin
  Result :=
    TMapTypeGUIConfigStatic.Create(
      FName,
      FSortIndex,
      FSeparator,
      FParentSubMenu,
      FEnabled
    );
end;

procedure TMapTypeGUIConfig.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGUIConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FName := AConfigData.ReadString('name', FName);
    FParentSubMenu := AConfigData.ReadString('ParentSubMenu', FParentSubMenu);
    FSeparator := AConfigData.ReadBool('separator', FSeparator);
    FEnabled := AConfigData.ReadBool('Enabled', FEnabled);
    FSortIndex := AConfigData.ReadInteger('pnum', FSortIndex);
    SetChanged;
  end;
end;

procedure TMapTypeGUIConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteString('name', FName);
  AConfigData.WriteString('ParentSubMenu', FParentSubMenu);
  AConfigData.WriteBool('separator', FSeparator);
  AConfigData.WriteBool('Enabled', FEnabled);
  AConfigData.WriteInteger('pnum', FSortIndex);
end;

function TMapTypeGUIConfig.GetEnabled: Boolean;
begin
  LockRead;
  try
    Result := FEnabled;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGUIConfig.GetName: string;
begin
  LockRead;
  try
    Result := FName;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGUIConfig.GetParentSubMenu: string;
begin
  LockRead;
  try
    Result := FParentSubMenu;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGUIConfig.GetSeparator: Boolean;
begin
  LockRead;
  try
    Result := FSeparator;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGUIConfig.GetSortIndex: Integer;
begin
  LockRead;
  try
    Result := FSortIndex;
  finally
    UnlockRead;
  end;
end;

function TMapTypeGUIConfig.GetStatic: IMapTypeGUIConfigStatic;
begin
  Result := FStatic;
end;

procedure TMapTypeGUIConfig.SetEnabled(const AValue: Boolean);
begin
  LockWrite;
  try
    if FEnabled <> AValue then begin
      FEnabled := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGUIConfig.SetName(const AValue: string);
begin
  LockWrite;
  try
    if FName <> AValue then begin
      FName := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGUIConfig.SetParentSubMenu(const AValue: string);
begin
  LockWrite;
  try
    if FParentSubMenu <> AValue then begin
      FParentSubMenu := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGUIConfig.SetSeparator(const AValue: Boolean);
begin
  LockWrite;
  try
    if FSeparator <> AValue then begin
      FSeparator := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapTypeGUIConfig.SetSortIndex(const AValue: Integer);
begin
  LockWrite;
  try
    if FSortIndex <> AValue then begin
      FSortIndex := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
