unit u_MainWindowPositionConfig;

interface

uses
  Types,
  i_IConfigDataElement,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_ConfigDataElementBase,
  i_IMainWindowPosition;

type
  TMainWindowPositionConfig = class(TConfigDataElementBase ,IMainWindowPosition)
  private
    FIsFullScreen: Boolean;
    FIsMaximized: Boolean;
    FBoundsRect: TRect;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetIsFullScreen: Boolean;
    function GetIsMaximized: Boolean;
    function GetBoundsRect: TRect;
    procedure SetFullScreen;
    procedure SetMaximized;
    procedure SetNormalWindow;
    procedure SetWindowPosition(ARect: TRect);
  public
    constructor Create(AStartRect: TRect);
  end;

implementation

{ TMainWindowPositionConfig }

constructor TMainWindowPositionConfig.Create(AStartRect: TRect);
begin
  FBoundsRect := AStartRect;
  FIsFullScreen := False;
  FIsMaximized := False;
end;

procedure TMainWindowPositionConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FBoundsRect := Bounds(
      AConfigData.ReadInteger('Left', FBoundsRect.Left),
      AConfigData.ReadInteger('Top', FBoundsRect.Top),
      AConfigData.ReadInteger('Width', FBoundsRect.Right -  FBoundsRect.Top),
      AConfigData.ReadInteger('Height', FBoundsRect.Bottom - FBoundsRect.Top)
    );
    FIsMaximized := AConfigData.ReadBool('Maximized', FIsMaximized);
    FIsFullScreen := AConfigData.ReadBool('FullScreen', FIsFullScreen);
  end;
end;

procedure TMainWindowPositionConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('FullScreen', FIsFullScreen);
  AConfigData.WriteBool('Maximized', FIsMaximized);
  AConfigData.WriteInteger('Left', FBoundsRect.Left);
  AConfigData.WriteInteger('Top', FBoundsRect.Top);
  AConfigData.WriteInteger('Width', FBoundsRect.Right - FBoundsRect.Left);
  AConfigData.WriteInteger('Height', FBoundsRect.Bottom - FBoundsRect.Top);
end;

function TMainWindowPositionConfig.GetBoundsRect: TRect;
begin
  LockRead;
  try
    Result := FBoundsRect;
  finally
    UnlockRead;
  end;
end;

function TMainWindowPositionConfig.GetIsFullScreen: Boolean;
begin
  LockRead;
  try
    Result := FIsFullScreen;
  finally
    UnlockRead;
  end;
end;

function TMainWindowPositionConfig.GetIsMaximized: Boolean;
begin
  LockRead;
  try
    Result := FIsMaximized;
  finally
    UnlockRead;
  end;
end;

procedure TMainWindowPositionConfig.SetFullScreen;
begin
  LockWrite;
  try
    FIsFullScreen := True;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetMaximized;
begin
  LockWrite;
  try
    FIsFullScreen := False;
    FIsMaximized := True;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetNormalWindow;
begin
  LockWrite;
  try
    FIsFullScreen := False;
    FIsMaximized := False;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetWindowPosition(ARect: TRect);
begin
  LockWrite;
  try
    FIsFullScreen := False;
    FIsMaximized := False;
    FBoundsRect := ARect;
  finally
    UnlockWrite;
  end;
end;

end.
