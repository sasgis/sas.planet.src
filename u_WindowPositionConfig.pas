unit u_WindowPositionConfig;

interface

uses
  Types,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase,
  i_WindowPositionConfig;

type
  TWindowPositionConfig = class(TConfigDataElementBase, IWindowPositionConfig)
  private
    FBoundsRect: TRect;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetBoundsRect: TRect;
    procedure SetWindowPosition(const ARect: TRect);
  public
    constructor Create;
  end;


implementation

{ TWindowPositionConfig }

constructor TWindowPositionConfig.Create;
begin
  inherited Create;
  FBoundsRect := Rect(0, 0, 0, 0);
end;

procedure TWindowPositionConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FBoundsRect := Bounds(
      AConfigData.ReadInteger('Left', FBoundsRect.Left),
      AConfigData.ReadInteger('Top', FBoundsRect.Top),
      AConfigData.ReadInteger('Width', FBoundsRect.Right - FBoundsRect.Top),
      AConfigData.ReadInteger('Height', FBoundsRect.Bottom - FBoundsRect.Top)
    );
    SetChanged;
  end;
end;

procedure TWindowPositionConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('Left', FBoundsRect.Left);
  AConfigData.WriteInteger('Top', FBoundsRect.Top);
  AConfigData.WriteInteger('Width', FBoundsRect.Right - FBoundsRect.Left);
  AConfigData.WriteInteger('Height', FBoundsRect.Bottom - FBoundsRect.Top);
end;

function TWindowPositionConfig.GetBoundsRect: TRect;
begin
  LockRead;
  try
    Result := FBoundsRect;
  finally
    UnlockRead;
  end;
end;

procedure TWindowPositionConfig.SetWindowPosition(const ARect: TRect);
begin
  LockWrite;
  try
    if not EqualRect(FBoundsRect, ARect) then begin
      FBoundsRect := ARect;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
