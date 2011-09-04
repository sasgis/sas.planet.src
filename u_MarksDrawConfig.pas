unit u_MarksDrawConfig;

interface

uses
  Types,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MarksDrawConfig,
  u_ConfigDataElementBase;

type
  TMarksDrawConfig = class(TConfigDataElementBase, IMarksDrawConfig)
  private
    FShowPointCaption: Boolean;
    FUseSimpleDrawOrder: Boolean;
    FOverSizeRect: TRect;
    FMagnetDraw: Boolean;

    FStatic: IMarksDrawConfigStatic;
    function CreateStatic: IMarksDrawConfigStatic;
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetShowPointCaption: Boolean;
    procedure SetShowPointCaption(AValue: Boolean);

    function GetUseSimpleDrawOrder: Boolean;
    procedure SetUseSimpleDrawOrder(AValue: Boolean);

    function GetOverSizeRect: TRect;
    procedure SetOverSizeRect(AValue: TRect);

    function GetMagnetDraw: Boolean;
    procedure SetMagnetDraw(AValue: Boolean);

    function GetStatic: IMarksDrawConfigStatic;
  public
    constructor Create();
  end;

implementation

uses
  u_MarksDrawConfigStatic;

{ TMarksDrawConfig }

constructor TMarksDrawConfig.Create;
begin
  inherited;

  FShowPointCaption := True;
  FUseSimpleDrawOrder := True;
  FOverSizeRect := Rect(256, 128, 64, 128);

  SetChanged;
end;

function TMarksDrawConfig.CreateStatic: IMarksDrawConfigStatic;
begin
  Result :=
    TMarksDrawConfigStatic.Create(
      FShowPointCaption,
      FUseSimpleDrawOrder,
      FMagnetDraw,
      FOverSizeRect
    );
end;

procedure TMarksDrawConfig.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TMarksDrawConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FShowPointCaption := AConfigData.ReadBool('ShowPointCaption', FShowPointCaption);
    FUseSimpleDrawOrder := AConfigData.ReadBool('UseSimpleDrawOrder', FUseSimpleDrawOrder);
    FMagnetDraw := AConfigData.ReadBool('MagnetDraw', FMagnetDraw);
    FOverSizeRect.Left := AConfigData.ReadInteger('OverSizeRect.Left', FOverSizeRect.Left);
    FOverSizeRect.Top := AConfigData.ReadInteger('OverSizeRect.Top', FOverSizeRect.Top);
    FOverSizeRect.Right := AConfigData.ReadInteger('OverSizeRect.Right', FOverSizeRect.Right);
    FOverSizeRect.Bottom := AConfigData.ReadInteger('OverSizeRect.Bottom', FOverSizeRect.Bottom);
    SetChanged;
  end;
end;

procedure TMarksDrawConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('ShowPointCaption', FShowPointCaption);
  AConfigData.WriteBool('UseSimpleDrawOrder', FUseSimpleDrawOrder);
  AConfigData.WriteBool('MagnetDraw', FMagnetDraw);
  AConfigData.WriteInteger('OverSizeRect.Left', FOverSizeRect.Left);
  AConfigData.WriteInteger('OverSizeRect.Top', FOverSizeRect.Top);
  AConfigData.WriteInteger('OverSizeRect.Right', FOverSizeRect.Right);
  AConfigData.WriteInteger('OverSizeRect.Bottom', FOverSizeRect.Bottom);
end;

function TMarksDrawConfig.GetOverSizeRect: TRect;
begin
  LockRead;
  try
    Result := FOverSizeRect;
  finally
    UnlockRead;
  end;
end;

function TMarksDrawConfig.GetShowPointCaption: Boolean;
begin
  LockRead;
  try
    Result := FShowPointCaption;
  finally
    UnlockRead;
  end;
end;

function TMarksDrawConfig.GetUseSimpleDrawOrder: Boolean;
begin
  LockRead;
  try
    Result := FUseSimpleDrawOrder;
  finally
    UnlockRead;
  end;
end;

function TMarksDrawConfig.GetMagnetDraw: Boolean;
begin
  LockRead;
  try
    Result := FMagnetDraw;
  finally
    UnlockRead;
  end;
end;

function TMarksDrawConfig.GetStatic: IMarksDrawConfigStatic;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

procedure TMarksDrawConfig.SetOverSizeRect(AValue: TRect);
begin
  LockWrite;
  try
    if not EqualRect(FOverSizeRect, AValue) then begin
      FOverSizeRect := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarksDrawConfig.SetShowPointCaption(AValue: Boolean);
begin
  LockWrite;
  try
    if FShowPointCaption <> AValue then begin
      FShowPointCaption := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarksDrawConfig.SetUseSimpleDrawOrder(AValue: Boolean);
begin
  LockWrite;
  try
    if FUseSimpleDrawOrder <> AValue then begin
      FUseSimpleDrawOrder := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMarksDrawConfig.SetMagnetDraw(AValue: Boolean);
begin
  LockWrite;
  try
    if FMagnetDraw <> AValue then begin
      FMagnetDraw := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
