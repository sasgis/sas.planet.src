unit u_MapZoomingConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapZoomingConfig,
  u_ConfigDataElementBase;

type
  TMapZoomingConfig = class(TConfigDataElementBase, IMapZoomingConfig)
  private
    FZoomingAtMousePos: Boolean;
    FAnimateZoom: Boolean;
    FAnimateZoomTime: Cardinal;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetZoomingAtMousePos: Boolean;
    procedure SetZoomingAtMousePos(AValue: Boolean);

    function GetAnimateZoom: Boolean;
    procedure SetAnimateZoom(AValue: Boolean);

    function GetAnimateZoomTime: Cardinal;
    procedure SetAnimateZoomTime(AValue: Cardinal);
  public
    constructor Create;
  end;

implementation

{ TMapZoomingConfig }

constructor TMapZoomingConfig.Create;
begin
  inherited;
  FZoomingAtMousePos := True;
  FAnimateZoom := True;
  FAnimateZoomTime := 500;
end;

procedure TMapZoomingConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FZoomingAtMousePos := AConfigData.ReadBool('ZoomingAtMousePos', FZoomingAtMousePos);
    FAnimateZoom := AConfigData.ReadBool('AnimateZoom', FAnimateZoom);
    FAnimateZoomTime := AConfigData.ReadInteger('AnimateZoomTime', FAnimateZoomTime);
    SetChanged;
  end;
end;

procedure TMapZoomingConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('ZoomingAtMousePos', FZoomingAtMousePos);
  AConfigData.WriteBool('AnimateZoom', FAnimateZoom);
  AConfigData.WriteInteger('AnimateZoomTime', FAnimateZoomTime);
end;

function TMapZoomingConfig.GetAnimateZoom: Boolean;
begin
  LockRead;
  try
    Result := FAnimateZoom;
  finally
    UnlockRead;
  end;
end;

function TMapZoomingConfig.GetAnimateZoomTime: Cardinal;
begin
  LockRead;
  try
    Result := FAnimateZoomTime;
  finally
    UnlockRead;
  end;
end;

function TMapZoomingConfig.GetZoomingAtMousePos: Boolean;
begin
  LockRead;
  try
    Result := FZoomingAtMousePos;
  finally
    UnlockRead;
  end;
end;

procedure TMapZoomingConfig.SetAnimateZoom(AValue: Boolean);
begin
  LockWrite;
  try
    if FAnimateZoom <> AValue then begin
      FAnimateZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapZoomingConfig.SetAnimateZoomTime(AValue: Cardinal);
begin
  LockWrite;
  try
    if FAnimateZoomTime <> AValue then begin
      FAnimateZoomTime := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapZoomingConfig.SetZoomingAtMousePos(AValue: Boolean);
begin
  LockWrite;
  try
    if FZoomingAtMousePos <> AValue then begin
      FZoomingAtMousePos := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
