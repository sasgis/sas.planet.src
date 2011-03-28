unit u_StatBarConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_StatBarConfig,
  u_ConfigDataElementBase;

type
  TStatBarConfig = class(TConfigDataElementBase, IStatBarConfig)
  private
    FVisible: Boolean;
    FHeight: Integer;
    FMinUpdateTickCount: Cardinal;
    FBgColor: TColor32;
    FTextColor: TColor32;
    FFontName: string;
    FFontSize: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);

    function GetMinUpdateTickCount: Cardinal;
    procedure SetMinUpdateTickCount(AValue: Cardinal);

    function GetBgColor: TColor32;
    procedure SetBgColor(AValue: TColor32);

    function GetTextColor: TColor32;
    procedure SetTextColor(AValue: TColor32);

    function GetFontName: string;
    procedure SetFontName(AValue: string);

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TMapLayerStatBarConfig }

constructor TStatBarConfig.Create;
begin
  inherited;
  FVisible := True;
  FHeight := 17;
  FMinUpdateTickCount := 200;
  FBgColor := SetAlpha(clWhite32, 160);
  FTextColor := clBlack32;
  FFontName := 'arial';
  FFontSize := 10;
end;

procedure TStatBarConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    FHeight := AConfigData.ReadInteger('Height', FHeight);
    FMinUpdateTickCount := AConfigData.ReadInteger('MinRedrawTime', FMinUpdateTickCount);
    FBgColor := ReadColor32(AConfigData, 'BackgroundColor', FBgColor);
    FTextColor := ReadColor32(AConfigData, 'TextColor', FTextColor);
    FFontName := AConfigData.ReadString('FontName', FFontName);
    FFontSize := AConfigData.ReadInteger('FontSize', FFontSize)
  end;
end;

procedure TStatBarConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
  AConfigData.WriteInteger('Height', FHeight);
  AConfigData.WriteInteger('MinRedrawTime', FMinUpdateTickCount);
  WriteColor32(AConfigData, 'BackgroundColor', FBgColor);
  WriteColor32(AConfigData, 'TextColor', FTextColor);
  AConfigData.WriteString('FontName', FFontName);
  AConfigData.WriteInteger('FontSize', FFontSize)
end;

function TStatBarConfig.GetBgColor: TColor32;
begin
  LockRead;
  try
    Result := FBgColor;
  finally
    UnlockRead;
  end;
end;

function TStatBarConfig.GetFontName: string;
begin
  LockRead;
  try
    Result := FFontName;
  finally
    UnlockRead;
  end;
end;

function TStatBarConfig.GetFontSize: Integer;
begin
  LockRead;
  try
    Result := FFontSize;
  finally
    UnlockRead;
  end;
end;

function TStatBarConfig.GetHeight: Integer;
begin
  LockRead;
  try
    Result := FHeight;
  finally
    UnlockRead;
  end;
end;

function TStatBarConfig.GetMinUpdateTickCount: Cardinal;
begin
  LockRead;
  try
    Result := FMinUpdateTickCount;
  finally
    UnlockRead;
  end;
end;

function TStatBarConfig.GetTextColor: TColor32;
begin
  LockRead;
  try
    Result := FTextColor;
  finally
    UnlockRead;
  end;
end;

function TStatBarConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TStatBarConfig.SetBgColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FBgColor <> AValue then begin
      FBgColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TStatBarConfig.SetFontName(AValue: string);
begin
  LockWrite;
  try
    if FFontName <> AValue then begin
      FFontName := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TStatBarConfig.SetFontSize(AValue: Integer);
begin
  LockWrite;
  try
    if FFontSize <> AValue then begin
      FFontSize := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TStatBarConfig.SetHeight(AValue: Integer);
begin
  LockWrite;
  try
    if FHeight <> AValue then begin
      FHeight := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TStatBarConfig.SetMinUpdateTickCount(AValue: Cardinal);
begin
  LockWrite;
  try
    if FMinUpdateTickCount <> AValue then begin
      FMinUpdateTickCount := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TStatBarConfig.SetTextColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FTextColor <> AValue then begin
      FTextColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TStatBarConfig.SetVisible(AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
