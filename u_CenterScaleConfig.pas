unit u_CenterScaleConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_CenterScaleConfig,
  u_ConfigDataElementBase;

type
  TCenterScaleConfig = class(TConfigDataElementBase, ICenterScaleConfig)
  private
    FVisible: Boolean;
    FBitmap: TCustomBitmap32;
    procedure CreateBitmap;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetBitmap: TCustomBitmap32;
    procedure SetBitmap(AValue: TCustomBitmap32);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  SysUtils;

{ TCenterScaleConfig }

constructor TCenterScaleConfig.Create;
begin
  inherited;
  FVisible := False;
  FBitmap := TCustomBitmap32.Create;
end;

procedure TCenterScaleConfig.CreateBitmap;
var
  VBitmap: TBitmap32;
  VHalfSize: TPoint;
  i: integer;
  r: Double;
  xy, xy1: TPoint;
  VSize: TPoint;
  VTextWdth: integer;
  VRadius: Integer;
  VFontSize: Integer;
  VDigitsOffset: Integer;
begin
  VBitmap := TBitmap32.Create;
  try
    VRadius := 115;
    VDigitsOffset := 20;
    VFontSize := 12;
    VBitmap.Font.Size := VFontSize;
    VTextWdth := VBitmap.TextWidth('270°');
    VSize := Point((VRadius * 2) + (VDigitsOffset * 2) + (VTextWdth * 2), (VRadius * 2) + (VDigitsOffset * 2) + (VTextWdth * 2));
    VHalfSize := Point(VSize.X div 2, VSize.Y div 2);
    VBitmap.SetSize(VSize.X, VSize.Y);
    VBitmap.Clear(0);
    VBitmap.Font.Size := VFontSize - 3;

    i := 0;
    While i < 360 do begin
      VBitmap.Font.Size := VFontSize - 3;
      if (i mod 90) = 0 then begin
        r := 0;
        VBitmap.Font.Size := VFontSize;
      end else if (i mod 45) = 0 then begin
        r := VRadius - 40;
        VBitmap.Font.Size := VFontSize - 1;
      end else begin
        r := VRadius - 10;
      end;
      xy.x := round(VHalfSize.X + VRadius * cos(i * (Pi / 180)));
      xy.y := round(VHalfSize.Y + VRadius * sin(i * (Pi / 180)));
      xy1.x := round(VHalfSize.X + r * cos(i * (Pi / 180)));
      xy1.y := round(VHalfSize.Y + r * sin(i * (Pi / 180)));
      VBitmap.LineFS(xy.x, xy.y, xy1.x, xy1.y, SetAlpha(clRed32, 180));
      if (i mod 15) = 0 then begin
        xy1.x := round(VHalfSize.X + (VRadius + VDigitsOffset) * cos(i * (Pi / 180))) - VBitmap.TextWidth(inttostr((i + 90) mod 360) + '°') div 2;
        xy1.y := round(VHalfSize.X + (VRadius + VDigitsOffset) * sin(i * (Pi / 180))) - 2 - VBitmap.Font.size div 2;
        VBitmap.RenderText(xy1.x + 1, xy1.y + 1, inttostr((i + 90) mod 360) + '°', 3, SetAlpha(clWhite32, 150));
        VBitmap.RenderText(xy1.x, xy1.y, inttostr((i + 90) mod 360) + '°', 3, SetAlpha(clBlue32, 210));
      end;
      inc(i, 5);
    end;
    FBitmap.Assign(VBitmap);
  finally
    VBitmap.Free;
  end;
end;

destructor TCenterScaleConfig.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TCenterScaleConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    CreateBitmap;
    SetChanged;
  end;
end;

procedure TCenterScaleConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
end;

function TCenterScaleConfig.GetBitmap: TCustomBitmap32;
begin
  LockRead;
  try
    Result := TCustomBitmap32.Create;
    Result.Assign(FBitmap);
  finally
    UnlockRead;
  end;
end;

function TCenterScaleConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TCenterScaleConfig.SetBitmap(AValue: TCustomBitmap32);
begin
  LockWrite;
  try
    FBitmap.Assign(AValue);
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TCenterScaleConfig.SetVisible(AValue: Boolean);
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
