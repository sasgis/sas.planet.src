unit u_VectorItemDrawConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_VectorItemDrawConfig,
  u_ConfigDataElementBase,
  u_BaseInterfacedObject;

type
  TVectorItemDrawConfigStatic = class(TBaseInterfacedObject, IVectorItemDrawConfigStatic)
  private
    FMainColor: TColor32;
    FShadowColor: TColor32;
  private
    function GetMainColor: TColor32;
    function GetShadowColor: TColor32;
  public
    constructor Create(
      const AMainColor: TColor32;
      const AShadowColor: TColor32
    );
  end;

  TVectorItemDrawConfig = class(TConfigDataElementWithStaticBase, IVectorItemDrawConfig)
  private
    FMainColor: TColor32;
    FShadowColor: TColor32;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetMainColor: TColor32;
    procedure SetMainColor(AValue: TColor32);

    function GetShadowColor: TColor32;
    procedure SetShadowColor(AValue: TColor32);

    function GetStatic: IVectorItemDrawConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_ConfigProviderHelpers;

{ TVectorItemDrawConfigStatic }

constructor TVectorItemDrawConfigStatic.Create(
  const AMainColor, AShadowColor: TColor32
);
begin
  inherited Create;
  FMainColor := AMainColor;
  FShadowColor := AShadowColor;
end;

function TVectorItemDrawConfigStatic.GetMainColor: TColor32;
begin
  Result := FMainColor;
end;

function TVectorItemDrawConfigStatic.GetShadowColor: TColor32;
begin
  Result := FShadowColor;
end;

{ TVectorItemDrawConfig }

constructor TVectorItemDrawConfig.Create;
begin
  inherited Create;
  FMainColor := clWhite32;
  FShadowColor := clBlack32;
end;

function TVectorItemDrawConfig.CreateStatic: IInterface;
var
  VResult: IVectorItemDrawConfigStatic;
begin
  VResult :=
    TVectorItemDrawConfigStatic.Create(
      FMainColor,
      FShadowColor
    );
  Result := VResult;
end;

procedure TVectorItemDrawConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FMainColor := ReadColor32(AConfigData, 'MainColor', FMainColor);
    FShadowColor := ReadColor32(AConfigData, 'ShadowColor', FShadowColor);
    SetChanged;
  end;
end;

procedure TVectorItemDrawConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  WriteColor32(AConfigData, 'MainColor', FMainColor);
  WriteColor32(AConfigData, 'ShadowColor', FShadowColor);
end;

function TVectorItemDrawConfig.GetMainColor: TColor32;
begin
  LockRead;
  try
    Result := FMainColor;
  finally
    UnlockRead;
  end;
end;

function TVectorItemDrawConfig.GetShadowColor: TColor32;
begin
  LockRead;
  try
    Result := FShadowColor;
  finally
    UnlockRead;
  end;
end;

function TVectorItemDrawConfig.GetStatic: IVectorItemDrawConfigStatic;
begin
  Result := IVectorItemDrawConfigStatic(GetStaticInternal);
end;

procedure TVectorItemDrawConfig.SetMainColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FMainColor <> AValue then begin
      FMainColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TVectorItemDrawConfig.SetShadowColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FShadowColor <> AValue then begin
      FShadowColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
