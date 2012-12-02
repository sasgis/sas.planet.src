unit u_UseTilePrevZoomConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_UseTilePrevZoomConfig,
  u_ConfigDataElementBase;

type
  TUseTilePrevZoomConfig = class(TConfigDataElementWithStaticBase, IUseTilePrevZoomConfig)
  private
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(const AValue: Boolean);

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(const AValue: Boolean);

    function GetStatic: IUseTilePrevZoomTileConfigStatic;
  public
    constructor Create;
  end;

implementation

uses
  u_BaseInterfacedObject;
  
type
  TUseTilePrevZoomTileConfigStatic = class(TBaseInterfacedObject, IUseTilePrevZoomTileConfigStatic)
  private
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
  private
    function GetUsePrevZoomAtMap: Boolean;
    function GetUsePrevZoomAtLayer: Boolean;
  public
    constructor Create(
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean
    );
  end;

{ TUseTilePrevZoomTileConfigStatic }

constructor TUseTilePrevZoomTileConfigStatic.Create(AUsePrevZoomAtMap,
  AUsePrevZoomAtLayer: Boolean);
begin
  inherited Create;
  FUsePrevZoomAtMap := AUsePrevZoomAtMap;
  FUsePrevZoomAtLayer := AUsePrevZoomAtLayer;
end;

function TUseTilePrevZoomTileConfigStatic.GetUsePrevZoomAtLayer: Boolean;
begin
  Result := FUsePrevZoomAtLayer;
end;

function TUseTilePrevZoomTileConfigStatic.GetUsePrevZoomAtMap: Boolean;
begin
  Result := FUsePrevZoomAtMap;
end;

{ TUseTilePrevZoomConfig }

constructor TUseTilePrevZoomConfig.Create;
begin
  inherited Create;
  FUsePrevZoomAtMap := True;
  FUsePrevZoomAtLayer := True;
end;

function TUseTilePrevZoomConfig.CreateStatic: IInterface;
var
  VStatic: IUseTilePrevZoomTileConfigStatic;
begin
  VStatic :=
    TUseTilePrevZoomTileConfigStatic.Create(
      FUsePrevZoomAtMap,
      FUsePrevZoomAtLayer
    );
  Result := VStatic;
end;

procedure TUseTilePrevZoomConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetUsePrevZoomAtMap(AConfigData.ReadBool('UsePrevZoomAtMap', FUsePrevZoomAtMap));
    SetUsePrevZoomAtLayer(AConfigData.ReadBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer));
  end;
end;

procedure TUseTilePrevZoomConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
  AConfigData.WriteBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
end;

function TUseTilePrevZoomConfig.GetStatic: IUseTilePrevZoomTileConfigStatic;
begin
  Result := IUseTilePrevZoomTileConfigStatic(GetStaticInternal);
end;

function TUseTilePrevZoomConfig.GetUsePrevZoomAtLayer: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtLayer;
  finally
    UnlockRead;
  end;
end;

function TUseTilePrevZoomConfig.GetUsePrevZoomAtMap: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtMap;
  finally
    UnlockRead;
  end;
end;

procedure TUseTilePrevZoomConfig.SetUsePrevZoomAtLayer(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtLayer <> AValue then begin
      FUsePrevZoomAtLayer := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TUseTilePrevZoomConfig.SetUsePrevZoomAtMap(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtMap <> AValue then begin
      FUsePrevZoomAtMap := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
