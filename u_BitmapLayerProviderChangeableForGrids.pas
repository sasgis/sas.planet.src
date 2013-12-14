unit u_BitmapLayerProviderChangeableForGrids;

interface

uses
  i_MapLayerGridsConfig,
  i_Bitmap32StaticFactory,
  i_ValueToStringConverter,
  i_BitmapLayerProvider,
  i_ListenerNotifierLinksList,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForGrids = class(TBitmapLayerProviderChangeableBase)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FConfig: IMapLayerGridsConfig;
    FValueToStringConverterConfig: IValueToStringConverterConfig;

    procedure OnConfigChange;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32StaticFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      const AConfig: IMapLayerGridsConfig
    );
  end;

implementation

uses
  GR32,
  u_ListenerByEvent,
  u_BitmapLayerProviderComplex,
  u_BitmapLayerProviderGridGenshtab,
  u_BitmapLayerProviderGridDegree,
  u_BitmapLayerProviderGridTiles;

{ TBitmapLayerProviderChangeableForGrids }

constructor TBitmapLayerProviderChangeableForGrids.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  const AConfig: IMapLayerGridsConfig);
begin
  inherited Create;
  FBitmapFactory := ABitmapFactory;
  FConfig := AConfig;
  FValueToStringConverterConfig := AValueToStringConverterConfig;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FConfig.TileGrid.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FConfig.GenShtabGrid.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FConfig.DegreeGrid.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FValueToStringConverterConfig.ChangeNotifier
  );
end;

function TBitmapLayerProviderChangeableForGrids.CreateStatic: IInterface;
var
  VVisible: Boolean;
  VColor: TColor32;
  VUseRelativeZoom: Boolean;
  VZoom: Integer;
  VShowText: Boolean;
  VShowLines: Boolean;
  VScale: Integer;
  VScaleDegree: Double;
  VProvider: IBitmapLayerProvider;
  VResult: IBitmapLayerProvider;
begin
  VResult := nil;
  FConfig.TileGrid.LockRead;
  try
    VVisible := FConfig.TileGrid.Visible;
    VColor := FConfig.TileGrid.GridColor;
    VUseRelativeZoom := FConfig.TileGrid.UseRelativeZoom;
    VZoom := FConfig.TileGrid.Zoom;
    VShowText := FConfig.TileGrid.ShowText;
    VShowLines := True;
  finally
    FConfig.TileGrid.UnlockRead;
  end;
  if VVisible then begin
    VResult :=
      TBitmapLayerProviderGridTiles.Create(
        FBitmapFactory,
        VColor,
        VUseRelativeZoom,
        VZoom,
        VShowText,
        VShowLines
      );
  end;
  FConfig.GenShtabGrid.LockRead;
  try
    VVisible := FConfig.GenShtabGrid.Visible;
    VColor := FConfig.GenShtabGrid.GridColor;
    VScale := FConfig.GenShtabGrid.Scale;
    VShowText := FConfig.GenShtabGrid.ShowText;
    VShowLines := True;
  finally
    FConfig.GenShtabGrid.UnlockRead;
  end;
  if VVisible then begin
    VProvider :=
      TBitmapLayerProviderGridGenshtab.Create(
        FBitmapFactory,
        VColor,
        VScale,
        VShowText,
        VShowLines
      );

    if VResult <> nil then begin
      VResult := TBitmapLayerProviderComplex.Create(FBitmapFactory, VResult, VProvider);
    end else begin
      VResult := VProvider;
    end;
  end;
  FConfig.DegreeGrid.LockRead;
  try
    VVisible := FConfig.DegreeGrid.Visible;
    VColor := FConfig.DegreeGrid.GridColor;
    VScaleDegree := FConfig.DegreeGrid.Scale;
    VShowText := FConfig.DegreeGrid.ShowText;
    VShowLines := True;
  finally
    FConfig.DegreeGrid.UnlockRead;
  end;
  if VVisible then begin
    VProvider :=
      TBitmapLayerProviderGridDegree.Create(
        FBitmapFactory,
        VColor,
        VScaleDegree,
        VShowText,
        VShowLines,
        FValueToStringConverterConfig.GetStatic
      );
    if VResult <> nil then begin
      VResult := TBitmapLayerProviderComplex.Create(FBitmapFactory, VResult, VProvider);
    end else begin
      VResult := VProvider;
    end;
  end;
  Result := VResult;
end;

procedure TBitmapLayerProviderChangeableForGrids.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
