unit u_BitmapMarkerProviderSimpleBase;

interface

uses
  Types,
  GR32,
  i_JclNotify,
  i_BitmapMarker,
  i_BitmapMarkerProviderSimpleConfig;

type
  TBitmapMarkerProviderSimpleBase = class(TInterfacedObject, IBitmapMarkerProvider)
  private
    FConfig: IBitmapMarkerProviderSimpleConfig;
    FMarker: IBitmapMarker;
    FConfigChangeListener: IJclListener;
    FChangeNotifier: IJclNotifier;
    procedure OnConfigChange(Sender: TObject);
  protected
    property Config: IBitmapMarkerProviderSimpleConfig read FConfig;
    function CreateMarker: IBitmapMarker; virtual; abstract;
  protected
    function GetMarker: IBitmapMarker;
    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(
      AConfig: IBitmapMarkerProviderSimpleConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_JclNotify,
  u_NotifyEventListener;

{ TBitmapMarkerProviderSimpleBase }

constructor TBitmapMarkerProviderSimpleBase.Create(
  AConfig: IBitmapMarkerProviderSimpleConfig);
begin
  FConfig := AConfig;

  FConfigChangeListener := TNotifyEventListener.Create(Self.OnConfigChange);
  FConfig.GetChangeNotifier.Add(FConfigChangeListener);

  FChangeNotifier := TJclBaseNotifier.Create;
  OnConfigChange(nil);
end;

destructor TBitmapMarkerProviderSimpleBase.Destroy;
begin
  FConfig.GetChangeNotifier.Remove(FConfigChangeListener);
  FConfigChangeListener := nil;

  inherited;
end;

function TBitmapMarkerProviderSimpleBase.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapMarkerProviderSimpleBase.GetMarker: IBitmapMarker;
begin
  Result := FMarker;
end;

procedure TBitmapMarkerProviderSimpleBase.OnConfigChange(Sender: TObject);
begin
  FMarker := CreateMarker;
  FChangeNotifier.Notify(nil);
end;

end.

