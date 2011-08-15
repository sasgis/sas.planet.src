unit u_BitmapMarkerProviderChangeableFaked;

interface

uses
  i_JclNotify,
  i_BitmapMarker;

type
  TBitmapMarkerProviderChangeableFaked = class(TInterfacedObject, IBitmapMarkerProviderChangeable)
  private
    FProviderStatic: IBitmapMarkerProvider;
    FChangeNotifier: IJclNotifier;
  protected
    function GetStatic: IBitmapMarkerProvider;
    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(
      AProviderStatic: IBitmapMarkerProvider
    );
  end;

implementation

uses
  u_JclNotify;

{ TBitmapMarkerProviderChangeableFaked }

constructor TBitmapMarkerProviderChangeableFaked.Create(
  AProviderStatic: IBitmapMarkerProvider);
begin
  FProviderStatic := AProviderStatic;
  FChangeNotifier := TJclBaseNotifierFaked.Create;
end;

function TBitmapMarkerProviderChangeableFaked.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapMarkerProviderChangeableFaked.GetStatic: IBitmapMarkerProvider;
begin
  Result := FProviderStatic;
end;

end.
