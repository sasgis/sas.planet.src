unit u_BitmapMarkerProviderSimpleConfigStatic;

interface

uses
  GR32,
  i_BitmapMarkerProviderSimpleConfig;

type
  TBitmapMarkerProviderSimpleConfigStatic = class(TInterfacedObject, IBitmapMarkerProviderSimpleConfigStatic)
  private
    FMarkerSize: Integer;
    FMarkerColor: TColor32;
    FBorderColor: TColor32;
  protected
    function GetMarkerSize: Integer;
    function GetMarkerColor: TColor32;
    function GetBorderColor: TColor32;
  public
    constructor Create(
      AMarkerSize: Integer;
      AMarkerColor: TColor32;
      ABorderColor: TColor32
    );
  end;

implementation

{ TBitmapMarkerProviderSimpleConfigStatic }

constructor TBitmapMarkerProviderSimpleConfigStatic.Create(
  AMarkerSize: Integer; AMarkerColor, ABorderColor: TColor32);
begin
  FMarkerSize :=  AMarkerSize;
  FMarkerColor := AMarkerColor;
  FBorderColor := ABorderColor;
end;

function TBitmapMarkerProviderSimpleConfigStatic.GetBorderColor: TColor32;
begin
  Result := FBorderColor;
end;

function TBitmapMarkerProviderSimpleConfigStatic.GetMarkerColor: TColor32;
begin
  Result := FMarkerColor;
end;

function TBitmapMarkerProviderSimpleConfigStatic.GetMarkerSize: Integer;
begin
  Result := FMarkerSize;
end;

end.
