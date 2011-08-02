unit u_MapVersionInfo;

interface

uses
  i_MapVersionInfo;

type
  TMapVersionInfo = class(TInterfacedObject, IMapVersionInfo)
  private
    FVersion: Variant;
  protected
    function GetVersion: Variant;
  public
    constructor Create(
      AVersion: Variant
    );
  end;

implementation

{ TMapVersionInfo }

constructor TMapVersionInfo.Create(AVersion: Variant);
begin
  FVersion := AVersion;
end;

function TMapVersionInfo.GetVersion: Variant;
begin
  Result := FVersion;
end;

end.
