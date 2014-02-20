unit u_TileDownloaderStateStatic;

interface

uses
  i_TileDownloaderState,
  u_BaseInterfacedObject;

type
  TTileDownloaderStateStatic = class(TBaseInterfacedObject, ITileDownloaderStateStatic)
  private
    FEnabled: Boolean;
    FReason: string;
  private
    function GetEnabled: Boolean;
    function GetDisableReason: string;
  public
    constructor Create(
      AEnabled: Boolean;
      const AReason: string
    );
  end;

implementation

{ TTileDownloaderStateStatic }

constructor TTileDownloaderStateStatic.Create(
  AEnabled: Boolean;
  const AReason: string
);
begin
  inherited Create;
  FEnabled := AEnabled;
  FReason := AReason;
end;

function TTileDownloaderStateStatic.GetDisableReason: string;
begin
  Result := FReason;
end;

function TTileDownloaderStateStatic.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

end.
