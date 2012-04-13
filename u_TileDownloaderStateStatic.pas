unit u_TileDownloaderStateStatic;

interface

uses
  i_TileDownloaderState;

type
  TTileDownloaderStateStatic = class(TInterfacedObject, ITileDownloaderStateStatic)
  private
    FEnabled: Boolean;
    FReason: string;
  protected
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
