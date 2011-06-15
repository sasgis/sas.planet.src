unit u_TileDownloaderConfigStatic;

interface

uses
  i_InetConfig,
  i_TileDownloaderConfig;

type
  TTileDownloaderConfigStatic = class(TInterfacedObject, ITileDownloaderConfigStatic)
  private
    FInetConfigStatic: IInetConfigStatic;
    FWaitInterval: Cardinal;
    FIgnoreMIMEType: Boolean;
    FExpectedMIMETypes: string;
    FDefaultMIMEType: string;
  protected
    function GetInetConfigStatic: IInetConfigStatic;
    function GetWaitInterval: Cardinal;
    function GetIgnoreMIMEType: Boolean;
    function GetExpectedMIMETypes: string;
    function GetDefaultMIMEType: string;
  public
    constructor Create(
      AInetConfigStatic: IInetConfigStatic;
      AWaitInterval: Cardinal;
      AIgnoreMIMEType: Boolean;
      AExpectedMIMETypes: string;
      ADefaultMIMEType: string
    );
  end;

implementation

{ TTileDownloaderConfigStatic }

constructor TTileDownloaderConfigStatic.Create(
  AInetConfigStatic: IInetConfigStatic;
  AWaitInterval: Cardinal;
  AIgnoreMIMEType: Boolean;
  AExpectedMIMETypes, ADefaultMIMEType: string
);
begin
  FInetConfigStatic := AInetConfigStatic;
  FWaitInterval := AWaitInterval;
  FIgnoreMIMEType := AIgnoreMIMEType;
  FExpectedMIMETypes := AExpectedMIMETypes;
  FDefaultMIMEType := ADefaultMIMEType;
end;

function TTileDownloaderConfigStatic.GetDefaultMIMEType: string;
begin
  Result := FDefaultMIMEType;
end;

function TTileDownloaderConfigStatic.GetExpectedMIMETypes: string;
begin
  Result := FExpectedMIMETypes;
end;

function TTileDownloaderConfigStatic.GetIgnoreMIMEType: Boolean;
begin
  Result := FIgnoreMIMEType;
end;

function TTileDownloaderConfigStatic.GetInetConfigStatic: IInetConfigStatic;
begin
  Result := FInetConfigStatic;
end;

function TTileDownloaderConfigStatic.GetWaitInterval: Cardinal;
begin
  Result := FWaitInterval;
end;

end.
