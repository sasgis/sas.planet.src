unit u_IeEmbeddedProtocolRegistration;

interface

uses
  UrlMon,
  ActiveX;

type
  TIeEmbeddedProtocolRegistration = class
  private
    FProtocol: WideString;
    FFactory : IClassFactory;
    FInternetSession: IInternetSession;
  public
    constructor Create(
      AProtocol: PWideChar;
      AFactory : IClassFactory
    );
    destructor Destroy; override;
  end;

implementation

const
  CIEEmbeddedProtocol_Class: TGUID = '{A9CA884C-253A-4662-A4F6-6926BAB877F9}';

{ TIeEmbeddedProtocolRegistration }

constructor TIeEmbeddedProtocolRegistration.Create(AProtocol: PWideChar;
  AFactory: IClassFactory);
begin
  FProtocol := AProtocol;
  FFactory := AFactory;
  CoInternetGetSession(0, FInternetSession, 0);
  FInternetSession.RegisterNameSpace(FFactory, CIEEmbeddedProtocol_Class, PWideChar(FProtocol), 0, nil, 0);
end;

destructor TIeEmbeddedProtocolRegistration.Destroy;
begin
  FInternetSession.UnregisterNameSpace(FFactory, PWideChar(FProtocol));
  FFactory := nil;
  FInternetSession := nil;
  inherited;
end;

end.
