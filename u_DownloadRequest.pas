unit u_DownloadRequest;

interface

uses
  i_InetConfig,
  i_BinaryData,
  i_DownloadRequest,
  u_BaseInterfacedObject;

type
  TDownloadRequest = class(TBaseInterfacedObject, IDownloadRequest)
  private
    FUrl: AnsiString;
    FRequestHeader: AnsiString;
    FInetConfig: IInetConfigStatic;
  protected
    function GetUrl: AnsiString;
    function GetRequestHeader: AnsiString;
    function GetInetConfig: IInetConfigStatic;
  public
    constructor Create(
      const AUrl: AnsiString;
      const ARequestHeader: AnsiString;
      const AInetConfig: IInetConfigStatic
    );
  end;

  TDownloadPostRequest = class(TDownloadRequest, IDownloadPostRequest)
  private
    FPostData: IBinaryData;
  private
    function GetPostData: IBinaryData;
  public
    constructor Create(
      const AUrl: AnsiString;
      const ARequestHeader: AnsiString;
      const APostData: IBinaryData;
      const AInetConfig: IInetConfigStatic
    );
  end;

implementation

{ TDownloadRequest }

constructor TDownloadRequest.Create(
  const AUrl, ARequestHeader: AnsiString;
  const AInetConfig: IInetConfigStatic
);
begin
  inherited Create;
  FUrl := AUrl;
  FRequestHeader := ARequestHeader;
  FInetConfig := AInetConfig;
end;

function TDownloadRequest.GetInetConfig: IInetConfigStatic;
begin
  Result := FInetConfig;
end;

function TDownloadRequest.GetRequestHeader: AnsiString;
begin
  Result := FRequestHeader;
end;

function TDownloadRequest.GetUrl: AnsiString;
begin
  Result := FUrl;
end;

{ TDownloadPostRequest }

constructor TDownloadPostRequest.Create(
  const AUrl, ARequestHeader: AnsiString;
  const APostData: IBinaryData;
  const AInetConfig: IInetConfigStatic
);
begin
  inherited Create(AUrl, ARequestHeader, AInetConfig);
  FPostData := APostData;
end;


function TDownloadPostRequest.GetPostData: IBinaryData;
begin
  Result := FPostData;
end;

end.
