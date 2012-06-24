unit u_DownloadRequest;

interface

uses
  i_InetConfig,
  i_BinaryData,
  i_DownloadRequest;

type
  TDownloadRequest = class(TInterfacedObject, IDownloadRequest)
  private
    FUrl: string;
    FRequestHeader: string;
    FInetConfig: IInetConfigStatic;
  protected
    function GetUrl: string;
    function GetRequestHeader: string;
    function GetInetConfig: IInetConfigStatic;
  public
    constructor Create(
      const AUrl: string;
      const ARequestHeader: string;
      const AInetConfig: IInetConfigStatic
    );
  end;

  TDownloadPostRequest = class(TDownloadRequest, IDownloadPostRequest)
  private
    FPostData: IBinaryData;
  protected
    function GetPostData: IBinaryData;
  public
    constructor Create(
      const AUrl: string;
      const ARequestHeader: string;
      const APostData: IBinaryData;
      const AInetConfig: IInetConfigStatic
    );
  end;

implementation

{ TDownloadRequest }

constructor TDownloadRequest.Create(
  const AUrl, ARequestHeader: string;
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

function TDownloadRequest.GetRequestHeader: string;
begin
  Result := FRequestHeader;
end;

function TDownloadRequest.GetUrl: string;
begin
  Result := FUrl;
end;

{ TDownloadPostRequest }

constructor TDownloadPostRequest.Create(
  const AUrl, ARequestHeader: string;
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
