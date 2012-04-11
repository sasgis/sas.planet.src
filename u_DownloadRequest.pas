unit u_DownloadRequest;

interface

uses
  Classes,
  i_InetConfig,
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
    FPostData: TMemoryStream;
  protected
    function GetPostData: Pointer;
    function GetPostDataSize: Integer;
  public
    constructor Create(
      const AUrl: string;
      const ARequestHeader: string;
      APostData: Pointer;
      APostDataSize: Integer;
      const AInetConfig: IInetConfigStatic
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TDownloadRequest }

constructor TDownloadRequest.Create(
  const AUrl, ARequestHeader: string;
  const AInetConfig: IInetConfigStatic
);
begin
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
  APostData: Pointer;
  APostDataSize: Integer;
  const AInetConfig: IInetConfigStatic);
begin
  inherited Create(AUrl, ARequestHeader, AInetConfig);
  FPostData := TMemoryStream.Create;
  FPostData.WriteBuffer(APostData^, APostDataSize);
end;

destructor TDownloadPostRequest.Destroy;
begin
  FreeAndNil(FPostData);
  inherited;
end;

function TDownloadPostRequest.GetPostData: Pointer;
begin
  Result := FPostData.Memory;
end;

function TDownloadPostRequest.GetPostDataSize: Integer;
begin
  Result := FPostData.Size;
end;

end.
