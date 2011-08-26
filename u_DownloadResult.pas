unit u_DownloadResult;

interface

uses
  Types,
  Classes,
  i_DownloadRequest,
  i_DownloadResult;

type
  TDownloadResult = class(TInterfacedObject, IDownloadResult)
  private
    FRequest: IDownloadRequest;
  protected
    function GetRequest: IDownloadRequest;
    function GetIsOk: Boolean; virtual; abstract;
    function GetIsServerExists: Boolean; virtual; abstract;
  public
    constructor Create(
      ARequest: IDownloadRequest
    );
  end;

  TDownloadResultCanceled = class(TDownloadResult, IDownloadResultCanceled)
  protected
    function GetIsServerExists: Boolean; override;
    function GetIsOk: Boolean; override;
  end;

  TDownloadResultOk = class(TDownloadResult, IDownloadResultOk)
  private
    FStatusCode: Cardinal;
    FRawResponseHeader: string;
    FContentType: string;
    FBuffer: TMemoryStream;
  protected
    function GetIsOk: Boolean; override;
    function GetIsServerExists: Boolean; override;
  protected
    function GetStatusCode: Cardinal;
    function GetRawResponseHeader: string;
    function GetContentType: string;
    function GetSize: Integer;
    function GetBuffer: Pointer;
  public
    constructor Create(
      ARequest: IDownloadRequest;
      AStatusCode: Cardinal;
      ARawResponseHeader: string;
      AContentType: string;
      ASize: Integer;
      ABuffer: Pointer
    );
    destructor Destroy; override;
  end;

  TDownloadResultError = class(TDownloadResult, IDownloadResultError)
  private
    FErrorText: string;
  protected
    function GetIsOk: Boolean; override;
  protected
    function GetErrorText: string;
  public
    constructor Create(
      ARequest: IDownloadRequest;
      AErrorText: string
    );
  end;

  TDownloadResultProxyError = class(TDownloadResultError, IDownloadResultProxyError)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultNoConnetctToServer = class(TDownloadResultError, IDownloadResultNoConnetctToServer)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultNoConnetctToServerByErrorCode = class(TDownloadResultNoConnetctToServer)
  public
    constructor Create(
      ARequest: IDownloadRequest;
      AErrorText: string;
      AErrorCode: DWORD
    );
  end;

  TDownloadResultLoadError = class(TDownloadResultError)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultLoadErrorByStatusCode = class(TDownloadResultLoadError)
  public
    constructor Create(
      ARequest: IDownloadRequest;
      AErrorText: string;
      AStatusCode: DWORD
    );
  end;

  TDownloadResultLoadErrorByUnknownStatusCode = class(TDownloadResultLoadError)
  public
    constructor Create(
      ARequest: IDownloadRequest;
      AErrorText: string;
      AStatusCode: DWORD
    );
  end;

  TDownloadResultLoadErrorByErrorCode = class(TDownloadResultLoadError)
  public
    constructor Create(
      ARequest: IDownloadRequest;
      AErrorText: string;
      AErrorCode: DWORD
    );
  end;

  TDownloadResultBanned = class(TDownloadResultError, IDownloadResultBanned)
  private
    FRawResponseHeader: string;
  protected
    function GetIsServerExists: Boolean; override;
    function GetRawResponseHeader: string;
  public
    constructor Create(
      ARequest: IDownloadRequest;
      ARawResponseHeader: string;
      AErrorText: string
    );
  end;

  TDownloadResultBadContentType = class(TDownloadResultError, IDownloadResultBadContentType)
  private
    FRawResponseHeader: string;
    FContentType: string;
  protected
    function GetContentType: string;
  protected
    function GetIsServerExists: Boolean; override;
    function GetRawResponseHeader: string;
  public
    constructor Create(
      ARequest: IDownloadRequest;
      AContentType: string;
      ARawResponseHeader: string;
      AErrorText: string
    );
  end;

  TDownloadResultDataNotExists = class(TDownloadResult, IDownloadResultDataNotExists)
  private
    FReasonText: string;
    FRawResponseHeader: string;
  protected
    function GetIsOk: Boolean; override;
    function GetIsServerExists: Boolean; override;
  protected
    function GetReasonText: string;
    function GetRawResponseHeader: string;
  public
    constructor Create(
      ARequest: IDownloadRequest;
      AReasonText: string;
      ARawResponseHeader: string
    );
  end;

  TDownloadResultDataNotExistsByStatusCode = class(TDownloadResultDataNotExists)
  public
    constructor Create(
      ARequest: IDownloadRequest;
      ARawResponseHeader: string;
      AErrorText: string;
      AStatusCode: DWORD
    );
  end;

  TDownloadResultDataNotExistsZeroSize = class(TDownloadResultDataNotExists)
  public
    constructor Create(
      ARequest: IDownloadRequest;
      ARawResponseHeader: string;
      AErrorText: string
    );
  end;

  TDownloadResultNotNecessary = class(TDownloadResult, IDownloadResultNotNecessary)
  private
    FReasonText: string;
    FRawResponseHeader: string;
  protected
    function GetIsOk: Boolean; override;
    function GetIsServerExists: Boolean; override;
  protected
    function GetReasonText: string;
    function GetRawResponseHeader: string;
  public
    constructor Create(
      ARequest: IDownloadRequest;
      AReasonText: string;
      ARawResponseHeader: string
    );
  end;

implementation

uses
  SysUtils;

{ TDownloadResult }

constructor TDownloadResult.Create(
  ARequest: IDownloadRequest
);
begin
  FRequest := ARequest;
end;

function TDownloadResult.GetRequest: IDownloadRequest;
begin
  Result := FRequest;
end;

{ TDownloadResultOk }

constructor TDownloadResultOk.Create(
  ARequest: IDownloadRequest;
  AStatusCode: Cardinal; ARawResponseHeader, AContentType: string;
  ASize: Integer;
  ABuffer: Pointer
);
begin
  inherited Create(ARequest);
  FStatusCode := AStatusCode;
  FRawResponseHeader := ARawResponseHeader;
  FContentType := AContentType;
  FBuffer := TMemoryStream.Create;
  FBuffer.WriteBuffer(ABuffer^, ASize);
end;

destructor TDownloadResultOk.Destroy;
begin
  FreeAndNil(FBuffer);
  inherited;
end;

function TDownloadResultOk.GetBuffer: Pointer;
begin
  Result := FBuffer.Memory;
end;

function TDownloadResultOk.GetContentType: string;
begin
  Result := FContentType;
end;

function TDownloadResultOk.GetIsOk: Boolean;
begin
  Result := True;
end;

function TDownloadResultOk.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultOk.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

function TDownloadResultOk.GetSize: Integer;
begin
  Result := FBuffer.Size;
end;

function TDownloadResultOk.GetStatusCode: Cardinal;
begin
  Result := FStatusCode;
end;

{ TDownloadResultError }

constructor TDownloadResultError.Create(
  ARequest: IDownloadRequest;
  AErrorText: string
);
begin
  inherited Create(ARequest);
  FErrorText := AErrorText;
end;

function TDownloadResultError.GetErrorText: string;
begin
  Result := FErrorText;
end;

function TDownloadResultError.GetIsOk: Boolean;
begin
  Result := False;
end;

{ TDownloadResultProxyError }

function TDownloadResultProxyError.GetIsServerExists: Boolean;
begin
  Result := False;
end;

{ TDownloadResultNoConnetctToServer }

function TDownloadResultNoConnetctToServer.GetIsServerExists: Boolean;
begin
  Result := False;
end;

{ TDownloadResultBanned }

constructor TDownloadResultBanned.Create(
  ARequest: IDownloadRequest;
  ARawResponseHeader, AErrorText: string
);
begin
  inherited Create(ARequest, AErrorText);
  FRawResponseHeader := ARawResponseHeader;
end;

function TDownloadResultBanned.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultBanned.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

{ TDownloadResultBadContentType }

constructor TDownloadResultBadContentType.Create(
  ARequest: IDownloadRequest;
  AContentType, ARawResponseHeader, AErrorText: string
);
begin
  inherited Create(ARequest, Format(AErrorText, [AContentType]));
  FContentType := AContentType;
  FRawResponseHeader := ARawResponseHeader;
end;

function TDownloadResultBadContentType.GetContentType: string;
begin
  Result := FContentType;
end;

function TDownloadResultBadContentType.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultBadContentType.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

{ TDownloadResultNoConnetctToServerByErrorCode }

constructor TDownloadResultNoConnetctToServerByErrorCode.Create(
  ARequest: IDownloadRequest;
  AErrorText: string;
  AErrorCode: DWORD
);
begin
  inherited Create(ARequest, Format(AErrorText, [AErrorCode]));
end;

{ TDownloadResultLoadErrorByStatusCode }

constructor TDownloadResultLoadErrorByStatusCode.Create(
  ARequest: IDownloadRequest;
  AErrorText: string;
  AStatusCode: DWORD
);
begin
  inherited Create(ARequest, Format(AErrorText, [AStatusCode]));
end;

{ TDownloadResultLoadErrorByErrorCode }

constructor TDownloadResultLoadErrorByErrorCode.Create(
  ARequest: IDownloadRequest;
  AErrorText: string;
  AErrorCode: DWORD
);
begin
  inherited Create(ARequest, Format(AErrorText, [AErrorCode]));
end;

{ TDownloadResultLoadError }

function TDownloadResultLoadError.GetIsServerExists: Boolean;
begin
  Result := True;
end;

{ TIDownloadResultDataNotExists }

constructor TDownloadResultDataNotExists.Create(
  ARequest: IDownloadRequest;
  AReasonText, ARawResponseHeader: string
);
begin
  inherited Create(ARequest);
  FReasonText := AReasonText;
  FRawResponseHeader := ARawResponseHeader;
end;

function TDownloadResultDataNotExists.GetIsOk: Boolean;
begin
  Result := True;
end;

function TDownloadResultDataNotExists.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultDataNotExists.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

function TDownloadResultDataNotExists.GetReasonText: string;
begin
  Result := FReasonText;
end;

{ TDownloadResultNotNecessary }

constructor TDownloadResultNotNecessary.Create(
  ARequest: IDownloadRequest;
  AReasonText, ARawResponseHeader: string
);
begin
  inherited Create(ARequest);
  FReasonText := AReasonText;
  FRawResponseHeader := ARawResponseHeader;
end;

function TDownloadResultNotNecessary.GetIsOk: Boolean;
begin
  Result := True;
end;

function TDownloadResultNotNecessary.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultNotNecessary.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

function TDownloadResultNotNecessary.GetReasonText: string;
begin
  Result := FReasonText;
end;

{ TDownloadResultDataNotExistsByStatusCode }

constructor TDownloadResultDataNotExistsByStatusCode.Create(
  ARequest: IDownloadRequest;
  ARawResponseHeader, AErrorText: string;
  AStatusCode: DWORD
);
begin
  inherited Create(ARequest, Format(AErrorText, [AStatusCode]), ARawResponseHeader);
end;

{ TDownloadResultDataNotExistsZeroSize }

constructor TDownloadResultDataNotExistsZeroSize.Create(
  ARequest: IDownloadRequest;
  ARawResponseHeader, AErrorText: string
);
begin
  inherited Create(ARequest, AErrorText, ARawResponseHeader);
end;

{ TDownloadResultLoadErrorByUnknownStatusCode }

constructor TDownloadResultLoadErrorByUnknownStatusCode.Create(
  ARequest: IDownloadRequest;
  AErrorText: string;
  AStatusCode: DWORD
);
begin
  inherited Create(ARequest, Format(AErrorText, [AStatusCode]));
end;

{ TDownloadResultCanceled }

function TDownloadResultCanceled.GetIsOk: Boolean;
begin
  Result := False;
end;

function TDownloadResultCanceled.GetIsServerExists: Boolean;
begin
  Result := False;
end;

end.
