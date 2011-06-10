unit u_DownloadResult;

interface

uses
  Types,
  Classes,
  i_DownloadResult;

type
  TDownloadResult = class(TInterfacedObject, IDownloadResult)
  private
    FUrl: string;
    FRequestHead: string;
  protected
    function GetUrl: string;
    function GetRequestHead: string;
    function GetIsOk: Boolean; virtual; abstract;
    function GetIsServerExists: Boolean; virtual; abstract;
  public
    constructor Create(
      AUrl: string;
      ARequestHead: string
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
      AUrl: string;
      ARequestHead: string;
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
      AUrl: string;
      ARequestHead: string;
      AErrorText: string
    );
  end;

  TDownloadResultProxyError = class(TDownloadResultError, IDownloadResultProxyError)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultUnexpectedProxyAuth = class(TDownloadResultProxyError)
  public
    constructor Create(
      AUrl: string;
      ARequestHead: string
    );
  end;

  TDownloadResultBadProxyAuth = class(TDownloadResultProxyError)
  public
    constructor Create(
      AUrl: string;
      ARequestHead: string
    );
  end;

  TDownloadResultNoConnetctToServer = class(TDownloadResultError, IDownloadResultNoConnetctToServer)
  protected
    function GetIsServerExists: Boolean; override;
  end;

  TDownloadResultNoConnetctToServerByErrorCode = class(TDownloadResultNoConnetctToServer)
  public
    constructor Create(
      AUrl: string;
      ARequestHead: string;
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
      AUrl: string;
      ARequestHead: string;
      AStatusCode: DWORD
    );
  end;

  TDownloadResultLoadErrorByUnknownStatusCode = class(TDownloadResultLoadError)
  public
    constructor Create(
      AUrl: string;
      ARequestHead: string;
      AStatusCode: DWORD
    );
  end;

  TDownloadResultLoadErrorByErrorCode = class(TDownloadResultLoadError)
  public
    constructor Create(
      AUrl: string;
      ARequestHead: string;
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
      AUrl: string;
      ARequestHead: string;
      ARawResponseHeader: string
    );
  end;

  TDownloadResultBadContentType = class(TDownloadResultError, IDownloadResultBadContentType)
  private
    FRawResponseHeader: string;
  protected
    function GetIsServerExists: Boolean; override;
    function GetRawResponseHeader: string;
  public
    constructor Create(
      AUrl: string;
      ARequestHead: string;
      AContentType: string;
      ARawResponseHeader: string
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
      AUrl: string;
      ARequestHead: string;
      AReasonText: string;
      ARawResponseHeader: string
    );
  end;

  TDownloadResultDataNotExistsByStatusCode = class(TDownloadResultDataNotExists)
  public
    constructor Create(
      AUrl: string;
      ARequestHead: string;
      ARawResponseHeader: string;
      AStatusCode: DWORD
    );
  end;

  TDownloadResultDataNotExistsZeroSize = class(TDownloadResultDataNotExists)
  public
    constructor Create(
      AUrl: string;
      ARequestHead: string;
      ARawResponseHeader: string
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
      AUrl: string;
      ARequestHead: string;
      AReasonText: string;
      ARawResponseHeader: string
    );
  end;

implementation

uses
  SysUtils;

{ TDownloadResult }

constructor TDownloadResult.Create(AUrl, ARequestHead: string);
begin
  FUrl := AUrl;
  FRequestHead := ARequestHead;
end;

function TDownloadResult.GetRequestHead: string;
begin
  Result := FRequestHead;
end;

function TDownloadResult.GetUrl: string;
begin
  Result := FUrl;
end;

{ TDownloadResultOk }

constructor TDownloadResultOk.Create(AUrl, ARequestHead: string;
  AStatusCode: Cardinal; ARawResponseHeader, AContentType: string;
  ASize: Integer;
  ABuffer: Pointer
);
begin
  inherited Create(AUrl, ARequestHead);
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

constructor TDownloadResultError.Create(AUrl, ARequestHead, AErrorText: string);
begin
  inherited Create(AUrl, ARequestHead);
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

constructor TDownloadResultBanned.Create(AUrl, ARequestHead, ARawResponseHeader: string);
begin
  inherited Create(AUrl, ARequestHead, 'Похоже вас забанили');
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

constructor TDownloadResultBadContentType.Create(AUrl, ARequestHead,
  AContentType, ARawResponseHeader: string);
begin
  inherited Create(AUrl, ARequestHead, Format('Неожиданный тип %s', [AContentType]));
  FRawResponseHeader := ARawResponseHeader;
end;

function TDownloadResultBadContentType.GetIsServerExists: Boolean;
begin
  Result := True;
end;

function TDownloadResultBadContentType.GetRawResponseHeader: string;
begin
  Result := FRawResponseHeader;
end;

{ TDownloadResultUnexpectedProxyAuth }

constructor TDownloadResultUnexpectedProxyAuth.Create(AUrl,
  ARequestHead: string);
begin
  inherited Create(AUrl, ARequestHead, 'Настройки не предусматривают авторизацию на прокси');
end;

{ TDownloadResultBadProxyAuth }

constructor TDownloadResultBadProxyAuth.Create(AUrl, ARequestHead: string);
begin
  inherited Create(AUrl, ARequestHead, 'Ошибка авторизации на прокси');
end;

{ TDownloadResultNoConnetctToServerByErrorCode }

constructor TDownloadResultNoConnetctToServerByErrorCode.Create(AUrl,
  ARequestHead: string; AErrorCode: DWORD);
begin
  inherited Create(AUrl, ARequestHead, Format('Ошибка подключения к серверу. Код ошибки %d', [AErrorCode]));
end;

{ TDownloadResultLoadErrorByStatusCode }

constructor TDownloadResultLoadErrorByStatusCode.Create(AUrl,
  ARequestHead: string; AStatusCode: DWORD);
begin
  inherited Create(AUrl, ARequestHead, Format('Ошибка загрузки. Статус %d', [AStatusCode]));
end;

{ TDownloadResultLoadErrorByErrorCode }

constructor TDownloadResultLoadErrorByErrorCode.Create(AUrl,
  ARequestHead: string; AErrorCode: DWORD);
begin
  inherited Create(AUrl, ARequestHead, Format('Ошибка загрузки. Код ошибки %d', [AErrorCode]));
end;

{ TDownloadResultLoadError }

function TDownloadResultLoadError.GetIsServerExists: Boolean;
begin
  Result := True;
end;

{ TIDownloadResultDataNotExists }

constructor TDownloadResultDataNotExists.Create(AUrl, ARequestHead,
  AReasonText, ARawResponseHeader: string);
begin
  inherited Create(AUrl, ARequestHead);
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

constructor TDownloadResultNotNecessary.Create(AUrl, ARequestHead,
  AReasonText, ARawResponseHeader: string);
begin
  inherited Create(AUrl, ARequestHead);
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

constructor TDownloadResultDataNotExistsByStatusCode.Create(AUrl,
  ARequestHead: string;
  ARawResponseHeader: string;
  AStatusCode: DWORD);
begin
  inherited Create(AUrl, ARequestHead, Format('Данныео отсутствуют. Статус %d', [AStatusCode]), ARawResponseHeader);
end;

{ TDownloadResultDataNotExistsZeroSize }

constructor TDownloadResultDataNotExistsZeroSize.Create(AUrl,
  ARequestHead, ARawResponseHeader: string);
begin
  inherited Create(AUrl, ARequestHead, 'Получен ответ нулевой длинны', ARawResponseHeader);
end;

{ TDownloadResultLoadErrorByUnknownStatusCode }

constructor TDownloadResultLoadErrorByUnknownStatusCode.Create(AUrl,
  ARequestHead: string; AStatusCode: DWORD);
begin
  inherited Create(AUrl, ARequestHead, Format('Неизвестный статус %d', [AStatusCode]));
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
