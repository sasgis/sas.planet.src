unit SwinHttp;
{* Модуль для работы с http/https }

interface

uses Windows, WinInet, SysUtils, Classes;

const
  default_buffer = 4096;

type
  TSwURL = class(TPersistent)
  {* URL parser. Можно использовать отдельно, как валидатор урлов, 
     либо, как конструктор урлов, позволяющий изменять отдельные части урла. }
  private
    FProto: dword;
    FHost: string;
    FPort: dword;
    FPath: string;
    FExtra: string;
    FUser: string;
    FPass: string;
    procedure SetUrl(const Value: string);
    function GetUrl: string;
    function GetSSL: boolean;
    procedure SetSSL(const Value: boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    procedure Clear;
    {* Очистить все поля }
  published
    property url: string read GetUrl write SetUrl;
    {* Сам URL в стандартном представлении. 
       Автоматически раскладывается на поля, идущие далее.
       Из них же и собирается для запроса. 
       То есть, назначив url, вы можете изменять его по частям,
       меняя свойства, идущии далее. }
    property HTTPS: boolean read GetSSL write SetSSL;
    {* Флаг использования https }
    property Host: string read FHost write FHost;
    {* Хост часть урла }
    property Port: dword read FPort write FPort;
    {* Порт. def=80 }
    property Path: string read FPath write FPath;
    {* Путь на сервере }
    property Extra: string read FExtra write FExtra;
    {* Данные GET запроса }
    property User: string read FUser write FUser;
    {* Имя юзера, если сервер требует авторизации }
    property Pass: string read FPass write FPass;
    {* Пароль юзера, если сервер требует авторизации }
  end;

  TSwRequest = class(TPersistent)
  {* Данные запроса }
  private
    FReferer: string;
    FPostData: string;
    FAgent: string;
    FUrl: TSwURL;
    FHeaders: TStringList;
    FTag: integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    {* Обнулить запрос }
  published
    property url: TSwURL read FUrl write FUrl;
    {* URL }
    property PostData: string read FPostData write FPostData;
    {* Данные, которые передать в качестве POST запроса }
    property Agent: string read FAgent write FAgent;
    {* Идентификатор браузера. def=IE6.0 }
    property Referer: string read FReferer write FReferer;
    {* Реф - url, откуда пришли }
    property Headers: TStringList read FHeaders write FHeaders;
    {* Заголовки http-запроса }
    property Tag: integer read FTag write FTag;
    {* Здесь можно хранить свои данные, как в обычном Tag }
  end;

  TSwResponse = class(TPersistent)
  {* Результаты запроса }
  private
    FCode: dword;
    FCStream: TMemoryStream;
    FContent: TStream;
    FHeaders: TStrings;
    FContentType: string;
    FCookies: string;
    FEncoding: string;
    FMime: string;
    FServer: string;
    FStatusText: string;
    FSize: dword;
    FLastModifed: TDateTime;
    FDate: TDateTime;
    procedure SetContent(const Value: TStream);
    function  GetContent: TStream;
    function GetBody: string;
  protected
    procedure FillResponse(h: HINTERNET);
  public
    constructor Create;
    destructor Destroy; override;
    procedure  Clear;
    {* Обнулить response }
  published
    property Headers: TStrings read FHeaders;
    {* Все заголовки ответа }
    property Body: string read GetBody;
    {* Содержимое ответа в виде string }
    property Content: TStream read GetContent write SetContent;
    {* Содержимое ответа в виде stream. Свойство доступно также на запись.
       | то есть перед запросом вы можете указать свой стрим, куда принимать данные.<br>
       По умолчанию используется TMemoryStream.
       Но допускается использование любого наследника TStream. 
       | Например TFileStream для скачивания больших файлов сразу на диск.<br>
       Примечание: Если собираетесь использовать другой стрим, то не
       забудьте его сами создать и потом уничтожить. }
    property ContentType: string read FContentType;
    {* content-type (например text/html) }
    property Cookies: string read FCookies;
    {* Новые cookies }
    property Mime: string read FMime;
    {* mime-type (например может быть base64) }
    property Server: string read FServer;
    {* Название и версия ПО сервера }
    property StatusText: string read FStatusText;
    {* Текст, сопровождающий код ответа (например 404 - Not found)  }
    property Code: dword read FCode;
    {* Код ответа сервера (200, 404 и т.п.) }
    property Size: dword read FSize;
    {* Размер принятых/принимаемых данных }
    property LastModifed: TDateTime read FLastModifed;
    {* Дата последнего изменения запрошенного документа.
       Если сервер не сообщил, то равна нулю. }
    property Date: TDateTime read FDate;
    {* Дата создания запрошенного документа.
       Если сервер не сообщил, то равна нулю. }
    property TransferEncoding: string read FEncoding;
    {* Кодировка содержимого. (например gzip) }
  end;

  TSwProxyProtocols = (ppAuto, ppNone, ppHttp, ppHttps, ppSocks);
  {* |Протокол прокси сервера. Например https траффик может идти через http или Socks прокси.<br>
     |ppAuto - автоопределение прокси (взять настройки IE),<br>
     |ppNone - Не использовать прокси,<br>
     |ppHttp - Обычный http прокси,<br>
     |ppHttps - https прокси,<br>
     |ppSocks - Socks прокси. }
  TSwProxy = class(TPersistent)
  {* Параметры Proxy сервера. }
  private
    FHost: string;
    FPort: integer;
    FProtocol: TSwProxyProtocols;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function ProxyStr: string;
  published
    property Protocol: TSwProxyProtocols read FProtocol write FProtocol;
    {* Протокол прокси сервера. По умолчанию ppAuto }
    property Host: string read FHost write FHost;
    {* Хост прокси сервера }
    property Port: integer read FPort write FPort default 80;
    {* Порт прокси сервера }
  end;

  TSwProxies = class(TPersistent)
  {* Настройки прокси серверов }
  private
    FUser: string;
    FPass: string;
    FHttpProxy: TSwProxy;
    FHttpsProxy: TSwProxy;
    FBypass: string;
  protected
    function List: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property HttpProxy: TSwProxy read FHttpProxy write FHttpProxy;
    {* Параметры http прокси }
    property HttpsProxy: TSwProxy read FHttpsProxy write FHttpsProxy;
    {* Параметры https прокси }
    property User: string read FUser write FUser;
    {* Логин на прокси }
    property Pass: string read FPass write FPass;
    {* |Пароль на прокси.<br>
       Логин и пароль прокси будут работать, даже если явно не прописать прокси,
       а использовать дефолтовое автоопределение. }
    property Bypass: string read FBypass write FBypass;
    {* Не использовать прокси для адресов, начинающихся с ...
       | (аналогично полю в настройках IE).<br>
       Работает только, если прокси указан вручную. 
       При ppAuto этот параметр также берётся автоматом из настроек IE }
  end;

  TSwinHttp = class;
  TSwNotify = procedure(Sender: TSwinHttp; Request: TSwRequest) of object;
  {* Событие TSwinHttp. На входе TSwinHttp, который инициировал событие,
     и запрос, который выполняется(лся) 
     (на случай асинхронной работы, если было поставлено несколько запросов одновременно) }
  TSwinHttp = class(TComponent)
  {*! Класс для http/https запросов }
  private
    FSilent: boolean;
    FOnWork: TSwNotify;
    FOnWorkEnd: TSwNotify;
    FOnWorkBegin: TSwNotify;
    FError: dword;
    FResponse: TSwResponse;
    FRequest: TSwRequest;
    FCurrReq: TSwRequest;
    FInThread: boolean;
    FBufferSize: dword;
    FThread: TThread;
    FReqList: TList;
    FReqCount: Integer;
    FProxy: TSwProxies;
    procedure SyncEvent(event: TSwNotify);
  protected
    hInet, hConnect, hFile: HINTERNET;
    procedure AssignTo(Dest: TPersistent); override;
    function  Open: boolean;
    procedure Close;
    function  OpenRequest: boolean;
    function  Read(Buf: pointer; sz: dword): dword;
    procedure ReceiveAll;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure  DoRequest;
    {* Выполнить запрос. Все данные запроса заполняются в поле Request }
    procedure  Get(url: string);
    {* Упрощённыя форма DoRequest для выполнения простых GET запросов.
       url автоматом назначается урлу Request'a }
    procedure  Post(url: string; PostData: string);
    {* Упрощённыя форма DoRequest для выполнения простых POST запросов
       url и PostData автоматом назначается Request'у }
    procedure  Clear;
    {* Очистить очередь запросов, прекратить закачки, сбросить все поля. }
    property Response: TSwResponse read FResponse;
    {* Здесь содержатся принятые данные. Подробнее - см TSwResponse }
  published
    property Error: dword read FError;
    {* Код ошибки. Если = 0 - ошибки не было }
    property Silent: boolean read FSilent write FSilent default false;
    {* Тихий режим - не выводить вопросы пользователю,
       а просто вернуть код ошибки, чтобы принять решение программно,
       |либо для создания своих аналогичных диалогов.<br>
       |На текущий момент существуют 2 диалога:<br>
       - Диалог ввода логина/пароля прокси сервера. Если silent,
         | то просто вернёт код 407, если нет, логин/пароль пропишутся в настройки прокси<br>
       - Диалог подтверждения приёма неверного https сертификата.
         Если Silent - автоматом принимать некорректные сертификаты
        }
    property InThread: boolean read FInThread write FInThread default true;
    {* Выполнять все запросы в фоновом режиме в отдельном потоке.
       По умолчанию включено. Если выключить, то будет работать подобно TIdHTTP }
    property BufferSize: dword read FBufferSize write FBufferSize default default_buffer;
    {* Размер буфера приёма. По умолчанию равен 4kb. Имеет смысл уменьшать только,
       если вы хотите, чтобы чаще вызывалось событие OnWork, но тогда уменьшится
       скорость скачивания. Если вы не используете OnWork, то можно увеличить.
       Допустимые значения от 1 до MAX_DWORD байт. }
    property Proxy: TSwProxies read FProxy write FProxy;
    {* Настройки прокси, на случай, если вас не устраивают настройки,
       прописанные в IE. }
    property Request: TSwRequest read FRequest write FRequest;
    {* Данные для запроса. Подробнее - см TSwRequest }
    property OnWorkBegin: TSwNotify read FOnWorkBegin write FOnWorkBegin;
    {* Событие, вызываемое после установки соединения и получения заголовков,
       но перед непосредственным приёмом данных.
       Можеть быть полезно, если вы хотите узнать размер/дату и т.д.
       перед началом приёма. }
    property OnWork: TSwNotify read FOnWork write FOnWork;
    {* Событие, вызываемое в процессе приёма данных, после приёма каждого блока.
       Размер блока указан в BufferSize. }
    property OnWorkEnd: TSwNotify read FOnWorkEnd write FOnWorkEnd;
    {* Событие, вызываемое по окончании загрузки.
       Если работа идёт в асинхронном режиме (InThread=true),
       то именно здесь вы должны забирать результаты запроса }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Internet', [TSwinHttp]);
end;

type
  TSwThread = class(TThread)
  private
    Fevent: TSwNotify;
    procedure Execute; override;
    procedure Sync;
    procedure Setevent(const Value: TSwNotify);
  public
    http: TSwinHttp;
  end;

{ TURL }

constructor TSwURL.Create;
begin
  inherited Create;
  Clear;
end;

procedure TSwURL.Clear;
begin
  host := ''; user := ''; pass := ''; path := ''; extra := '';
  Port := 80; FProto := INTERNET_SCHEME_HTTP;
end;

function TSwURL.GetSSL: boolean;
begin
  result := FProto = INTERNET_SCHEME_HTTPS;
end;

procedure TSwURL.SetSSL(const Value: boolean);
begin
  if Value then FProto := INTERNET_SCHEME_HTTPS
  else FProto := INTERNET_SCHEME_HTTP;
end;

procedure TSwURL.AssignTo(Dest: TPersistent);
begin
  TSwURL(Dest).SetUrl(GetUrl);
end;

procedure TSwURL.SetUrl(const Value: string);
var
  u: URL_COMPONENTS;
  hst, usr, pas, pth, ext: string;
  procedure ZeroStr(var s: string; var pch: PChar; var sz: dword);
  begin
    sz := INTERNET_MAX_PATH_LENGTH;
    SetLength(s, sz); FillChar(s[1], sz, 0); pch := PChar(s);
  end;
begin
  FillChar(u, SizeOf(u), 0); u.dwStructSize := sizeOf(u);
  ZeroStr(hst, u.lpszHostName, u.dwHostNameLength);
  ZeroStr(usr, u.lpszUserName, u.dwUserNameLength);
  ZeroStr(pas, u.lpszPassword, u.dwPasswordLength);
  ZeroStr(pth, u.lpszUrlPath, u.dwUrlPathLength);
  ZeroStr(ext, u.lpszExtraInfo, u.dwExtraInfoLength);
  if InternetCrackUrl(PChar(Value), Length(Value), ICU_DECODE, u) then
  begin
    host := PChar(hst); user := PChar(usr); pass := PChar(pas);
    path := PChar(pth); extra := PChar(ext);
    Port := u.nPort; FProto := u.nScheme;
  end;
end;

function TSwURL.GetUrl: string;
var
  u: URL_COMPONENTS;
  d: dword;
begin
  FillChar(u, SizeOf(u), 0);
  u.dwStructSize := sizeOf(u);
  u.lpszHostName := PChar(FHost); u.dwHostNameLength := Length(FHost);
  if FUser <> '' then begin
    u.lpszUserName := PChar(FUser); u.dwUserNameLength := Length(FUser);
    u.lpszPassword := PChar(FPass); u.dwPasswordLength := Length(FPass);
  end;
  if FPath <> '' then u.lpszUrlPath := PChar(FPath);
  u.dwUrlPathLength := Length(FPath);
  if FExtra <> '' then u.lpszExtraInfo := PChar(FExtra);
  u.dwExtraInfoLength := Length(FExtra);
  u.nScheme := FProto; u.nPort := FPort;
  d := INTERNET_MAX_PATH_LENGTH;
  SetLength(result, d); FillChar(result[1], d, 0);
  if InternetCreateUrl(u, ICU_ESCAPE, PChar(result), d) then
    result := copy(result, 1, d)
  else result := '';
end;

{ TSwResponse }

procedure TSwResponse.Clear;
begin
  FCStream.Clear; FHeaders.Clear;
  FContent := nil; FCode := 0;
end;

constructor TSwResponse.Create;
begin
  FHeaders := TStringList.Create;
  FCStream := TMemoryStream.Create;
  Clear;
end;

destructor TSwResponse.Destroy;
begin
  FHeaders.Free; FCStream.Free;
  inherited;
end;

procedure TSwResponse.FillResponse(h: HINTERNET);
  function GetStrParam(Flag: dword): string;
  var
    d, sz: dword;
    p:     PChar;
  begin
    sz := 0; d := 0; p := nil;
    HttpQueryInfo(h, Flag, p, sz, d);
    sz := sz+1; GetMem(p, sz); FillChar(p^, sz, 0);
    HttpQueryInfo(h, Flag, p, sz, d);
    result := p; FreeMem(p);
  end;
  function GetDwordParam(Flag: dword): dword;
  var
    d, sz: dword;
  begin
    sz := SizeOf(result); d := 0;
    HttpQueryInfo(h, Flag or HTTP_QUERY_FLAG_NUMBER, @result, sz, d);
  end;
  function GetDateParam(Flag: dword): TDateTime;
  var
    d, sz: dword;
    dt: _SYSTEMTIME;
  begin
    sz := SizeOf(dt); d := 0;
    if HttpQueryInfo(h, Flag or HTTP_QUERY_FLAG_SYSTEMTIME, @dt, sz, d) then
      result := SystemTimeToDateTime(dt) else result := 0;
  end;
begin
  FHeaders.Text := GetStrParam(HTTP_QUERY_RAW_HEADERS_CRLF);
  FEncoding := GetStrParam(HTTP_QUERY_CONTENT_TRANSFER_ENCODING);
  FContentType := GetStrParam(HTTP_QUERY_CONTENT_TYPE);
  FCookies := GetStrParam(HTTP_QUERY_SET_COOKIE );
  FMime := GetStrParam(HTTP_QUERY_MIME_VERSION);
  FServer := GetStrParam(HTTP_QUERY_SERVER);
  FStatusText := GetStrParam(HTTP_QUERY_STATUS_TEXT);
  FCode := GetDwordParam(HTTP_QUERY_STATUS_CODE);
  FSize := GetDwordParam(HTTP_QUERY_CONTENT_LENGTH);
  FLastModifed := GetDateParam(HTTP_QUERY_LAST_MODIFIED);
  FDate := GetDateParam(HTTP_QUERY_DATE);
  {
  HTTP_QUERY_PROXY_AUTHENTICATE
  HTTP_QUERY_PROXY_AUTHORIZATION
  HTTP_QUERY_PROXY_CONNECTION
  HTTP_QUERY_WWW_AUTHENTICATE
  }
end;

function TSwResponse.GetBody: string;
var t: Int64;
begin
  with Content do begin
    t := Position; Position := 0;
    SetLength(result, Size);
    Read(result[1], Length(result));
    Position := t;
  end;
end;

function TSwResponse.GetContent: TStream;
begin
  if FContent = nil then result := FCStream
  else result := FContent;
end;

procedure TSwResponse.SetContent(const Value: TStream);
begin
  FContent := Value;
end;

{ TSwRequest }

procedure TSwRequest.AssignTo(Dest: TPersistent);
var d: TSwRequest;
begin
  d := TSwRequest(Dest);
  d.Referer := FReferer;
  d.FPostData := FPostData;
  d.FAgent := FAgent;
  d.FUrl.Assign(FUrl);
  d.FHeaders.Assign(FHeaders);
  d.Tag := Tag;
end;

procedure TSwRequest.Clear;
begin
  FAgent := 'User-Agent: Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)';
  FReferer := ''; FPostData := ''; FUrl.Clear;
end;

constructor TSwRequest.Create;
begin
  FUrl := TSwURL.Create; FHeaders := TStringList.Create;
  Clear;
end;

destructor TSwRequest.Destroy;
begin
  FUrl.Free; FHeaders.Free;
  inherited;
end;

procedure TSwProxy.AssignTo(Dest: TPersistent);
var d: TSwProxy;
begin
  d := TSwProxy(Dest);
  d.FProtocol := FProtocol;
  d.FHost := FHost;
  d.FPort := FPort;
end;

{ TSwHttp }

procedure TSwinHttp.AssignTo(Dest: TPersistent);
begin
  { TODO : AssignTo - Надо бы сделать... }
end;

constructor TSwinHttp.Create(AOwner: TComponent);
begin
  inherited;
  FReqList := TList.Create;
  FRequest := TSwRequest.Create;
  FResponse := TSwResponse.Create;
  FProxy := TSwProxies.Create;
  FSilent := false; FInThread := true; FThread := nil;
  FBufferSize := default_buffer;
  Clear;
end;

destructor TSwinHttp.Destroy;
begin
  Clear; FResponse.Free; FRequest.Free; FProxy.Free; FReqList.Free;
  inherited;
end;

function TSwinHttp.Open: boolean;
var
  d: dword;
  user, pass: PChar;
  procedure SetProxySetts(p: TSwProxy);
  begin
    user := nil; pass := nil;
    case p.Protocol of
      ppAuto: d := INTERNET_OPEN_TYPE_PRECONFIG;
      ppNone: d := INTERNET_OPEN_TYPE_DIRECT;
      else begin
        user := PChar(Proxy.List);
        d := INTERNET_OPEN_TYPE_PROXY;
      end;
    end;
  end;
begin
  if hInet <> nil then Close;
  result := true;
  case FCurrReq.url.Fproto of
    INTERNET_SCHEME_HTTP: SetProxySetts(FProxy.HttpProxy);
    INTERNET_SCHEME_HTTPS: SetProxySetts(FProxy.HttpsProxy);
    else result := false;
  end;
  if result then
    hInet := InternetOpen(PChar(FCurrReq.Agent), d, user, PChar(FProxy.Bypass), 0);
  result := hInet <> nil;
  if result then begin
    d := 0;
    if FCurrReq.url.User = '' then begin user := nil; pass := nil; end
    else begin user := PChar(FCurrReq.url.User); pass := PChar(FCurrReq.url.Pass); end;
    hConnect := InternetConnect(hInet, PChar(FCurrReq.url.Host), FCurrReq.url.Port,
                user, pass, INTERNET_SERVICE_HTTP, 0, d);
    result := hConnect <> nil;
    if FProxy.User <> '' then
      InternetSetOption(hConnect, INTERNET_OPTION_PROXY_USERNAME, PChar(FProxy.User), Length(FProxy.User));
    if FProxy.Pass <> '' then
      InternetSetOption(hConnect, INTERNET_OPTION_PROXY_PASSWORD, PChar(FProxy.Pass), Length(FProxy.Pass));
  end;
  if not result then FError := GetLastError;
end;

procedure TSwinHttp.Close;
begin
  InternetCloseHandle(hFile);
  InternetCloseHandle(hConnect);
  InternetCloseHandle(hInet);
  hInet := nil; hConnect := nil; hFile := nil;
end;

function TSwinHttp.OpenRequest: boolean;
  function GetHFile: Boolean;
  var
    d, context: dword;
    p: PChar;
  begin
    d := INTERNET_FLAG_RELOAD;
    if FCurrReq.url.Fproto = INTERNET_SCHEME_HTTPS then
      d := d or INTERNET_FLAG_SECURE
             or INTERNET_FLAG_IGNORE_CERT_CN_INVALID
             or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;
    if FCurrReq.PostData = '' then p := nil else p := 'POST';
    setlasterror(0); context := 0;
    hFile := HTTPOpenRequest(hConnect, p, PChar(FCurrReq.url.Path + FCurrReq.url.Extra),
                             HTTP_VERSION,
                             PChar(FCurrReq.Referer), nil, d, context);
    result := hFile <> nil;
    if not result then FError := GetLastError;
  end;
  function SendRequest: boolean;
    function FixInvalidCA: Boolean;
    var d, df: dword;
    begin
      d := 0; df := 0; result := true;
      if Silent then begin
        InternetQueryOption (hFile, INTERNET_OPTION_SECURITY_FLAGS, @df, d);
        df := df or SECURITY_FLAG_IGNORE_UNKNOWN_CA;
        InternetSetOption (hFile, INTERNET_OPTION_SECURITY_FLAGS, @df, SizeOf(df));
      end else begin
        df := InternetErrorDlg(GetDesktopWindow(), hFile, ERROR_INTERNET_INVALID_CA,
              FLAGS_ERROR_UI_FILTER_FOR_ERRORS or FLAGS_ERROR_UI_FLAGS_GENERATE_DATA or FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS,
              Pointer(d));
        if df = 0 then Silent := true else result := false;
      end;
    end;
  var h:     PChar;
      postptr:pointer;
      postsize:integer;
  begin
    if FCurrReq.PostData = '' then begin
      postptr := nil;
      postsize := 0;
    end else begin
      postptr := @FCurrReq.PostData[1];
      postsize:=length(FCurrReq.PostData);
    end;
    if FCurrReq.Headers.Text = '' then h := nil
    else h := PChar(FCurrReq.FHeaders.Text);
    repeat
      SetLastError(0);
      if not HTTPSendRequest(hFile, h, Length(h), postptr, postsize)
      then FError := getlasterror;
      if FError = ERROR_INTERNET_INVALID_CA then
        if not FixInvalidCA then break;
    until (FError <> ERROR_INTERNET_INVALID_CA);
    result := FError = 0;
  end;
begin
  result := false; setlasterror(0);
  FResponse.Content.Size := 0; FError := 0;
  if not Open then exit;
  if not GetHFile then Exit;
  if not SendRequest then Exit;
  if FError = 0 then FResponse.FillResponse(hFile);
end;

function TSwinHttp.Read(Buf: pointer; sz: dword): dword;
begin
  if hFile = nil then result := 0
  else if not InternetReadFile(hFile, Buf, sz, result) then result := 0;
end;

procedure TSwinHttp.DoRequest;
var
  NewReq: TSwRequest;
begin
  NewReq := TSwRequest.Create;
  NewReq.Assign(FRequest);
  FReqList.Add(FRequest);
  FRequest := NewReq;
  InterlockedIncrement(FReqCount);
  if FInThread then
  begin
    if FThread <> nil then Exit;
    FThread := TSwThread.Create(true);
    with TSwThread(FThread) do
    begin
      FThread.FreeOnTerminate := true;
      http := Self;
      Resume;
    end;
  end else begin
    FThread := nil;
    ReceiveAll;
  end;
end;

procedure TSwinHttp.ReceiveAll;
var
  b:  string;
  d:  dword;
  procedure ProxyAuttDialog;
  const ERROR_INTERNET_FORCE_RETRY = 12032;
  var
    d: dword;
    s: string;
  begin
    if Silent then Exit;
    repeat
      d := InternetErrorDlg(GetDesktopWindow, hFile, ERROR_INTERNET_INCORRECT_PASSWORD,
      FLAGS_ERROR_UI_FILTER_FOR_ERRORS or FLAGS_ERROR_UI_FLAGS_GENERATE_DATA or FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS,
      Pointer(d));
      if (d = ERROR_INTERNET_FORCE_RETRY) or (d = ERROR_SUCCESS) then
      begin
        d := INTERNET_MAX_PASSWORD_LENGTH; SetLength(s, d);
        InternetQueryOption(hFile, INTERNET_OPTION_PROXY_USERNAME, pointer(s), d);
        FProxy.User := PChar(s);
        d := INTERNET_MAX_PASSWORD_LENGTH;
        InternetQueryOption(hFile, INTERNET_OPTION_PROXY_PASSWORD, pointer(s), d);
        FProxy.Pass := PChar(s);
        Close; OpenRequest;
        d := Response.Code;
        if d <> HTTP_STATUS_PROXY_AUTH_REQ then d := 0; 
      end else d := 0;
    until d = 0;
  end;
begin
  if FReqCount = 0 then Exit;
  SetLength(b, FBufferSize);
  FCurrReq := TSwRequest(FReqList[0]);
  OpenRequest;
  case Response.Code of
    HTTP_STATUS_PROXY_AUTH_REQ: ProxyAuttDialog;
  end;
  SyncEvent(FOnWorkBegin);
  repeat
    d := Read(Pointer(b), FBufferSize);
    FResponse.Content.Write(b[1], d);
    SyncEvent(FOnWork);
    //FResponse.Content.Position := FResponse.Content.Size;
  until d = 0;
  try
    FResponse.Content.Position := 0;
  finally
    Close;
  end;
  SyncEvent(OnWorkEnd);
  FCurrReq.Free; FCurrReq := nil; FReqList.Delete(0);
  InterlockedDecrement(FReqCount);
end;

procedure TSwinHttp.SyncEvent(event: TSwNotify);
begin
  if Assigned(event) then
  try
    if FThread <> nil then TSwThread(FThread).Setevent(event)
    else event(Self, FCurrReq);
  except end;
end;

procedure TSwinHttp.Get(url: string);
begin
  Request.url.url := url;
  DoRequest;
end;

procedure TSwinHttp.Post(url, PostData: string);
begin
  Request.PostData := PostData; Get(url);
end;

procedure TSwinHttp.Clear;
var i: integer;
begin
  FReqCount := 0; Close;
  if FThread <> nil then FThread.Terminate;
  FThread := nil;
  Request.Clear; Response.Clear;
  for i := FReqList.Count - 1 downto 0 do
  begin
    TSwRequest(FReqList[i]).Free;
    FReqList.Delete(i);
  end;
end;

{ TSwThread }

procedure TSwThread.Execute;
begin
  inherited;
  while not Terminated do
  begin
    if http.FReqCount > 0 then http.ReceiveAll;
    Sleep(30);
  end;
end;

procedure TSwThread.Setevent(const Value: TSwNotify);
begin
  Fevent := Value;
  if @Fevent <> nil then Synchronize(Sync);
  Fevent := nil;
end;

procedure TSwThread.Sync;
begin
  Fevent(http, http.FCurrReq);
end;

{ TSwProxies }

constructor TSwProxies.Create;
begin
  FHttpProxy := TSwProxy.Create;
  FHttpProxy.FProtocol := ppAuto;
  FHttpsProxy := TSwProxy.Create;
  FHttpsProxy.Protocol := ppAuto;
end;

destructor TSwProxies.Destroy;
begin
  FHttpProxy.Free; FHttpsProxy.Free;
  inherited;
end;

function TSwProxies.List: string;
begin
  result := FHttpProxy.ProxyStr;
  if result <> '' then result := 'http=' + result;
  if FHttpsProxy.Protocol <> ppAuto then
  begin
    if result <> '' then result := result + ' ';
    result := result + FHttpsProxy.ProxyStr;
  end;
end;

function TSwProxy.ProxyStr: string;
begin
  case FProtocol of
    ppHttp: result := 'http';
    ppHttps: result := 'https';
    ppSocks: result := 'socks';
    else result := '';
  end;
  if result = '' then Exit;
  result := result + '://' + FHost;
  if FPort <> 0 then result := result + ':' + IntToStr(FPort);
end;

end.

