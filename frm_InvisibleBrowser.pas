unit frm_InvisibleBrowser;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  SyncObjs,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  OleCtrls,
  SHDocVw_EWB,
  EwbCore,
  EmbeddedWB,
  i_ProxySettings;

type
  TfrmInvisibleBrowser = class(TForm)
    WebBrowser1: TEmbeddedWB;
    procedure FormCreate(Sender: TObject);
    procedure WebBrowser1Authenticate(Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName, szPassWord: WideString; var Rezult: HRESULT);
  private
    FCS: TCriticalSection;
    FProxyConfig: IProxyConfigStatic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure NavigateAndWait(AUrl: WideString; AProxyConfig: IProxyConfigStatic);
  end;

var
  frmInvisibleBrowser: TfrmInvisibleBrowser;

procedure OpenUrlInBrowser(URL: string);
function GetStreamFromURL(var ms:TMemoryStream;url:string;conttype:string; AProxyConfig: IProxyConfigStatic):integer;

implementation

{$R *.dfm}

uses
  StrUtils,
  ShellAPI,
  WinInet,
  u_GlobalState;

constructor TfrmInvisibleBrowser.Create(AOwner: TComponent);
begin
  inherited;
  FCS := TCriticalSection.Create;
end;

destructor TfrmInvisibleBrowser.Destroy;
begin
  FreeAndNil(FCS);
  inherited;
end;

procedure TfrmInvisibleBrowser.FormCreate(Sender: TObject);
begin
  WebBrowser1.Navigate('about:blank');
end;

{ TfrmInvisibleBrowser }

procedure TfrmInvisibleBrowser.NavigateAndWait(AUrl: WideString; AProxyConfig: IProxyConfigStatic);
begin
  FCS.Acquire;
  try
    FProxyConfig := AProxyConfig;
    try
      WebBrowser1.NavigateWait(AUrl, 10000);
    finally
      FProxyConfig := nil;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TfrmInvisibleBrowser.WebBrowser1Authenticate(
  Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName,
  szPassWord: WideString; var Rezult: HRESULT);
var
  VUseLogin: Boolean;
begin
  if FProxyConfig <> nil then begin
    VUselogin := (not FProxyConfig.UseIESettings) and FProxyConfig.UseProxy and FProxyConfig.UseLogin;
    if VUselogin then begin
      szUserName := FProxyConfig.Login;
      szPassWord := FProxyConfig.Password;
    end;
  end;
end;

procedure OpenUrlInBrowser(URL: string);
begin
  ShellExecute(0, nil, PChar(URL), nil, nil, SW_RESTORE);
end;

function GetStreamFromURL(var ms:TMemoryStream;url:string;conttype:string; AProxyConfig: IProxyConfigStatic):integer;
var par,ty:string;
    err:boolean;
    Buffer:array [1..64535] of char;
    BufferLen:LongWord;
    hSession,hFile:Pointer;
    dwtype: array [1..20] of char;
    dwindex, dwcodelen,dwReserv: dword;
    FProxyConfig: IProxyConfig;
    VUselogin: Boolean;
    VLogin: string;
    VPassword: string;
begin
  FProxyConfig := GState.InetConfig.ProxyConfig;
  FProxyConfig.LockRead;
  try
    VUselogin := (not FProxyConfig.GetUseIESettings) and FProxyConfig.GetUseProxy and FProxyConfig.GetUseLogin;
    VLogin := FProxyConfig.GetLogin;
    VPassword := FProxyConfig.GetPassword;
  finally
    FProxyConfig.UnlockRead;
  end;

 hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
 if Assigned(hSession)
  then begin
        hFile:=InternetOpenURL(hSession,PChar(URL),PChar(par),length(par), INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_RELOAD,0);
        if Assigned(hFile)then
         begin
          dwcodelen:=150; dwReserv:=0; dwindex:=0;
          if HttpQueryInfo(hFile,HTTP_QUERY_STATUS_CODE,@dwtype, dwcodelen, dwReserv)
           then dwindex:=strtoint(pchar(@dwtype));
          if (dwindex=HTTP_STATUS_PROXY_AUTH_REQ) then
           begin
            if VUselogin then
             begin
              InternetSetOption (hFile, INTERNET_OPTION_PROXY_USERNAME,PChar(VLogin), length(VLogin));
              InternetSetOption (hFile, INTERNET_OPTION_PROXY_PASSWORD,PChar(VPassword), length(VPassword));
              HttpSendRequest(hFile, nil, 0,Nil, 0);
             end;
            dwcodelen:=150; dwReserv:=0; dwindex:=0;
            if HttpQueryInfo(hFile,HTTP_QUERY_STATUS_CODE,@dwtype, dwcodelen, dwReserv)
             then dwindex:=strtoint(pchar(@dwtype));
            if (dwindex=HTTP_STATUS_PROXY_AUTH_REQ) then //Неверные пароль логин
             begin
            	result:=-3;
              InternetCloseHandle(hFile);
              InternetCloseHandle(hSession);
              exit
             end;
           end;
          result:=0;
          dwindex:=0; dwcodelen:=150; ty:='';
          fillchar(dwtype,sizeof(dwtype),0);
          if HttpQueryInfo(hfile,HTTP_QUERY_CONTENT_TYPE, @dwtype,dwcodelen,dwindex)
           then ty:=PChar(@dwtype);
          if (PosEx(conttype,ty,1)>0) then
          repeat
           err:=not(internetReadFile(hFile,@Buffer,SizeOf(Buffer),BufferLen));
           ms.Write(Buffer,BufferLen);
           inc(result,BufferLen)
          until (BufferLen=0)and(BufferLen<SizeOf(Buffer))and(err=false)
          else result:=-1;
         end
        else result:=0;
       end
  else result:=0;
  ms.Position:=0;
end;

end.
