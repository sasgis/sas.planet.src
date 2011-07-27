unit frm_InvisibleBrowser;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  OleCtrls,
  SHDocVw_EWB,
  EwbCore,
  EmbeddedWB;

type
  TfrmInvisibleBrowser = class(TForm)
    WebBrowser1: TEmbeddedWB;
    procedure FormCreate(Sender: TObject);
    procedure WebBrowser1Authenticate(Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName, szPassWord: WideString; var Rezult: HRESULT);
  private
    { Private declarations }
  public
    procedure NavigateAndWait(AUrl: WideString);
  end;

var
  frmInvisibleBrowser: TfrmInvisibleBrowser;

procedure OpenUrlInBrowser(URL: string);
function GetStreamFromURL(var ms:TMemoryStream;url:string;conttype:string):integer;

implementation

{$R *.dfm}

uses
  StrUtils,
  ShellAPI,
  WinInet,
  i_ProxySettings,
  u_GlobalState;

procedure TfrmInvisibleBrowser.FormCreate(Sender: TObject);
begin
  WebBrowser1.Navigate('about:blank');
end;

{ TfrmInvisibleBrowser }

procedure TfrmInvisibleBrowser.NavigateAndWait(AUrl: WideString);
begin
  WebBrowser1.NavigateWait(AUrl, 10000);
end;

procedure TfrmInvisibleBrowser.WebBrowser1Authenticate(
  Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName,
  szPassWord: WideString; var Rezult: HRESULT);
var
  VProxyConfig: IProxyConfig;
  VUseLogin: Boolean;
begin
  VProxyConfig := GState.InetConfig.ProxyConfig;
  VProxyConfig.LockRead;
  try
    VUselogin := (not VProxyConfig.GetUseIESettings) and VProxyConfig.GetUseProxy and VProxyConfig.GetUseLogin;
    if VUseLogin then begin
      szUserName := VProxyConfig.GetLogin;
      szPassWord := VProxyConfig.GetPassword;
    end;
  finally
    VProxyConfig.UnlockRead;
  end;
end;

procedure OpenUrlInBrowser(URL: string);
begin
  ShellExecute(0, nil, PChar(URL), nil, nil, SW_RESTORE);
end;

function GetStreamFromURL(var ms:TMemoryStream;url:string;conttype:string):integer;
var par,ty:string;
    err:boolean;
    Buffer:array [1..64535] of char;
    BufferLen:LongWord;
    hSession,hFile:Pointer;
    dwtype: array [1..20] of char;
    dwindex, dwcodelen,dwReserv: dword;
    VProxyConfig: IProxyConfig;
    VUselogin: Boolean;
    VLogin: string;
    VPassword: string;
begin
  VProxyConfig := GState.InetConfig.ProxyConfig;
  VProxyConfig.LockRead;
  try
    VUselogin := (not VProxyConfig.GetUseIESettings) and VProxyConfig.GetUseProxy and VProxyConfig.GetUseLogin;
    VLogin := VProxyConfig.GetLogin;
    VPassword := VProxyConfig.GetPassword;
  finally
    VProxyConfig.UnlockRead;
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
