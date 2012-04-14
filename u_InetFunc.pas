unit u_InetFunc;

interface

uses
  Windows,
  Classes,
  i_ProxySettings;

procedure OpenUrlInBrowser(const URL: string);
function GetStreamFromURL(
  ms: TMemoryStream;
  const url:string;
  const conttype:string;
  const AProxyConfig: IProxyConfigStatic
):integer;

implementation

uses
  SysUtils,
  StrUtils,
  ShellAPI,
  WinInet;

procedure OpenUrlInBrowser(const URL: string);
begin
  ShellExecute(0, nil, PChar(URL), nil, nil, SW_RESTORE);
end;

function GetStreamFromURL(
  ms: TMemoryStream;
  const url:string;
  const conttype:string;
  const AProxyConfig: IProxyConfigStatic
):integer;
var par,ty:string;
    err:boolean;
    Buffer:array [1..64535] of char;
    BufferLen:LongWord;
    hSession,hFile:Pointer;
    dwtype: array [1..20] of char;
    dwindex, dwcodelen,dwReserv: dword;
    VUselogin: Boolean;
    VLogin: string;
    VPassword: string;
begin
  VUselogin := (not AProxyConfig.UseIESettings) and AProxyConfig.UseProxy and AProxyConfig.UseLogin;
  VLogin := AProxyConfig.Login;
  VPassword := AProxyConfig.Password;

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
