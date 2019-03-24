{
  https://support.microsoft.com/en-us/kb/183110

  Summary

  WinInet limits the number of simultaneous connections that it makes to a
  single HTTP server. If you exceed this limit, the requests block until one of
  the current connections has completed. This is by design and is in agreement
  with the HTTP specification and industry standards.

  More information

  WinInet limits connections to a single HTTP 1.0 server to four simultaneous
  connections. Connections to a single HTTP 1.1 server are limited to two
  simultaneous connections. The HTTP 1.1 specification (RFC2616) mandates
  the two-connection limit. The four-connection limit for HTTP 1.0 is a
  self-imposed restriction that coincides with the standard that is used by a
  number of popular Web browsers.

  The only evidence of this limitation to your application is that calls such
  as HttpSendRequest and InternetOpenURL appear to take longer to complete
  because they wait for previous connections to be freed up before their
  requests are sent.

  You can configure WinInet to exceed this limit by creating and setting the
  following registry entries:

  Note By changing these settings, you cause WinInet to go against the HTTP
  protocol specification recommendation. You should only do this if absolutely
  necessary and then you should avoid doing standard Web browsing while these
  settings are in effect:

  HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Internet Settings

  MaxConnectionsPerServer REG_DWORD
  (Default 2)
  Sets the number of simultaneous requests to a single HTTP 1.1 Server

  MaxConnectionsPer1_0Server REG_DWORD
  (Default 4)
  Sets the number of simultaneous requests to a single HTTP 1.0 Server
  These settings are made for a particular user and will have no affect on
  other users who log on to the computer.

  In Internet Explorer 5, it is possible to change the connection limit
  programmatically by calling the InternetSetOption function on NULL handle
  with the following flags (note that it will change connection limit for the
  whole process):

  INTERNET_OPTION_MAX_CONNS_PER_SERVER
  INTERNET_OPTION_MAX_CONNS_PER_1_0_SERVER

  Note If the process has established a connection to a server, if you change
  the connection limit by calling InternetSetOption, the function does not have
  any effect to subsequent connections on the same server. This occurs even
  if a previous connection is disconnected prior to the call to InternetSetOption.
  Connection limit does affect all other servers.

}

unit WinInetFix;

interface

procedure SetMaxConnsPerServerLimit(const ALimit: Cardinal);
procedure SetMaxConnsPerProxyLimit(const ALimit: Cardinal);

function IsMaxConnsPerProxyAvailable: Boolean;

implementation

uses
  Windows,
  SysUtils,
  Registry,
  WinInet;

const
  INTERNET_OPTION_MAX_CONNS_PER_SERVER = 73;
  INTERNET_OPTION_MAX_CONNS_PER_1_0_SERVER = 74;
  INTERNET_OPTION_MAX_CONNS_PER_PROXY = 103;

procedure _SetConnsLimit(ALimit: Cardinal; AOptionID: Cardinal); inline;
begin
  if not InternetSetOption(nil, AOptionID, @ALimit, SizeOf(ALimit)) then begin
    RaiseLastOSError;
  end;
end;

procedure SetMaxConnsPerServerLimit(const ALimit: Cardinal);
begin
  // HTTP 1.1
  _SetConnsLimit(ALimit, INTERNET_OPTION_MAX_CONNS_PER_SERVER);

  // HTTP 1.0
  _SetConnsLimit(ALimit, INTERNET_OPTION_MAX_CONNS_PER_1_0_SERVER);
end;

procedure SetMaxConnsPerProxyLimit(const ALimit: Cardinal);
begin
  _SetConnsLimit(ALimit, INTERNET_OPTION_MAX_CONNS_PER_PROXY);
end;

function IsMaxConnsPerProxyAvailable: Boolean;

  function _GetIEVersion: string;
  var
    VReg: TRegistry;
  begin
    VReg := TRegistry.Create;
    try
      VReg.RootKey := HKEY_LOCAL_MACHINE;
      if VReg.OpenKeyReadOnly('Software\Microsoft\Internet Explorer') then
      try
        Result := VReg.ReadString('svcVersion'); // IE 10 and newer
        if Result = '' then begin
          Result := VReg.ReadString('Version'); // IE 9 and older
        end;
      finally
        VReg.CloseKey;
      end;
    finally
      VReg.Free;
    end;
  end;

var
  I: Integer;
  VStr: string;
  VMajor: string;
begin
  Result := False;
  VStr := _GetIEVersion;
  I := Pos('.', VStr);
  if I > 0 then begin
    VMajor := Copy(VStr, 1, I - 1);
    if (VMajor <> '') and TryStrToInt(VMajor, I) then begin
      Result := I >= 8;
    end;
  end;
end;

end.
