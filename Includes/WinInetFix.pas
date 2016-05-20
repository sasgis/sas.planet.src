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

implementation

uses
  Windows,
  SysUtils,
  WinInet;

const
  INTERNET_OPTION_MAX_CONNS_PER_SERVER = 73;
  INTERNET_OPTION_MAX_CONNS_PER_1_0_SERVER = 74;

procedure FixMaxConnsPerServerLimit(const ALimit: ULONG = 64);
var
  VBuf: ULONG;
  VRes: Boolean;
begin
  // HTTP 1.1 (Default 2)
  VBuf := ALimit;
  VRes := InternetSetOption(
    nil,
    INTERNET_OPTION_MAX_CONNS_PER_SERVER,
    @VBuf,
    SizeOf(VBuf)
  );

  if not VRes then begin
    RaiseLastWin32Error;
  end;

  // HTTP 1.0 (Default 4)
  VBuf := ALimit;
  VRes := InternetSetOption(
    nil,
    INTERNET_OPTION_MAX_CONNS_PER_1_0_SERVER,
    @VBuf,
    SizeOf(VBuf)
  );

  if not VRes then begin
    RaiseLastWin32Error;
  end;
end;

initialization
  FixMaxConnsPerServerLimit;

end.
