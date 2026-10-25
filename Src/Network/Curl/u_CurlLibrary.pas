{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_CurlLibrary;

interface

uses
  SyncObjs,
  mormot.core.os,
  mormot.lib.curl,
  u_BaseInterfacedObject;

type
  {
    Own late binding of a libcurl library instance.

    Unlike the process-wide mormot.lib.curl binding (LibCurlInitialize +
    the global "curl" object), every TCurlLibrary instance loads and binds
    the requested dll on its own. This allows the standard libcurl.dll and
    the impersonate build to coexist in one process.

    The instance never raises in its constructor: on any loading or
    binding problem Available stays False and LoadError holds the reason.
  }
  TCurlLibrary = class(TSynLibrary)
  private
    FAvailable: Boolean;
    FLoadError: string;
    FVersionNum: Cardinal;
  public
    // standard libcurl API
    global_init: function(flags: TCurlGlobalInit): TCurlResult; cdecl;
    version_info: function(age: TCurlVersion): PCurlVersionInfo; cdecl;
    easy_init: function: TCurl; cdecl;
    easy_setopt: function(curl: TCurl; option: TCurlOption): TCurlResult; cdecl varargs;
    easy_perform: function(curl: TCurl): TCurlResult; cdecl;
    easy_cleanup: procedure(curl: TCurl); cdecl;
    easy_getinfo: function(curl: TCurl; info: TCurlInfo; out value): TCurlResult; cdecl;
    easy_reset: procedure(curl: TCurl); cdecl;
    easy_strerror: function(code: TCurlResult): PAnsiChar; cdecl;
    slist_append: function(list: TCurlSList; s: PAnsiChar): TCurlSList; cdecl;
    slist_free_all: procedure(list: TCurlSList); cdecl;
    // curl-impersonate extension; stays nil in the standard libcurl
    easy_impersonate: function(curl: TCurl; target: PAnsiChar; default_headers: Integer): TCurlResult; cdecl;
  public
    property Available: Boolean read FAvailable;
    property LoadError: string read FLoadError;
    property VersionNum: Cardinal read FVersionNum; // libcurl version number, e.g. $072000 for 7.32.0
  public
    constructor Create(
      const ADllName: string;
      const ARequireImpersonate: Boolean
    );
  end;

  {
    Thread-safe owner of the shared TCurlLibrary instances: one for
    the standard libcurl.dll and one for libcurl-impersonate.dll.
    Both are created lazily on the first request and then reused by
    all the http clients.
  }
  ICurlLibraryProvider = interface
    ['{67AA7F25-B057-41CA-B983-56FFBB127035}']
    function GetLibrary(const AImpersonate: Boolean): TCurlLibrary;
  end;

  TCurlLibraryProvider = class(TBaseInterfacedObject, ICurlLibraryProvider)
  private
    FLock: TCriticalSection;
    FCurlLib: TCurlLibrary;
    FCurlImpersonateLib: TCurlLibrary;
  private
    function GetLibrary(const AImpersonate: Boolean): TCurlLibrary;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_GlobalDllName;

{ TCurlLibrary }

constructor TCurlLibrary.Create(
  const ADllName: string;
  const ARequireImpersonate: Boolean
);
var
  VInfo: PCurlVersionInfo;
  VRes: TCurlResult;
begin
  inherited Create;

  FAvailable := False;
  Self.TryFromExecutableFolder := True;

  try
    if not Self.TryLoadLibrary([ADllName], ECurl) then begin
      FLoadError := Format('Can''t load library %s', [ADllName]);
      Exit;
    end;

    Self.Resolve('curl_', 'global_init', @@global_init, ECurl);
    Self.Resolve('curl_', 'version_info', @@version_info, ECurl);
    Self.Resolve('curl_', 'easy_init', @@easy_init, ECurl);
    Self.Resolve('curl_', 'easy_setopt', @@easy_setopt, ECurl);
    Self.Resolve('curl_', 'easy_perform', @@easy_perform, ECurl);
    Self.Resolve('curl_', 'easy_cleanup', @@easy_cleanup, ECurl);
    Self.Resolve('curl_', 'easy_getinfo', @@easy_getinfo, ECurl);
    Self.Resolve('curl_', 'easy_reset', @@easy_reset, ECurl);
    Self.Resolve('curl_', 'easy_strerror', @@easy_strerror, ECurl);
    Self.Resolve('curl_', 'slist_append', @@slist_append, ECurl);
    Self.Resolve('curl_', 'slist_free_all', @@slist_free_all, ECurl);

    // optional: only the impersonate build exports it
    Self.Resolve('curl_', 'easy_impersonate', @@easy_impersonate);
    if ARequireImpersonate and (@easy_impersonate = nil) then begin
      FLoadError := Format(
        '%s does not export curl_easy_impersonate - is this the impersonate build of libcurl?',
        [ADllName]
      );
      Self.FreeLib;
      Exit;
    end;

    // https://curl.se/libcurl/c/curl_version_info.html
    VInfo := version_info(cv11);
    if VInfo <> nil then begin
      FVersionNum := VInfo^.version_num;
    end;

    // curl_global_init() is not thread-safe - serialize even between
    // instances bound to the same dll module; GlobalLock is mORMot's
    // process-wide lock meant exactly for one-time initialization of
    // external libraries (reentrant, no deadlock: the lock is held
    // only around this single call which never re-enters our code)
    GlobalLock;
    try
      VRes := global_init([giAll]);
    finally
      GlobalUnLock;
    end;
    if VRes <> crOK then begin
      FLoadError := Format('curl_global_init() failed with error %d', [Ord(VRes)]);
      Exit;
    end;

    FAvailable := True;
  except
    on E: Exception do begin
      FLoadError := E.Message;
      Self.FreeLib;
    end;
  end;
end;

{ TCurlLibraryProvider }

constructor TCurlLibraryProvider.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FCurlLib := nil;
  FCurlImpersonateLib := nil;
end;

destructor TCurlLibraryProvider.Destroy;
begin
  FreeAndNil(FCurlImpersonateLib);
  FreeAndNil(FCurlLib);
  FreeAndNil(FLock);
  inherited Destroy;
end;

function TCurlLibraryProvider.GetLibrary(const AImpersonate: Boolean): TCurlLibrary;
begin
  FLock.Enter;
  try
    if AImpersonate then begin
      if FCurlImpersonateLib = nil then begin
        FCurlImpersonateLib := TCurlLibrary.Create(GDllName.CurlImpersonate, True);
      end;
      Result := FCurlImpersonateLib;
    end else begin
      if FCurlLib = nil then begin
        FCurlLib := TCurlLibrary.Create(GDllName.Curl, False);
      end;
      Result := FCurlLib;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
