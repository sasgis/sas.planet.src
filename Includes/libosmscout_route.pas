unit libosmscout_route;

interface

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

uses
  SysUtils;

const
  libosmscout_route_dll = 'libosmscout_route.dll';

{$MINENUMSIZE 4}
type
  uint32_t = Cardinal;

  TRouteProfile = (
    ROUTE_PROFILE_CAR,
    ROUTE_PROFILE_BIKE,
    ROUTE_PROFILE_FOOT,
    ROUTE_PROFILE_UNDEF
  );

  TRouterResult  = (
    ROUTER_RESULT_OK            = 0,
    ROUTER_RESULT_ERROR         = 1,
    ROUTER_RESULT_NODATA_START  = 100,
    ROUTER_RESULT_NODATA_TARGET = 101,
    ROUTER_RESULT_NODATA_ROUTE  = 102
  );

  point_t = record
    lon, lat: double;
  end;
  ppoint_t = ^point_t;

  router_version_t = record
    libosmscout_db_file_version: uint32_t;
    libosmscout_commit_hash: PAnsiChar;
  end;
  prouter_version_t = ^router_version_t;

var
  router: packed record
    Handle: THandle;

    init: procedure(); cdecl;

    new: function(out ctx: pointer; const opt: pointer): TRouterResult; cdecl;
    del: procedure(ctx: pointer); cdecl;

    calc: function(ctx: pointer; profile: TRouteProfile; const p1, p2: ppoint_t;
            out out_count: uint32_t; out out_points: ppoint_t): TRouterResult; cdecl;

    clear: procedure(ctx: pointer); cdecl;
    get_error_message: function(ctx: pointer): PAnsiChar; cdecl;

    get_version: function(): prouter_version_t; cdecl;

    // options
    opt_new: function(out opt: pointer): TRouterResult; cdecl;
    opt_del: procedure(opt: pointer); cdecl;
    opt_set_dbpath: function(opt: pointer; const db_path: PAnsiChar; db_count: uint32_t): TRouterResult; cdecl;
    opt_set_rnode_radius: function(opt: pointer; radius: uint32_t): TRouterResult; cdecl;
  end;

type
  ELibOsmScoutRouteError = class(Exception);

procedure LibOsmScoutRouteInitialize(const dllname: string = libosmscout_route_dll);
function IsLibOsmScoutRouteAvailable(const dllname: string = libosmscout_route_dll): Boolean;

procedure RiseLibOsmScoutError(const ACtx: Pointer; const AFuncName: string);

implementation

uses
  Windows,
  SyncObjs;

var
  GLock: TCriticalSection = nil;
  GInitialized: Boolean = False;

function GetFullLibName(const dllname: string): string; inline;
begin
  Result := ExtractFilePath(ParamStr(0)) + dllname;
end;

function IsLibOsmScoutRouteAvailable(const dllname: string): Boolean;
begin
  try
    if not FileExists(GetFullLibName(dllname)) then begin
      Result := False;
      Exit;
    end;
    if not GInitialized then begin
      LibOsmScoutRouteInitialize(dllname);
    end;
    Result := router.Handle > 0;
  except
    Result := False;
  end;
end;

procedure LibOsmScoutRouteInitialize(const dllname: string);
const
  CFuncNames: array[0..10] of string = (
    'init', 'new', 'del', 'calc', 'clear', 'get_error_message', 'get_version',
    'opt_new', 'opt_del', 'opt_set_dbpath', 'opt_set_rnode_radius'
  );
var
  I: Integer;
  P: PPointer;
  VHandle: THandle;
begin
  GLock.Acquire;
  try
    VHandle := 0;
    if not GInitialized then
    try
      GInitialized := True;
      VHandle := SafeLoadLibrary(GetFullLibName(dllname));
      if VHandle = 0 then begin
        VHandle := SafeLoadLibrary(dllname);
      end;
      if VHandle = 0 then begin
        raise ELibOsmScoutRouteError.CreateFmt('Unable to load library %s', [dllname]);
      end;

      P := @@router.init;
      for I := Low(CFuncNames) to High(CFuncNames) do begin
        P^ := GetProcAddress(VHandle, PChar('router_' + CFuncNames[I]));
        if P^ = nil then begin
          raise ELibOsmScoutRouteError.CreateFmt(
            'Unable to find %s() in %s', [CFuncNames[I], dllname]
          );
        end;
        Inc(P);
      end;

      router.Handle := VHandle;
      router.init;

    except
      on E: Exception do begin
        if VHandle <> 0 then begin
          FreeLibrary(VHandle);
        end;
        router.Handle := 0;
        raise;
      end;
    end;
  finally
    GLock.Release;
  end;
end;

procedure RiseLibOsmScoutError(const ACtx: Pointer; const AFuncName: string);
var
  VErr: AnsiString;
  VMsg: string;
begin
  VErr := '';
  if ACtx <> nil then begin
    VErr := router.get_error_message(ACtx);
  end;
  if VErr = '' then begin
    VMsg := Format(
      'Function "router_%s" failed with no error message!', [AFuncName]
    );
  end else begin
    VMsg := Format(
      'Function "router_%s" failed with error: %s', [AFuncName, VErr]
    );
  end;
  raise ELibOsmScoutRouteError.Create(VMsg);
end;

initialization
  router.Handle := 0;
  GLock := TCriticalSection.Create;

finalization
  FreeAndNil(GLock);
  if router.Handle > 0 then begin
    FreeLibrary(router.Handle);
    router.Handle := 0;
  end;

end.
