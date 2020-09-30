unit libosmscout_route;

interface

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
    ROUTE_PROFILE_FOOT
  );

  TRouteCaclResult  = (
    CALC_RESULT_OK,
    CALC_RESULT_NODATA,
    CALC_RESULT_ERROR
  );

  point_t = packed record
    lon, lat: double;
  end;
  ppoint_t = ^point_t;

var
  router: packed record
    Handle: THandle;

    init: procedure(); cdecl;

    new: function(out ctx: pointer; const db_path: PAnsiChar): boolean; cdecl;
    del: procedure(ctx: pointer); cdecl;

    calc: function(ctx: pointer; profile: TRouteProfile; const p1, p2: ppoint_t;
            out out_count: uint32_t; out out_points: ppoint_t): TRouteCaclResult; cdecl;

    clear: procedure(ctx: pointer); cdecl;
    get_error_message: function(ctx: pointer): PAnsiChar; cdecl;
  end;

type
  ELibOsmScoutRouteError = Exception;

procedure LibOsmScoutRouteInitialize(const dllname: string = libosmscout_route_dll);
function IsLibOsmScoutRouteAvailable: Boolean;

procedure RiseLibOsmScoutError(const ACtx: Pointer; const AFuncName: string);

implementation

uses
  Windows,
  SyncObjs;

var
  GLock: TCriticalSection = nil;
  GInitialized: Boolean = False;

function IsLibOsmScoutRouteAvailable: Boolean;
begin
  try
    if not GInitialized then begin
      LibOsmScoutRouteInitialize;
    end;
    Result := router.Handle > 0;
  except
    Result := False;
  end;
end;

procedure LibOsmScoutRouteInitialize(const dllname: string);
const
  CFuncNames: array[0..5] of string = (
    'init', 'new', 'del', 'calc', 'clear', 'get_error_message'
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

      VHandle := SafeLoadLibrary(ExtractFilePath(ParamStr(0)) + dllname);
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
  VErr: PAnsiChar;
  VErrStr: string;
begin
  VErrStr := 'Unknown error';
  if ACtx <> nil then begin
    VErr := router.get_error_message(ACtx);
    if VErr <> '' then begin
      VErrStr := string(AnsiString(VErr));
    end;
  end;
  raise ELibOsmScoutRouteError.Create(
    '"router_' + AFuncName + '" failed with error: ' + VErrStr
  );
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
