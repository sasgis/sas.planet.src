// --------------------------------------------------------------------------
// Object Pascal Wrapper for Shapefile C Library - compact subset edition
//
//    This unit is based on the work of
//      Keven Meyer (kevin@cybertracker.co.za), 2002, MIT/LGPL lic
//      Alexander Weidauer (alex.weidauer@huckfinn.de), 2003, MIT/LGPL lic
//      Javier Santo Domingo (j-a-s-d@coderesearchlabs.com), 2006-2011, MIT lic
//
// --------------------------------------------------------------------------

unit libshp;

interface

uses
  Windows,
  SysUtils;

{$MINENUMSIZE 4}
{$ALIGN 8}
{$POINTERMATH ON}

type
  ELibShpError = class(Exception);

  PDouble = ^Double;
  PLongInt = ^LongInt;

  // C: "struct SAHooks" - 10 IO/error callbacks + user data. Filled by
  // SASetupUtf8Hooks and passed back to the *LL open/create functions; our
  // code never calls the callbacks itself, so plain pointers are enough.
  SAHooks = record
    FOpen: Pointer;       // SAOpenFn
    FRead: Pointer;       // SAReadFn
    FWrite: Pointer;      // SAWriteFn
    FSeek: Pointer;       // SASeekFn
    FTell: Pointer;       // SATellFn
    FFlush: Pointer;      // SAFlushFn
    FClose: Pointer;      // SACloseFn
    FRemove: Pointer;     // SARemoveFn
    Error: Pointer;       // SAErrorFn
    Atof: Pointer;        // SAAtofFn
    pvUserData: Pointer;
  end;
  PSAHooks = ^SAHooks;

  PShpObject = ^ShpObject;

  // SHPObject - represents one shape (without attributes) read from the
  // .shp file. The array members are plain C pointers, use pointer indexing
  // (e.g. obj.padfX[i], obj.panPartStart[i]) to access their elements.
  ShpObject = record
    nSHPType: Integer;
    nShapeId: Integer; // -1 is unknown/unassigned
    nParts: Integer;

    panPartStart: PLongInt;
    panPartType: PLongInt;

    nVertices: Integer;

    padfX: PDouble;
    padfY: PDouble;
    padfZ: PDouble;
    padfM: PDouble;

    dfXMin: Double;
    dfYMin: Double;
    dfZMin: Double;
    dfMMin: Double;

    dfXMax: Double;
    dfYMax: Double;
    dfZMax: Double;
    dfMMax: Double;

    bMeasureIsUsed: Integer;
    bFastModeReadObject: Integer;
  end;

  // opaque handles: "struct SHPInfo*" / "struct DBFInfo*" - always use the
  // API functions below.
  SHPHandle = Pointer;
  DBFHandle = Pointer;

  DBFFieldType = (
    ftString,
    ftInteger,
    ftDouble,
    ftLogical,
    ftDate,
    ftInvalid
  );

const
  libshp_dll = 'libshp.dll';

  // Shape Types (nSHPType)
  SHPT_NULL         = 0;
  // 2D Shape Types
  SHPT_POINT        = 1; // Points
  SHPT_ARC          = 3; // Arcs (Polylines, possible in parts)
  SHPT_POLYGON      = 5; // Polygons (possible in parts)
  SHPT_MULTIPOINT   = 8; // MultiPoint (related points)
  // 3D Shape Types (may include "measure" values for vertices)
  SHPT_POINTZ       = 11;
  SHPT_ARCZ         = 13;
  SHPT_POLYGONZ     = 15;
  SHPT_MULTIPOINTZ  = 18;
  // 2D + Measure Types
  SHPT_POINTM       = 21;
  SHPT_ARCM         = 23;
  SHPT_POLYGONM     = 25;
  SHPT_MULTIPOINTM  = 28;

  // Part type - everything but SHPT_MULTIPATCH just uses SHPP_RING.
  SHPP_RING         = 5;

  XBASE_FLDNAME_LEN_READ = 11;

var
  // IO/error hooks setup
  SASetupUtf8Hooks: procedure(psHooks: PSAHooks); cdecl;

  // SHP API
  SHPOpenLL: function(const pszShapeFile: PAnsiChar; const pszAccess: PAnsiChar;
    psHooks: PSAHooks): SHPHandle; cdecl;
  SHPCreateLL: function(pszShapeFile: PAnsiChar; nShapeType: LongInt;
    psHooks: PSAHooks): SHPHandle; cdecl;
  SHPGetInfo: procedure(hSHP: SHPHandle; pnEntities: PLongInt;
    pnShapeType: PLongInt; padfMinBound: PDouble;
    padfMaxBound: PDouble); cdecl;
  SHPReadObject: function(hSHP: SHPHandle; iShape: LongInt): PShpObject; cdecl;
  SHPWriteObject: function(hSHP: SHPHandle; iShape: LongInt;
    psObject: PShpObject): LongInt; cdecl;
  SHPDestroyObject: procedure(psObject: PShpObject); cdecl;
  SHPComputeExtents: procedure(psObject: PShpObject); cdecl;
  SHPCreateObject: function(nSHPType: LongInt; nShapeId: LongInt;
    nParts: LongInt; panPartStart: PLongInt; panPartType: PLongInt;
    nVertices: LongInt; padfX: PDouble; padfY: PDouble; padfZ: PDouble;
    padfM: PDouble): PShpObject; cdecl;
  SHPCreateSimpleObject: function(nSHPType: LongInt; nVertices: LongInt;
    padfX: PDouble; padfY: PDouble; padfZ: PDouble): PShpObject; cdecl;
  SHPClose: procedure(hSHP: SHPHandle); cdecl;
  SHPRewindObject: function(hSHP: SHPHandle;
    psObject: PShpObject): LongInt; cdecl;

  // DBF API
  DBFOpenLL: function(const pszDBFFile: PAnsiChar; const pszAccess: PAnsiChar;
    pHooks: PSAHooks): DBFHandle; cdecl;
  DBFCreateLL: function(pszDBFFile: PAnsiChar; pszCodePage: PAnsiChar;
    psHooks: PSAHooks): DBFHandle; cdecl;
  DBFGetFieldCount: function(psDBF: DBFHandle): LongInt; cdecl;
  DBFAddField: function(hDBF: DBFHandle; pszFieldName: PAnsiChar;
    eType: DBFFieldType; nWidth: LongInt; nDecimals: LongInt): LongInt; cdecl;
  DBFGetFieldInfo: function(psDBF: DBFHandle; iField: LongInt;
    pszFieldName: PAnsiChar; pnWidth: PLongInt;
    pnDecimals: PLongInt): DBFFieldType; cdecl;
  DBFReadStringAttribute: function(hDBF: DBFHandle; iShape: LongInt;
    iField: LongInt): PAnsiChar; cdecl;
  DBFIsAttributeNULL: function(hDBF: DBFHandle; iShape: LongInt;
    iField: LongInt): LongInt; cdecl;
  DBFWriteStringAttribute: function(hDBF: DBFHandle; iShape: LongInt;
    iField: LongInt; pszFieldValue: PAnsiChar): LongInt; cdecl;
  DBFWriteNULLAttribute: function(hDBF: DBFHandle; iShape: LongInt;
    iField: LongInt): LongInt; cdecl;
  DBFIsRecordDeleted: function(hDBF: DBFHandle;
    iShape: LongInt): LongInt; cdecl;
  DBFClose: procedure(hDBF: DBFHandle); cdecl;
  DBFGetCodePage: function(hDBF: DBFHandle): PAnsiChar; cdecl;

function LoadLibShp(const ADllName: string = libshp_dll; const ARaiseException: Boolean = True): Boolean;
procedure UnLoadLibShp;

function IsLibShpLoaded: Boolean;

implementation

uses
  SyncObjs;

type
  TLibState = record
    FState: Integer;
    function GetValue: Integer; inline;
    procedure SetValue(const AValue: Integer); inline;
    property Value: Integer read GetValue write SetValue;
  end;

{ TLibState }

const
  LIBSHP_STATE_NONE = 0;
  LIBSHP_STATE_LOADED = 1;
  LIBSHP_STATE_LOAD_ERROR = 2;

function TLibState.GetValue: Integer;
begin
  Result := InterlockedCompareExchange(FState, 0, 0);
end;

procedure TLibState.SetValue(const AValue: Integer);
begin
  InterlockedExchange(FState, AValue);
end;

var
  GState: TLibState;
  GHandle: THandle = 0;
  GLock: TCriticalSection;

function IsLibShpLoaded: Boolean;
begin
  Result := GState.Value = LIBSHP_STATE_LOADED;
end;

function LoadLibShp(const ADllName: string; const ARaiseException: Boolean): Boolean;

  function GetProcAddr(const AProcName: string): Pointer;
  begin
    Result := GetProcAddress(GHandle, PChar(AProcName));
    if Result = nil then begin
      raise ELibShpError.Create('Cannot load function "' + AProcName + '" from ' + ADllName);
    end;
  end;

var
  VState: Integer;
begin
  VState := GState.Value;

  if VState = LIBSHP_STATE_LOADED then begin
    Result := True;
    Exit;
  end;

  if VState = LIBSHP_STATE_LOAD_ERROR then begin
    Result := False;
    Exit;
  end;

  GLock.Acquire;
  try
    VState := GState.Value;

    if VState = LIBSHP_STATE_LOADED then begin
      Result := True;
      Exit;
    end;

    if VState = LIBSHP_STATE_LOAD_ERROR then begin
      Result := False;
      Exit;
    end;

    GHandle := LoadLibrary(PChar(ADllName));
    try
      if GHandle = 0 then begin
        raise ELibShpError.Create('Cannot load ' + ADllName);
      end;

      SASetupUtf8Hooks := GetProcAddr('SASetupUtf8Hooks');

      SHPOpenLL := GetProcAddr('SHPOpenLL');
      SHPCreateLL := GetProcAddr('SHPCreateLL');
      SHPGetInfo := GetProcAddr('SHPGetInfo');
      SHPReadObject := GetProcAddr('SHPReadObject');
      SHPWriteObject := GetProcAddr('SHPWriteObject');
      SHPDestroyObject := GetProcAddr('SHPDestroyObject');
      SHPComputeExtents := GetProcAddr('SHPComputeExtents');
      SHPCreateObject := GetProcAddr('SHPCreateObject');
      SHPCreateSimpleObject := GetProcAddr('SHPCreateSimpleObject');
      SHPClose := GetProcAddr('SHPClose');
      SHPRewindObject := GetProcAddr('SHPRewindObject');

      DBFOpenLL := GetProcAddr('DBFOpenLL');
      DBFCreateLL := GetProcAddr('DBFCreateLL');
      DBFGetFieldCount := GetProcAddr('DBFGetFieldCount');
      DBFAddField := GetProcAddr('DBFAddField');
      DBFGetFieldInfo := GetProcAddr('DBFGetFieldInfo');
      DBFReadStringAttribute := GetProcAddr('DBFReadStringAttribute');
      DBFIsAttributeNULL := GetProcAddr('DBFIsAttributeNULL');
      DBFWriteStringAttribute := GetProcAddr('DBFWriteStringAttribute');
      DBFWriteNULLAttribute := GetProcAddr('DBFWriteNULLAttribute');
      DBFIsRecordDeleted := GetProcAddr('DBFIsRecordDeleted');
      DBFClose := GetProcAddr('DBFClose');
      DBFGetCodePage := GetProcAddr('DBFGetCodePage');

      GState.Value := LIBSHP_STATE_LOADED;
      Result := True;
    except
      on E: Exception do begin
        GState.Value := LIBSHP_STATE_LOAD_ERROR;

        if GHandle <> 0 then begin
          FreeLibrary(GHandle);
          GHandle := 0;
        end;

        if ARaiseException then begin
          raise;
        end else begin
          Result := False;
        end;
      end;
    end;
  finally
    GLock.Release;
  end;
end;

procedure UnLoadLibShp;
begin
  GLock.Acquire;
  try
    GState.Value := LIBSHP_STATE_NONE;

    SASetupUtf8Hooks := nil;

    SHPOpenLL := nil;
    SHPCreateLL := nil;
    SHPGetInfo := nil;
    SHPReadObject := nil;
    SHPWriteObject := nil;
    SHPDestroyObject := nil;
    SHPComputeExtents := nil;
    SHPCreateObject := nil;
    SHPCreateSimpleObject := nil;
    SHPClose := nil;
    SHPRewindObject := nil;

    DBFOpenLL := nil;
    DBFCreateLL := nil;
    DBFGetFieldCount := nil;
    DBFAddField := nil;
    DBFGetFieldInfo := nil;
    DBFReadStringAttribute := nil;
    DBFIsAttributeNULL := nil;
    DBFWriteStringAttribute := nil;
    DBFWriteNULLAttribute := nil;
    DBFIsRecordDeleted := nil;
    DBFClose := nil;
    DBFGetCodePage := nil;

    if GHandle <> 0 then begin
      FreeLibrary(GHandle);
      GHandle := 0;
    end;
  finally
    GLock.Release;
  end;
end;

initialization
  GLock := TCriticalSection.Create;
  GState.FState := LIBSHP_STATE_NONE;

finalization
  UnLoadLibShp;
  FreeAndNil(GLock);

end.
