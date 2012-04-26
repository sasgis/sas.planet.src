unit u_proj4;

interface

{ proj 4.4.7 }

uses
  Windows,
  SysUtils,
  t_GeoTypes;

type
  PProjPJ = Pointer;

  // full interface
  IProj4Full = interface
  ['{BE0ADFF4-D0C5-4BD7-B09A-C4D9F8D9A273}']
    function Available: Boolean;
    function MakeProj(const Args, APath: String): PProjPJ; stdcall;
    function KillProj(AProjPJ: PProjPJ): Integer; stdcall;
    function LonLat2XY(const AProjLP: TDoublePoint; const projPJ: PProjPJ): TDoublePoint; stdcall;
    function XY2LonLat(const AProjXY: TDoublePoint; const projPJ: PProjPJ): TDoublePoint; stdcall;
  end;

  // single projection converter interface
  IProj4Conv = interface
  ['{74309735-EB2E-4996-A226-1C172F076737}']
    function AvailableConv: Boolean; stdcall;
    function SetProj(const Args, APath: String): Boolean; stdcall;
    function SetEPSG(const AEPSG: Integer): Boolean; stdcall;
    function LonLat2XY(const AProjLP: TDoublePoint): TDoublePoint; stdcall;
    function XY2LonLat(const AProjXY: TDoublePoint): TDoublePoint; stdcall;
  end;
  
  EProj4Unavailable = class(Exception);

// make full proj4 object
function CreateProj4Full: IProj4Full;
// make single projection converter
function CreateProj4Conv: IProj4Conv;

implementation

const
   proj447_dll='proj447.dll';

const
 { Scaling value radiant to decimal degree }
 RAD_TO_DEG   =	57.29577951308232;
 { Scaling value decimal degree to radiant }
 DEG_TO_RAD   =	0.0174532925199432958;

type
  TInitStatus = (is_Unknown, is_Available, is_Failed);

  TProj4Abstract = class(TInterfacedObject)
  protected
    FInitStatus: TInitStatus;
    FDLLHandle: THandle;
    // routines
    F_pj_init_plus_path: Pointer;
    F_pj_fwd: Pointer;
    F_pj_inv: Pointer;
    F_pj_free: Pointer;
  protected
    function InternalInitLib: Boolean;
    function InternalLoadLib: Boolean;
    procedure InternalZero;
  protected
    { IProj4Full }
    function Available: Boolean;
    function MakeProj(const Args, APath: String): PProjPJ; stdcall;
    function KillProj(AProjPJ: PProjPJ): Integer; stdcall;
    function LonLat2XY(const AProjLP: TDoublePoint; const projPJ: PProjPJ): TDoublePoint; stdcall;
    function XY2LonLat(const AProjXY: TDoublePoint; const projPJ: PProjPJ): TDoublePoint; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TProj4Full = class(TProj4Abstract, IProj4Full)
  end;

  TProj4Conv = class(TProj4Abstract, IProj4Conv)
  protected
    FProjPJ: PProjPJ;
  protected
    procedure InternalKillProj;
    { IProj4Conv }
    function AvailableConv: Boolean; stdcall;
    function SetProj(const Args, APath: String): Boolean; stdcall;
    function SetEPSG(const AEPSG: Integer): Boolean; stdcall;
    function LonLat2XY(const AProjLP: TDoublePoint): TDoublePoint; stdcall;
    function XY2LonLat(const AProjXY: TDoublePoint): TDoublePoint; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

type
  // proj4 routines (as types)
  TProjXY = TDoublePoint;
  TProjLP = TDoublePoint;

  T_pj_init_plus_path = function(
    const Args: PAnsiChar;
    const Path: PAnsiChar
  ): PProjPJ; cdecl;

  { forward projection normally Longitude Latitude to plain xy Coordinates }
  T_pj_fwd = function(
    ProjLP: TProjLP;
    const projPJ: PProjPJ
  ): TProjXY; cdecl;

  { inverse projection normally plain xy coordinates to longitude latitude coordinates }
  T_pj_inv = function(
    ProjXY: TProjXY;
    const projPJ: PProjPJ
  ): TProjLP; cdecl;

  { Free the allocated projections }
  T_pj_free = function(
    projPJ: PProjPJ
  ): Integer; cdecl;

function CreateProj4Full: IProj4Full;
begin
  //Result := g_Proj4;
  Result := TProj4Full.Create;
end;

function CreateProj4Conv: IProj4Conv;
begin
  Result := TProj4Conv.Create;
end;

{ TProj4Abstract }

function TProj4Abstract.Available: Boolean;
begin
  if (is_Unknown = FInitStatus) then begin
    // load on demand
    InternalLoadLib;
  end;
  Result := (is_Available = FInitStatus);
end;

constructor TProj4Abstract.Create;
begin
  inherited Create;
  FDLLHandle := 0;
  FInitStatus := is_Unknown;
  InternalZero;
end;

destructor TProj4Abstract.Destroy;
begin
  if (FDLLHandle <> 0) then begin
    FreeLibrary(FDLLHandle);
    FDLLHandle := 0;
    InternalZero;
  end;
  inherited;
end;

function TProj4Abstract.InternalInitLib: Boolean;
begin
  F_pj_init_plus_path:=GetProcAddress(FDLLHandle,'_pj_init_plus_path');
  F_pj_fwd:=GetProcAddress(FDLLHandle,'_pj_fwd');
  F_pj_inv:=GetProcAddress(FDLLHandle,'_pj_inv');
  F_pj_free:=GetProcAddress(FDLLHandle,'_pj_free');
  Result := (F_pj_init_plus_path <> nil) and
            (F_pj_fwd <> nil) and
            (F_pj_inv <> nil) and
            (F_pj_free <> nil);
end;

function TProj4Abstract.InternalLoadLib: Boolean;
begin
  Result := TRUE;
  
  if (FDLLHandle<>0) then
    Exit;

  // try to load library
  FDLLHandle := LoadLibrary(proj447_dll);
  if (FDLLHandle<>0) then begin
    // try to check routines
    if InternalInitLib then
      FInitStatus := is_Available
    else begin
      FInitStatus := is_Failed;
      InternalZero;
      Result := FALSE;
    end;
  end else begin
    // failed
    FInitStatus := is_Failed;
    Result := FALSE;
  end;
end;

procedure TProj4Abstract.InternalZero;
begin
  F_pj_init_plus_path:=nil;
  F_pj_fwd:=nil;
  F_pj_inv:=nil;
  F_pj_free:=nil;
end;

function TProj4Abstract.KillProj(AProjPJ: PProjPJ): Integer;
begin
  if Available and (F_pj_free<>nil) then
    Result := T_pj_free(F_pj_free)(AProjPJ)
  else
    raise EProj4Unavailable.Create('');
end;

function TProj4Abstract.LonLat2XY(const AProjLP: TDoublePoint; const projPJ: PProjPJ): TDoublePoint;
var
  VProjLP: TProjLP;
begin
  if Available and (F_pj_fwd<>nil) then begin
    VProjLP.X := AProjLP.X * DEG_TO_RAD;
    VProjLP.Y := AProjLP.Y * DEG_TO_RAD;
    Result := T_pj_fwd(F_pj_fwd)(VProjLP, projPJ);
  end else
    raise EProj4Unavailable.Create('');;
end;

function TProj4Abstract.MakeProj(const Args, APath: String): PProjPJ;
begin
  if Available and (F_pj_init_plus_path<>nil) then
    Result := T_pj_init_plus_path(F_pj_init_plus_path)(PAnsiChar(Args), PAnsiChar(APath))
  else
    raise EProj4Unavailable.Create('');
end;

function TProj4Abstract.XY2LonLat(const AProjXY: TDoublePoint; const projPJ: PProjPJ): TDoublePoint;
begin
  if Available and (F_pj_inv<>nil) then begin
    Result := T_pj_inv(F_pj_inv)(AProjXY, projPJ);
    Result.X := Result.X * RAD_TO_DEG;
    Result.Y := Result.Y * RAD_TO_DEG;
  end else
    raise EProj4Unavailable.Create('');
end;

{ TProj4Conv }

function TProj4Conv.AvailableConv: Boolean;
begin
  Result := Available and (FProjPJ<>nil);
end;

constructor TProj4Conv.Create;
begin
  inherited Create;
  // init
  FProjPJ := nil;
end;

destructor TProj4Conv.Destroy;
begin
  InternalKillProj;
  inherited;
end;

procedure TProj4Conv.InternalKillProj;
begin
  if FProjPJ<>nil then begin
    KillProj(FProjPJ);
    FProjPJ:=nil;
  end;
end;

function TProj4Conv.LonLat2XY(const AProjLP: TDoublePoint): TDoublePoint;
begin
  if AvailableConv then
    Result := inherited LonLat2XY(AProjLP, FProjPJ)
  else
    raise EProj4Unavailable.Create('');
end;

function TProj4Conv.SetEPSG(const AEPSG: Integer): Boolean;
begin
  if (AEPSG>=32630) and (AEPSG<32660) then begin
    // known
    Result := SetProj('+proj=utm +zone='+IntToStr(AEPSG-32600)+' +ellps=WGS84 +datum=WGS84 +units=m +no_defs', '');
  end else begin
    // not implemented yet
    Result := FALSE;
  end;
end;

function TProj4Conv.SetProj(const Args, APath: String): Boolean;
begin
  Result := FALSE;
  InternalKillProj;
  // make proj
  if Available then begin
    FProjPJ := MakeProj(Args, APath);
    Result := (FProjPJ<>nil);
  end;
end;

function TProj4Conv.XY2LonLat(const AProjXY: TDoublePoint): TDoublePoint;
begin
  if AvailableConv then
    Result := inherited XY2LonLat(AProjXY, FProjPJ)
  else
    raise EProj4Unavailable.Create('');
end;

end.
