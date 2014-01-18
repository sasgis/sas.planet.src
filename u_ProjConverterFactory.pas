unit u_ProjConverterFactory;

interface

uses
  Windows,
  SysUtils,
  i_ProjConverter,
  u_BaseInterfacedObject;

type
  TProjConverterFactory = class(TBaseInterfacedObject, IProjConverterFactory)
  private
    FSync: IReadWriteSync;
    FDllFailed: Boolean;
    FDllHolder: IInterface;
  private
    procedure InitDll;
    function GetArgsByEpsg(const AEPSG: Integer): AnsiString;
    function GetByInitStringInternal(const AArgs: AnsiString): IProjConverter;
  private
    function GetByEPSG(const AEPSG: Integer): IProjConverter;
    function GetByInitString(const AArgs: AnsiString): IProjConverter;
  public
    constructor Create;
  end;

implementation

uses
  ALString,
  t_GeoTypes,
  u_Synchronizer;

type
  // proj4 routines (as types)
  TProjXY = TDoublePoint;
  TProjLP = TDoublePoint;
  PProjPJ = Pointer;

type
  IProjDLLHolder = interface
    function ProjInit(
      const Args: PAnsiChar
    ): PProjPJ;
    function ProjFree(
      projPJ: PProjPJ
    ): Integer;

    function ProjForward(
      ProjLP: TProjLP;
      const projPJ: PProjPJ
    ): TProjXY;
    function ProjInverse(
      ProjXY: TProjXY;
      const projPJ: PProjPJ
    ): TProjLP;
  end;

type
  T_pj_init_plus = function(
    const Args: PAnsiChar
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

type
  TProjDLLHolder = class(TBaseInterfacedObject, IProjDLLHolder)
  private
    FDLLHandle: THandle;
    F_pj_init_plus: T_pj_init_plus;
    F_pj_fwd: T_pj_fwd;
    F_pj_inv: T_pj_inv;
    F_pj_free: T_pj_free;
  private
    function ProjInit(
      const Args: PAnsiChar
    ): PProjPJ;
    function ProjFree(
      projPJ: PProjPJ
    ): Integer;

    function ProjForward(
      ProjLP: TProjLP;
      const projPJ: PProjPJ
    ): TProjXY;
    function ProjInverse(
      ProjXY: TProjXY;
      const projPJ: PProjPJ
    ): TProjLP;
  public
    constructor Create(const ADllFullName: string);
    destructor Destroy; override;
  end;

{ TProjDLLHolder }

constructor TProjDLLHolder.Create(const ADllFullName: string);
begin
  inherited Create;
  // try to load library
  FDLLHandle := LoadLibrary(PChar(ADllFullName));
  if (FDLLHandle<>0) then begin
    try
      F_pj_init_plus := GetProcAddress(FDLLHandle, 'pj_init_plus');
      if Addr(F_pj_init_plus) = nil then begin
        RaiseLastOSError;
      end;

      F_pj_fwd := GetProcAddress(FDLLHandle, 'pj_fwd');
      if Addr(F_pj_fwd) = nil then begin
        RaiseLastOSError;
      end;
      F_pj_inv := GetProcAddress(FDLLHandle, 'pj_inv');
      if Addr(F_pj_inv) = nil then begin
        RaiseLastOSError;
      end;
      F_pj_free := GetProcAddress(FDLLHandle, 'pj_free');
      if Addr(F_pj_free) = nil then begin
        RaiseLastOSError;
      end;
    except
      FreeLibrary(FDLLHandle);
      FDLLHandle := 0;
      raise;
    end;
  end else begin
    RaiseLastOSError;
  end;
end;

destructor TProjDLLHolder.Destroy;
begin
  if (FDLLHandle <> 0) then begin
    FreeLibrary(FDLLHandle);
    FDLLHandle := 0;
  end;
  inherited;
end;

function TProjDLLHolder.ProjForward(ProjLP: TProjLP;
  const projPJ: PProjPJ): TProjXY;
begin
  Result := F_pj_fwd(ProjLP, projPJ);
end;

function TProjDLLHolder.ProjFree(projPJ: PProjPJ): Integer;
begin
  Result := F_pj_free(projPJ);
end;

function TProjDLLHolder.ProjInit(const Args: PAnsiChar): PProjPJ;
begin
  Result := F_pj_init_plus(Args);
end;

function TProjDLLHolder.ProjInverse(ProjXY: TProjXY;
  const projPJ: PProjPJ): TProjLP;
begin
  Result := F_pj_inv(ProjXY, projPJ);
end;

type
  TProjConverterByDll = class(TBaseInterfacedObject, IProjConverter)
  private
    FDllHolder: IProjDLLHolder;
    FprojPJ: PProjPJ;
  private
    function LonLat2XY(const AProjLP: TDoublePoint): TDoublePoint;
    function XY2LonLat(const AProjXY: TDoublePoint): TDoublePoint;
  public
    constructor Create(const ADllHolder: IProjDLLHolder; AprojPJ: PProjPJ);
    destructor Destroy; override;
  end;

{ TProjConverterByDll }

constructor TProjConverterByDll.Create(
  const ADllHolder: IProjDLLHolder;
  AprojPJ: PProjPJ
);
begin
  inherited Create;
  Assert(ADllHolder <> nil);
  FDllHolder := ADllHolder;
  Assert(AprojPJ <> nil);
  FprojPJ := AprojPJ;
end;

destructor TProjConverterByDll.Destroy;
begin
  if Assigned(FDllHolder) then begin
    FDllHolder.ProjFree(FprojPJ);
  end;
  FprojPJ := nil;
  FDllHolder := nil;
  inherited;
end;

function TProjConverterByDll.LonLat2XY(
  const AProjLP: TDoublePoint): TDoublePoint;
const
 { Scaling value decimal degree to radiant }
 DEG_TO_RAD   =	0.0174532925199432958;
var
  VProjLP: TProjLP;
begin
  VProjLP.X := AProjLP.X * DEG_TO_RAD;
  VProjLP.Y := AProjLP.Y * DEG_TO_RAD;
  Result := FDllHolder.ProjForward(VProjLP, FprojPJ);
end;

function TProjConverterByDll.XY2LonLat(
  const AProjXY: TDoublePoint): TDoublePoint;
const
 { Scaling value radiant to decimal degree }
 RAD_TO_DEG   =	57.29577951308232;
var
  VProjLP: TProjLP;
begin
  VProjLP := FDllHolder.ProjInverse(AProjXY, FprojPJ);
  Result.X := VProjLP.X * RAD_TO_DEG;
  Result.Y := VProjLP.Y * RAD_TO_DEG;
end;

{ TProjConverterFactory }

constructor TProjConverterFactory.Create;
begin
  inherited Create;
  FSync := MakeSyncRW_Std(Self, FALSE);
  FDllFailed := False;
end;

function TProjConverterFactory.GetArgsByEpsg(const AEPSG: Integer): AnsiString;
var
  i: Integer;
begin
  Result := '';
  // known EPSGs
  if (AEPSG=53004) then begin
    // Sphere Mercator ESRI:53004
    Result := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs';
  end else if (AEPSG=3785) then begin
    // Popular Visualisation CRS / Mercator
    Result := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs';
  end else if (AEPSG=3395) then begin
    // WGS 84 / World Mercator
    Result := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs';
  end else if (AEPSG=4269) then begin
    // NAD83
    Result := '+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs';
  end else if (AEPSG=4326) then begin
    // WGS 84
    Result := '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs';
  end else if (AEPSG>=2463) and (AEPSG<=2491) then begin
    // 2463-2491 = Pulkovo 1995 / Gauss-Kruger CM
    i := 21 + (AEPSG-2463)*6;
    if (i>180) then
      i := i - 360;
    Result := '+proj=tmerc +lat_0=0 +lon_0='+ALIntToStr(i)+' +k=1 +x_0=500000 +y_0=0 +ellps=krass +units=m +no_defs';
  end else if (AEPSG>=2492) and (AEPSG<=2522) then begin
    // 2492-2522 = Pulkovo 1942 / Gauss-Kruger CM
    i := 9 + (AEPSG-2492)*6;
    if (i>180) then
      i := i - 360;
    Result := '+proj=tmerc +lat_0=0 +lon_0='+ALIntToStr(i)+' +k=1 +x_0=500000 +y_0=0 +ellps=krass +units=m +no_defs';
  end else if (AEPSG>=32601) and (AEPSG<=32660) then begin
    // 32601-32660 = WGS 84 / UTM zone N
    Result := '+proj=utm +zone='+ALIntToStr(AEPSG-32600)+' +ellps=WGS84 +datum=WGS84 +units=m +no_defs';
  end;
end;

function TProjConverterFactory.GetByEPSG(const AEPSG: Integer): IProjConverter;
var
  VArgs: AnsiString;
begin
  Result := nil;
  VArgs := GetArgsByEpsg(AEPSG);
  if VArgs <> '' then begin
    Result := GetByInitStringInternal(VArgs);
  end;
end;

function TProjConverterFactory.GetByInitString(
  const AArgs: AnsiString
): IProjConverter;
const
  c_EPSG: AnsiString = 'EPSG:';
var
  VEPSG: Integer;
  VArgs: AnsiString;
begin
  VArgs := AArgs;
  if ALSameText(ALCopyStr(AArgs, 1, Length(c_EPSG)), c_EPSG) then begin
    if ALTryStrToInt(ALCopyStr(AArgs, Length(c_EPSG)+1, Length(AArgs)), VEPSG) then begin
      VArgs := GetArgsByEpsg(VEPSG);
    end;
  end;
  Result := GetByInitStringInternal(VArgs);
end;

function TProjConverterFactory.GetByInitStringInternal(
  const AArgs: AnsiString): IProjConverter;
var
  VDll: IProjDLLHolder;
  VProj: PProjPJ;
begin
  Result := nil;
  if AArgs <> '' then begin
    VDll := nil;
    FSync.BeginWrite;
    try
      VDll := IProjDLLHolder(FDllHolder);
      if VDll = nil  then begin
        if not FDllFailed then begin
          try
            InitDll;
            VDll := IProjDLLHolder(FDllHolder);
            if VDll = nil then begin
              FDllFailed := True;
            end;
          except
            FDllFailed := True;
            FDllHolder := nil;
          end;
        end;
      end;
    finally
      FSync.EndWrite;
    end;
    if VDll <> nil  then begin
      VProj := VDll.ProjInit(PAnsiChar(AArgs));
      if VProj <> nil then begin
        Result := TProjConverterByDll.Create(VDll, VProj);      
      end;
    end;
  end;
end;

procedure TProjConverterFactory.InitDll;
const
   proj4_dll='proj480.dll';
var
  VDll: IProjDLLHolder;
begin
  VDll := TProjDLLHolder.Create(proj4_dll);
  FDllHolder := VDll;
end;

end.
