{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_proj4;

interface

{ proj 4.4.7 }

uses
  Windows,
  SysUtils,
  i_proj4,
  t_GeoTypes;

type
  EProj4Error          = class(Exception);
  EProj4NotAvailable   = class(EProj4Error);
  EProj4NotInitialized = class(EProj4Error);

// make full proj4 object
function CreateProj4Full: IProj4Full;
// make single projection converter
function CreateProj4Converter: IProj4Converter;

implementation

uses
  u_Synchronizer;

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
    function InternalAvailable(const AMakeOnDemand: Boolean): Boolean;
    procedure InternalZero;
  protected
    { IProj4Full }
    function AvailableFull: Boolean;
    function MakeProj(const AArgs, APath: String): PProjPJ; stdcall;
    function KillProj(AProjPJ: PProjPJ): Integer; stdcall;
    function LonLat2XY(const AProjLP: TDoublePoint; const projPJ: PProjPJ): TDoublePoint; stdcall;
    function XY2LonLat(const AProjXY: TDoublePoint; const projPJ: PProjPJ): TDoublePoint; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TProj4Full = class(TProj4Abstract, IProj4Full)
  end;

  TProj4Conv = class(TProj4Abstract, IProj4Converter)
  protected
    FProjPJ: PProjPJ;
    FSync: IReadWriteSync;
    // proj params
    FEPSG: Integer;
    FArgs, FPath: String;
  protected
    procedure InternalKillProj;
    function InternalSetProj(const AArgs, APath: String): Boolean;
    function GetPredefinedEPSGParams(const AEPSG: Integer; out AOutArgs: String): Boolean;
  protected
    { IProj4Conv }
    function Available: Boolean; stdcall;
    function SetProj(const AArgs, APath: String): Boolean; stdcall;
    function SetEPSG(const AEPSG: Integer; const APath: String): Boolean; stdcall;
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

function CreateProj4Converter: IProj4Converter;
begin
  Result := TProj4Conv.Create;
end;

{ TProj4Abstract }

function TProj4Abstract.AvailableFull: Boolean;
begin
  Result := InternalAvailable(TRUE);
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

function TProj4Abstract.InternalAvailable(const AMakeOnDemand: Boolean): Boolean;
begin
  if AMakeOnDemand then
  if (is_Unknown = FInitStatus) then begin
    // load on demand
    InternalLoadLib;
  end;

  Result := (is_Available = FInitStatus);
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
  if InternalAvailable(FALSE) and (F_pj_free<>nil) then
    Result := T_pj_free(F_pj_free)(AProjPJ)
  else
    raise EProj4NotAvailable.Create('');
end;

function TProj4Abstract.LonLat2XY(const AProjLP: TDoublePoint; const projPJ: PProjPJ): TDoublePoint;
var
  VProjLP: TProjLP;
begin
  if InternalAvailable(FALSE) and (F_pj_fwd<>nil) then begin
    VProjLP.X := AProjLP.X * DEG_TO_RAD;
    VProjLP.Y := AProjLP.Y * DEG_TO_RAD;
    Result := T_pj_fwd(F_pj_fwd)(VProjLP, projPJ);
  end else
    raise EProj4NotAvailable.Create('');;
end;

function TProj4Abstract.MakeProj(const AArgs, APath: String): PProjPJ;
begin
  if InternalAvailable(TRUE) and (F_pj_init_plus_path<>nil) then
    Result := T_pj_init_plus_path(F_pj_init_plus_path)(PAnsiChar(AArgs), PAnsiChar(APath))
  else
    raise EProj4NotAvailable.Create('');
end;

function TProj4Abstract.XY2LonLat(const AProjXY: TDoublePoint; const projPJ: PProjPJ): TDoublePoint;
begin
  if InternalAvailable(FALSE) and (F_pj_inv<>nil) then begin
    Result := T_pj_inv(F_pj_inv)(AProjXY, projPJ);
    Result.X := Result.X * RAD_TO_DEG;
    Result.Y := Result.Y * RAD_TO_DEG;
  end else
    raise EProj4NotAvailable.Create('');
end;

{ TProj4Conv }

function TProj4Conv.Available: Boolean;
begin
  FSync.BeginRead;
  try
    Result := InternalAvailable(FALSE);
  finally
    FSync.EndRead;
  end;

  if (not Result) then begin
    FSync.BeginWrite;
    try
      Result := InternalAvailable(TRUE);
    finally
      FSync.EndWrite;
    end;
  end;

  if Result then
    Result := (FProjPJ<>nil);
end;

constructor TProj4Conv.Create;
begin
  inherited Create;
  // init
  FSync := MakeSyncRW_Std(Self, FALSE);
  FProjPJ := nil;
  FEPSG := 0;
  FArgs := '';
  FPath := '';
end;

destructor TProj4Conv.Destroy;
begin
  // kill proj
  FSync.BeginWrite;
  try
    InternalKillProj;
  finally
    FSync.EndWrite;
  end;
  // kill sync
  FSync := nil;
  inherited;
end;

function TProj4Conv.GetPredefinedEPSGParams(
  const AEPSG: Integer;
  out AOutArgs: String
): Boolean;
begin
  Result := FALSE;
  AOutArgs := '';

  // known EPSGs
  if (AEPSG=53004) then begin
    // Sphere Mercator ESRI:53004
    AOutArgs := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs';
    Result := TRUE;
  end else if (AEPSG=3785) then begin
    // Popular Visualisation CRS / Mercator
    AOutArgs := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs';
    Result := TRUE;
  end else if (AEPSG=3395) then begin
    // WGS 84 / World Mercator
    AOutArgs := '+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs';
    Result := TRUE;
  end else if (AEPSG=4326) then begin
    // WGS 84
    AOutArgs := '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs';
    Result := TRUE;
  end else if (AEPSG>=32630) and (AEPSG<32660) then begin
    // WGS 84 / UTM zone
    AOutArgs := '+proj=utm +zone='+IntToStr(AEPSG-32600)+' +ellps=WGS84 +datum=WGS84 +units=m +no_defs';
    Result := TRUE;
  end;
end;

procedure TProj4Conv.InternalKillProj;
begin
  if FProjPJ<>nil then begin
    KillProj(FProjPJ);
    FProjPJ:=nil;
  end;
end;

function TProj4Conv.InternalSetProj(const AArgs, APath: String): Boolean;
begin
  Result := FALSE;
  InternalKillProj;
  // make proj
  if InternalAvailable(TRUE) then begin
    FProjPJ := MakeProj(AArgs, APath);
    Result := (FProjPJ<>nil);
  end;
end;

function TProj4Conv.LonLat2XY(const AProjLP: TDoublePoint): TDoublePoint;
begin
  if Available then begin
    FSync.BeginRead;
    try
      Result := inherited LonLat2XY(AProjLP, FProjPJ);
    finally
      FSync.EndRead;
    end;
  end else
    raise EProj4NotAvailable.Create('');
end;

function TProj4Conv.SetEPSG(const AEPSG: Integer; const APath: String): Boolean;
var VArgs: String;
begin
  FSync.BeginWrite;
  try
    if (FEPSG<>AEPSG) or (not AnsiSameText(FPath,APath)) then begin
      // should set new proj
      if GetPredefinedEPSGParams(AEPSG, VArgs) then begin
        // ok
        FEPSG := AEPSG;
        FArgs := VArgs;
        FPath := APath;
        // call
        Result := InternalSetProj(FArgs, FPath);
      end else begin
        // failed - nothing to do
        Result := FALSE;
      end;
    end else begin
      // already ok
      Result := TRUE;
    end;
  finally
    FSync.EndWrite;
  end;
end;

function TProj4Conv.SetProj(const AArgs, APath: String): Boolean;
const
  c_EPSG = 'EPSG:';
var
  VEPSG: Integer;
  VArgs: String;
begin
  FSync.BeginWrite;
  try
    if (not AnsiSameText(FArgs,AArgs)) or (not AnsiSameText(FPath,APath)) then begin
      // check special 'EPSG:32640' form
      if SameText(System.Copy(AArgs, 1, Length(c_EPSG)), c_EPSG) then
      if TryStrToInt(System.Copy(AArgs, Length(c_EPSG)+1, Length(AArgs)), VEPSG) then
      if GetPredefinedEPSGParams(VEPSG, VArgs) then begin
        // set as EPSG
        FEPSG := VEPSG;
        FArgs := AArgs;
        FPath := APath;
        Result := InternalSetProj(VArgs, APath);
        Exit;
      end;

      // set as raw params
      FArgs := AArgs;
      FPath := APath;
      FEPSG := 0;
      Result := InternalSetProj(AArgs, APath);
    end else begin
      // already ok
      Result := TRUE;
    end;
  finally
    FSync.EndWrite;
  end;
end;

function TProj4Conv.XY2LonLat(const AProjXY: TDoublePoint): TDoublePoint;
begin
  if Available then begin
    FSync.BeginRead;
    try
      Result := inherited XY2LonLat(AProjXY, FProjPJ);
    finally
      FSync.EndRead;
    end;
  end else
    raise EProj4NotAvailable.Create('');
end;

end.
