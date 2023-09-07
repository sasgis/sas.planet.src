unit Proj4.GaussKruger;

interface

type
  TGaussKrugerCoord = record
    Zone: Integer;
    IsNorth: Boolean;
    X: Double;
    Y: Double;
  end;

  TGaussKruger = class
  private
    FGeogInit: AnsiString;
    FProjInitFmt: AnsiString;
  protected
    function GetGeogInit: AnsiString; virtual;
    function GetProjInit(const AZone: Integer; const AIsNorth: Boolean): AnsiString; virtual;
  public
    class function geog_long_to_zone(const ALon: Double): Integer; inline;
    class function zone_to_geog_lon(const AZone: Integer): Double; inline;

    function wgs84_to_geog(var ALon, ALat: Double): Boolean; inline;
    function geog_to_wgs84(var ALon, ALat: Double): Boolean; inline;

    function geog_to_proj(const ALon, ALat: Double; out ACoord: TGaussKrugerCoord): Boolean;
    function wgs84_to_proj(const ALon, ALat: Double; out ACoord: TGaussKrugerCoord): Boolean;

    function proj_to_wgs84(const ACoord: TGaussKrugerCoord; out ALon, ALat: Double): Boolean;
    function proj_to_geog(const ACoord: TGaussKrugerCoord; out ALon, ALat: Double): Boolean;
  public
    constructor Create(
      const AGeogInit: AnsiString;
      const AProjInitFmt: AnsiString
    );
  end;

  TGaussKrugerFactory = record
    class function BuildSK42: TGaussKruger; static;
    class function BuildGSK2011: TGaussKruger; static;
  end;

implementation

uses
  Math,
  SysUtils,
  Proj4.Defines,
  Proj4.Utils;

{ TGaussKruger }

constructor TGaussKruger.Create(
  const AGeogInit: AnsiString;
  const AProjInitFmt: AnsiString
);
begin
  inherited Create;

  FGeogInit := AGeogInit;
  FProjInitFmt := AProjInitFmt;
end;

class function TGaussKruger.geog_long_to_zone(const ALon: Double): Integer;
begin
  if ALon > 0 then begin
    Result := Floor(ALon / 6) + 1;
  end else begin
    Result := Floor((180 + ALon) / 6) + 30 + 1;
  end;
end;

class function TGaussKruger.zone_to_geog_lon(const AZone: Integer): Double;
begin
  Result := (AZone - 1) * 6;
  if AZone > 30 then begin
    Result := Result - 360;
  end;
end;

function TGaussKruger.GetGeogInit: AnsiString;
begin
  Result := FGeogInit;
end;

function TGaussKruger.GetProjInit(const AZone: Integer; const AIsNorth: Boolean): AnsiString;
var
  lon_0: Integer;
  x_0, y_0: Integer;
begin
  lon_0 := AZone * 6 - 3;
  if AZone > 30 then begin
    lon_0 := lon_0 - 360;
  end;
  x_0 := AZone * 1000000 + 500000;
  if AIsNorth then begin
    y_0 := 0;
  end else begin
    y_0 := 10000000;
  end;
  Result := AnsiString(Format(FProjInitFmt, [lon_0, x_0, y_0]));
end;

function TGaussKruger.wgs84_to_geog(var ALon, ALat: Double): Boolean;
begin
  Result := geodetic_cs_to_cs(wgs_84, GetGeogInit, ALon, ALat);
end;

function TGaussKruger.geog_to_wgs84(var ALon, ALat: Double): Boolean;
begin
  Result := geodetic_cs_to_cs(GetGeogInit, wgs_84, ALon, ALat);
end;

function TGaussKruger.geog_to_proj(const ALon, ALat: Double; out ACoord: TGaussKrugerCoord): Boolean;
var
  VInitStr: AnsiString;
begin
  ACoord.Zone := geog_long_to_zone(ALon);
  ACoord.IsNorth := ALat >= 0;

  VInitStr := GetProjInit(ACoord.Zone, ACoord.IsNorth);
  Result := geodetic_cs_to_projected_cs(GetGeogInit, VInitStr, ALon, ALat, ACoord.X, ACoord.Y);
end;

function TGaussKruger.wgs84_to_proj(const ALon, ALat: Double; out ACoord: TGaussKrugerCoord): Boolean;
var
  VLon, VLat: Double;
begin
  VLon := ALon;
  VLat := ALat;

  Result :=
    geodetic_cs_to_cs(wgs_84, GetGeogInit, VLon, VLat) and
    geog_to_proj(VLon, VLat, ACoord);
end;

function TGaussKruger.proj_to_wgs84(const ACoord: TGaussKrugerCoord; out ALon, ALat: Double): Boolean;
begin
  Result :=
    proj_to_geog(ACoord, ALon, ALat) and
    geodetic_cs_to_cs(GetGeogInit, wgs_84, ALon, ALat);
end;

function TGaussKruger.proj_to_geog(const ACoord: TGaussKrugerCoord; out ALon, ALat: Double): Boolean;
var
  VInitStr: AnsiString;
begin
  VInitStr := GetProjInit(ACoord.Zone, ACoord.IsNorth);
  Result := projected_cs_to_geodetic_cs(VInitStr, GetGeogInit, ACoord.X, ACoord.Y, ALon, ALat);
end;

{ TGaussKrugerFactory }

class function TGaussKrugerFactory.BuildSK42: TGaussKruger;
begin
  Result := TGaussKruger.Create(sk_42, sk_42_gauss_kruger_fmt);
end;

class function TGaussKrugerFactory.BuildGSK2011: TGaussKruger;
begin
  Result := TGaussKruger.Create(gsk_2011, gsk_2011_gauss_kruger_fmt);
end;

end.
