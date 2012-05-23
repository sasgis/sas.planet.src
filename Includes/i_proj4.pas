unit i_proj4;

interface

{ proj 4.4.7 }

uses
  Windows,
  t_GeoTypes;

type
  PProjPJ = Pointer;

  TProj4Info = record
    Proj4Args: String;
    Proj4Path: String;
  end;


  // full interface
  IProj4Full = interface
  ['{BE0ADFF4-D0C5-4BD7-B09A-C4D9F8D9A273}']
    function AvailableFull: Boolean;
    function MakeProj(const AArgs, APath: String): PProjPJ; stdcall;
    function KillProj(AProjPJ: PProjPJ): Integer; stdcall;
    function LonLat2XY(const AProjLP: TDoublePoint; const projPJ: PProjPJ): TDoublePoint; stdcall;
    function XY2LonLat(const AProjXY: TDoublePoint; const projPJ: PProjPJ): TDoublePoint; stdcall;
  end;

  // single projection converter interface
  IProj4Converter = interface
  ['{74309735-EB2E-4996-A226-1C172F076737}']
    function Available: Boolean; stdcall;
    function SetProj(const AArgs, APath: String): Boolean; stdcall;
    function SetEPSG(const AEPSG: Integer; const APath: String): Boolean; stdcall;
    function LonLat2XY(const AProjLP: TDoublePoint): TDoublePoint; stdcall;
    function XY2LonLat(const AProjXY: TDoublePoint): TDoublePoint; stdcall;
  end;

function Proj4InfoIsEmpty(const AInfo: TProj4Info): Boolean;
procedure Proj4InfoClear(var AInfo: TProj4Info);
  
implementation

function Proj4InfoIsEmpty(const AInfo: TProj4Info): Boolean;
begin
  Result := (0 = Length(AInfo.Proj4Args)) and (0 = Length(AInfo.Proj4Path));
end;

procedure Proj4InfoClear(var AInfo: TProj4Info);
begin
  AInfo.Proj4Args := '';
  AInfo.Proj4Path := '';
end;

end.
