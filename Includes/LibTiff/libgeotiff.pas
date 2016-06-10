unit libgeotiff;

interface

{.$DEFINE GEOTIFF_STATIC_LINK}

uses
  libtiff;

const
  libgeotiff_dll = 'geotiff.dll';

type
  PGTIFF = Pointer;

{$IFDEF GEOTIFF_STATIC_LINK}
{ Registers an extension with libtiff for adding GeoTIFF tags.
After this one-time intialization, any TIFF open function may be called in
the usual manner to create a TIFF file that compatible with libgeotiff.
The XTIFF open functions are simply for convenience: they call this
and then pass their parameters on to the appropriate TIFF open function.
This function may be called any number of times safely, since it will
only register the extension the first time it is called.}
procedure XTIFFInitialize(); cdecl; external libgeotiff_dll;

{* GeoTIFF compatible TIFF file open function.
 *
 * @param name The filename of a TIFF file to open.
 * @param mode The open mode ("r", "w" or "a").
 *
 * @return a TIFF * for the file, or NULL if the open failed.
 *
This function is used to open GeoTIFF files instead of TIFFOpen() from
libtiff.  Internally it calls TIFFOpen(), but sets up some extra hooks
so that GeoTIFF tags can be extracted from the file.  If XTIFFOpen() isn't
used, GTIFNew() won't work properly.  Files opened
with XTIFFOpen() should be closed with XTIFFClose().

The name of the file to be opened should be passed as <b>name</b>, and an
opening mode ("r", "w" or "a") acceptable to TIFFOpen() should be passed as the
<b>mode</b>.<p>

If XTIFFOpen() fails it will return NULL.  Otherwise, normal TIFFOpen()
error reporting steps will have already taken place.<p>}
function  XTIFFOpen(const FileName: PAnsiChar; const Mode: PAnsiChar): PTIFF; cdecl; external libgeotiff_dll;
function XTIFFClientOpen(
  const Name: PAnsiChar;
  const Mode: PAnsiChar;
  ClientData: Cardinal;
  ReadProc: TIFFReadWriteProc;
  WriteProc: TIFFReadWriteProc;
  SeekProc: TIFFSeekProc;
  CloseProc: TIFFCloseProc;
  SizeProc: TIFFSizeProc;
  MapProc: TIFFMapFileProc;
  UnmapProc: TIFFUnmapFileProc
): PTIFF; cdecl; external libtiff_dll;

{* Close a file opened with XTIFFOpen().
 *
 * @param tif The file handle returned by XTIFFOpen().
 *
 * If a GTIF structure was created with GTIFNew()
 * for this file, it should be freed with GTIFFree()
 * <i>before</i> calling XTIFFClose()}
procedure XTIFFClose(Handle: PTIFF); cdecl; external libgeotiff_dll;

// TIFF-level interface
function  GTIFNew(Handle: PTIFF): PGTIFF; cdecl; external libgeotiff_dll;
procedure GTIFFree(Handle: PGTIFF); cdecl; external libgeotiff_dll;
function  GTIFWriteKeys(Handle: PGTIFF): Integer; cdecl; external libgeotiff_dll;

// GeoKey Access
function  GTIFKeySet(Handle: PGTIFF; KeyID: Integer; TagType: Integer; Count: Integer): Integer; cdecl; external libgeotiff_dll; varargs;
{$ELSE}
var
  XTIFFInitialize: procedure(); cdecl;
  XTIFFOpen: function(const FileName: PAnsiChar; const Mode: PAnsiChar): PTIFF; cdecl;
  XTIFFClientOpen: function(
    const Name: PAnsiChar;
    const Mode: PAnsiChar;
    ClientData: Cardinal;
    ReadProc: TIFFReadWriteProc;
    WriteProc: TIFFReadWriteProc;
    SeekProc: TIFFSeekProc;
    CloseProc: TIFFCloseProc;
    SizeProc: TIFFSizeProc;
    MapProc: TIFFMapFileProc;
    UnmapProc: TIFFUnmapFileProc
  ): PTIFF; cdecl;
  XTIFFClose: procedure(Handle: PTIFF); cdecl;
  GTIFNew: function(Handle: PTIFF): PGTIFF; cdecl;
  GTIFFree: procedure(Handle: PGTIFF); cdecl;
  GTIFWriteKeys: function(Handle: PGTIFF): Integer; cdecl;
  GTIFKeySet: function(Handle: PGTIFF; KeyID: Integer; TagType: Integer; Count: Integer): Integer; cdecl varargs;
{$ENDIF}

{$REGION 'GeoTiff constants'}
// www.remotesensing.org/geotiff/spec/geotiff6.html
const
  TIFFTAG_GEOPIXELSCALE      = 33550;
  TIFFTAG_INTERGRAPH_MATRIX  = 33920;
  TIFFTAG_GEOTIEPOINTS       = 33922;
  TIFFTAG_JPL_CARTO_IFD      = 34263;
  TIFFTAG_GEOTRANSMATRIX     = 34264;
  TIFFTAG_GEOKEYDIRECTORY    = 34735;
  TIFFTAG_GEODOUBLEPARAMS    = 34736;
  TIFFTAG_GEOASCIIPARAMS     = 34737;

  TIFFPRINT_GEOKEYDIRECTORY	= $80000000;
  TIFFPRINT_GEOKEYPARAMS		= $40000000;

const
  TYPE_BYTE     = 1;
  TYPE_SHORT    = 2;
  TYPE_LONG     = 3;
  TYPE_RATIONAL = 4;
  TYPE_ASCII    = 5;
  TYPE_FLOAT    = 6;
  TYPE_DOUBLE   = 7;
  TYPE_SBYTE    = 8;
  TYPE_SSHORT   = 9;
  TYPE_SLONG    = 10;
  TYPE_UNKNOWN  = 11;

const
  KvUndefined   = 0;
  KvUserDefined = 32767;

const
  ModelTypeProjected  = 1;  // Projection Coordinate System
	ModelTypeGeographic = 2;  // Geographic latitude-longitude System
	ModelTypeGeocentric = 3;  // Geocentric (X,Y,Z) Coordinate System
	ModelProjected  = ModelTypeProjected;   // alias
	ModelGeographic = ModelTypeGeographic;  // alias
	ModelGeocentric = ModelTypeGeocentric;  // alias

const
	RasterPixelIsArea   = 1;  // Standard pixel-fills-grid-cell
	RasterPixelIsPoint  = 2;  // Pixel-at-grid-vertex

const
  GTModelTypeGeoKey  = 1024;
  GTRasterTypeGeoKey = 1025;
  GTCitationGeoKey   = 1026;

  GeographicTypeGeoKey            = 2048;      // Section 6.3.2.1 Codes
  GeogCitationGeoKey              = 2049;      // documentation
  GeogGeodeticDatumGeoKey         = 2050;      // Section 6.3.2.2 Codes
  GeogPrimeMeridianGeoKey         = 2051;      // Section 6.3.2.4 codes
  GeogLinearUnitsGeoKey           = 2052;      // Section 6.3.1.3 Codes
  GeogLinearUnitSizeGeoKey        = 2053;      // meters
  GeogAngularUnitsGeoKey          = 2054;      // Section 6.3.1.4 Codes
  GeogAngularUnitSizeGeoKey       = 2055;      // radians
  GeogEllipsoidGeoKey             = 2056;      // Section 6.3.2.3 Codes
  GeogSemiMajorAxisGeoKey         = 2057;      // GeogLinearUnits
  GeogSemiMinorAxisGeoKey         = 2058;      // GeogLinearUnits
  GeogInvFlatteningGeoKey         = 2059;      // ratio
  GeogAzimuthUnitsGeoKey          = 2060;      // Section 6.3.1.4 Codes
  GeogPrimeMeridianLongGeoKey     = 2061;      // GeoAngularUnit

  ProjectedCSTypeGeoKey           = 3072;      // Section 6.3.3.1 codes
  PCSCitationGeoKey               = 3073;      // documentation
  ProjectionGeoKey                = 3074;      // Section 6.3.3.2 codes
  ProjCoordTransGeoKey            = 3075;      // Section 6.3.3.3 codes
  ProjLinearUnitsGeoKey           = 3076;      // Section 6.3.1.3 codes
  ProjLinearUnitSizeGeoKey        = 3077;      // meters
  ProjStdParallel1GeoKey          = 3078;      // GeogAngularUnit
  ProjStdParallelGeoKey           = ProjStdParallel1GeoKey;  // ** alias **
  ProjStdParallel2GeoKey          = 3079;      // GeogAngularUnit
  ProjNatOriginLongGeoKey         = 3080;      // GeogAngularUnit
  ProjOriginLongGeoKey            = ProjNatOriginLongGeoKey;  // ** alias **
  ProjNatOriginLatGeoKey          = 3081;      // GeogAngularUnit
  ProjOriginLatGeoKey             = ProjNatOriginLatGeoKey;    // ** alias **
  ProjFalseEastingGeoKey          = 3082;      // ProjLinearUnits
  ProjFalseNorthingGeoKey         = 3083;      // ProjLinearUnits
  ProjFalseOriginLongGeoKey       = 3084;      // GeogAngularUnit
  ProjFalseOriginLatGeoKey        = 3085;      // GeogAngularUnit
  ProjFalseOriginEastingGeoKey    = 3086;      // ProjLinearUnits
  ProjFalseOriginNorthingGeoKey   = 3087;      // ProjLinearUnits
  ProjCenterLongGeoKey            = 3088;      // GeogAngularUnit
  ProjCenterLatGeoKey             = 3089;      // GeogAngularUnit
  ProjCenterEastingGeoKey         = 3090;      // ProjLinearUnits
  ProjCenterNorthingGeoKey        = 3091;      // ProjLinearUnits
  ProjScaleAtNatOriginGeoKey      = 3092;      // ratio
  ProjScaleAtOriginGeoKey         = ProjScaleAtNatOriginGeoKey;   // ** alias **
  ProjScaleAtCenterGeoKey         = 3093;      // ratio
  ProjAzimuthAngleGeoKey          = 3094;      // GeogAzimuthUnit
  ProjStraightVertPoleLongGeoKey  = 3095;      // GeogAngularUnit
  ProjRectifiedGridAngleGeoKey    = 3096;      // GeogAngularUnit

  VerticalCSTypeGeoKey            = 4096;      // Section 6.3.4.1 codes
  VerticalCitationGeoKey          = 4097;      // documentation
  VerticalDatumGeoKey             = 4098;      // Section 6.3.4.2 codes
  VerticalUnitsGeoKey             = 4099;       // Section 6.3.1 (.x;  codes

const
  Linear_Meter =	9001;
  Linear_Foot =	9002;
  Linear_Foot_US_Survey =	9003;
  Linear_Foot_Modified_American =	9004;
  Linear_Foot_Clarke =	9005;
  Linear_Foot_Indian =	9006;
  Linear_Link =	9007;
  Linear_Link_Benoit =	9008;
  Linear_Link_Sears =	9009;
  Linear_Chain_Benoit =	9010;
  Linear_Chain_Sears =	9011;
  Linear_Yard_Sears =	9012;
  Linear_Yard_Indian =	9013;
  Linear_Fathom =	9014;
  Linear_Mile_International_Nautical =	9015;

  Angular_Radian =	9101;
  Angular_Degree =	9102;
  Angular_Arc_Minute =	9103;
  Angular_Arc_Second =	9104;
  Angular_Grad =	9105;
  Angular_Gon =	9106;
  Angular_DMS =	9107;
  Angular_DMS_Hemisphere =	9108;

  GCS_WGS_84 = 4326;

  Ellipse_Airy_1830 =	7001;
  Ellipse_Airy_Modified_1849 =	7002;
  Ellipse_Australian_National_Spheroid =	7003;
  Ellipse_Bessel_1841 =	7004;
  Ellipse_Bessel_Modified =	7005;
  Ellipse_Bessel_Namibia =	7006;
  Ellipse_Clarke_1858 =	7007;
  Ellipse_Clarke_1866 =	7008;
  Ellipse_Clarke_1866_Michigan =	7009;
  Ellipse_Clarke_1880_Benoit =	7010;
  Ellipse_Clarke_1880_IGN =	7011;
  Ellipse_Clarke_1880_RGS =	7012;
  Ellipse_Clarke_1880_Arc =	7013;
  Ellipse_Clarke_1880_SGA_1922 =	7014;
  Ellipse_Everest_1830_1937_Adjustment =	7015;
  Ellipse_Everest_1830_1967_Definition =	7016;
  Ellipse_Everest_1830_1975_Definition =	7017;
  Ellipse_Everest_1830_Modified =	7018;
  Ellipse_GRS_1980 =	7019;
  Ellipse_Helmert_1906 =	7020;
  Ellipse_Indonesian_National_Spheroid =	7021;
  Ellipse_International_1924 =	7022;
  Ellipse_International_1967 =	7023;
  Ellipse_Krassowsky_1940 =	7024;
  Ellipse_NWL_9D =	7025;
  Ellipse_NWL_10D =	7026;
  Ellipse_Plessis_1817 =	7027;
  Ellipse_Struve_1860 =	7028;
  Ellipse_War_Office =	7029;
  Ellipse_WGS_84 =	7030;
  Ellipse_GEM_10C =	7031;
  Ellipse_OSU86F =	7032;
  Ellipse_OSU91A =	7033;
  Ellipse_Clarke_1880 =	7034;
  Ellipse_Sphere =	7035;

  Datum_Dealul_Piscului_1970 =6317;

  // Datums for which only the ellipsoid is known
  DatumE_Airy1830 =	6001;
  DatumE_AiryModified1849 =	6002;
  DatumE_AustralianNationalSpheroid =	6003;
  DatumE_Bessel1841 =	6004;
  DatumE_BesselModified =	6005;
  DatumE_BesselNamibia =	6006;
  DatumE_Clarke1858 =	6007;
  DatumE_Clarke1866 =	6008;
  DatumE_Clarke1866Michigan =	6009;
  DatumE_Clarke1880_Benoit =	6010;
  DatumE_Clarke1880_IGN =	6011;
  DatumE_Clarke1880_RGS =	6012;
  DatumE_Clarke1880_Arc =	6013;
  DatumE_Clarke1880_SGA1922 =	6014;
  DatumE_Everest1830_1937Adjustment =	6015;
  DatumE_Everest1830_1967Definition =	6016;
  DatumE_Everest1830_1975Definition =	6017;
  DatumE_Everest1830Modified =	6018;
  DatumE_GRS1980 =	6019;
  DatumE_Helmert1906 =	6020;
  DatumE_IndonesianNationalSpheroid =	6021;
  DatumE_International1924 =	6022;
  DatumE_International1967 =	6023;
  DatumE_Krassowsky1960 =	6024;
  DatumE_NWL9D =	6025;
  DatumE_NWL10D =	6026;
  DatumE_Plessis1817 =	6027;
  DatumE_Struve1860 =	6028;
  DatumE_WarOffice =	6029;
  DatumE_WGS84 =	6030;
  DatumE_GEM10C =	6031;
  DatumE_OSU86F =	6032;
  DatumE_OSU91A =	6033;
  DatumE_Clarke1880 =	6034;
  DatumE_Sphere =	6035;

  // standard datums
  Datum_Adindan =	6201;
  Datum_Australian_Geodetic_Datum_1966 =	6202;
  Datum_Australian_Geodetic_Datum_1984 =	6203;
  Datum_Ain_el_Abd_1970 =	6204;
  Datum_Afgooye =	6205;
  Datum_Agadez =	6206;
  Datum_Lisbon =	6207;
  Datum_Aratu =	6208;
  Datum_Arc_1950 =	6209;
  Datum_Arc_1960 =	6210;
  Datum_Batavia =	6211;
  Datum_Barbados =	6212;
  Datum_Beduaram =	6213;
  Datum_Beijing_1954 =	6214;
  Datum_Reseau_National_Belge_1950 =	6215;
  Datum_Bermuda_1957 =	6216;
  Datum_Bern_1898 =	6217;
  Datum_Bogota =	6218;
  Datum_Bukit_Rimpah =	6219;
  Datum_Camacupa =	6220;
  Datum_Campo_Inchauspe =	6221;
  Datum_Cape =	6222;
  Datum_Carthage =	6223;
  Datum_Chua =	6224;
  Datum_Corrego_Alegre =	6225;
  Datum_Cote_d_Ivoire =	6226;
  Datum_Deir_ez_Zor =	6227;
  Datum_Douala =	6228;
  Datum_Egypt_1907 =	6229;
  Datum_European_Datum_1950 =	6230;
  Datum_European_Datum_1987 =	6231;
  Datum_Fahud =	6232;
  Datum_Gandajika_1970 =	6233;
  Datum_Garoua =	6234;
  Datum_Guyane_Francaise =	6235;
  Datum_Hu_Tzu_Shan =	6236;
  Datum_Hungarian_Datum_1972 =	6237;
  Datum_Indonesian_Datum_1974 =	6238;
  Datum_Indian_1954 =	6239;
  Datum_Indian_1975 =	6240;
  Datum_Jamaica_1875 =	6241;
  Datum_Jamaica_1969 =	6242;
  Datum_Kalianpur =	6243;
  Datum_Kandawala =	6244;
  Datum_Kertau =	6245;
  Datum_Kuwait_Oil_Company =	6246;
  Datum_La_Canoa =	6247;
  Datum_Provisional_S_American_Datum_1956 =	6248;
  Datum_Lake =	6249;
  Datum_Leigon =	6250;
  Datum_Liberia_1964 =	6251;
  Datum_Lome =	6252;
  Datum_Luzon_1911 =	6253;
  Datum_Hito_XVIII_1963 =	6254;
  Datum_Herat_North =	6255;
  Datum_Mahe_1971 =	6256;
  Datum_Makassar =	6257;
  Datum_European_Reference_System_1989 =	6258;
  Datum_Malongo_1987 =	6259;
  Datum_Manoca =	6260;
  Datum_Merchich =	6261;
  Datum_Massawa =	6262;
  Datum_Minna =	6263;
  Datum_Mhast =	6264;
  Datum_Monte_Mario =	6265;
  Datum_M_poraloko =	6266;
  Datum_North_American_Datum_1927 =	6267;
  Datum_NAD_Michigan =	6268;
  Datum_North_American_Datum_1983 =	6269;
  Datum_Nahrwan_1967 =	6270;
  Datum_Naparima_1972 =	6271;
  Datum_New_Zealand_Geodetic_Datum_1949 =	6272;
  Datum_NGO_1948 =	6273;
  Datum_Datum_73 =	6274;
  Datum_Nouvelle_Triangulation_Francaise =	6275;
  Datum_NSWC_9Z_2 =	6276;
  Datum_OSGB_1936 =	6277;
  Datum_OSGB_1970_SN =	6278;
  Datum_OS_SN_1980 =	6279;
  Datum_Padang_1884 =	6280;
  Datum_Palestine_1923 =	6281;
  Datum_Pointe_Noire =	6282;
  Datum_Geocentric_Datum_of_Australia_1994 =	6283;
  Datum_Pulkovo_1942 =	6284;
  Datum_Qatar =	6285;
  Datum_Qatar_1948 =	6286;
  Datum_Qornoq =	6287;
  Datum_Loma_Quintana =	6288;
  Datum_Amersfoort =	6289;
  Datum_RT38 =	6290;
  Datum_South_American_Datum_1969 =	6291;
  Datum_Sapper_Hill_1943 =	6292;
  Datum_Schwarzeck =	6293;
  Datum_Segora =	6294;
  Datum_Serindung =	6295;
  Datum_Sudan =	6296;
  Datum_Tananarive_1925 =	6297;
  Datum_Timbalai_1948 =	6298;
  Datum_TM65 =	6299;
  Datum_TM75 =	6300;
  Datum_Tokyo =	6301;
  Datum_Trinidad_1903 =	6302;
  Datum_Trucial_Coast_1948 =	6303;
  Datum_Voirol_1875 =	6304;
  Datum_Voirol_Unifie_1960 =	6305;
  Datum_Bern_1938 =	6306;
  Datum_Nord_Sahara_1959 =	6307;
  Datum_Stockholm_1938 =	6308;
  Datum_Yacare =	6309;
  Datum_Yoff =	6310;
  Datum_Zanderij =	6311;
  Datum_Militar_Geographische_Institut =	6312;
  Datum_Reseau_National_Belge_1972 =	6313;
  Datum_Deutsche_Hauptdreiecksnetz =	6314;
  Datum_Conakry_1905 =	6315;
  Datum_WGS72 =	6322;
  Datum_WGS72_Transit_Broadcast_Ephemeris =	6324;
  Datum_WGS84 =	6326;
  Datum_Ancienne_Triangulation_Francaise =	6901;
  Datum_Nord_de_Guerre =	6902;
{$ENDREGION}

procedure InitLibGeoTiff(const ALibName: string = libgeotiff_dll);

implementation

{$IFDEF GEOTIFF_STATIC_LINK}
procedure InitLibGeoTiff(const ALibName: string);
begin
  // empty
end;
{$ELSE}
uses
  Windows,
  SysUtils,
  SyncObjs;

var
  gHandle: THandle = 0;
  gLock: TCriticalSection = nil;
  gIsInitialized: Boolean = False;

function GetProcAddr(const AProcName: PAnsiChar): Pointer;
begin
  Result := GetProcAddress(gHandle, AProcName);
  if Addr(Result) = nil then begin
    RaiseLastOSError;
  end;
end;

procedure InitLibGeoTiff(const ALibName: string);
begin
  if gIsInitialized then begin
    Exit;
  end;

  gLock.Acquire;
  try
    if gIsInitialized then begin
      Exit;
    end;

    if gHandle = 0 then begin
      gHandle := LoadLibrary(PChar(ALibName));
    end;

    if gHandle <> 0 then begin
      XTIFFInitialize := GetProcAddr('XTIFFInitialize');
      XTIFFOpen := GetProcAddr('XTIFFOpen');
      XTIFFClientOpen := GetProcAddr('XTIFFClientOpen');
      XTIFFClose := GetProcAddr('XTIFFClose');
      GTIFNew := GetProcAddr('GTIFNew');
      GTIFFree := GetProcAddr('GTIFFree');
      GTIFWriteKeys := GetProcAddr('GTIFWriteKeys');
      GTIFKeySet := GetProcAddr('GTIFKeySet');
    end else begin
      RaiseLastOSError;
    end;

    gIsInitialized := True;
  finally
    gLock.Release;
  end;
end;

procedure FinLibTiff;
begin
  gLock.Acquire;
  try
    gIsInitialized := False;

    if gHandle <> 0 then begin
      FreeLibrary(gHandle);
      gHandle := 0;
    end;

    XTIFFInitialize := nil;
    XTIFFOpen := nil;
    XTIFFClientOpen := nil;
    XTIFFClose := nil;
    GTIFNew := nil;
    GTIFFree := nil;
    GTIFWriteKeys := nil;
    GTIFKeySet := nil;
  finally
    gLock.Release;
  end;
end;

initialization
  gLock := TCriticalSection.Create;

finalization
  FinLibTiff;
  FreeAndNil(gLock);
{$ENDIF}

end.
