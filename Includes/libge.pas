unit libge;

interface

uses
  Types,
  SysUtils,
  i_BinaryData;

type
  EGoogleEarthException = class(Exception);

  IGoogleEarthImageTileProvider = interface
    ['{94E7D5DC-6601-483B-86AD-366CB10E6EB9}']
    function GetBMP: IBinaryData; safecall;
    function GetDDS: IBinaryData; safecall;
    function GetJPEG: IBinaryData; safecall;

    function GetExifDate: TDateTime; safecall;
    property ExifDate: TDateTime read GetExifDate;

    function GetJpegQuality: Integer; safecall;
    procedure SetJpegQuality(const AValue: Integer); safecall;
    property CompressJpegQuality: Integer read GetJpegQuality write SetJpegQuality;
  end;
  
  IGoogleEarthImageTileProviderFactory = interface
    ['{E3954F0A-FEC3-49D1-9F16-1B250182940B}']
    function CreateImageTileProviderWithDataOwn(
      const AData: IBinaryData
    ): IGoogleEarthImageTileProvider; safecall;
  end;

  IGoogleEarthTerrainTileList = interface
    ['{51534AA0-F941-4A42-A4BD-1749E58EDBC9}']
    procedure Add(
      const X, Y: Integer;
      const AZoom: Byte;
      const AData: IBinaryData;
      const AIsOcean: Boolean
    ); safecall;

    function Get(
      const AIndex: Integer;
      out X, Y: Integer;
      out AZoom: Byte;
      out AIsOcean: Boolean
    ): IBinaryData; safecall;

    function GetCount: Integer; safecall;
    property Count: Integer read GetCount;
  end;

  IGoogleEarthTerrainTileProvider = interface
    ['{F369DDE7-D239-4BBF-B199-41C67BFDBDCD}']
    function PointElevation(const ALon: Double; const ALat: Double): Single; safecall;

    function GetKML: IGoogleEarthTerrainTileList; overload; safecall;
    function GetKML(const X, Y: Integer; const AZoom: Byte): IBinaryData; overload; safecall;

    function GetKMZ: IGoogleEarthTerrainTileList; overload; safecall;
    function GetKMZ(const X, Y: Integer; const AZoom: Byte): IBinaryData; overload; safecall;

    function GetSTL: IGoogleEarthTerrainTileList; overload; safecall;
    function GetSTL(const X, Y: Integer; const AZoom: Byte): IBinaryData; overload; safecall;

    function GetBT: IGoogleEarthTerrainTileList; overload; safecall;
    function GetBT(const X, Y: Integer; const AZoom: Byte): IBinaryData; overload; safecall;
  end;
  
  IGoogleEarthTerrainTileProviderFactory = interface
    ['{2CF62C0C-1D60-453C-A1A1-521A9BDDDD42}']
    function CreateEarthTerrainTileProviderWithDataOwn(
      const AData: IBinaryData
    ): IGoogleEarthTerrainTileProvider; safecall;

    function CreateMarsTerrainTileProviderWithDataOwn(
      const AData: IBinaryData
    ): IGoogleEarthTerrainTileProvider; safecall;

    function CreateMoonTerrainTileProviderWithDataOwn(
      const AData: IBinaryData
    ): IGoogleEarthTerrainTileProvider; safecall;
  end;

  IGoogleEarthTileInfoList = interface
    ['{306A8AB1-FD0A-4997-8A11-5DB6301077E6}']  
    function Add(
      const ASize: Integer;
      const AVersion: Word;
      const AHistoryDate: TDateTime
    ): Integer; safecall;

    function Get(
      const AIndex: Integer;
      out ASize: Integer;
      out AVersion: Word;
      out AHistoryDate: TDateTime
    ): Boolean; safecall;

    function GetCount: Integer; safecall;
    property Count: Integer read GetCount;

    procedure SortByDate; safecall;

    procedure SortByVersion; safecall;
  end;

  IGoogleEarthCacheProvider = interface
    ['{74439293-C057-4E64-8F93-A0C64BA203B8}']
    function GetTileInfo(
      const APoint: TPoint;
      const AZoom: Byte;
      const AVersion: Word;
      const ADate: TDateTime;
      const ASearchAnyVersion: Boolean;
      const ASearchAnyDate: Boolean;
      const AWithData: Boolean;
      out ATileSize: Integer;
      out ATileVersion: Word;
      out ATileDate: TDateTime;
      out ATileContentProvider: IInterface
    ): Boolean; safecall;

    function GetListOfTileVersions(
      const APoint: TPoint;
      const AZoom: Byte;
      const AVersion: Word;
      const ADate: TDateTime
    ): IGoogleEarthTileInfoList; safecall;
  end;

  IGoogleEarthCacheProviderFactory = interface
    ['{701FE5A0-65EE-4F99-8C75-5C5E06CE011D}']
    // Images
    function CreateEarthProvider(const APath: PAnsiChar; out AErrMsg: WideString): IGoogleEarthCacheProvider; safecall;
    function CreateEarthTmProvider(const APath: PAnsiChar; out AErrMsg: WideString): IGoogleEarthCacheProvider; safecall;
    function CreateSkyProvider(const APath: PAnsiChar; out AErrMsg: WideString): IGoogleEarthCacheProvider; safecall;
    function CreateMarsProvider(const APath: PAnsiChar; out AErrMsg: WideString): IGoogleEarthCacheProvider; safecall;
    function CreateMoonProvider(const APath: PAnsiChar; out AErrMsg: WideString): IGoogleEarthCacheProvider; safecall;
    // Terrains
    function CreateEarthTerrainProvider(const APath: PAnsiChar; out AErrMsg: WideString): IGoogleEarthCacheProvider; safecall;
    function CreateMarsTerrainProvider(const APath: PAnsiChar; out AErrMsg: WideString): IGoogleEarthCacheProvider; safecall;
    function CreateMoonTerrainProvider(const APath: PAnsiChar; out AErrMsg: WideString): IGoogleEarthCacheProvider; safecall;
  end;

  IGeoCacherCacheProviderFactory = interface(IGoogleEarthCacheProviderFactory)
    ['{3A06A553-C2CD-4797-8FF5-922901B77067}']
  end;

function CreateGeoCacherCacheProviderFactory: IGoogleEarthCacheProviderFactory;
function CreateGoogleEarthCacheProviderFactory: IGoogleEarthCacheProviderFactory;
function CreateGoogleEarthImageTileProviderFactory: IGoogleEarthImageTileProviderFactory;
function CreateGoogleEarthTerrainTileProviderFactory: IGoogleEarthTerrainTileProviderFactory;

procedure RaiseGoogleEarthExceptionIfError(const AErrorMsg: WideString);

procedure CheckGoogleEarthTerrainTileZoom(var AZoom: Byte);

implementation

uses
  Windows,
  SyncObjs;

const
  libge_dll = 'libge.dll';

var
  libge_Handle: THandle = 0;
  libge_Lock: TCriticalSection = nil;
  libge_CreateObject: function(const IID: TGUID): IInterface; safecall;

function libge_LoadLibrary: Boolean;
begin
  libge_Lock.Acquire;
  try
    if libge_Handle = 0 then begin
      libge_Handle := LoadLibrary(libge_dll);
      if libge_Handle > 0 then begin
        libge_CreateObject := GetProcAddress(libge_Handle, 'CreateObject');
      end;
    end;
    Result := (libge_Handle > 0) and (Addr(libge_CreateObject) <> nil);
  finally
    libge_Lock.Release;
  end;
end;

procedure libge_UnloadLibrary;
begin
  libge_Lock.Acquire;
  try
    if libge_Handle > 0 then begin
      FreeLibrary(libge_Handle);
      libge_Handle := 0;
      libge_CreateObject := nil;
    end;
  finally
    libge_Lock.Release;
  end;
end;

function CreateGeoCacherCacheProviderFactory: IGoogleEarthCacheProviderFactory;
var
  VResult: IInterface;
begin
  Result := nil;
  if libge_LoadLibrary then begin
    VResult := libge_CreateObject(IGeoCacherCacheProviderFactory);
    Supports(VResult, IGoogleEarthCacheProviderFactory, Result);
  end;
end;

function CreateGoogleEarthCacheProviderFactory: IGoogleEarthCacheProviderFactory;
var
  VResult: IInterface;
begin
  Result := nil;
  if libge_LoadLibrary then begin
    VResult := libge_CreateObject(IGoogleEarthCacheProviderFactory);
    Supports(VResult, IGoogleEarthCacheProviderFactory, Result);
  end;
end;

function CreateGoogleEarthImageTileProviderFactory: IGoogleEarthImageTileProviderFactory;
var
  VResult: IInterface;
begin
  Result := nil;
  if libge_LoadLibrary then begin
    VResult := libge_CreateObject(IGoogleEarthImageTileProviderFactory);
    Supports(VResult, IGoogleEarthImageTileProviderFactory, Result);
  end;
end;

function CreateGoogleEarthTerrainTileProviderFactory: IGoogleEarthTerrainTileProviderFactory;
var
  VResult: IInterface;
begin
  Result := nil;
  if libge_LoadLibrary then begin
    VResult := libge_CreateObject(IGoogleEarthTerrainTileProviderFactory);
    Supports(VResult, IGoogleEarthTerrainTileProviderFactory, Result);
  end;
end;

procedure RaiseGoogleEarthExceptionIfError(const AErrorMsg: WideString);
begin
  if AErrorMsg <> '' then begin
    raise EGoogleEarthException.Create(AErrorMsg);
  end;
end;

procedure CheckGoogleEarthTerrainTileZoom(var AZoom: Byte);
begin
  if AZoom < 2 then begin
    AZoom := 2;
  end else if (AZoom mod 2) > 0 then begin
    AZoom := AZoom - 1;
  end;
end;

initialization
  libge_Lock := TCriticalSection.Create;

finalization
  libge_UnloadLibrary;
  libge_Lock.Free;

end.
