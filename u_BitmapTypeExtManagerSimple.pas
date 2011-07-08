unit u_BitmapTypeExtManagerSimple;

interface

uses
  i_BitmapTileSaveLoad,
  i_BitmapTypeExtManager;

type
  TBitmapTypeExtManagerSimple = class(TInterfacedObject, IBitmapTypeExtManager)
  private
    FLoaders: array of IBitmapTileLoader;
    FSavers: array of IBitmapTileSaver;
  public
    constructor Create;
    destructor Destroy; override;
    function GetIsBitmapType(const AType: String): Boolean;
    function GetIsBitmapExt(const AExt: String): Boolean;
    function GetExtForType(const AType: String): string;
    function GetBitmapLoaderForExt(const AExt: String): IBitmapTileLoader;
    function GetBitmapSaverForExt(const AExt: String): IBitmapTileSaver;
    function GetBitmapLoaderForType(const AType: String): IBitmapTileLoader;
    function GetBitmapSaverForType(const AType: String): IBitmapTileSaver;
  end;

implementation

uses
  SysUtils,
  u_ResStrings,
  u_BitmapTileVampyreLoader,
  u_BitmapTileVampyreSaver;


{ TBitmapTypeExtManagerSimple }

constructor TBitmapTypeExtManagerSimple.Create;
begin
  SetLength(FLoaders, 4);
  FLoaders[0] := TVampyreBasicBitmapTileLoaderJPEG.Create;
  FLoaders[1] := TVampyreBasicBitmapTileLoaderPNG.Create;
  FLoaders[2] := TVampyreBasicBitmapTileLoaderGIF.Create;
  FLoaders[3] := TVampyreBasicBitmapTileLoaderBMP.Create;
  SetLength(FSavers, 4);
  FSavers[0] := TVampyreBasicBitmapTileSaverJPG.Create(85);
  FSavers[1] := TVampyreBasicBitmapTileSaverPNG.Create;
  FSavers[2] := TVampyreBasicBitmapTileSaverGIF.Create;
  FSavers[3] := TVampyreBasicBitmapTileSaverBMP.Create;
end;

destructor TBitmapTypeExtManagerSimple.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FLoaders) - 1 do begin
    FLoaders[i] := nil;
  end;
  FLoaders := nil;

  for i := 0 to Length(FSavers) - 1 do begin
    FSavers[i] := nil;
  end;
  FSavers := nil;

  inherited;
end;

function TBitmapTypeExtManagerSimple.GetBitmapLoaderForExt(
  const AExt: String): IBitmapTileLoader;
begin
  if SameText(AExt, '.jpg') then begin
    Result := FLoaders[0];
  end else if SameText(AExt, '.png') then begin
    Result := FLoaders[1];
  end else if SameText(AExt, '.gif') then begin
    Result := FLoaders[2];
  end else if SameText(AExt, '.bmp') then begin
    Result := FLoaders[3];
  end else begin
    Result := nil;
  end;
end;

function TBitmapTypeExtManagerSimple.GetBitmapLoaderForType(
  const AType: String): IBitmapTileLoader;
begin
  if SameText(AType, 'image/jpg') or SameText(AType, 'image/jpeg') or SameText(AType, 'image/pjpeg') then begin
    Result := FLoaders[0];
  end else if SameText(AType, 'image/png') or SameText(AType, 'image/x-png') or SameText(AType, 'image/png; mode=24bit') then begin
    Result := FLoaders[1];
  end else if SameText(AType, 'image/gif') then begin
    Result := FLoaders[2];
  end else if SameText(AType, 'image/bmp') or SameText(AType, 'image/x-ms-bmp') or SameText(AType, 'image/x-windows-bmp') then begin
    Result := FLoaders[3];
  end else begin
    Result := nil;
  end;
end;

function TBitmapTypeExtManagerSimple.GetBitmapSaverForExt(
  const AExt: String): IBitmapTileSaver;
begin
  if SameText(AExt, '.jpg') then begin
    Result := FSavers[0];
  end else if SameText(AExt, '.png') then begin
    Result := FSavers[1];
  end else if SameText(AExt, '.gif') then begin
    Result := FSavers[2];
  end else if SameText(AExt, '.bmp') then begin
    Result := FSavers[3];
  end else begin
    raise Exception.Create(SAS_ERR_UnknownImageExt);
  end;
end;

function TBitmapTypeExtManagerSimple.GetBitmapSaverForType(
  const AType: String): IBitmapTileSaver;
begin
  if SameText(AType, 'image/jpg') or SameText(AType, 'image/jpeg') or SameText(AType, 'image/pjpeg') then begin
    Result := FSavers[0];
  end else if SameText(AType, 'image/png') or SameText(AType, 'image/x-png') or SameText(AType, 'image/png; mode=24bit') then begin
    Result := FSavers[1];
  end else if SameText(AType, 'image/gif') then begin
    Result := FSavers[2];
  end else if SameText(AType, 'image/bmp') or SameText(AType, 'image/x-ms-bmp') or SameText(AType, 'image/x-windows-bmp') then begin
    Result := FSavers[3];
  end else begin
    raise Exception.Create(SAS_ERR_UnknownImageMIMEType);
  end;
end;

function TBitmapTypeExtManagerSimple.GetExtForType(const AType: String): string;
begin
  if SameText(AType, 'image/jpg') or SameText(AType, 'image/jpeg') or SameText(AType, 'image/pjpeg') then begin
    Result := '.jpg';
  end else if SameText(AType, 'image/png') or SameText(AType, 'image/x-png') or SameText(AType, 'image/png; mode=24bit') then begin
    Result := '.png';
  end else if SameText(AType, 'image/gif') then begin
    Result := '.gif';
  end else if SameText(AType, 'image/bmp') or SameText(AType, 'image/x-ms-bmp') or SameText(AType, 'image/x-windows-bmp') then begin
    Result := '.bmp';
  end else begin
    Result := '';
  end;
end;

function TBitmapTypeExtManagerSimple.GetIsBitmapExt(const AExt: String): Boolean;
begin
  if SameText(AExt, '.jpg') then begin
    Result := true;
  end else if SameText(AExt, '.png') then begin
    Result := True;
  end else if SameText(AExt, '.gif') then begin
    Result := True;
  end else if SameText(AExt, '.bmp') then begin
    Result := True;
  end else begin
    Result := False;
  end;
end;

function TBitmapTypeExtManagerSimple.GetIsBitmapType(
  const AType: String): Boolean;
begin
  if SameText(AType, 'image/jpg') or SameText(AType, 'image/jpeg') or SameText(AType, 'image/pjpeg') then begin
    Result := true;
  end else if SameText(AType, 'image/png') or SameText(AType, 'image/x-png') or SameText(AType, 'image/png; mode=24bit') then begin
    Result := True;
  end else if SameText(AType, 'image/gif') then begin
    Result := True;
  end else if SameText(AType, 'image/bmp') or SameText(AType, 'image/x-ms-bmp') or SameText(AType, 'image/x-windows-bmp') then begin
    Result := True;
  end else begin
    Result := False;
  end;
end;

end.
