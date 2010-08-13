unit u_BitmapTypeExtManagerSimple;

interface

uses
  i_BitmapTileSaveLoad,
  i_IBitmapTypeExtManager;

type
  TBitmapTypeExtManagerSimple = class(TInterfacedObject, IBitmapTypeExtManager)
  private
    FLoaders: array of IBitmapTileLoader;
    FSavers: array of IBitmapTileSaver;
  public
    constructor Create;
    destructor Destroy; override;
    function GetIsBitmapType(AType: String): Boolean;
    function GetIsBitmapExt(AExt: String): Boolean;
    function GetExtForType(AType: String): string;
    function GetBitmapLoaderForExt(AExt: String): IBitmapTileLoader;
    function GetBitmapSaverForExt(AExt: String): IBitmapTileSaver;
    function GetBitmapLoaderForType(AType: String): IBitmapTileLoader;
    function GetBitmapSaverForType(AType: String): IBitmapTileSaver;
  end;

implementation

uses
  SysUtils,
  u_BitmapTileJpegLoader,
  u_BitmapTileJpegSaverIJL,
  u_BitmapTileVampyreLoader,
  u_BitmapTileVampyreSaver,
  u_BitmapTilePngSaver,
  u_BitmapTileGifLoader,
  u_BitmapTileGifSaver,
  u_BitmapTileBmpLoader,
  u_BitmapTileBmpSaver;


{ TBitmapTypeExtManagerSimple }

constructor TBitmapTypeExtManagerSimple.Create;
begin
  SetLength(FLoaders, 4);
  FLoaders[0] := TJpegBitmapTileLoader.Create;
  FLoaders[1] := TVampyreBasicBitmapTileLoaderPNG.Create;
  FLoaders[2] := TVampyreBasicBitmapTileLoaderGIF.Create;
  FLoaders[3] := TVampyreBasicBitmapTileLoaderBMP.Create;
  SetLength(FSavers, 4);
  FSavers[0] := TJpegBitmapTileSaverIJL.Create(85);
//  FSavers[1] := TPngBitmapTileSaver.Create;
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
  AExt: String): IBitmapTileLoader;
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
    raise Exception.Create('Неизвестное расширение. Не знаю как загружать.');
  end;
end;

function TBitmapTypeExtManagerSimple.GetBitmapLoaderForType(
  AType: String): IBitmapTileLoader;
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
    raise Exception.Create('Неизвестный тип. Не знаю как загружать.');
  end;
end;

function TBitmapTypeExtManagerSimple.GetBitmapSaverForExt(
  AExt: String): IBitmapTileSaver;
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
    raise Exception.Create('Неизвестное расширение. Не знаю как сохранять.');
  end;
end;

function TBitmapTypeExtManagerSimple.GetBitmapSaverForType(
  AType: String): IBitmapTileSaver;
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
    raise Exception.Create('Неизвестный тип. Не знаю как сохранять.');
  end;
end;

function TBitmapTypeExtManagerSimple.GetExtForType(AType: String): string;
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

function TBitmapTypeExtManagerSimple.GetIsBitmapExt(AExt: String): Boolean;
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
  AType: String): Boolean;
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
