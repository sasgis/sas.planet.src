unit u_TileDownloaderViaPHP;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_ConfigDataProvider,
  i_CoordConverter,
  i_TileDownlodSession,
  php4delphi;

type
  TPhpScript = class
    private
      FURLBase: string;
      FDefUrlBase: string;
      FZMPFileName: string; // путь к zmp
      FScriptPath: string;  // путь к скрипту
      FScriptStr: string;   // сам скрипт
      FLastError: string;   // последняя ошибка ИЗ скрипта
      FCookieStr: string;   // буферная строка для межскриптовых операции
      FEngine: TPHPEngine;
      FEngineInitialized: Boolean;
      FCoordConverter: ICoordConverterSimple;
      procedure Initialize(AConfig: IConfigDataProvider);
    public
      constructor Create(AConfig: IConfigDataProvider; const ZmpFileName: string);
      destructor Destroy; override;
      function DownloadTile(ACheckTileSize: Boolean; AOldTileSize: Cardinal; ATile: TPoint; AZoom: Byte; out AUrl, AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult;

      property Enabled: Boolean read FEngineInitialized;
      property UrlBase: string write FURLBase;
      property DefUrlBase: string write FDefUrlBase;
  end;

const
  ZmpScriptName = 'downloader.php';

implementation

uses
  t_GeoTypes,
  u_GlobalState;

type
  TPHPReader = class
    private
      FTile: TPoint;
      FZoom: Byte;
      FOldTileSize: Integer;
      FCookieStr: string;
      FUrl: string;
      FDefUrl: string;
      FMIMEType: string;
      FBody: TMemoryStream;
      FPHP: TpsvPHP;
      FScript: string;
      FCoordConverter: ICoordConverterSimple;
      procedure OnReadResult (Sender : TObject; Stream : TStream);
    public
      constructor Create(const AFileName: string; const AScript: string = '');
      destructor Destroy; override;
      function Exec (out ExecError: string): integer;

      property Tile: TPoint write FTile;
      property Zoom: Byte write FZoom;
      property FileBuf: TMemoryStream read FBody write FBody;
      property OldTileSize: Integer write FOldTileSize;
      property Cookie: string read FCookieStr write FCookieStr;
      property Url: string read FUrl write FUrl;
      property DefUrl: string write FDefUrl;
      property MIMEType: string read FMIMEType;
  end;

  TSpetialParams = record
    LLon  : TDoublePoint;
    TLat  : TDoublePoint;
    RLon  : TDoublePoint;
    BLat  : TDoublePoint;
    LMetr : TDoublePoint;
    TMetr : TDoublePoint;
    RMetr : TDoublePoint;
    BMetr : TDoublePoint;
  end;

var
  PhpThreadSafe: TRTLCriticalSection;

{ TPhpScript }

constructor TPhpScript.Create(AConfig: IConfigDataProvider; const ZmpFileName: string);
begin
  inherited Create;
  FEngineInitialized := False;
  FURLBase := '';
  FDefUrlBase := '';
  FScriptPath := '';
  FLastError := '';
  FCookieStr := '';
  FZMPFileName := ZmpFileName;
  Initialize(AConfig);

end;

destructor TPhpScript.Destroy;
begin
  try
    if FEngineInitialized then    
      FEngine.ShutdownEngine;
    FreeAndNil(FEngine);
  finally
    inherited Destroy;
  end;
end;

procedure TPhpScript.Initialize(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
  VCoordConverter: ICoordConverter;
  VMem: TMemoryStream;
begin
  try
    VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
    VCoordConverter := GState.CoordConverterFactory.GetCoordConverterByConfig(VParams);
    FCoordConverter := VCoordConverter as ICoordConverterSimple;

    VParams := AConfig.GetSubItem('params.txt').GetSubItem('PHP');
    if VParams <> nil then
      if VParams.ReadBool('Use', False) then
        if FileExists(FZMPFileName + '\' + ZmpScriptName) then
        begin
          FScriptPath := FZMPFileName + '\' + ZmpScriptName;

          if VParams.ReadBool('Preload', False) then
          begin  // загружаем скрипт в память, для экономии дисковых операций
              VMem := TMemoryStream.Create;
            try
              VMem.LoadFromFile(FScriptPath);
              if VMem.Size > 0 then
              begin
                SetLength(FScriptStr, VMem.Size);
                VMem.Position := 0;
                VMem.ReadBuffer(FScriptStr[1], VMem.Size);
              end;
            finally
              FreeAndNil(VMem);
            end;
          end;

          if FScriptPath <> '' then
          begin
            FEngine := TPHPEngine.Create(nil);
            FEngine.HandleErrors := VParams.ReadBool('Debug', False);
            FEngine.StartupEngine;
            FEngineInitialized := True;
          end;
        end;
  except
    FEngineInitialized := False;
  end;
end;

function TPhpScript.DownloadTile(ACheckTileSize: Boolean; AOldTileSize: Cardinal; ATile: TPoint; AZoom: Byte; out AUrl: string; out AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult;
var
  VPhpReader: TPHPReader;
  VErrStr: string;
begin
  try
      VPhpReader := TPHPReader.Create(FScriptPath, FScriptStr);
    try
      VPhpReader.Url := FURLBase;
      VPhpReader.DefUrl := FDefUrlBase;
      VPhpReader.Tile := ATile;
      VPhpReader.Zoom := AZoom;

      if ACheckTileSize then
        VPhpReader.OldTileSize := AOldTileSize
      else
        VPhpReader.OldTileSize := -1;

      VPhpReader.FileBuf := fileBuf;

        EnterCriticalSection(PhpThreadSafe);
      try
        VPhpReader.Cookie := FCookieStr;
      finally
        LeaveCriticalSection(PhpThreadSafe);
      end;

      try
        VErrStr := '';
        Result := TDownloadTileResult( VPhpReader.Exec(VErrStr) );
        if VErrStr <> '' then
          raise Exception.Create(VErrStr);
      except
        Result := dtrUnknownError;
      end;

      if Result <> dtrUnknownError then
      begin
        AUrl := VPhpReader.FUrl;
        AContentType := VPhpReader.MIMEType;

          EnterCriticalSection(PhpThreadSafe);
        try
          FCookieStr := VPhpReader.Cookie;
        finally
          LeaveCriticalSection(PhpThreadSafe);
        end;
      end;

    finally
      FreeAndNil(VPhpReader);
    end;
  except
    Result := dtrUnknownError;
  end;
end;

{ TPHPReader }

constructor TPHPReader.Create(const AFileName: string; const AScript: string = '');
begin
  inherited Create;
  FScript := AScript;
  FBody := nil;
  FPHP    := TpsvPHP.Create(nil);
  FPHP.FileName := AFileName;
  FPHP.OnReadResult := Self.OnReadResult;
end;

destructor TPHPReader.Destroy;
begin
  try
    FPHP.Free;
  finally
    inherited Destroy;
  end;
end;

procedure TPHPReader.OnReadResult (Sender : TObject; Stream : TStream);
begin
  if Assigned(Stream) and Assigned(FBody) then
  if Stream.Size > 0 then  
  begin
    FBody.Clear;
    FBody.LoadFromStream(Stream);
  end;
end;

function TPHPReader.Exec (out ExecError: string): integer;
begin
  ExecError := '';

  // служебные параметры:                    
  FPHP.Variables.Add.Name := 'errstr';
  FPHP.Variables.ByName('errstr').AsString := '';

  FPHP.Variables.Add.Name := 'download_result';
  FPHP.Variables.ByName('download_result').AsInteger := 0;

  FPHP.Variables.Add.Name := 'url';
  FPHP.Variables.ByName('url').AsString := FUrl;

  FPHP.Variables.Add.Name := 'def_url';
  FPHP.Variables.ByName('def_url').AsString := FDefUrl;

  FPHP.Variables.Add.Name := 'cookie';
  FPHP.Variables.ByName('cookie').AsString := FCookieStr;

  FPHP.Variables.Add.Name := 'mime_type';

  // параметры запроса
  FPHP.Variables.Add.Name := 'x';
  FPHP.Variables.ByName('x').AsInteger := FTile.X;

  FPHP.Variables.Add.Name := 'y';
  FPHP.Variables.ByName('y').AsInteger := FTile.Y;

  FPHP.Variables.Add.Name := 'z';
  FPHP.Variables.ByName('z').AsInteger := FZoom;

  FPHP.Variables.Add.Name := 'old_tile_sz';
  FPHP.Variables.ByName('old_tile_sz').AsInteger := FOldTileSize;

  // дополнительные параметры

  try
    // запускаем скрипт
    if FScript = '' then
      FPHP.Execute
    else
      FPHP.RunCode(FScript);

    // забираем результат
    ExecError := FPHP.Variables.ByName('errstr').AsString;

    Result := FPHP.Variables.ByName('download_result').AsInteger;

    FUrl := FPHP.Variables.ByName('url').AsString;

    FCookieStr := FPHP.Variables.ByName('cookie').AsString;

    FMIMEType := FPHP.Variables.ByName('mime_type').AsString;

  except
    Result := Integer(dtrUnknownError);
  end;
end;

{
procedure TUrlGenerator.SetVar(AXY: TPoint; AZoom: Byte);
var
  XY: TPoint;
  Ll: TDoublePoint;
begin
  FpGetX.Data := AXY.X;
  FpGetY.Data := AXY.Y;
  FpGetZ.Data := AZoom + 1;
  Ll := FCoordConverter.Pos2LonLat(AXY, AZoom);
  FpGetLlon.Data := Ll.X;
  FpGetTLat.Data := Ll.Y;
  Ll := FCoordConverter.LonLat2Metr(LL);
  FpGetLMetr.Data := Ll.X;
  FpGetTMetr.Data := Ll.Y;
  XY := AXY;
  Inc(XY.X);
  Inc(XY.Y);
  Ll := FCoordConverter.Pos2LonLat(XY, AZoom);
  FpGetRLon.Data := Ll.X;
  FpGetBLat.Data := Ll.Y;
  Ll := FCoordConverter.LonLat2Metr(LL);
  FpGetRMetr.Data := Ll.X;
  FpGetBMetr.Data := Ll.Y;
  FpConverter.Data := FCoordConverter;
  FpGetURLBase.Data := FURLBase;
end;
 }


initialization
InitializeCriticalSection(PhpThreadSafe);

finalization
DeleteCriticalSection(PhpThreadSafe);

end.
