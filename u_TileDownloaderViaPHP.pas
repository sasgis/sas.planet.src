unit u_TileDownloaderViaPHP;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_TileDownlodSession,
  php4delphi;

type

  TPhpScript = class
    private
      FScriptPath: string;
      FLastError: string;
      FCookieStr: string;
      FEngine: TPHPEngine;
    public
      constructor Create;
      destructor Destroy; override;
      function DownloadTile(ACheckTileSize: Boolean; AOldTileSize: Cardinal; ATile: TPoint; AZoom: Byte; out AUrl, AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult;

      property ScriptPath: string read FScriptPath write FScriptPath;
  end;


implementation

type

  TPHPReader = class
    private
      FTile: TPoint;
      FZoom: Byte;
      FOldTileSize: Integer;
      FCookieStr: string;
      FUrl: string;
      FMMEType: string;
      FHeaders: string;
      FBody: TMemoryStream;
      FPHP: TpsvPHP;
      //FEngine: TPHPEngine;

      procedure OnReadResult (Sender : TObject; Stream : TStream);
    public
      constructor Create(const AFileName: string);
      destructor Destroy; override;
      function Exec (out ExecError: string): integer;

      property Tile: TPoint write FTile;
      property Zoom: Byte write FZoom;
      property FileBuf: TMemoryStream read FBody write FBody;
      property OldTileSize: Integer write FOldTileSize;
      property Cookie: string read FCookieStr write FCookieStr;
      property Url: string read FUrl;
      property MMEType: string read FMMEType;
  end;

var
  PhpThreadSafe: TRTLCriticalSection;

{ TPhpScript }

constructor TPhpScript.Create;
begin
  inherited Create;
  FScriptPath := '';
  FLastError := '';
  FCookieStr := '';
  FEngine := TPHPEngine.Create(nil);
  FEngine.StartupEngine;
end;

destructor TPhpScript.Destroy;
begin
  try
    FEngine.ShutdownEngine;
    FreeAndNil(FEngine);
  finally
    inherited Destroy;
  end;
end;

function TPhpScript.DownloadTile(ACheckTileSize: Boolean; AOldTileSize: Cardinal; ATile: TPoint; AZoom: Byte; out AUrl: string; out AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult;
var
  VPhpReader: TPHPReader;
  VStr: string;
begin
    //Result := dtrUnknownError;
  try
      VPhpReader := TPHPReader.Create(FScriptPath);
    try
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
        Result := TDownloadTileResult( VPhpReader.Exec(VStr) );
      except

      end;

      AUrl := VPhpReader.FUrl;

      AContentType := VPhpReader.MMEType;

      fileBuf.Position := 0;

        EnterCriticalSection(PhpThreadSafe);
      try
        FCookieStr := VPhpReader.Cookie;
        FLastError := VStr;
      finally
        LeaveCriticalSection(PhpThreadSafe);
      end;

    finally
      FreeAndNil(VPhpReader);
    end;
  except
    Result := dtrUnknownError;
  end;
end;

{ TPHPReader }

constructor TPHPReader.Create(const AFileName: string);
begin
  inherited Create;
  FHeaders := '';
  FBody := nil;
  FPHP    := TpsvPHP.Create(nil);
  FPHP.FileName := AFileName;
  FPHP.OnReadResult := Self.OnReadResult;
end;

destructor TPHPReader.Destroy;
begin
  try
    FPHP.Free;
    //FEngine.Free;
  finally
    inherited Destroy;
  end;
end;

procedure TPHPReader.OnReadResult (Sender : TObject; Stream : TStream);
begin
  if Assigned(Sender) then
    FHeaders := ( (Sender as TpsvCustomPHP).Headers.GetHeaders );

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
    //Result := -1;
    //FEngine.StartupEngine;
  try
    // служебные параметры:                    
    FPHP.Variables.Add.Name := 'errstr';
    FPHP.Variables.ByName('errstr').AsString := '';
    FPHP.Variables.Add.Name := 'download_result';
    FPHP.Variables.ByName('download_result').AsInteger := 0;
    FPHP.Variables.Add.Name := 'url';
    FPHP.Variables.ByName('url').AsString := '';
    FPHP.Variables.Add.Name := 'cookie';
    FPHP.Variables.ByName('cookie').AsString := FCookieStr;
    FPHP.Variables.Add.Name := 'mme_type';

    // параметры запроса
    FPHP.Variables.Add.Name := 'x';
    FPHP.Variables.ByName('x').AsInteger := FTile.X;
    FPHP.Variables.Add.Name := 'y';
    FPHP.Variables.ByName('y').AsInteger := FTile.Y;
    FPHP.Variables.Add.Name := 'z';
    FPHP.Variables.ByName('z').AsInteger := FZoom;
    FPHP.Variables.Add.Name := 'old_tile_sz';
    FPHP.Variables.ByName('old_tile_sz').AsInteger := FOldTileSize;

    // запускаем скрипт
    FPHP.Execute;

    // забираем результат
    ExecError := FPHP.Variables.ByName('errstr').AsString;
    Result := FPHP.Variables.ByName('download_result').AsInteger;
    FUrl := FPHP.Variables.ByName('url').AsString;
    FCookieStr := FPHP.Variables.ByName('cookie').AsString;
    FMMEType := FPHP.Variables.ByName('mme_type').AsString;

  finally
    //FEngine.ShutdownEngine;
  end;
end;

initialization
InitializeCriticalSection(PhpThreadSafe);

finalization
DeleteCriticalSection(PhpThreadSafe);

end.
