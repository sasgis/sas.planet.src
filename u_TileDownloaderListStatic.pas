unit u_TileDownloaderListStatic;

interface

uses
  Classes,
  i_TileDownloaderState,
  i_TileDownloader,
  i_TileDownloaderList;

type
  TTileDownloaderListStatic = class(TInterfacedObject, ITileDownloaderListStatic)
  private
    FState: ITileDownloaderStateStatic;
    FList: array of ITileDownloader;
    FCount: Integer;
  protected
    function GetState: ITileDownloaderStateStatic;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): ITileDownloader;
  public
    constructor Create(
      AState: ITileDownloaderStateStatic;
      AList: array of ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

{ TTileDownloaderListStatic }

constructor TTileDownloaderListStatic.Create(
  AState: ITileDownloaderStateStatic;
  AList: array of ITileDownloader
);
var
  i: Integer;
begin
  FState := AState;
  if FState.Enabled then begin
    FCount := Length(AList);
    SetLength(FList, FCount);
    for i := 0 to FCount - 1 do begin
      FList[i] := AList[i];
    end;
  end else begin
    FCount := 0;
    FList := nil;
  end;
end;

destructor TTileDownloaderListStatic.Destroy;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do begin
    FList[i] := nil;
  end;
  FCount := 0;
  FList := nil;
  FState := nil;
  inherited;
end;

function TTileDownloaderListStatic.GetCount: Integer;
begin
  Result := FCount;
end;

function TTileDownloaderListStatic.GetItem(AIndex: Integer): ITileDownloader;
begin
  Result := FList[AIndex];
end;

function TTileDownloaderListStatic.GetState: ITileDownloaderStateStatic;
begin
  Result := FState;
end;

end.
