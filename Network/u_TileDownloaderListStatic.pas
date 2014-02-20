unit u_TileDownloaderListStatic;

interface

uses
  i_TileDownloader,
  i_TileDownloaderList,
  u_BaseInterfacedObject;

type
  TTileDownloaderListStatic = class(TBaseInterfacedObject, ITileDownloaderListStatic)
  private
    FList: array of ITileDownloader;
    FCount: Integer;
  private
    function GetCount: Integer;
    function GetItem(AIndex: Integer): ITileDownloader;
  public
    constructor Create(
      const AList: array of ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

{ TTileDownloaderListStatic }

constructor TTileDownloaderListStatic.Create(
  const AList: array of ITileDownloader
);
var
  i: Integer;
begin
  inherited Create;
  FCount := Length(AList);
  SetLength(FList, FCount);
  for i := 0 to FCount - 1 do begin
    FList[i] := AList[i];
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

end.
