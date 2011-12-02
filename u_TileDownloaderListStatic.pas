unit u_TileDownloaderListStatic;

interface

uses
  Classes,
  i_TileDownloader,
  i_TileDownloaderList;

type
  TTileDownloaderListStatic = class(TInterfacedObject, ITileDownloaderListStatic)
  private
    FList: array of ITileDownloader;
    FCount: Integer;
  protected
    function GetCount: Integer;
    function GetItem(AIndex: Integer): ITileDownloader;
  public
    constructor Create(
      AList: array of ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

{ TTileDownloaderListStatic }

constructor TTileDownloaderListStatic.Create(
  AList: array of ITileDownloader
);
var
  i: Integer;
begin
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
