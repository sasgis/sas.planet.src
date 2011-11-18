unit u_TileRequest;

interface

uses
  Types,
  i_JclNotify,
  i_ZmpInfo,
  i_TileRequest,
  i_TileDownloadChecker,
  i_MapVersionInfo;

type
  TTileRequest = class(TInterfacedObject, ITileRequest, ITileRequestWithChecker)
  private
    FZmp: IZmpInfo;
    FTile: TPoint;
    FZoom: Byte;
    FVersionInfo: IMapVersionInfo;
    FChecker: ITileDownloadChecker;
    FStartNotifier: IJclNotifier;
    FFinishNotifier: IJclNotifier;
  protected
    function GetZmp: IZmpInfo;
    function GetTile: TPoint;
    function GetZoom: Byte;
    function GetVersionInfo: IMapVersionInfo;
    function GetStartNotifier: IJclNotifier;
    function GetFinishNotifier: IJclNotifier;
  protected
    function GetChecker: ITileDownloadChecker;
  public
    constructor Create(
      AZmp: IZmpInfo;
      ATile: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionInfo;
      AChecker: ITileDownloadChecker
    );
  end;

  TTileRequestWithSizeCheck = class(TTileRequest, ITileRequestWithSizeCheck);

implementation

uses
  u_JclNotify;

{ TTileRequest }

constructor TTileRequest.Create(
  AZmp: IZmpInfo;
  ATile: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo;
  AChecker: ITileDownloadChecker
);
begin
  FZmp := AZmp;
  FTile := ATile;
  FZoom := AZoom;
  FVersionInfo := AVersionInfo;
  FChecker := AChecker;
  FFinishNotifier := TJclBaseNotifier.Create;
  FStartNotifier := TJclBaseNotifier.Create;
end;

function TTileRequest.GetChecker: ITileDownloadChecker;
begin
  Result := FChecker;
end;

function TTileRequest.GetFinishNotifier: IJclNotifier;
begin
  Result := FFinishNotifier;
end;

function TTileRequest.GetStartNotifier: IJclNotifier;
begin
  Result := FStartNotifier;
end;

function TTileRequest.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileRequest.GetVersionInfo: IMapVersionInfo;
begin
  Result := FVersionInfo;
end;

function TTileRequest.GetZmp: IZmpInfo;
begin
  Result := FZmp;
end;

function TTileRequest.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
