unit u_TileRequest;

interface

uses
  Types,
  i_JclNotify,
  i_ZmpInfo,
  i_OperationNotifier,
  i_TileRequest,
  i_TileDownloadChecker,
  i_MapVersionInfo;

type
  TTileRequest = class(TInterfacedObject, ITileRequest, ITileRequestWithChecker)
  private
    FTile: TPoint;
    FZoom: Byte;
    FVersionInfo: IMapVersionInfo;
    FChecker: ITileDownloadChecker;
    FStartNotifier: IJclNotifier;
    FFinishNotifier: IJclNotifier;
    FCancelNotifier: IOperationNotifier;
    FOperationID: Integer;
  protected
    function GetTile: TPoint;
    function GetZoom: Byte;
    function GetVersionInfo: IMapVersionInfo;
    function GetStartNotifier: IJclNotifier;
    function GetFinishNotifier: IJclNotifier;
    function GetCancelNotifier: IOperationNotifier;
    function GetOperationID: Integer;
  protected
    function GetChecker: ITileDownloadChecker;
  public
    constructor Create(
      ATile: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionInfo;
      AChecker: ITileDownloadChecker;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    );
  end;

  TTileRequestWithSizeCheck = class(TTileRequest, ITileRequestWithSizeCheck);

implementation

uses
  u_JclNotify;

{ TTileRequest }

constructor TTileRequest.Create(
  ATile: TPoint;
  AZoom: Byte;
  AVersionInfo: IMapVersionInfo;
  AChecker: ITileDownloadChecker;
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
);
begin
  FTile := ATile;
  FZoom := AZoom;
  FVersionInfo := AVersionInfo;
  FChecker := AChecker;
  FFinishNotifier := TJclBaseNotifier.Create;
  FStartNotifier := TJclBaseNotifier.Create;
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
end;

function TTileRequest.GetCancelNotifier: IOperationNotifier;
begin
  Result := FCancelNotifier;
end;

function TTileRequest.GetChecker: ITileDownloadChecker;
begin
  Result := FChecker;
end;

function TTileRequest.GetFinishNotifier: IJclNotifier;
begin
  Result := FFinishNotifier;
end;

function TTileRequest.GetOperationID: Integer;
begin
  Result := FOperationID;
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

function TTileRequest.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
