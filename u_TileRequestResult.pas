unit u_TileRequestResult;

interface

uses
  i_TileRequest,
  i_DownloadRequest,
  i_DownloadResult,
  i_TileDownloadRequest,
  i_TileRequestResult;

type
  TTileRequestResult = class(TInterfacedObject, ITileRequestResult)
  private
    FRequest: ITileRequest;
  protected
    function GetRequest: ITileRequest;
  public
    constructor Create(
      ARequest: ITileRequest
    );
  end;

  TTileRequestResultCanceledBeforBuildDownloadRequest = class(TTileRequestResult, ITileRequestResultCanceled)
  end;

  TTileRequestResultCanceledAfterBuildDownloadRequest = class(TTileRequestResultCanceledBeforBuildDownloadRequest, ITileRequestResultWithDownloadRequest)
  private
    FDownloadRequest: ITileDownloadRequest;
  protected
    function GetDownloadRequest: ITileDownloadRequest;
  public
    constructor Create(
      ADownloadRequest: ITileDownloadRequest
    );
  end;

  TTileRequestResultCanceledAfterDownloadRequest = class(TTileRequestResultCanceledAfterBuildDownloadRequest, ITileRequestResultWithDownloadResult)
  private
    FDownloadResult: IDownloadResult;
  protected
    function GetDownloadResult: IDownloadResult;
  public
    constructor Create(
      ADownloadResult: IDownloadResult
    );
  end;

  TTileRequestResultOk = class(TTileRequestResult, ITileRequestResultOk, ITileRequestResultWithDownloadRequest, ITileRequestResultWithDownloadResult)
  private
    FDownloadRequest: ITileDownloadRequest;
    FDownloadResult: IDownloadResult;
  protected
    function GetDownloadRequest: ITileDownloadRequest;
  protected
    function GetDownloadResult: IDownloadResult;
  public
    constructor Create(
      ADownloadResult: IDownloadResult
    );
  end;

  TTileRequestResultErrorBeforBuildDownloadRequest = class(TTileRequestResult, ITileRequestResultError)
  end;

  TTileRequestResultErrorAfterBuildDownloadRequest = class(TTileRequestResultErrorBeforBuildDownloadRequest, ITileRequestResultWithDownloadRequest)
  private
    FDownloadRequest: ITileDownloadRequest;
  protected
    function GetDownloadRequest: ITileDownloadRequest;
  public
    constructor Create(
      ADownloadRequest: ITileDownloadRequest
    );
  end;

  TTileRequestResultErrorAfterDownloadRequest = class(TTileRequestResultErrorAfterBuildDownloadRequest, ITileRequestResultWithDownloadResult)
  private
    FDownloadResult: IDownloadResult;
  protected
    function GetDownloadResult: IDownloadResult;
  public
    constructor Create(
      ADownloadResult: IDownloadResult
    );
  end;



implementation

{ TTileRequestResult }

constructor TTileRequestResult.Create(ARequest: ITileRequest);
begin
  FRequest := ARequest;
end;

function TTileRequestResult.GetRequest: ITileRequest;
begin
  Result := FRequest;
end;

{ TTileRequestResultCanceledAfterBuildDownloadRequest }

constructor TTileRequestResultCanceledAfterBuildDownloadRequest.Create(
  ADownloadRequest: ITileDownloadRequest);
begin
  FDownloadRequest := ADownloadRequest;
  inherited Create(FDownloadRequest.Source);
end;

function TTileRequestResultCanceledAfterBuildDownloadRequest.GetDownloadRequest: ITileDownloadRequest;
begin
  Result := FDownloadRequest;
end;

{ TTileRequestResultCanceledAfterDownloadRequest }

constructor TTileRequestResultCanceledAfterDownloadRequest.Create(
  ADownloadResult: IDownloadResult
);
var
  VRequest: ITileDownloadRequest;
begin
  FDownloadResult := ADownloadResult;
  VRequest := FDownloadResult.Request as ITileDownloadRequest;
  inherited Create(VRequest);
end;

function TTileRequestResultCanceledAfterDownloadRequest.GetDownloadResult: IDownloadResult;
begin
  Result := FDownloadResult;
end;

{ TTileRequestResultOk }

constructor TTileRequestResultOk.Create(ADownloadResult: IDownloadResult);
begin
  FDownloadResult := ADownloadResult;
  FDownloadRequest := FDownloadResult.Request as ITileDownloadRequest;;
  inherited Create(FDownloadRequest.Source);
end;

function TTileRequestResultOk.GetDownloadRequest: ITileDownloadRequest;
begin
  Result := FDownloadRequest;
end;

function TTileRequestResultOk.GetDownloadResult: IDownloadResult;
begin
  Result := FDownloadResult;
end;

{ TTileRequestResultErrorAfterBuildDownloadRequest }

constructor TTileRequestResultErrorAfterBuildDownloadRequest.Create(
  ADownloadRequest: ITileDownloadRequest);
begin
  FDownloadRequest := ADownloadRequest;
  inherited Create(FDownloadRequest.Source);
end;

function TTileRequestResultErrorAfterBuildDownloadRequest.GetDownloadRequest: ITileDownloadRequest;
begin
  Result := FDownloadRequest;
end;

{ TTileRequestResultErrorAfterDownloadRequest }

constructor TTileRequestResultErrorAfterDownloadRequest.Create(
  ADownloadResult: IDownloadResult);
var
  VRequest: ITileDownloadRequest;
begin
  FDownloadResult := ADownloadResult;
  VRequest := FDownloadResult.Request as ITileDownloadRequest;
  inherited Create(VRequest);
end;

function TTileRequestResultErrorAfterDownloadRequest.GetDownloadResult: IDownloadResult;
begin
  Result := FDownloadResult;
end;

end.
