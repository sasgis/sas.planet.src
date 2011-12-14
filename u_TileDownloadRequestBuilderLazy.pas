unit u_TileDownloadRequestBuilderLazy;

interface

uses
  i_OperationNotifier,
  i_TileRequest,
  i_LastResponseInfo,
  i_TileDownloadRequest,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderFactory;

type
  TTileDownloadRequestBuilderLazy = class(TInterfacedObject, ITileDownloadRequestBuilder)
  private
    FFactory: ITileDownloadRequestBuilderFactory;
    FBuilder: ITileDownloadRequestBuilder;
  protected
    function BuildRequest(
      ASource: ITileRequest;
      ALastResponseInfo: ILastResponseInfo;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    ): ITileDownloadRequest;
  public
    constructor Create(AFactory: ITileDownloadRequestBuilderFactory);
  end;

implementation

{ TTileDownloadRequestBuilderLazy }

constructor TTileDownloadRequestBuilderLazy.Create(
  AFactory: ITileDownloadRequestBuilderFactory);
begin
  FFactory := AFactory;
end;

function TTileDownloadRequestBuilderLazy.BuildRequest(
  ASource: ITileRequest;
  ALastResponseInfo: ILastResponseInfo;
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
): ITileDownloadRequest;
begin
  Result := nil;
  if (ACancelNotifier <> nil) and (not ACancelNotifier.IsOperationCanceled(AOperationID)) then begin
    if FFactory.State.GetStatic.Enabled then begin
      if FBuilder = nil then begin
        FBuilder := FFactory.BuildRequestBuilder;
      end;
      if FBuilder <> nil then begin
        Result := FBuilder.BuildRequest(ASource, ALastResponseInfo, ACancelNotifier, AOperationID);
      end;
    end;
  end;
end;

end.
