unit u_TileDownloadRequestBuilderLazy;

interface

uses
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
      ALastResponseInfo: ILastResponseInfo
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

function TTileDownloadRequestBuilderLazy.BuildRequest(ASource: ITileRequest;
  ALastResponseInfo: ILastResponseInfo): ITileDownloadRequest;
begin
  Result := nil;
  if FFactory.State.GetStatic.Enabled then begin
    if FBuilder = nil then begin
      FBuilder := FFactory.BuildRequestBuilder;
    end;
    if FBuilder <> nil then begin
      Result := FBuilder.BuildRequest(ASource, ALastResponseInfo);
    end;
  end;
end;

end.
