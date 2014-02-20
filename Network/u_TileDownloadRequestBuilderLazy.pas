unit u_TileDownloadRequestBuilderLazy;

interface

uses
  SysUtils,
  i_NotifierOperation,
  i_TileRequest,
  i_Downloader,
  i_LastResponseInfo,
  i_TileDownloadRequest,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderFactory,
  u_BaseInterfacedObject;

type
  TTileDownloadRequestBuilderLazy = class(TBaseInterfacedObject, ITileDownloadRequestBuilder)
  private
    FFactory: ITileDownloadRequestBuilderFactory;
    FDownloader: IDownloader;
    FBuilder: ITileDownloadRequestBuilder;
    FBuilderCS: IReadWriteSync;
  private
    function BuildRequest(
      const ASource: ITileRequest;
      const ALastResponseInfo: ILastResponseInfo;
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer
    ): ITileDownloadRequest;
  public
    constructor Create(
      const ADownloader: IDownloader;
      const AFactory: ITileDownloadRequestBuilderFactory
    );
  end;

implementation

uses
  u_Synchronizer;

{ TTileDownloadRequestBuilderLazy }

constructor TTileDownloadRequestBuilderLazy.Create(
  const ADownloader: IDownloader;
  const AFactory: ITileDownloadRequestBuilderFactory
);
begin
  inherited Create;
  FBuilderCS := MakeSyncRW_Var(Self, False);
  FDownloader := ADownloader;
  FFactory := AFactory;
end;

function TTileDownloadRequestBuilderLazy.BuildRequest(
  const ASource: ITileRequest;
  const ALastResponseInfo: ILastResponseInfo;
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer
): ITileDownloadRequest;
var
  VBuilder: ITileDownloadRequestBuilder;
begin
  Result := nil;
  if (ACancelNotifier <> nil) and (not ACancelNotifier.IsOperationCanceled(AOperationID)) then begin
    if FFactory.State.GetStatic.Enabled then begin
      // allow build
      FBuilderCS.BeginWrite;
      try
        VBuilder := FBuilder;
        if VBuilder = nil then begin
          if FFactory.State.GetStatic.Enabled then begin
            VBuilder := FFactory.BuildRequestBuilder(FDownloader);
            if VBuilder <> nil then begin
              FBuilder := VBuilder;
            end;
          end;
        end;
      finally
        FBuilderCS.EndWrite;
      end;

      if VBuilder <> nil then begin
        Result := VBuilder.BuildRequest(ASource, ALastResponseInfo, ACancelNotifier, AOperationID);
      end;
    end;
  end;
end;

end.
