unit u_TileDownloadRequestBuilderLazy;

interface

uses
  SyncObjs,
  i_OperationNotifier,
  i_TileRequest,
  i_Downloader,
  i_LastResponseInfo,
  i_TileDownloadRequest,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderFactory;

type
  TTileDownloadRequestBuilderLazy = class(TInterfacedObject, ITileDownloadRequestBuilder)
  private
    FFactory: ITileDownloadRequestBuilderFactory;
    FDownloader: IDownloader;
    FBuilder: ITileDownloadRequestBuilder;
    FBuilderCS: TCriticalSection;
  protected
    function BuildRequest(
      ASource: ITileRequest;
      ALastResponseInfo: ILastResponseInfo;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    ): ITileDownloadRequest;
  public
    constructor Create(
      ADownloader: IDownloader;
      AFactory: ITileDownloadRequestBuilderFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TTileDownloadRequestBuilderLazy }

constructor TTileDownloadRequestBuilderLazy.Create(
  ADownloader: IDownloader;
  AFactory: ITileDownloadRequestBuilderFactory
);
begin
  FBuilderCS := TCriticalSection.Create;
  FDownloader := ADownloader;
  FFactory := AFactory;
end;

destructor TTileDownloadRequestBuilderLazy.Destroy;
begin
  FreeAndNil(FBuilderCS);
  inherited;
end;

function TTileDownloadRequestBuilderLazy.BuildRequest(
  ASource: ITileRequest;
  ALastResponseInfo: ILastResponseInfo;
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
): ITileDownloadRequest;
var
  VBuilder: ITileDownloadRequestBuilder;
begin
  Result := nil;
  if (ACancelNotifier <> nil) and (not ACancelNotifier.IsOperationCanceled(AOperationID)) then begin
    if FFactory.State.GetStatic.Enabled then begin
      FBuilderCS.Acquire;
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
        FBuilderCS.Release;
      end;
      if VBuilder <> nil then begin
        Result := VBuilder.BuildRequest(ASource, ALastResponseInfo, ACancelNotifier, AOperationID);
      end;
    end;
  end;
end;

end.
