unit u_DownloadResultFactoryProvider;

interface

uses
  Types,
  i_DownloadResultFactory,
  i_DownloadResultFactoryProvider,
  i_DownloadRequest,
  i_DownloadResultTextProvider;

type
  TDownloadResultFactoryProvider = class(TInterfacedObject, IDownloadResultFactoryProvider)
  private
    FTextProvider: IDownloadResultTextProvider;
  protected
    function BuildFactory(
      ARequest: IDownloadRequest
    ): IDownloadResultFactory;
  public
    constructor Create(
      ATextProvider: IDownloadResultTextProvider
    );
  end;

implementation

uses
  u_DownloadResultFactoryTileDownload;

{ TDownloadResultFactoryProvider }

constructor TDownloadResultFactoryProvider.Create(
  ATextProvider: IDownloadResultTextProvider
);
begin
  FTextProvider := ATextProvider;
end;

function TDownloadResultFactoryProvider.BuildFactory(
  ARequest: IDownloadRequest
): IDownloadResultFactory;
begin
  Result :=
    TDownloadResultFactory.Create(
      FTextProvider,
      ARequest
    );
end;

end.
