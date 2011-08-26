unit i_DownloadResultFactoryProvider;

interface

uses
  Types,
  i_DownloadRequest,
  i_DownloadResultFactory;

type
  IDownloadResultFactoryProvider = interface
    ['{6509A2DE-8260-474F-A9E0-E23E2D21D448}']
    function BuildFactory(
      ARequest: IDownloadRequest
    ): IDownloadResultFactory;
  end;


implementation

end.
