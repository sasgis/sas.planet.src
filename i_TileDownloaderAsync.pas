unit i_TileDownloaderAsync;

interface

uses
  i_OperationNotifier,
  i_TileRequest;

type
  ITileDownloader = interface
    ['{79AB7B90-1F22-4B2E-B14A-BBAD3F94E26C}']
    procedure Download(
      ATileRequest: ITileRequest;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    );
  end;

implementation

end.
