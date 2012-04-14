unit i_TileDownloader;

interface

uses
  i_TileRequest;

type
  ITileDownloader = interface
    ['{79AB7B90-1F22-4B2E-B14A-BBAD3F94E26C}']
    procedure Download(
      const ATileRequest: ITileRequest
    );
  end;

implementation

end.
