unit i_TileDownloadResultFactoryProvider;

interface

uses
  Types,
  i_DownloadResultFactory;

type
  ITileDownloadResultFactoryProvider = interface
    ['{6509A2DE-8260-474F-A9E0-E23E2D21D448}']
    function BuildFactory(
      AZoom: Byte;
      AXY: TPoint;
      AUrl: string;
      ARequestHead: string
    ): IDownloadResultFactory;
  end;


implementation

end.
