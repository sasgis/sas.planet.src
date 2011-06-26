unit u_TileDownloadResultFactoryProvider;

interface

uses
  Types,
  i_DownloadResultFactory,
  i_TileDownloadResultFactoryProvider,
  i_DownloadResultTextProvider,
  u_MapType;

type
  TTileDownloadResultFactoryProvider = class(TInterfacedObject, ITileDownloadResultFactoryProvider)
  private
    FMapType: TMapType;
    FTextProvider: IDownloadResultTextProvider;
  protected
    function BuildFactory(
      AZoom: Byte;
      AXY: TPoint;
      AUrl: string;
      ARequestHead: string
    ): IDownloadResultFactory;
  public
    constructor Create(
      AMapType: TMapType;
      ATextProvider: IDownloadResultTextProvider
    );
  end;

implementation

uses
  u_DownloadResultFactoryTileDownload;

{ TTileDownloadResultFactoryProvider }

constructor TTileDownloadResultFactoryProvider.Create(AMapType: TMapType;
  ATextProvider: IDownloadResultTextProvider);
begin
  FMapType := AMapType;
  FTextProvider := ATextProvider;
end;

function TTileDownloadResultFactoryProvider.BuildFactory(AZoom: Byte;
  AXY: TPoint; AUrl, ARequestHead: string): IDownloadResultFactory;
begin
  Result :=
    TDownloadResultFactoryTileDownload.Create(
      FTextProvider,
      AZoom,
      AXY,
      FMapType,
      AUrl,
      ARequestHead
    );
end;

end.
