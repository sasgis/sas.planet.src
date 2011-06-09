unit u_DownloadResultFactoryTileDownload;

interface

uses
  Types,
  i_DownloadResult,
  i_TileDownloadResult,
  u_MapType,
  i_DownloadResultFactory;

type
  TDownloadResultFactoryTileDownload = class(TInterfacedObject, IDownloadResultFactory)
  private
    FUrl: string;
    FRequestHead: string;
    FTileInfo: ITileInfo;
  protected
    function BuildOk(
      AStatusCode: Cardinal;
      ARawResponseHeader: string;
      AContentType: string;
      ASize: Integer;
      ABuffer: Pointer
    ): IDownloadResultOk;
    function BuildUnexpectedProxyAuth: IDownloadResultProxyError;
    function BuildBadProxyAuth: IDownloadResultProxyError;
    function BuildNoConnetctToServerByErrorCode(AErrorCode: DWORD): IDownloadResultNoConnetctToServer;
    function BuildLoadErrorByStatusCode(AStatusCode: DWORD): IDownloadResultError;
    function BuildLoadErrorByUnknownStatusCode(AStatusCode: DWORD): IDownloadResultError;
    function BuildLoadErrorByErrorCode(AErrorCode: DWORD): IDownloadResultError;
    function BuildBadContentType(AContentType: string): IDownloadResultBadContentType;
    function BuildBanned: IDownloadResultBanned;
    function BuildDataNotExists(AReasonText: string): IDownloadResultDataNotExists;
    function BuildDataNotExistsByStatusCode(AStatusCode: DWORD): IDownloadResultDataNotExists;
    function BuildDataNotExistsZeroSize: IDownloadResultDataNotExists;
    function BuildNotNecessary(AReasonText: string): IDownloadResultNotNecessary;
  public
    constructor Create(
      AZoom: Byte;
      AXY: TPoint;
      AMapType: TMapType;
      AUrl: string;
      ARequestHead: string
    );
  end;

implementation

uses
  u_TileDownloadResult;

{ TDownloadResultFactorySimpleDownload }

constructor TDownloadResultFactoryTileDownload.Create(
  AZoom: Byte;
  AXY: TPoint;
  AMapType: TMapType;
  AUrl, ARequestHead: string
);
begin
  FTileInfo := TTileInfo.Create(AZoom, AXY, AMapType);
  FUrl := AUrl;
  FRequestHead := ARequestHead;
end;

function TDownloadResultFactoryTileDownload.BuildBadContentType(
  AContentType: string): IDownloadResultBadContentType;
begin
  Result := TTileDownloadResultBadContentType.Create(FTileInfo, FUrl, FRequestHead, AContentType);
end;

function TDownloadResultFactoryTileDownload.BuildBadProxyAuth: IDownloadResultProxyError;
begin
  Result := TTileDownloadResultBadProxyAuth.Create(FTileInfo, FUrl, FRequestHead);
end;

function TDownloadResultFactoryTileDownload.BuildBanned: IDownloadResultBanned;
begin
  Result := TTileDownloadResultBanned.Create(FTileInfo, FUrl, FRequestHead);
end;

function TDownloadResultFactoryTileDownload.BuildDataNotExists(
  AReasonText: string): IDownloadResultDataNotExists;
begin
  Result := TTileDownloadResultDataNotExists.Create(FTileInfo, FUrl, FRequestHead, AReasonText);
end;

function TDownloadResultFactoryTileDownload.BuildDataNotExistsByStatusCode(
  AStatusCode: DWORD): IDownloadResultDataNotExists;
begin
  Result := TTileDownloadResultDataNotExistsByStatusCode.Create(FTileInfo, FUrl, FRequestHead, AStatusCode);
end;

function TDownloadResultFactoryTileDownload.BuildDataNotExistsZeroSize: IDownloadResultDataNotExists;
begin
  Result := TTileDownloadResultDataNotExistsZeroSize.Create(FTileInfo, FUrl, FRequestHead);
end;

function TDownloadResultFactoryTileDownload.BuildLoadErrorByErrorCode(
  AErrorCode: DWORD): IDownloadResultError;
begin
  Result := TTileDownloadResultLoadErrorByErrorCode.Create(FTileInfo, FUrl, FRequestHead, AErrorCode);
end;

function TDownloadResultFactoryTileDownload.BuildLoadErrorByStatusCode(
  AStatusCode: DWORD): IDownloadResultError;
begin
  Result := TTileDownloadResultLoadErrorByStatusCode.Create(FTileInfo, FUrl, FRequestHead, AStatusCode);
end;

function TDownloadResultFactoryTileDownload.BuildLoadErrorByUnknownStatusCode(
  AStatusCode: DWORD): IDownloadResultError;
begin
  Result := TTileDownloadResultLoadErrorByUnknownStatusCode.Create(FTileInfo, FUrl, FRequestHead, AStatusCode);
end;

function TDownloadResultFactoryTileDownload.BuildNoConnetctToServerByErrorCode(
  AErrorCode: DWORD): IDownloadResultNoConnetctToServer;
begin
  Result := TTileDownloadResultNoConnetctToServerByErrorCode.Create(FTileInfo, FUrl, FRequestHead, AErrorCode);
end;

function TDownloadResultFactoryTileDownload.BuildNotNecessary(
  AReasonText: string): IDownloadResultNotNecessary;
begin
  Result := TTileDownloadResultNotNecessary.Create(FTileInfo, FUrl, FRequestHead, AReasonText);
end;

function TDownloadResultFactoryTileDownload.BuildOk(
  AStatusCode: Cardinal;
  ARawResponseHeader, AContentType: string;
  ASize: Integer;
  ABuffer: Pointer
): IDownloadResultOk;
begin
  Result := TTileDownloadResultOk.Create(FTileInfo, FUrl, FRequestHead, AStatusCode, ARawResponseHeader, AContentType, ASize, ABuffer);
end;

function TDownloadResultFactoryTileDownload.BuildUnexpectedProxyAuth: IDownloadResultProxyError;
begin
  Result := TTileDownloadResultUnexpectedProxyAuth.Create(FTileInfo, FUrl, FRequestHead);
end;

end.

