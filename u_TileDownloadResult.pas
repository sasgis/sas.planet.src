unit u_TileDownloadResult;

interface

uses
  Types,
  Classes,
  i_DownloadResult,
  u_MapType,
  i_TileDownloadResult,
  u_DownloadResult;

type
  TTileInfo = class(TInterfacedObject, ITileInfo)
  private
    FZoom: Byte;
    FXY: TPoint;
    FMapType: TMapType;
  protected
    function GetZoom: Byte;
    function GetXY: TPoint;
    function GetMapType: TMapType;
  public
    constructor Create(
      AZoom: Byte;
      AXY: TPoint;
      AMapType: TMapType
    );
  end;

  TTileDownloadResultCanceled = class(TDownloadResultCanceled, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string
    );
  end;

  TTileDownloadResultOk = class(TDownloadResultOk, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      AStatusCode: Cardinal;
      ARawResponseHeader: string;
      AContentType: string;
      ASize: Integer;
      ABuffer: Pointer
    );
  end;

  TTileDownloadResultProxyError = class(TDownloadResultProxyError, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      AErrorText: string
    );
  end;

  TTileDownloadResultNoConnetctToServerByErrorCode = class(TDownloadResultNoConnetctToServerByErrorCode, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      AErrorText: string;
      AErrorCode: DWORD
    );
  end;

  TTileDownloadResultLoadErrorByStatusCode = class(TDownloadResultLoadErrorByStatusCode, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      AErrorText: string;
      AStatusCode: DWORD
    );
  end;

  TTileDownloadResultLoadErrorByUnknownStatusCode = class(TDownloadResultLoadErrorByUnknownStatusCode, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      AErrorText: string;
      AStatusCode: DWORD
    );
  end;

  TTileDownloadResultLoadErrorByErrorCode = class(TDownloadResultLoadErrorByErrorCode, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      AErrorText: string;
      AErrorCode: DWORD
    );
  end;

  TTileDownloadResultBanned = class(TDownloadResultBanned, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      ARawResponseHeader: string;
      AErrorText: string
    );
  end;

  TTileDownloadResultBadContentType = class(TDownloadResultBadContentType, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      AContentType: string;
      ARawResponseHeader: string;
      AErrorText: string
    );
  end;

  TTileDownloadResultDataNotExists = class(TDownloadResultDataNotExists, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      AReasonText: string;
      ARawResponseHeader: string
    );
  end;

  TTileDownloadResultDataNotExistsByStatusCode = class(TDownloadResultDataNotExistsByStatusCode, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      ARawResponseHeader: string;
      AErrorText: string;
      AStatusCode: DWORD
    );
  end;

  TTileDownloadResultDataNotExistsZeroSize = class(TDownloadResultDataNotExistsZeroSize, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      ARawResponseHeader: string;
      AErrorText: string
    );
  end;

  TTileDownloadResultNotNecessary = class(TDownloadResultNotNecessary, ITileInfo)
  private
    FTileInfo: ITileInfo;
  protected
    property TileInfo: ITileInfo read FTileInfo implements ITileInfo;
  public
    constructor Create(
      ATileInfo: ITileInfo;
      AUrl: string;
      ARequestHead: string;
      AReasonText: string;
      ARawResponseHeader: string
    );
  end;


implementation

{ TTileDownloadResult }

constructor TTileInfo.Create(AZoom: Byte; AXY: TPoint;
  AMapType: TMapType);
begin
  FZoom := AZoom;
  FXY := AXY;
  FMapType := AMapType;
end;

function TTileInfo.GetMapType: TMapType;
begin
  Result := FMapType;
end;

function TTileInfo.GetXY: TPoint;
begin
  Result := FXY;
end;

function TTileInfo.GetZoom: Byte;
begin
  Result := FZoom;
end;

{ TTileDownloadResultOk }

constructor TTileDownloadResultOk.Create(
  ATileInfo: ITileInfo;
  AUrl, ARequestHead: string; AStatusCode: Cardinal;
  ARawResponseHeader, AContentType: string;
  ASize: Integer;
  ABuffer: Pointer
);
begin
  inherited Create(AUrl, ARequestHead, AStatusCode, ARawResponseHeader, AContentType, ASize, ABuffer);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultProxyError }

constructor TTileDownloadResultProxyError.Create(ATileInfo: ITileInfo;
  AUrl, ARequestHead, AErrorText: string);
begin
  inherited Create(AUrl, ARequestHead, AErrorText);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultNoConnetctToServerByErrorCode }

constructor TTileDownloadResultNoConnetctToServerByErrorCode.Create(
  ATileInfo: ITileInfo; AUrl, ARequestHead, AErrorText: string; AErrorCode: DWORD);
begin
  inherited Create(AUrl, ARequestHead, AErrorText, AErrorCode);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultLoadErrorByStatusCode }

constructor TTileDownloadResultLoadErrorByStatusCode.Create(
  ATileInfo: ITileInfo; AUrl, ARequestHead, AErrorText: string; AStatusCode: DWORD);
begin
  inherited Create(AUrl, ARequestHead, AErrorText, AStatusCode);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultLoadErrorByUnknownStatusCode }

constructor TTileDownloadResultLoadErrorByUnknownStatusCode.Create(
  ATileInfo: ITileInfo; AUrl, ARequestHead, AErrorText: string; AStatusCode: DWORD);
begin
  inherited Create(AUrl, ARequestHead, AErrorText, AStatusCode);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultLoadErrorByErrorCode }

constructor TTileDownloadResultLoadErrorByErrorCode.Create(ATileInfo: ITileInfo;
  AUrl, ARequestHead, AErrorText: string; AErrorCode: DWORD);
begin
  inherited Create(AUrl, ARequestHead, AErrorText, AErrorCode);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultBanned }

constructor TTileDownloadResultBanned.Create(ATileInfo: ITileInfo; AUrl,
  ARequestHead, ARawResponseHeader, AErrorText: string);
begin
  inherited Create(AUrl, ARequestHead, ARawResponseHeader, AErrorText);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultBadContentType }

constructor TTileDownloadResultBadContentType.Create(ATileInfo: ITileInfo; AUrl,
  ARequestHead, AContentType, ARawResponseHeader, AErrorText: string);
begin
  inherited Create(AUrl, ARequestHead, AContentType, ARawResponseHeader, AErrorText);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultDataNotExistsByStatusCode }

constructor TTileDownloadResultDataNotExistsByStatusCode.Create(
  ATileInfo: ITileInfo; AUrl, ARequestHead, ARawResponseHeader, AErrorText: string; AStatusCode: DWORD);
begin
  inherited Create(AUrl, ARequestHead, ARawResponseHeader, AErrorText, AStatusCode);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultDataNotExistsZeroSize }

constructor TTileDownloadResultDataNotExistsZeroSize.Create(
  ATileInfo: ITileInfo; AUrl, ARequestHead, ARawResponseHeader, AErrorText: string);
begin
  inherited Create(AUrl, ARequestHead, ARawResponseHeader, AErrorText);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultNotNecessary }

constructor TTileDownloadResultNotNecessary.Create(ATileInfo: ITileInfo; AUrl,
  ARequestHead, AReasonText, ARawResponseHeader: string);
begin
  inherited Create(AUrl, ARequestHead, AReasonText, ARawResponseHeader);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultDataNotExists }

constructor TTileDownloadResultDataNotExists.Create(ATileInfo: ITileInfo; AUrl,
  ARequestHead, AReasonText, ARawResponseHeader: string);
begin
  inherited Create(AUrl, ARequestHead, AReasonText, ARawResponseHeader);
  FTileInfo := ATileInfo;
end;

{ TTileDownloadResultCanceled }

constructor TTileDownloadResultCanceled.Create(ATileInfo: ITileInfo; AUrl,
  ARequestHead: string);
begin
  inherited Create(AUrl, ARequestHead);
  FTileInfo := ATileInfo;
end;

end.
