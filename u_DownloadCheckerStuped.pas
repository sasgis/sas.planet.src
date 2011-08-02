unit u_DownloadCheckerStuped;

interface

uses
  Classes,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_DownloadChecker;

type
  TDownloadCheckerStuped = class(TInterfacedObject, IDownloadChecker)
  private
    FResultFactory: IDownloadResultFactory;
    FIgnoreMIMEType: Boolean;
    FExpectedMIMETypes: string;
    FDefaultMIMEType: string;
    FCheckTileSize: Boolean;
    FExistsFileSize: Integer;
  protected
    function BeforeRequest(
      const AUrl:  string;
      const ARequestHead: string
    ): IDownloadResult;
    function AfterResponse(
      var AStatusCode: Cardinal;
      var AContentType: string;
      var AResponseHead: string
    ): IDownloadResult;
    function AfterReciveData(
      const ARecivedSize: Integer;
      const ARecivedBuffer: Pointer;
      var AStatusCode: Cardinal;
      var AResponseHead: string
    ): IDownloadResult;
  public
    constructor Create(
      AResultFactory: IDownloadResultFactory;
      AIgnoreMIMEType: Boolean;
      AExpectedMIMETypes: string;
      ADefaultMIMEType: string;
      ACheckTileSize: Boolean;
      AExistsFileSize: Integer
    );
  end;

implementation

uses
  SysUtils,
  u_TileRequestBuilderHelpers;

{ TDownloadCheckerStuped }

constructor TDownloadCheckerStuped.Create(
  AResultFactory: IDownloadResultFactory;
  AIgnoreMIMEType: Boolean;
  AExpectedMIMETypes, ADefaultMIMEType: string;
  ACheckTileSize: Boolean;
  AExistsFileSize: Integer
);
begin
  FResultFactory := AResultFactory;
  FIgnoreMIMEType := AIgnoreMIMEType;
  FExpectedMIMETypes := AExpectedMIMETypes;
  FDefaultMIMEType := ADefaultMIMEType;
  FCheckTileSize := ACheckTileSize;
  FExistsFileSize := AExistsFileSize;
end;

function TDownloadCheckerStuped.BeforeRequest(
  const AUrl, ARequestHead: string
): IDownloadResult;
begin
end;

function TDownloadCheckerStuped.AfterResponse(
  var AStatusCode: Cardinal;
  var AContentType: string;
  var AResponseHead: string
): IDownloadResult;
var
  VContentLenAsStr: string;
  VContentLen: Int64;
begin
  if FIgnoreMIMEType then begin
    AContentType := FDefaultMIMEType;
  end else begin
    if (AContentType = '') then begin
      AContentType := FDefaultMIMEType;
    end else if (Pos(AContentType, FExpectedMIMETypes) <= 0) then begin
      Result := FResultFactory.BuildBadContentType(AContentType, AResponseHead);
      Exit;
    end;
  end;
  if FCheckTileSize then begin
    VContentLenAsStr := GetHeaderValue(AResponseHead, 'Content-Length');
    if VContentLenAsStr <> '' then begin
      if TryStrToInt64(VContentLenAsStr, VContentLen) then begin
        if VContentLen = FExistsFileSize then begin
          Result := FResultFactory.BuildNotNecessary('Одинаковый размер тайла', AResponseHead);
          Exit;
        end;
      end;
    end;
  end;
end;

function TDownloadCheckerStuped.AfterReciveData(
  const ARecivedSize: Integer;
  const ARecivedBuffer: Pointer;
  var AStatusCode: Cardinal;
  var AResponseHead: string
): IDownloadResult;
begin
  if FCheckTileSize then begin
    if ARecivedSize = FExistsFileSize then begin
      Result := FResultFactory.BuildNotNecessary('Одинаковый размер тайла', AResponseHead);
      Exit;
    end;
  end;
end;

end.
