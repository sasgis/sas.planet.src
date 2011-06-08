unit u_DownloadCheckerStuped;

interface

uses
  Classes,
  i_DownloadChecker;

type
  TDownloadCheckerStuped = class(TInterfacedObject, IDownloadChecker)
  private
    FIgnoreMIMEType: Boolean;
    FExpectedMIMETypes: string;
    FDefaultMIMEType: string;
    FCheckTileSize: Boolean;
    FExistsFileSize: Cardinal;
  protected
    procedure BeforeRequest(var AUrl:  string; var ARequestHead: string);
    procedure AfterResponce(
      var AStatusCode: Cardinal;
      var AContentType: string;
      var AResponseHead: string
    );
    procedure AfterReciveData(
      ARecivedData: TMemoryStream;
      var AStatusCode: Cardinal;
      var AResponseHead: string
    );
  public
    constructor Create(
      AIgnoreMIMEType: Boolean;
      AExpectedMIMETypes: string;
      ADefaultMIMEType: string;
      ACheckTileSize: Boolean;
      AExistsFileSize: Cardinal
    );
  end;

implementation

uses
  SysUtils,
  u_UrlGeneratorHelpers,
  u_DownloadExceptions;

{ TDownloadCheckerStuped }

constructor TDownloadCheckerStuped.Create(
  AIgnoreMIMEType: Boolean;
  AExpectedMIMETypes, ADefaultMIMEType: string;
  ACheckTileSize: Boolean;
  AExistsFileSize: Cardinal
);
begin
  FIgnoreMIMEType := AIgnoreMIMEType;
  FExpectedMIMETypes := AExpectedMIMETypes;
  FDefaultMIMEType := ADefaultMIMEType;
  FCheckTileSize := ACheckTileSize;
  FExistsFileSize := AExistsFileSize;
end;

procedure TDownloadCheckerStuped.BeforeRequest(
  var AUrl, ARequestHead: string
);
begin
// Делаем ничего
end;

procedure TDownloadCheckerStuped.AfterResponce(
  var AStatusCode: Cardinal;
  var AContentType: string;
  var AResponseHead: string
);
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
      raise EMimeTypeError.CreateFmt('Неожиданный тип %s', [AContentType]);
    end;
  end;
  if FCheckTileSize then begin
    VContentLenAsStr := GetHeaderValue(AResponseHead, 'Conternt Length');
    if VContentLenAsStr <> '' then begin
      if TryStrToInt64(VContentLenAsStr, VContentLen) then begin
        if VContentLen = FExistsFileSize then begin
          raise ESameTileSize.Create('Одинаковый размер тайла');
        end;
      end;
    end;
  end;
end;

procedure TDownloadCheckerStuped.AfterReciveData(
  ARecivedData: TMemoryStream;
  var AStatusCode: Cardinal;
  var AResponseHead: string
);
begin
  if FCheckTileSize then begin
    if ARecivedData.Size = FExistsFileSize then begin
      raise ESameTileSize.Create('Одинаковый размер тайла');
    end;
  end;
end;

end.
