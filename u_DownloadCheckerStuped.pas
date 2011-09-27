{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_DownloadCheckerStuped;

interface

uses
  Classes,
  i_DownloadRequest,
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
      ARequest: IDownloadRequest
    ): IDownloadResult;
    function AfterResponse(
      ARequest: IDownloadRequest;
      var AStatusCode: Cardinal;
      var AContentType: string;
      var AResponseHead: string
    ): IDownloadResult;
    function AfterReciveData(
      ARequest: IDownloadRequest;
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
  ARequest: IDownloadRequest
): IDownloadResult;
begin
end;

function TDownloadCheckerStuped.AfterResponse(
  ARequest: IDownloadRequest;
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
      Result := FResultFactory.BuildBadContentType(ARequest, AContentType, AResponseHead);
      Exit;
    end;
  end;
  if FCheckTileSize then begin
    VContentLenAsStr := GetHeaderValue(AResponseHead, 'Content-Length');
    if VContentLenAsStr <> '' then begin
      if TryStrToInt64(VContentLenAsStr, VContentLen) then begin
        if VContentLen = FExistsFileSize then begin
          Result := FResultFactory.BuildNotNecessary(ARequest, 'Одинаковый размер тайла', AResponseHead);
          Exit;
        end;
      end;
    end;
  end;
end;

function TDownloadCheckerStuped.AfterReciveData(
  ARequest: IDownloadRequest;
  const ARecivedSize: Integer;
  const ARecivedBuffer: Pointer;
  var AStatusCode: Cardinal;
  var AResponseHead: string
): IDownloadResult;
begin
  if FCheckTileSize then begin
    if ARecivedSize = FExistsFileSize then begin
      Result := FResultFactory.BuildNotNecessary(ARequest, 'Одинаковый размер тайла', AResponseHead);
      Exit;
    end;
  end;
end;

end.
