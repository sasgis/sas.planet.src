{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
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
