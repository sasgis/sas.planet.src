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
  i_DownloadChecker,
  i_AntiBan,
  i_TileInfoBasic,
  u_TileStorageAbstract;

type
  TDownloadCheckerStuped = class(TInterfacedObject, IDownloadChecker)
  private
    FIgnoreMIMEType: Boolean;
    FExpectedMIMETypes: string;
    FDefaultMIMEType: string;
    FCheckTileSize: Boolean;
    FExistsTileInfo: ITileInfoBasic;
    FStorage: TTileStorageAbstract;
    FAntiBan: IAntiBan;
    function PrepareOldTileInfo(ARequest: IDownloadRequest): ITileInfoBasic;
    function CheckOldTileSize(ARequest: IDownloadRequest; ANewSize: Cardinal): Boolean;
  protected
    function BeforeRequest(
      AResultFactory: IDownloadResultFactory;
      ARequest: IDownloadRequest
    ): IDownloadResult;
    function AfterResponse(
      AResultFactory: IDownloadResultFactory;
      ARequest: IDownloadRequest;
      var AStatusCode: Cardinal;
      var AContentType: string;
      var AResponseHead: string
    ): IDownloadResult;
    function AfterReciveData(
      AResultFactory: IDownloadResultFactory;
      ARequest: IDownloadRequest;
      const ARecivedSize: Integer;
      const ARecivedBuffer: Pointer;
      var AStatusCode: Cardinal;
      var AResponseHead: string
    ): IDownloadResult;
  public
    constructor Create(
      AAntiBan: IAntiBan;
      AIgnoreMIMEType: Boolean;
      AExpectedMIMETypes: string;
      ADefaultMIMEType: string;
      ACheckTileSize: Boolean;
      AStorage: TTileStorageAbstract
    );
  end;

implementation

uses
  SysUtils,
  i_TileDownloadRequest,
  u_TileRequestBuilderHelpers;

{ TDownloadCheckerStuped }

constructor TDownloadCheckerStuped.Create(
  AAntiBan: IAntiBan;
  AIgnoreMIMEType: Boolean;
  AExpectedMIMETypes, ADefaultMIMEType: string;
  ACheckTileSize: Boolean;
  AStorage: TTileStorageAbstract
);
begin
  FAntiBan := AAntiBan;
  FIgnoreMIMEType := AIgnoreMIMEType;
  FExpectedMIMETypes := AExpectedMIMETypes;
  FDefaultMIMEType := ADefaultMIMEType;
  FCheckTileSize := ACheckTileSize;
  FStorage := AStorage;
end;

function TDownloadCheckerStuped.PrepareOldTileInfo(
  ARequest: IDownloadRequest): ITileInfoBasic;
var
  VTileDownloadRequest: ITileDownloadRequest;
begin
  if Supports(ARequest, ITileDownloadRequest, VTileDownloadRequest) then begin
    Result := FStorage.GetTileInfo(VTileDownloadRequest.Source.Tile, VTileDownloadRequest.Source.Zoom, VTileDownloadRequest.Source.VersionInfo);
  end;
end;

function TDownloadCheckerStuped.CheckOldTileSize(ARequest: IDownloadRequest;
  ANewSize: Cardinal): Boolean;
var
  VOldTileSize: Cardinal;
begin
  Result := False;
  if FExistsTileInfo = nil then begin
    FExistsTileInfo := PrepareOldTileInfo(ARequest);
  end;
  if FExistsTileInfo <> nil then begin
    VOldTileSize := FExistsTileInfo.GetSize;
    if ANewSize = VOldTileSize then begin
      Result := True;
    end;
  end;
end;

function TDownloadCheckerStuped.BeforeRequest(
  AResultFactory: IDownloadResultFactory;
  ARequest: IDownloadRequest
): IDownloadResult;
begin
  if FAntiBan <> nil then begin
    FAntiBan.PreDownload(ARequest);
  end;
end;

function TDownloadCheckerStuped.AfterResponse(
  AResultFactory: IDownloadResultFactory;
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
      Result := AResultFactory.BuildBadContentType(ARequest, AContentType, AResponseHead);
      Exit;
    end;
  end;
  if FCheckTileSize then begin
    VContentLenAsStr := GetHeaderValue(AResponseHead, 'Content-Length');
    if VContentLenAsStr <> '' then begin
      if TryStrToInt64(VContentLenAsStr, VContentLen) then begin
        if CheckOldTileSize(ARequest, VContentLen) then begin
          Result := AResultFactory.BuildNotNecessary(ARequest, 'Одинаковый размер тайла', AResponseHead);
          Exit;
        end;
      end;
    end;
  end;
end;

function TDownloadCheckerStuped.AfterReciveData(
  AResultFactory: IDownloadResultFactory;
  ARequest: IDownloadRequest;
  const ARecivedSize: Integer;
  const ARecivedBuffer: Pointer;
  var AStatusCode: Cardinal;
  var AResponseHead: string
): IDownloadResult;
begin
  if FCheckTileSize then begin
    if CheckOldTileSize(ARequest, ARecivedSize) then begin
      Result := AResultFactory.BuildNotNecessary(ARequest, 'Одинаковый размер тайла', AResponseHead);
      Exit;
    end;
  end;
  if FAntiBan <> nil then begin
    Result := FAntiBan.PostCheckDownload(
      AResultFactory,
      ARequest,
      ARecivedSize,
      ARecivedBuffer,
      AStatusCode,
      AResponseHead
    );
  end;
end;

end.
