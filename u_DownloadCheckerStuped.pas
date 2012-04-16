{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
  i_BinaryData,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_DownloadChecker,
  i_TileDownloaderConfig,
  i_AntiBan,
  i_TileInfoBasic,
  u_TileStorageAbstract;

type
  TDownloadCheckerStuped = class(TInterfacedObject, IDownloadChecker)
  private
    FTileDownloaderConfig: ITileDownloaderConfig;
    FStorage: TTileStorageAbstract;
    FAntiBan: IAntiBan;
    function PrepareOldTileInfo(const ARequest: IDownloadRequest): ITileInfoBasic;
    function CheckOldTileSize(const ARequest: IDownloadRequest; ANewSize: Cardinal): Boolean;
    function IsNeedCheckTileSize(const ARequest: IDownloadRequest): Boolean;
  protected
    function BeforeRequest(
      const AResultFactory: IDownloadResultFactory;
      const ARequest: IDownloadRequest
    ): IDownloadResult;
    function AfterReciveData(
      const AResultFactory: IDownloadResultFactory;
      const ARequest: IDownloadRequest;
      const ARecivedData: IBinaryData;
      var AStatusCode: Cardinal;
      var AContentType: string;
      var AResponseHead: string
    ): IDownloadResult;
  public
    constructor Create(
      const AAntiBan: IAntiBan;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      AStorage: TTileStorageAbstract
    );
  end;

implementation

uses
  SysUtils,
  i_TileRequest,
  i_TileDownloadRequest;

{ TDownloadCheckerStuped }

constructor TDownloadCheckerStuped.Create(
  const AAntiBan: IAntiBan;
  const ATileDownloaderConfig: ITileDownloaderConfig;
  AStorage: TTileStorageAbstract
);
begin
  inherited Create;
  FAntiBan := AAntiBan;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FStorage := AStorage;
end;

function TDownloadCheckerStuped.IsNeedCheckTileSize(
  const ARequest: IDownloadRequest
): Boolean;
var
  VTileDownloadRequest: ITileDownloadRequest;
begin
  Result := False;
  if Supports(ARequest, ITileDownloadRequest, VTileDownloadRequest) then begin
    Result := Supports(VTileDownloadRequest.Source, ITileRequestWithSizeCheck);
  end;
end;

function TDownloadCheckerStuped.PrepareOldTileInfo(
  const ARequest: IDownloadRequest
): ITileInfoBasic;
var
  VTileDownloadRequest: ITileDownloadRequest;
begin
  Result := nil;
  if Supports(ARequest, ITileDownloadRequest, VTileDownloadRequest) then begin
    Result := FStorage.GetTileInfo(VTileDownloadRequest.Source.Tile, VTileDownloadRequest.Source.Zoom, VTileDownloadRequest.Source.VersionInfo);
  end;
end;

function TDownloadCheckerStuped.CheckOldTileSize(
  const ARequest: IDownloadRequest;
  ANewSize: Cardinal
): Boolean;
var
  VOldTileSize: Cardinal;
  VExistsTileInfo: ITileInfoBasic;
begin
  Result := False;
  VExistsTileInfo := PrepareOldTileInfo(ARequest);
  if VExistsTileInfo <> nil then begin
    VOldTileSize := VExistsTileInfo.GetSize;
    if ANewSize = VOldTileSize then begin
      Result := True;
    end;
  end;
end;

function TDownloadCheckerStuped.BeforeRequest(
  const AResultFactory: IDownloadResultFactory;
  const ARequest: IDownloadRequest
): IDownloadResult;
begin
  Result := nil;
  if FAntiBan <> nil then begin
    FAntiBan.PreDownload(ARequest);
  end;
end;

function TDownloadCheckerStuped.AfterReciveData(
  const AResultFactory: IDownloadResultFactory;
  const ARequest: IDownloadRequest;
  const ARecivedData: IBinaryData;
  var AStatusCode: Cardinal;
  var AContentType: string;
  var AResponseHead: string
): IDownloadResult;
var
  VConfig: ITileDownloaderConfigStatic;
begin
  Result := nil;
  VConfig := FTileDownloaderConfig.GetStatic;
  if VConfig.IgnoreMIMEType then begin
    AContentType := VConfig.DefaultMIMEType;
  end else begin
    if (AContentType = '') then begin
      AContentType := VConfig.DefaultMIMEType;
    end else if (Pos(AContentType, VConfig.ExpectedMIMETypes) <= 0) then begin
      Result := AResultFactory.BuildBadContentType(ARequest, AContentType, AStatusCode, AResponseHead);
      Exit;
    end;
  end;
  if IsNeedCheckTileSize(ARequest) then begin
    if CheckOldTileSize(ARequest, ARecivedData.Size) then begin
      Result := AResultFactory.BuildNotNecessary(ARequest, 'Одинаковый размер тайла', AStatusCode, AResponseHead);
      Exit;
    end;
  end;
  if FAntiBan <> nil then begin
    Result := FAntiBan.PostCheckDownload(
      AResultFactory,
      ARequest,
      ARecivedData,
      AStatusCode,
      AResponseHead
    );
  end;
end;

end.
