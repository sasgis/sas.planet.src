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
  Types,
  GR32,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_DownloadChecker,
  i_TileDownloadChecker,
  i_TileDownloaderConfig,
  i_MapVersionInfo,
  i_LastResponseInfo,
  i_ContentTypeInfo,
  i_ContentTypeManager,
  i_ContentTypeSubst,
  i_SimpleTileStorageConfig,
  i_AntiBan,
  i_TilePostDownloadCropConfig,
  i_GlobalDownloadConfig,
  i_TileInfoBasic,
  u_TileStorageAbstract;

type
  TDownloadCheckerStuped = class(TInterfacedObject, IDownloadChecker, ITileDownloadChecker)
  private
    FTileDownloaderConfig: ITileDownloaderConfig;
    FDownloadConfig: IGlobalDownloadConfig;
    FStorageConfig: ISimpleTileStorageConfig;
    FStorage: TTileStorageAbstract;
    FContentType: IContentTypeInfoBasic;
    FAntiBan: IAntiBan;
    FContentTypeManager: IContentTypeManager;
    FTilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
    FContentTypeSubst: IContentTypeSubst;
    function PrepareOldTileInfo(ARequest: IDownloadRequest): ITileInfoBasic;
    function CheckOldTileSize(ARequest: IDownloadRequest; ANewSize: Cardinal): Boolean;
    function IsNeedCheckTileSize(ARequest: IDownloadRequest): Boolean;
    procedure SaveTileDownload(
      AXY: TPoint;
      AZoom: byte;
      AVersionInfo: IMapVersionInfo;
      ATileStream: TCustomMemoryStream;
      AContenType: string
    );
    procedure CropOnDownload(
      ABtm: TCustomBitmap32;
      ACropRect: TRect;
      ATileSize: TPoint
    );
  protected
    function BeforeRequest(
      AResultFactory: IDownloadResultFactory;
      ARequest: IDownloadRequest
    ): IDownloadResult;
    function AfterReciveData(
      AResultFactory: IDownloadResultFactory;
      ARequest: IDownloadRequest;
      const ARecivedSize: Integer;
      const ARecivedBuffer: Pointer;
      var AStatusCode: Cardinal;
      var AContentType: string;
      var AResponseHead: string
    ): IDownloadResult;
  protected
    procedure AfterDownload(AResult: IDownloadResult);
  public
    constructor Create(
      AAntiBan: IAntiBan;
      ATileDownloaderConfig: ITileDownloaderConfig;
      ADownloadConfig: IGlobalDownloadConfig;
      AContentTypeManager: IContentTypeManager;
      AContentTypeSubst: IContentTypeSubst;
      ATilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
      AStorageConfig: ISimpleTileStorageConfig;
      AStorage: TTileStorageAbstract
    );
  end;

implementation

uses
  SysUtils,
  GR32_Resamplers,
  i_TileRequest,
  i_TileDownloadRequest,
  i_ContentConverter,
  i_BitmapTileSaveLoad,
  u_ResStrings,
  u_TileRequestBuilderHelpers;

{ TDownloadCheckerStuped }

constructor TDownloadCheckerStuped.Create(
  AAntiBan: IAntiBan;
  ATileDownloaderConfig: ITileDownloaderConfig;
  ADownloadConfig: IGlobalDownloadConfig;
  AContentTypeManager: IContentTypeManager;
  AContentTypeSubst: IContentTypeSubst;
  ATilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
  AStorageConfig: ISimpleTileStorageConfig;
  AStorage: TTileStorageAbstract
);
begin
  FAntiBan := AAntiBan;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FDownloadConfig := ADownloadConfig;
  FContentTypeManager := AContentTypeManager;
  FContentTypeSubst := AContentTypeSubst;
  FTilePostDownloadCropConfig := ATilePostDownloadCropConfig;
  FStorageConfig := AStorageConfig;
  FStorage := AStorage;
  FContentType := FStorage.GetMainContentType;
end;

procedure TDownloadCheckerStuped.CropOnDownload(
  ABtm: TCustomBitmap32;
  ACropRect: TRect;
  ATileSize: TPoint
);
var
  VBtmSrc: TCustomBitmap32;
  VBtmDest: TCustomBitmap32;
begin
  VBtmSrc := TCustomBitmap32.Create;
  try
    VBtmSrc.Assign(ABtm);
    VBtmSrc.Resampler := TLinearResampler.Create;
    VBtmDest := TCustomBitmap32.Create;
    try
      VBtmDest.SetSize(ATileSize.X, ATileSize.Y);
      VBtmDest.Draw(Bounds(0, 0, ATileSize.X, ATileSize.Y), ACropRect, VBtmSrc);
      ABtm.Assign(VBtmDest);
    finally
      VBtmDest.Free;
    end;
  finally
    VBtmSrc.Free;
  end;
end;

function TDownloadCheckerStuped.IsNeedCheckTileSize(
  ARequest: IDownloadRequest): Boolean;
var
  VTileDownloadRequest: ITileDownloadRequest;
begin
  Result := False;
  if Supports(ARequest, ITileDownloadRequest, VTileDownloadRequest) then begin
    Result := Supports(VTileDownloadRequest.Source, ITileRequestWithSizeCheck);
  end;
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

procedure TDownloadCheckerStuped.SaveTileDownload(
  AXY: TPoint;
  AZoom: byte;
  AVersionInfo: IMapVersionInfo;
  ATileStream: TCustomMemoryStream;
  AContenType: string
);
var
  btmSrc: TCustomBitmap32;
  VContentTypeInfo: IContentTypeInfoBasic;
  VContentTypeBitmap: IContentTypeInfoBitmap;
  VConverter: IContentConverter;
  VLoader: IBitmapTileLoader;
  VMemStream: TMemoryStream;
  VTargetContentTypeBitmap: IContentTypeInfoBitmap;
begin
  if FStorageConfig.AllowAdd then begin
    if Supports(FContentType, IContentTypeInfoBitmap, VTargetContentTypeBitmap) and FTilePostDownloadCropConfig.IsCropOnDownload then begin
      VContentTypeInfo := FContentTypeManager.GetInfo(AContenType);
      if VContentTypeInfo <> nil then begin
        if Supports(VContentTypeInfo, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
          VLoader := VContentTypeBitmap.GetLoader;
          if VLoader <> nil then begin
            btmsrc := TCustomBitmap32.Create;
            try
              ATileStream.Position := 0;
              VLoader.LoadFromStream(ATileStream, btmSrc);
              CropOnDownload(
                btmSrc,
                FTilePostDownloadCropConfig.CropRect,
                FStorageConfig.CoordConverter.GetTileSize(AXY, Azoom)
              );
              VMemStream := TMemoryStream.Create;
              try
                VTargetContentTypeBitmap.GetSaver.SaveToStream(btmSrc, VMemStream);
                FStorage.SaveTile(AXY, Azoom, AVersionInfo, VMemStream);
              finally
                VMemStream.Free;
              end;
            finally
              FreeAndNil(btmSrc);
            end;
          end else begin
            raise Exception.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AContenType]);
          end;
        end else begin
          raise Exception.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AContenType]);
        end;
      end else begin
        raise Exception.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AContenType]);
      end;
    end else begin
      VConverter := FContentTypeManager.GetConverter(AContenType, FContentType.GetContentType);
      if VConverter <> nil then begin
        if VConverter.GetIsSimpleCopy then begin
          FStorage.SaveTile(AXY, Azoom, AVersionInfo, ATileStream);
        end else begin
          VMemStream := TMemoryStream.Create;
          try
            ATileStream.Position := 0;
            VConverter.ConvertStream(ATileStream, VMemStream);
            FStorage.SaveTile(AXY, Azoom, AVersionInfo, VMemStream);
          finally
            VMemStream.Free;
          end;
        end;
      end else begin
        raise Exception.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AContenType]);
      end;
    end;
  end else begin
    raise Exception.Create('Для этой карты запрещено добавление тайлов.');
  end;
end;

function TDownloadCheckerStuped.CheckOldTileSize(ARequest: IDownloadRequest;
  ANewSize: Cardinal): Boolean;
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
  AResultFactory: IDownloadResultFactory;
  ARequest: IDownloadRequest
): IDownloadResult;
begin
  if FAntiBan <> nil then begin
    FAntiBan.PreDownload(ARequest);
  end;
end;

procedure TDownloadCheckerStuped.AfterDownload(AResult: IDownloadResult);
var
  VResultOk: IDownloadResultOk;
  VResultStream: TMemoryStream;
  VContentType: string;
  VTileDownloadRequest: ITileDownloadRequest;
  VTileRequest: ITileRequest;
begin
  if Assigned(AResult) then begin
    if Supports(AResult.Request, ITileDownloadRequest, VTileDownloadRequest) then begin
      VTileRequest := VTileDownloadRequest.Source;
      if Supports(AResult, IDownloadResultOk, VResultOk) then begin
        VResultStream := TMemoryStream.Create;
        try
          VResultStream.WriteBuffer(VResultOk.Buffer^, VResultOk.Size);
          VContentType := VResultOk.ContentType;
          VContentType := FContentTypeSubst.GetContentType(VContentType);
          SaveTileDownload(VTileRequest.Tile, VTileRequest.Zoom, VTileRequest.VersionInfo, VResultStream, VContentType);
        finally
          VResultStream.Free;
        end;
      end else if Supports(AResult, IDownloadResultDataNotExists) then begin
        if FDownloadConfig.IsSaveTileNotExists then begin
          FStorage.SaveTNE(VTileRequest.Tile, VTileRequest.Zoom, VTileRequest.VersionInfo);
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
  var AContentType: string;
  var AResponseHead: string
): IDownloadResult;
var
  VConfig: ITileDownloaderConfigStatic;
begin
  VConfig := FTileDownloaderConfig.GetStatic;
  if VConfig.IgnoreMIMEType then begin
    AContentType := VConfig.DefaultMIMEType;
  end else begin
    if (AContentType = '') then begin
      AContentType := VConfig.DefaultMIMEType;
    end else if (Pos(AContentType, VConfig.ExpectedMIMETypes) <= 0) then begin
      Result := AResultFactory.BuildBadContentType(ARequest, AContentType, AResponseHead);
      Exit;
    end;
  end;
  if IsNeedCheckTileSize(ARequest) then begin
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
