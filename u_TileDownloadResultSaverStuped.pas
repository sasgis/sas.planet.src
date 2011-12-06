unit u_TileDownloadResultSaverStuped;

interface

uses
  Classes,
  Types,
  GR32,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_ContentTypeSubst,
  i_ContentTypeManager,
  i_GlobalDownloadConfig,
  i_TilePostDownloadCropConfig,
  i_DownloadResult,
  i_TileDownloadResultSaver,
  i_SimpleTileStorageConfig,
  u_TileStorageAbstract;

type
  TTileDownloadResultSaverStuped = class(TInterfacedObject, ITileDownloadResultSaver)
  private
    FDownloadConfig: IGlobalDownloadConfig;
    FContentTypeSubst: IContentTypeSubst;
    FTilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
    FStorage: TTileStorageAbstract;
    FStorageConfig: ISimpleTileStorageConfig;
    FContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;

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
    procedure SaveDownloadResult(AResult: IDownloadResult);
  public
    constructor Create(
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
  i_ContentConverter,
  i_BitmapTileSaveLoad,
  i_TileRequest,
  i_TileDownloadRequest,
  u_ResStrings;

{ TTileDownloadResultSaverStuped }

constructor TTileDownloadResultSaverStuped.Create(
  ADownloadConfig: IGlobalDownloadConfig;
  AContentTypeManager: IContentTypeManager;
  AContentTypeSubst: IContentTypeSubst;
  ATilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
  AStorageConfig: ISimpleTileStorageConfig;
  AStorage: TTileStorageAbstract
);
begin
  FDownloadConfig := ADownloadConfig;
  FContentTypeManager := AContentTypeManager;
  FContentTypeSubst := AContentTypeSubst;
  FTilePostDownloadCropConfig := ATilePostDownloadCropConfig;
  FStorageConfig := AStorageConfig;
  FStorage := AStorage;
  FContentType := FStorage.GetMainContentType;
end;

procedure TTileDownloadResultSaverStuped.CropOnDownload(
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

procedure TTileDownloadResultSaverStuped.SaveDownloadResult(
  AResult: IDownloadResult);
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

procedure TTileDownloadResultSaverStuped.SaveTileDownload(
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

end.
