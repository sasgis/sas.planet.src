unit u_TileDownloadResultSaverStuped;

interface

uses
  Types,
  SysUtils,
  GR32,
  i_Notifier,
  i_Listener,
  i_BinaryData,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_ContentTypeSubst,
  i_ContentTypeManager,
  i_GlobalDownloadConfig,
  i_ImageResamplerConfig,
  i_TilePostDownloadCropConfig,
  i_DownloadResult,
  i_TileDownloaderState,
  i_TileDownloadResultSaver,
  i_SimpleTileStorageConfig,
  u_TileDownloaderStateInternal,
  u_TileStorageAbstract;

type
  TTileDownloadResultSaverStuped = class(TInterfacedObject, ITileDownloadResultSaver)
  private
    FDownloadConfig: IGlobalDownloadConfig;
    FImageResamplerConfig: IImageResamplerConfig;
    FContentTypeSubst: IContentTypeSubst;
    FTilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
    FStorage: TTileStorageAbstract;
    FStorageConfig: ISimpleTileStorageConfig;
    FContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;

    FStorageStateListener: IListener;

    FState: ITileDownloaderStateChangeble;
    FStateInternal: ITileDownloaderStateInternal;

    procedure OnStorageStateChange;

    procedure SaveTileDownload(
      const AXY: TPoint;
      AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AData: IBinaryData;
      const AContenType: string
    );
    procedure CropOnDownload(
      ABtm: TCustomBitmap32;
      const ACropRect: TRect;
      const ATileSize: TPoint
    );
  protected
    function GetState: ITileDownloaderStateChangeble;
    procedure SaveDownloadResult(const AResult: IDownloadResult);
  public
    constructor Create(
      const ADownloadConfig: IGlobalDownloadConfig;
      const AImageResamplerConfig: IImageResamplerConfig;
      const AContentTypeManager: IContentTypeManager;
      const AContentTypeSubst: IContentTypeSubst;
      const ATilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
      const AStorageConfig: ISimpleTileStorageConfig;
      AStorage: TTileStorageAbstract
    );
    destructor Destroy; override;
  end;

  ESaveTileDownloadError = class(Exception);

implementation

uses
  GR32_Resamplers,
  t_CommonTypes,
  i_Bitmap32Static,
  i_ContentConverter,
  i_BitmapTileSaveLoad,
  i_TileRequest,
  i_TileDownloadRequest,
  u_ListenerByEvent,
  u_Bitmap32Static,
  u_ResStrings;

{ TTileDownloadResultSaverStuped }

constructor TTileDownloadResultSaverStuped.Create(
  const ADownloadConfig: IGlobalDownloadConfig;
  const AImageResamplerConfig: IImageResamplerConfig;
  const AContentTypeManager: IContentTypeManager;
  const AContentTypeSubst: IContentTypeSubst;
  const ATilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
  const AStorageConfig: ISimpleTileStorageConfig;
  AStorage: TTileStorageAbstract
);
var
  VState: TTileDownloaderStateInternal;
begin
  inherited Create;
  FDownloadConfig := ADownloadConfig;
  FImageResamplerConfig := AImageResamplerConfig;
  FContentTypeManager := AContentTypeManager;
  FContentTypeSubst := AContentTypeSubst;
  FTilePostDownloadCropConfig := ATilePostDownloadCropConfig;
  FStorageConfig := AStorageConfig;
  FStorage := AStorage;
  FContentType := FStorage.GetMainContentType;

  VState := TTileDownloaderStateInternal.Create;
  FStateInternal := VState;
  FState := VState;

  FStorageStateListener := TNotifyNoMmgEventListener.Create(Self.OnStorageStateChange);
  FStorage.State.ChangeNotifier.Add(FStorageStateListener);

  OnStorageStateChange;
end;

destructor TTileDownloadResultSaverStuped.Destroy;
begin
  FStorage.State.ChangeNotifier.Add(FStorageStateListener);
  FStorageStateListener := nil;

  inherited;
end;

function TTileDownloadResultSaverStuped.GetState: ITileDownloaderStateChangeble;
begin
  Result := FState;
end;

procedure TTileDownloadResultSaverStuped.CropOnDownload(
  ABtm: TCustomBitmap32;
  const ACropRect: TRect;
  const ATileSize: TPoint
);
var
  VBtmDest: TCustomBitmap32;
  VResampler: TCustomResampler;
begin
  VResampler := FImageResamplerConfig.GetActiveFactory.CreateResampler;
  try
    VBtmDest := TCustomBitmap32.Create;
    try
      VBtmDest.SetSize(ATileSize.X, ATileSize.Y);
      StretchTransfer(
        VBtmDest,
        Bounds(0, 0, ATileSize.X, ATileSize.Y),
        VBtmDest.ClipRect,
        ABtm,
        ACropRect,
        VResampler,
        dmOpaque
      );
      ABtm.Assign(VBtmDest);
    finally
      VBtmDest.Free;
    end;
  finally
    VResampler.Free;
  end;
end;

procedure TTileDownloadResultSaverStuped.OnStorageStateChange;
begin
  if not Assigned(FStateInternal) then begin
    Exit;
  end;
  if FStorage.State.GetStatic.WriteAccess = asDisabled then begin
    FStateInternal.Disable('No write access to tile storage');
  end else begin
    FStateInternal.Enable;
  end;
end;

procedure TTileDownloadResultSaverStuped.SaveDownloadResult(
  const AResult: IDownloadResult
);
var
  VResultOk: IDownloadResultOk;
  VContentType: string;
  VTileDownloadRequest: ITileDownloadRequest;
  VTileRequest: ITileRequest;
begin
  if Assigned(AResult) then begin
    if Supports(AResult.Request, ITileDownloadRequest, VTileDownloadRequest) then begin
      VTileRequest := VTileDownloadRequest.Source;
      if Supports(AResult, IDownloadResultOk, VResultOk) then begin
        VContentType := VResultOk.ContentType;
        VContentType := FContentTypeSubst.GetContentType(VContentType);
        SaveTileDownload(VTileRequest.Tile, VTileRequest.Zoom, VTileRequest.VersionInfo, VResultOk.Data, VContentType);
      end else if Supports(AResult, IDownloadResultDataNotExists) then begin
        if FDownloadConfig.IsSaveTileNotExists then begin
          FStorage.SaveTNE(VTileRequest.Tile, VTileRequest.Zoom, VTileRequest.VersionInfo);
        end;
      end;
    end;
  end;
end;

procedure TTileDownloadResultSaverStuped.SaveTileDownload(
  const AXY: TPoint;
  AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AData: IBinaryData;
  const AContenType: string
);
var
  VBitmap: TCustomBitmap32;
  VContentTypeInfo: IContentTypeInfoBasic;
  VContentTypeBitmap: IContentTypeInfoBitmap;
  VConverter: IContentConverter;
  VLoader: IBitmapTileLoader;
  VTargetContentTypeBitmap: IContentTypeInfoBitmap;
  VBitmapStatic: IBitmap32Static;
  VData: IBinaryData;
  // cut images
  VCutCount, VCutSize, VCutTile: TPoint;
  i, j: Integer;
  VPos: TPoint;
  VCutBitmapStatic: IBitmap32Static;
begin
  if FStorageConfig.AllowAdd then begin
    if Supports(FContentType, IContentTypeInfoBitmap, VTargetContentTypeBitmap) and
      (FTilePostDownloadCropConfig.IsCropOnDownload or FTilePostDownloadCropConfig.IsCutOnDownload) then begin
      VContentTypeInfo := FContentTypeManager.GetInfo(AContenType);
      if VContentTypeInfo <> nil then begin
        if Supports(VContentTypeInfo, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
          VLoader := VContentTypeBitmap.GetLoader;
          if VLoader <> nil then begin
            // full downloaded image
            VBitmapStatic := VLoader.Load(AData);
            // TODO: crop before cut
            if FTilePostDownloadCropConfig.IsCutOnDownload then begin
              // cut into multiple tiles
              // define parts
              VCutCount := FTilePostDownloadCropConfig.CutCount;
              VCutSize := FTilePostDownloadCropConfig.CutSize;
              VCutTile := FTilePostDownloadCropConfig.CutTile;

              if (0 = VCutSize.X) or (0 = VCutSize.Y) then begin
                VCutSize := FStorageConfig.CoordConverter.GetTileSize(AXY, Azoom);
              end;

              // define counts
              if (0 = VCutCount.X) or (0 = VCutCount.Y) then begin
                // define count by image size
                if (VCutSize.X > 0) then begin
                  VCutCount.X := VBitmapStatic.Bitmap.Width div VCutSize.X;
                end;
                if (VCutSize.Y > 0) then begin
                  VCutCount.Y := VBitmapStatic.Bitmap.Height div VCutSize.Y;
                end;
              end;

              if (VCutCount.X > 0) and (VCutCount.Y > 0) then begin
                // cut in loop
                for i := 0 to VCutCount.X - 1 do begin
                  for j := 0 to VCutCount.Y - 1 do // dummy loop indeed
                  begin
                    VPos.X := i;
                    VPos.Y := j;

                    if not FTilePostDownloadCropConfig.CutSkipItem(VPos, VCutCount) then begin
                      // position of item (>=0 - ordinal, <0 - relative to count)
                      VPos.X := VPos.X + AXY.X - VCutTile.X;
                      if VCutTile.X < 0 then begin
                        VPos.X := VPos.X - VCutCount.X;
                      end;
                      VPos.Y := VPos.Y + AXY.Y - VCutTile.Y;
                      if VCutTile.Y < 0 then begin
                        VPos.Y := VPos.Y - VCutCount.Y;
                      end;

                      // crop single part
                      VBitmap := TCustomBitmap32.Create;
                      try
                        VBitmap.Assign(VBitmapStatic.Bitmap);
                        CropOnDownload(
                          VBitmap,
                          Rect(VCutSize.X * i, VCutSize.Y * j, VCutSize.X * (i + 1), VCutSize.Y * (j + 1)),
                          FStorageConfig.CoordConverter.GetTileSize(VPos, Azoom)
                        );

                        VCutBitmapStatic := TBitmap32Static.CreateWithOwn(VBitmap);
                        VBitmap := nil;
                      finally
                        VBitmap.Free;
                      end;

                      // save
                      VData := VTargetContentTypeBitmap.GetSaver.Save(VCutBitmapStatic);
                      FStorage.SaveTile(VPos, Azoom, AVersionInfo, VData);
                    end;
                  end;
                end;
              end;
            end else begin
              // crop single tile
              VBitmap := TCustomBitmap32.Create;
              try
                VBitmap.Assign(VBitmapStatic.Bitmap);
                CropOnDownload(
                  VBitmap,
                  FTilePostDownloadCropConfig.CropRect,
                  FStorageConfig.CoordConverter.GetTileSize(AXY, Azoom)
                );
                VBitmapStatic := TBitmap32Static.CreateWithOwn(VBitmap);
                VBitmap := nil;
              finally
                VBitmap.Free;
              end;
              VData := VTargetContentTypeBitmap.GetSaver.Save(VBitmapStatic);
              FStorage.SaveTile(AXY, Azoom, AVersionInfo, VData);
            end;
          end else begin
            raise ESaveTileDownloadError.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AContenType]);
          end;
        end else begin
          raise ESaveTileDownloadError.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AContenType]);
        end;
      end else begin
        raise ESaveTileDownloadError.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AContenType]);
      end;
    end else begin
      VConverter := FContentTypeManager.GetConverter(AContenType, FContentType.GetContentType);
      if VConverter <> nil then begin
        FStorage.SaveTile(AXY, Azoom, AVersionInfo, VConverter.Convert(AData));
      end else begin
        raise ESaveTileDownloadError.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AContenType]);
      end;
    end;
  end else begin
    raise ESaveTileDownloadError.Create('Для этой карты запрещено добавление тайлов.');
  end;
end;

end.
