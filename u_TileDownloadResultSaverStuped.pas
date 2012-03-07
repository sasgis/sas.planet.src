unit u_TileDownloadResultSaverStuped;

interface

uses
  Classes,
  Types,
  GR32,
  i_JclNotify,
  i_BinaryData,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_ContentTypeSubst,
  i_ContentTypeManager,
  i_GlobalDownloadConfig,
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
    FContentTypeSubst: IContentTypeSubst;
    FTilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
    FStorage: TTileStorageAbstract;
    FStorageConfig: ISimpleTileStorageConfig;
    FContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;

    FStorageStateListener: IJclListener;

    FState: ITileDownloaderStateChangeble;
    FStateInternal: ITileDownloaderStateInternal;

    procedure OnStorageStateChange;

    procedure SaveTileDownload(
      AXY: TPoint;
      AZoom: byte;
      AVersionInfo: IMapVersionInfo;
      AData: IBinaryData;
      AContenType: string
    );
    procedure CropOnDownload(
      ABtm: TCustomBitmap32;
      ACropRect: TRect;
      ATileSize: TPoint
    );
  protected
    function GetState: ITileDownloaderStateChangeble;
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
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_Resamplers,
  t_CommonTypes,
  i_Bitmap32Static,
  i_ContentConverter,
  i_BitmapTileSaveLoad,
  i_TileRequest,
  i_TileDownloadRequest,
  u_NotifyEventListener,
  u_Bitmap32Static,
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
var
  VState: TTileDownloaderStateInternal;
begin
  FDownloadConfig := ADownloadConfig;
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

procedure TTileDownloadResultSaverStuped.OnStorageStateChange;
begin
  if FStorage.State.GetStatic.WriteAccess = asDisabled then begin
    FStateInternal.Disable('No write access to tile storage');
  end else begin
    FStateInternal.Enable;
  end;
end;

procedure TTileDownloadResultSaverStuped.SaveDownloadResult(
  AResult: IDownloadResult
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
  AXY: TPoint;
  AZoom: byte;
  AVersionInfo: IMapVersionInfo;
  AData: IBinaryData;
  AContenType: string
);
var
  btmSrc: TCustomBitmap32;
  VContentTypeInfo: IContentTypeInfoBasic;
  VContentTypeBitmap: IContentTypeInfoBitmap;
  VConverter: IContentConverter;
  VLoader: IBitmapTileLoader;
  VTargetContentTypeBitmap: IContentTypeInfoBitmap;
  VBitmapStatic: IBitmap32Static;
  VData: IBinaryData;
begin
  if FStorageConfig.AllowAdd then begin
    if Supports(FContentType, IContentTypeInfoBitmap, VTargetContentTypeBitmap) and FTilePostDownloadCropConfig.IsCropOnDownload then begin
      VContentTypeInfo := FContentTypeManager.GetInfo(AContenType);
      if VContentTypeInfo <> nil then begin
        if Supports(VContentTypeInfo, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
          VLoader := VContentTypeBitmap.GetLoader;
          if VLoader <> nil then begin
            VBitmapStatic := VLoader.Load(AData);
            btmsrc := TCustomBitmap32.Create;
            try
              btmSrc.Assign(VBitmapStatic.Bitmap);
              CropOnDownload(
                btmSrc,
                FTilePostDownloadCropConfig.CropRect,
                FStorageConfig.CoordConverter.GetTileSize(AXY, Azoom)
              );
            except
              FreeAndNil(btmSrc);
              raise;
            end;
            VBitmapStatic := TBitmap32Static.CreateWithOwn(btmSrc);
            VData := VTargetContentTypeBitmap.GetSaver.Save(VBitmapStatic);
            FStorage.SaveTile(AXY, Azoom, AVersionInfo, VData);
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
        FStorage.SaveTile(AXY, Azoom, AVersionInfo, VConverter.Convert(AData));
      end else begin
        raise Exception.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [AContenType]);
      end;
    end;
  end else begin
    raise Exception.Create('Для этой карты запрещено добавление тайлов.');
  end;
end;

end.
