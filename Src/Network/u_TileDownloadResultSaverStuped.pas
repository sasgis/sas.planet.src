{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_TileDownloadResultSaverStuped;

interface

uses
  Types,
  SysUtils,
  i_Notifier,
  i_Listener,
  i_BinaryData,
  i_PredicateByBinaryData,
  i_BitmapTileSaveLoad,
  i_DownloadResultFactory,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_ContentTypeSubst,
  i_ContentTypeManager,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_GlobalDownloadConfig,
  i_ImageResamplerFactoryChangeable,
  i_TilePostDownloadCropConfig,
  i_DownloadResult,
  i_TileRequestResult,
  i_TileDownloaderState,
  i_TileDownloadResultSaver,
  i_TileStorage,
  u_TileDownloaderStateInternal,
  u_BaseInterfacedObject;

type
  TTileDownloadResultSaverStuped = class(TBaseInterfacedObject, ITileDownloadResultSaver)
  private
    FDownloadConfig: IGlobalDownloadConfig;
    FImageResampler: IImageResamplerFactoryChangeable;
    FContentTypeSubst: IContentTypeSubst;
    FTilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
    FStorage: ITileStorage;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;
    FEmptyTilePredicate: IPredicateByBinaryData;
    FResultFactory: IDownloadResultFactory;

    FStorageStateListener: IListener;

    FState: ITileDownloaderStateChangeble;
    FStateInternal: ITileDownloaderStateInternal;

    procedure OnStorageStateChange;

    function SaveTileDownload(
      const ADownloadResult: IDownloadResultOk
    ): IDownloadResult;
    procedure CutDownloadedBitmap(
      const ASaver: IBitmapTileSaver;
      const AXY: TPoint;
      AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const ABtm: IBitmap32Static
    );
    function CropOnDownload(
      const ABtm: IBitmap32Static;
      const ACropRect: TRect;
      const ATileSize: TPoint
    ): IBitmap32Static;
    procedure SaveOneTile(
      const AXY: TPoint;
      AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AData: IBinaryData
    );
  private
    function GetState: ITileDownloaderStateChangeble;
    function SaveDownloadResult(const AResult: IDownloadResult): ITileRequestResult;
  public
    constructor Create(
      const ADownloadConfig: IGlobalDownloadConfig;
      const AImageResampler: IImageResamplerFactoryChangeable;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AEmptyTilePredicate: IPredicateByBinaryData;
      const AResultFactory: IDownloadResultFactory;
      const AContentTypeManager: IContentTypeManager;
      const AContentTypeSubst: IContentTypeSubst;
      const ASaveContentType: IContentTypeInfoBasic;
      const ATilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
      const AStorage: ITileStorage
    );
    destructor Destroy; override;
  end;

  ESaveTileDownloadError = class(Exception);

implementation

uses
  gnugettext,
  GR32,
  t_CommonTypes,
  i_ContentConverter,
  i_ProjectionInfo,
  i_ProjectionSet,
  i_TileRequest,
  i_TileDownloadRequest,
  u_ListenerByEvent,
  u_TileRequestResult,
  u_Bitmap32ByStaticBitmap,
  u_BitmapFunc,
  u_ResStrings;

type
  ESaveTileEmptyError = class(Exception);

{ TTileDownloadResultSaverStuped }

constructor TTileDownloadResultSaverStuped.Create(
  const ADownloadConfig: IGlobalDownloadConfig;
  const AImageResampler: IImageResamplerFactoryChangeable;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AEmptyTilePredicate: IPredicateByBinaryData;
  const AResultFactory: IDownloadResultFactory;
  const AContentTypeManager: IContentTypeManager;
  const AContentTypeSubst: IContentTypeSubst;
  const ASaveContentType: IContentTypeInfoBasic;
  const ATilePostDownloadCropConfig: ITilePostDownloadCropConfigStatic;
  const AStorage: ITileStorage
);
var
  VState: TTileDownloaderStateInternal;
begin
  Assert(Assigned(ADownloadConfig));
  Assert(Assigned(AImageResampler));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AResultFactory));
  Assert(Assigned(AContentTypeManager));
  Assert(Assigned(AContentTypeSubst));
  Assert(Assigned(ASaveContentType));
  Assert(Assigned(ATilePostDownloadCropConfig));
  Assert(Assigned(AStorage));
  inherited Create;
  FDownloadConfig := ADownloadConfig;
  FImageResampler := AImageResampler;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FEmptyTilePredicate := AEmptyTilePredicate;
  FResultFactory := AResultFactory;
  FContentTypeManager := AContentTypeManager;
  FContentTypeSubst := AContentTypeSubst;
  FTilePostDownloadCropConfig := ATilePostDownloadCropConfig;
  FStorage := AStorage;
  FContentType := ASaveContentType;

  VState := TTileDownloaderStateInternal.Create;
  FStateInternal := VState;
  FState := VState;

  FStorageStateListener := TNotifyNoMmgEventListener.Create(Self.OnStorageStateChange);
  FStorage.State.ChangeNotifier.Add(FStorageStateListener);

  OnStorageStateChange;
end;

destructor TTileDownloadResultSaverStuped.Destroy;
begin
  if Assigned(FStorage) and Assigned(FStorageStateListener) then begin
    FStorage.State.ChangeNotifier.Add(FStorageStateListener);
    FStorageStateListener := nil;
    FStorage := nil;
  end;
  inherited;
end;

function TTileDownloadResultSaverStuped.GetState: ITileDownloaderStateChangeble;
begin
  Result := FState;
end;

function TTileDownloadResultSaverStuped.CropOnDownload(
  const ABtm: IBitmap32Static;
  const ACropRect: TRect;
  const ATileSize: TPoint
): IBitmap32Static;
var
  VBitmap: TBitmap32ByStaticBitmap;
  VResampler: TCustomResampler;
begin
  VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
  try
    VBitmap.SetSize(ATileSize.X, ATileSize.Y);
    VResampler := FImageResampler.GetStatic.CreateResampler;
    try
      StretchTransfer(
        VBitmap,
        Bounds(0, 0, ATileSize.X, ATileSize.Y),
        ABtm,
        ACropRect,
        VResampler,
        dmOpaque
      );
    finally
      VResampler.Free;
    end;
    Result := VBitmap.MakeAndClear;
  finally
    VBitmap.Free;
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

function TTileDownloadResultSaverStuped.SaveDownloadResult(
  const AResult: IDownloadResult
): ITileRequestResult;
var
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
  VTileDownloadRequest: ITileDownloadRequest;
  VTileRequest: ITileRequest;
begin
  Assert(AResult <> nil);
  Result := nil;
  VResult := AResult;
  if Assigned(VResult) then begin
    if FStorage.State.GetStatic.AddAccess <> asDisabled then begin
      if Supports(VResult, IDownloadResultOk, VResultOk) then begin
        try
          VResult := SaveTileDownload(VResultOk);
          Result := TTileRequestResultOk.Create(AResult);
        except
          on E: Exception do begin
            Result :=
              TTileRequestResultErrorAfterDownloadRequest.Create(
                AResult,
                E.Message
              );
          end;
        end;
      end;
      if not Assigned(Result) then begin
        if Supports(VResult, IDownloadResultDataNotExists) then begin
          if Supports(AResult.Request, ITileDownloadRequest, VTileDownloadRequest) then begin
            VTileRequest := VTileDownloadRequest.Source;
            SaveOneTile(
              VTileRequest.Tile,
              VTileRequest.Zoom,
              VTileRequest.VersionInfo,
              nil
            );
          end else begin
            raise Exception.Create('This was not tile request');
          end;
          Result := TTileRequestResultOk.Create(VResult);
        end;
      end;
    end else begin
      raise ESaveTileDownloadError.Create('Для этой карты запрещено добавление тайлов.');
    end;
  end;
end;

procedure TTileDownloadResultSaverStuped.SaveOneTile(
  const AXY: TPoint;
  AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AData: IBinaryData
);
begin
  if Assigned(AData) then begin
    FStorage.SaveTile(
      AXY,
      AZoom,
      AVersionInfo,
      Now,
      FContentType,
      AData,
      true
    );
  end else begin
    if FDownloadConfig.IsSaveTileNotExists then begin
      FStorage.SaveTile(
        AXY,
        AZoom,
        AVersionInfo,
        Now,
        nil,
        nil,
        true
      );
    end;
  end;

end;

procedure TTileDownloadResultSaverStuped.CutDownloadedBitmap(
  const ASaver: IBitmapTileSaver;
  const AXY: TPoint;
  AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const ABtm: IBitmap32Static
);
var
  VProjectionSet: IProjectionSet;
  VProjection: IProjectionInfo;
  // cut images
  VCutCount, VCutSize, VCutTile: TPoint;
  i, j: Integer;
  VPos: TPoint;
  VCutBitmapStatic: IBitmap32Static;
  VData: IBinaryData;
begin
  VProjectionSet := FStorage.ProjectionSet;
  VProjection := VProjectionSet.Zooms[AZoom];
  // cut into multiple tiles
  // define parts
  VCutCount := FTilePostDownloadCropConfig.CutCount;
  VCutSize := FTilePostDownloadCropConfig.CutSize;
  VCutTile := FTilePostDownloadCropConfig.CutTile;

  if (0 = VCutSize.X) or (0 = VCutSize.Y) then begin
    VCutSize := VProjection.GetTileSize(AXY);
  end;

  // define counts
  if (0 = VCutCount.X) or (0 = VCutCount.Y) then begin
    // define count by image size
    if (VCutSize.X > 0) then begin
      VCutCount.X := ABtm.Size.X div VCutSize.X;
    end;
    if (VCutSize.Y > 0) then begin
      VCutCount.Y := ABtm.Size.Y div VCutSize.Y;
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
          VCutBitmapStatic :=
            CropOnDownload(
              ABtm,
              Rect(VCutSize.X * i, VCutSize.Y * j, VCutSize.X * (i + 1), VCutSize.Y * (j + 1)),
              VProjection.GetTileSize(VPos)
            );

          // save
          VData := ASaver.Save(VCutBitmapStatic);
          if Assigned(FEmptyTilePredicate) then begin
            if FEmptyTilePredicate.Check(VData) then begin
              VData := nil;
            end;
          end;
          SaveOneTile(VPos, AZoom, AVersionInfo, VData);
        end;
      end;
    end;
  end;
end;

function TTileDownloadResultSaverStuped.SaveTileDownload(
  const ADownloadResult: IDownloadResultOk
): IDownloadResult;
var
  VContentType: AnsiString;
  VTileDownloadRequest: ITileDownloadRequest;
  VTileRequest: ITileRequest;
  VContentTypeInfo: IContentTypeInfoBasic;
  VContentTypeBitmap: IContentTypeInfoBitmap;
  VConverter: IContentConverter;
  VLoader: IBitmapTileLoader;
  VTargetContentTypeBitmap: IContentTypeInfoBitmap;
  VBitmapStatic: IBitmap32Static;
  VData: IBinaryData;
  VProjectionSet: IProjectionSet;
begin
  Result := ADownloadResult;
  VContentType := ADownloadResult.ContentType;
  VContentType := FContentTypeSubst.GetContentType(VContentType);
  if Supports(ADownloadResult.Request, ITileDownloadRequest, VTileDownloadRequest) then begin
    VTileRequest := VTileDownloadRequest.Source;
  end else begin
    raise Exception.Create('This was not tile request');
  end;
  VProjectionSet := FStorage.ProjectionSet;
  if Supports(FContentType, IContentTypeInfoBitmap, VTargetContentTypeBitmap) and
    (FTilePostDownloadCropConfig.IsCropOnDownload or FTilePostDownloadCropConfig.IsCutOnDownload) then begin
    VContentTypeInfo := FContentTypeManager.GetInfo(VContentType);
    if VContentTypeInfo <> nil then begin
      if Supports(VContentTypeInfo, IContentTypeInfoBitmap, VContentTypeBitmap) then begin
        VLoader := VContentTypeBitmap.GetLoader;
        if VLoader <> nil then begin
          // full downloaded image
          VBitmapStatic := VLoader.Load(ADownloadResult.Data);
          // TODO: crop before cut
          if FTilePostDownloadCropConfig.IsCutOnDownload then begin
            CutDownloadedBitmap(
              VTargetContentTypeBitmap.GetSaver,
              VTileRequest.Tile,
              VTileRequest.Zoom,
              VTileRequest.VersionInfo,
              VBitmapStatic
            );
          end else begin
            // crop single tile
            VBitmapStatic :=
              CropOnDownload(
                VBitmapStatic,
                FTilePostDownloadCropConfig.CropRect,
                VProjectionSet.Zooms[VTileRequest.Zoom].GetTileSize(VTileRequest.Tile)
              );
            VData := VTargetContentTypeBitmap.GetSaver.Save(VBitmapStatic);
            if Assigned(FEmptyTilePredicate) then begin
              if FEmptyTilePredicate.Check(VData) then begin
                VData := nil;
              end;
            end;
            if not Assigned(VData) then begin
              Result :=
                FResultFactory.BuildDataNotExists(
                  ADownloadResult.Request,
                  gettext_NoOp('Tile is recognized as empty'),
                  [],
                  ADownloadResult.StatusCode,
                  ADownloadResult.RawResponseHeader
                );
            end;
            SaveOneTile(VTileRequest.Tile, VTileRequest.Zoom, VTileRequest.VersionInfo, VData);
          end;
        end else begin
          raise ESaveTileDownloadError.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [VContentType]);
        end;
      end else begin
        raise ESaveTileDownloadError.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [VContentType]);
      end;
    end else begin
      raise ESaveTileDownloadError.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [VContentType]);
    end;
  end else begin
    VConverter := FContentTypeManager.GetConverter(VContentType, FContentType.GetContentType);
    if VConverter <> nil then begin
      VData := VConverter.Convert(ADownloadResult.Data);
      if Assigned(FEmptyTilePredicate) then begin
        if FEmptyTilePredicate.Check(VData) then begin
          VData := nil;
        end;
      end;
      if not Assigned(VData) then begin
        Result :=
          FResultFactory.BuildDataNotExists(
            ADownloadResult.Request,
            gettext_NoOp('Tile is recognized as empty'),
            [],
            ADownloadResult.StatusCode,
            ADownloadResult.RawResponseHeader
          );
      end;
      SaveOneTile(VTileRequest.Tile, VTileRequest.Zoom, VTileRequest.VersionInfo, VData);
    end else begin
      raise ESaveTileDownloadError.CreateResFmt(@SAS_ERR_BadMIMEForDownloadRastr, [VContentType]);
    end;
  end;
end;

end.
