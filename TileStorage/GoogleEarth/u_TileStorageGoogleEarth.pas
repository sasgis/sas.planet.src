{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit u_TileStorageGoogleEarth;

interface

uses
  Windows,
  SysUtils,
  libge,
  i_BinaryData,
  i_MapVersionInfo,
  i_MapVersionFactory,
  i_MapVersionListStatic,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_BasicMemCache,
  i_CoordConverter,
  i_TileStorage,
  i_TileInfoBasicMemCache,
  u_TileStorageAbstract;

type
  TTileStorageGoogleEarth = class(TTileStorageAbstract, IBasicMemCache, IEnumTileInfo)
  private
    FCachePath: string;
    FDatabaseName: string;
    FIsTerrainStorage: Boolean;
    FMainContentType: IContentTypeInfoBasic;
    FTileNotExistsTileInfo: ITileInfoBasic;
    FTileInfoMemCache: ITileInfoBasicMemCache;
    FCacheProvider: IGoogleEarthCacheProvider;
    FCacheTmProvider: IGoogleEarthCacheProvider;
    FLock: IReadWriteSync;
    FIsProvidersCreated: Boolean;
  private
    function InternalGetTileInfo(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic;
  protected
    function LazyBuildProviders: Boolean;
  protected
    { ITileStorage }
    function GetIsFileCache: Boolean; override;

    function GetIsCanSaveMultiVersionTiles: Boolean; override;
    function AllowListOfTileVersions: Boolean; override;
    function AllowShowPrevVersion: Boolean; override;

    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): string; override;

    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; override;

    function GetTileRectInfo(
      const ARect: TRect;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileRectInfo; override;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    procedure SaveTile(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData
    ); override;

    procedure SaveTNE(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ALoadDate: TDateTime
    ); override;

    function GetListOfTileVersions(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): IMapVersionListStatic; override;

    function ScanTiles(
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    ): IEnumTileInfo; override;
  private
    { IBasicMemCache }
    procedure ClearMemCache;
    procedure IBasicMemCache.Clear = ClearMemCache;
  private
    { IEnumTileInfo }
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const AGeoConverter: ICoordConverter;
      const AStoragePath: string;
      const ANameInCache: string;
      const AIsTerrainStorage: Boolean;
      const ATileInfoMemCache: ITileInfoBasicMemCache;
      const AMapVersionFactory: IMapVersionFactory;
      const AMainContentType: IContentTypeInfoBasic
    );
    destructor Destroy; override;
  end;

implementation

uses
  t_GeoTypes,
  t_CommonTypes,
  i_TileIterator,
  i_InterfaceListSimple,
  u_GeoFun,
  u_InterfaceListSimple,
  u_MapVersionListStatic,
  u_TileRectInfoShort,
  u_TileIteratorByRect,
  u_TileStorageTypeAbilities,
  u_TileInfoBasic,
  u_Synchronizer;

{ TTileStorageGoogleEarth }

constructor TTileStorageGoogleEarth.Create(
  const AGeoConverter: ICoordConverter;
  const AStoragePath: string;
  const ANameInCache: string;
  const AIsTerrainStorage: Boolean;
  const ATileInfoMemCache: ITileInfoBasicMemCache;
  const AMapVersionFactory: IMapVersionFactory;
  const AMainContentType: IContentTypeInfoBasic
);
begin
  inherited Create(
    TTileStorageTypeAbilitiesGE.Create,
    AMapVersionFactory,
    AGeoConverter,
    AStoragePath
  );

  FCachePath := AStoragePath;
  FDatabaseName := ANameInCache;
  FIsTerrainStorage := AIsTerrainStorage;

  FMainContentType := AMainContentType;
  FTileInfoMemCache := ATileInfoMemCache;
  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);

  FLock := MakeSyncRW_Var(Self, False);
  FIsProvidersCreated := False;

  FCacheProvider := nil;
  FCacheTmProvider := nil;
end;

destructor TTileStorageGoogleEarth.Destroy;
begin
  FCacheProvider := nil;
  FCacheTmProvider := nil;
  FTileInfoMemCache := nil;
  FMainContentType := nil;
  FTileNotExistsTileInfo := nil;
  FLock := nil;
  inherited;
end;

function TTileStorageGoogleEarth.LazyBuildProviders: Boolean;
var
  VCachePath: PAnsiChar;
  VOpenErrorMsg: WideString;
  VCacheFactory: IGoogleEarthCacheProviderFactory;
begin
  FLock.BeginRead;
  try
    Result := FIsProvidersCreated;
  finally
    FLock.EndRead;
  end;

  if not Result then begin  
    FLock.BeginWrite;
    try
      if not FIsProvidersCreated then begin
        FIsProvidersCreated := True;

        FCacheProvider := nil;
        FCacheTmProvider := nil;

        VCachePath := PAnsiChar(FCachePath);
        VCacheFactory := libge.CreateGoogleEarthCacheProviderFactory;

        if VCacheFactory <> nil then begin
          if (FDatabaseName = '') or SameText(FDatabaseName, 'earth')  then begin
            if not FIsTerrainStorage then begin
              FCacheProvider := VCacheFactory.CreateEarthProvider(VCachePath, VOpenErrorMsg);
              RaiseGoogleEarthExceptionIfError(VOpenErrorMsg);
              FCacheTmProvider := VCacheFactory.CreateEarthTmProvider(VCachePath, VOpenErrorMsg);
            end else begin
              FCacheProvider := VCacheFactory.CreateEarthTerrainProvider(VCachePath, VOpenErrorMsg);
            end;
          end else if SameText(FDatabaseName, 'mars')  then begin
            if not FIsTerrainStorage then begin
              FCacheProvider := VCacheFactory.CreateMarsProvider(VCachePath, VOpenErrorMsg);
            end else begin
              FCacheProvider := VCacheFactory.CreateMarsTerrainProvider(VCachePath, VOpenErrorMsg);
            end;
          end else if SameText(FDatabaseName, 'moon')  then begin
            if not FIsTerrainStorage then begin
              FCacheProvider := VCacheFactory.CreateMoonProvider(VCachePath, VOpenErrorMsg);
            end else begin
              FCacheProvider := VCacheFactory.CreateMoonTerrainProvider(VCachePath, VOpenErrorMsg);
            end;
          end else if SameText(FDatabaseName, 'sky')  then begin
            if not FIsTerrainStorage then begin
              FCacheProvider := VCacheFactory.CreateSkyProvider(VCachePath, VOpenErrorMsg);
            end;
          end;
          RaiseGoogleEarthExceptionIfError(VOpenErrorMsg);
          Result := (FCacheProvider <> nil) or (FCacheTmProvider <> nil);
        end;
      end;
    finally
      FLock.EndWrite;
    end;
  end;
end;

function TTileStorageGoogleEarth.GetIsFileCache: Boolean;
begin
  Result := False;
end;

function TTileStorageGoogleEarth.GetIsCanSaveMultiVersionTiles: Boolean;
begin
  Result := True;
end;

function TTileStorageGoogleEarth.AllowListOfTileVersions: Boolean;
begin
  Result := True;
end;

function TTileStorageGoogleEarth.AllowShowPrevVersion: Boolean;
begin
  Result := True;
end;

function TTileStorageGoogleEarth.GetTileFileName(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): string;
begin
  Result := StoragePath;
end;

procedure ParseVersionInfo(
  const AVersionInfo: IMapVersionInfo;
  out ATileVersion: Word;
  out ATileDate: TDateTime;
  out ASearchAnyVersion: Boolean;
  out ASearchAnyDate: Boolean;
  out AIsTmVersion: Boolean
);
var
  I: Integer;
  VStr: string;
begin
  ATileVersion := 0;
  ATileDate := 0;
  ASearchAnyVersion := True;
  ASearchAnyDate := True;
  AIsTmVersion := False;

  if Assigned(AVersionInfo) and (AVersionInfo.StoreString <> '') then begin
    I := Pos('::', AVersionInfo.StoreString);
    if I > 0 then begin
      AIsTmVersion := True;

      VStr := Copy(AVersionInfo.StoreString, I + 3, Length(AVersionInfo.StoreString) - I - 2);
      ATileDate := StrToDateDef(VStr, 0);

      VStr := Copy(AVersionInfo.StoreString, 1, I - 2);
      ATileVersion := StrToIntDef(VStr, 0);

      ASearchAnyVersion := AVersionInfo.ShowPrevVersion;
      ASearchAnyDate := AVersionInfo.ShowPrevVersion;
    end else if AVersionInfo.StoreString <> '' then begin
      ATileVersion := StrToIntDef(AVersionInfo.StoreString, 0);
      ASearchAnyVersion := AVersionInfo.ShowPrevVersion;
    end;
  end;
end;

function BuildVersionStr(
  const ATileVersion: Word;
  const ATileDate: TDateTime;
  const AIsTmVersion: Boolean
): string;
begin
  if AIsTmVersion then begin
    Result := IntToStr(ATileVersion) + ' :: ' + DateToStr(ATileDate);
  end else begin
    Result := IntToStr(ATileVersion);
  end;
end;

function TTileStorageGoogleEarth.InternalGetTileInfo(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  I: Integer;
  VResult: Boolean;
  VData: IInterface;
  VXY: TPoint;
  VLonLat: TDoublePoint;
  VZoom: Byte;
  VInTileVersion: Word;
  VInTileDate: TDateTime;
  VOutTileVersion: Word;
  VOutTileDate: TDateTime;
  VTileSize: Integer;
  VWithData: Boolean;
  VSearchAnyVersion: Boolean;
  VSearchAnyDate: Boolean;
  VIsTmVersion: Boolean;
  VBinData, VTmpData: IBinaryData;
  VTileVersionStr: string;
  VTileVersionInfo: IMapVersionInfo;
  VIsOceanTerrain: Boolean;
  VCoordConverter: ICoordConverter;
  VTerrainList: IGoogleEarthTerrainTileList;
  VImageTileContentProvider: IGoogleEarthImageTileProvider;
  VTerrainTileContentProvider: IGoogleEarthTerrainTileProvider;
begin
  Result := nil;

  if not LazyBuildProviders then begin
    Exit;
  end;

  VXY.X := AXY.X;
  VXY.Y := AXY.Y;
  VZoom := AZoom;

  if FIsTerrainStorage then begin
    CheckGoogleEarthTerrainTileZoom(VZoom);
    if VZoom <> AZoom then begin
      VCoordConverter := Self.GeoConverter;
      VLonLat := VCoordConverter.TilePos2LonLat(AXY, AZoom);
      VXY := PointFromDoublePoint(
        VCoordConverter.LonLat2TilePosFloat(VLonLat, VZoom),
        prToTopLeft
      );
    end;
  end;

  ParseVersionInfo(
    AVersionInfo,
    VInTileVersion,
    VInTileDate,
    VSearchAnyVersion,
    VSearchAnyDate,
    VIsTmVersion
  );

  VWithData := AMode in [gtimWithData];
  VResult := False;

  if VIsTmVersion then begin
    if (FCacheTmProvider <> nil) then begin
      VResult := FCacheTmProvider.GetTileInfo(
        VXY,
        VZoom,
        VInTileVersion,
        VInTileDate,
        True,
        VSearchAnyDate,
        VWithData,
        VTileSize,
        VOutTileVersion,
        VOutTileDate,
        VData
      );
    end;
    if not VResult and VSearchAnyVersion and (FCacheProvider <> nil) then begin
      VResult := FCacheProvider.GetTileInfo(
        VXY,
        VZoom,
        0,
        0,
        True,
        True,
        VWithData,
        VTileSize,
        VOutTileVersion,
        VOutTileDate,
        VData
      );
    end;
  end else begin
    if (FCacheProvider <> nil) then begin
      VResult := FCacheProvider.GetTileInfo(
        VXY,
        VZoom,
        VInTileVersion,
        VInTileDate,
        VSearchAnyVersion,
        VSearchAnyDate,
        VWithData,
        VTileSize,
        VOutTileVersion,
        VOutTileDate,
        VData
      );
    end;
    if not VResult and VSearchAnyVersion and (FCacheTmProvider <> nil) then begin
      VResult := FCacheTmProvider.GetTileInfo(
        VXY,
        VZoom,
        0,
        0,
        True,
        True,
        VWithData,
        VTileSize,
        VOutTileVersion,
        VOutTileDate,
        VData
      );
    end;
  end; 

  if VResult then begin
    VTileVersionStr := BuildVersionStr(VOutTileVersion, VOutTileDate, VIsTmVersion);
    VTileVersionInfo := MapVersionFactory.CreateByStoreString(VTileVersionStr, VSearchAnyVersion);
    if VWithData then begin
      if Supports(VData, IGoogleEarthImageTileProvider, VImageTileContentProvider) then begin
        VBinData := VImageTileContentProvider.GetJPEG;
      end else if Supports(VData, IGoogleEarthTerrainTileProvider, VTerrainTileContentProvider) then begin
        VBinData := nil;
        VTerrainList := VTerrainTileContentProvider.GetKML;
        if Assigned(VTerrainList) then begin
          for I := 0 to VTerrainList.Count - 1 do begin
            VTmpData := VTerrainList.Get(I, VXY.X, VXY.Y, VZoom, VIsOceanTerrain);
            if (VXY.X = AXY.X) and (VXY.Y = AXY.Y) and (VZoom = AZoom) then begin
              VBinData := VTmpData;
            end;
            if Assigned(FTileInfoMemCache) then begin
              Result := TTileInfoBasicExistsWithTile.Create(
                VOutTileDate,
                VTmpData,
                VTileVersionInfo,
                FMainContentType
              );
              FTileInfoMemCache.Add(VXY, VZoom, VTileVersionInfo, Result);
            end;
          end;
          Result := nil;
        end else begin
          Assert(False);
        end;
      end else begin
        VBinData := nil;
      end;

      if Assigned(VBinData) then begin
        Result :=
          TTileInfoBasicExistsWithTile.Create(
            VOutTileDate,
            VBinData,
            VTileVersionInfo,
            FMainContentType
          );
      end;
    end else begin
      Result :=
        TTileInfoBasicExists.Create(
          VOutTileDate,
          VTileSize,
          VTileVersionInfo,
          FMainContentType
        );
    end;
  end; 
end;

function TTileStorageGoogleEarth.GetTileInfo(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
begin
  if Assigned(FTileInfoMemCache) then begin
    Result := FTileInfoMemCache.Get(AXY, AZoom, AVersionInfo, AMode, True);
    if Result <> nil then begin
      Exit;
    end;
  end;

  Result := FTileNotExistsTileInfo;

  if GetState.GetStatic.ReadAccess <> asDisabled then begin
    Result := InternalGetTileInfo(AXY, AZoom, AVersionInfo, AMode);

    if not Assigned(Result) then begin
      Result := TTileInfoBasicNotExists.Create(0, AVersionInfo);
    end;
  end;

  if Assigned(FTileInfoMemCache) then begin
    FTileInfoMemCache.Add(AXY, AZoom, AVersionInfo, Result);
  end;
end;

function TTileStorageGoogleEarth.GetListOfTileVersions(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): IMapVersionListStatic;

  procedure _TileInfoListToListSimple(
    const ATileInfoList: IGoogleEarthTileInfoList;
    const AListSimple: IInterfaceListSimple;
    const AShowPrevVersion: Boolean;
    const AIsTmVersion: Boolean
  );
  var
    I: Integer;
    VTileVersion: Word;
    VTileDate: TDateTime;
    VTileSize: Integer;
    VVersionStr: string;
    VMapVersionInfo: IMapVersionInfo;
  begin
    Assert(Assigned(AListSimple));
    if Assigned(ATileInfoList) then begin
      if AIsTmVersion then begin
        ATileInfoList.SortByDate;
      end else begin
        ATileInfoList.SortByVersion;
      end;
      for I := 0 to ATileInfoList.Count - 1 do begin
        if ATileInfoList.Get(I, VTileSize, VTileVersion, VTileDate) then begin
          VVersionStr := BuildVersionStr(VTileVersion, VTileDate, AIsTmVersion);
          VMapVersionInfo := MapVersionFactory.CreateByStoreString(VVersionStr, AShowPrevVersion);
          AListSimple.Add(VMapVersionInfo);
        end;
      end;
    end;
  end;

var
  VShowPrevVersion: Boolean;
  VList: IGoogleEarthTileInfoList;
  VListSimple: IInterfaceListSimple;
begin
  Result := nil;
  if GetState.GetStatic.ReadAccess <> asDisabled then begin
    if not LazyBuildProviders then begin
      Exit;
    end;

    VShowPrevVersion := Assigned(AVersionInfo) and AVersionInfo.ShowPrevVersion;
    VListSimple := TInterfaceListSimple.Create;

    if FCacheProvider <> nil then begin
      VList := FCacheProvider.GetListOfTileVersions(AXY, AZoom, 0, 0);
      _TileInfoListToListSimple(VList, VListSimple, VShowPrevVersion, False);
    end;

    if FCacheTmProvider <> nil then begin 
      VList := FCacheTmProvider.GetListOfTileVersions(AXY, AZoom, 0, 0);
      _TileInfoListToListSimple(VList, VListSimple, VShowPrevVersion, True);
    end;

    if VListSimple.Count > 0 then begin
      Result := TMapVersionListStatic.Create(VListSimple.MakeStaticAndClear, True);
    end;
  end;
end;

function TTileStorageGoogleEarth.GetTileRectInfo(
  const ARect: TRect;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): ITileRectInfo;
var
  VRect: TRect;
  VZoom: Byte;
  VCount: TPoint;
  VItems: TArrayOfTileInfoShortInternal;
  VIndex: Integer;
  VTile: TPoint;
  VIterator: ITileIterator;
  VTileInfo: ITileInfoBasic;
begin
  Result := nil;
  if GetState.GetStatic.ReadAccess <> asDisabled then begin
    if not LazyBuildProviders then begin
      Exit;
    end;
    VRect := ARect;
    VZoom := AZoom;
    GeoConverter.CheckTileRect(VRect, VZoom);
    VCount.X := VRect.Right - VRect.Left;
    VCount.Y := VRect.Bottom - VRect.Top;
    if (VCount.X > 0) and (VCount.Y > 0) and (VCount.X <= 2048) and (VCount.Y <= 2048) then begin
      SetLength(VItems, VCount.X * VCount.Y);
      VIterator := TTileIteratorByRect.Create(VRect);
      while VIterator.Next(VTile) do begin
        VIndex := TTileRectInfoShort.TileInRectToIndex(VTile, VRect);
        Assert(VIndex >=0);
        if VIndex >= 0 then begin
          VTileInfo := InternalGetTileInfo(VTile, VZoom, AVersionInfo, gtimWithoutData);
          if Assigned(VTileInfo) then begin
            VItems[VIndex].FLoadDate := 0;
            VItems[VIndex].FSize := VTileInfo.Size;
            VItems[VIndex].FInfoType := titExists;
          end else begin
            VItems[VIndex].FLoadDate := 0;
            VItems[VIndex].FSize := 0;
            VItems[VIndex].FInfoType := titNotExists;
          end;
        end;
      end;
      Result :=
        TTileRectInfoShort.CreateWithOwn(
          VRect,
          VZoom,
          nil,
          FMainContentType,
          VItems
        );
      VItems := nil;
    end;
  end;
end;

procedure TTileStorageGoogleEarth.SaveTile(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AContentType: IContentTypeInfoBasic;
  const AData: IBinaryData
);
begin
  Abort;
end;

procedure TTileStorageGoogleEarth.SaveTNE(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ALoadDate: TDateTime
);
begin
  Abort;
end;

function TTileStorageGoogleEarth.DeleteTile(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
begin
  Result := False;
end;

function TTileStorageGoogleEarth.ScanTiles(
  const AIgnoreTNE: Boolean;
  const AIgnoreMultiVersionTiles: Boolean
): IEnumTileInfo;
begin
  // ToDo: Prepare tile iterator
  Result := Self as IEnumTileInfo;
end;

function TTileStorageGoogleEarth.Next(var ATileInfo: TTileInfo): Boolean;
begin
  Result := False; // ToDo
end;

procedure TTileStorageGoogleEarth.ClearMemCache;
begin
  if Assigned(FTileInfoMemCache) then begin
    FTileInfoMemCache.Clear;
  end;
end;

end.
