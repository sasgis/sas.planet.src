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

unit u_ProviderTilesGenPrev;

interface

uses
  Types,
  Forms,
  i_LanguageManager,
  i_GeometryLonLat,
  i_ImageResamplerFactory,
  i_ImageResamplerConfig,
  i_TileIteratorFactory,
  i_Bitmap32BufferFactory,
  i_GlobalViewMainConfig,
  i_RegionProcessTask,
  i_RegionProcessProgressInfo,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_TilesGenPrev;

type
  TProviderTilesGenPrev = class(TExportProviderBase)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FImageResamplerFactoryList: IImageResamplerFactoryList;
    FImageResamplerConfig: IImageResamplerConfig;
    FViewConfig: IGlobalViewMainConfig;
  protected
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    function PrepareTask(
      const APolygon: IGeometryLonLatPolygon;
      const AProgressInfo: IRegionProcessProgressInfoInternal
    ): IRegionProcessTask; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ATileIteratorFactory: ITileIteratorFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AViewConfig: IGlobalViewMainConfig;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AImageResamplerFactoryList: IImageResamplerFactoryList;
      const AImageResamplerConfig: IImageResamplerConfig
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  GR32,
  gnugettext,
  i_MapType,
  i_ContentTypeInfo,
  i_RegionProcessParamsFrame,
  u_ThreadGenPrevZoom;

{ TProviderTilesGenPrev }

constructor TProviderTilesGenPrev.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ATileIteratorFactory: ITileIteratorFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AViewConfig: IGlobalViewMainConfig;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AImageResamplerFactoryList: IImageResamplerFactoryList;
  const AImageResamplerConfig: IImageResamplerConfig
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder,
    ATileIteratorFactory
  );
  FBitmapFactory := ABitmapFactory;
  FViewConfig := AViewConfig;
  FImageResamplerFactoryList := AImageResamplerFactoryList;
  FImageResamplerConfig := AImageResamplerConfig;
end;

function TProviderTilesGenPrev.CreateFrame: TFrame;
begin
  Result :=
    TfrTilesGenPrev.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder,
      FImageResamplerFactoryList,
      FImageResamplerConfig
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTilesGenPrev));
end;

function TProviderTilesGenPrev.GetCaption: string;
begin
  Result := _('Generate');
end;

function TProviderTilesGenPrev.PrepareTask(
  const APolygon: IGeometryLonLatPolygon;
  const AProgressInfo: IRegionProcessProgressInfoInternal
): IRegionProcessTask;
var
  VInZooms: TByteDynArray;
  VMapType: IMapType;
  VResampler: IImageResamplerFactory;
  VBgColor: TColor32;
begin
  inherited;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VInZooms := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VResampler := (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).Resampler;
  if VMapType.Zmp.IsLayer then begin
    VBgColor := 0;
  end else begin
    VBgColor := Color32(FViewConfig.BackGroundColor);
  end;

  Result :=
    TThreadGenPrevZoom.Create(
      AProgressInfo,
      Self.TileIteratorFactory,
      FBitmapFactory,
      VInZooms,
      APolygon,
      VMapType.ContentType as IContentTypeInfoBitmap,
      VMapType,
      VMapType.VersionRequest.GetStatic,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsReplace,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsSaveFullOnly,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsCreateAllFromFirstZoom,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsUseTilesFromPrevZoom,
      VBgColor,
      VResampler
    );
end;

end.
