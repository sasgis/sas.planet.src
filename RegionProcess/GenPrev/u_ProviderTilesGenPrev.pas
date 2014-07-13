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
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_ImageResamplerFactory,
  i_ImageResamplerConfig,
  i_CoordConverterFactory,
  i_GeometryProjectedFactory,
  i_Bitmap32BufferFactory,
  i_GlobalViewMainConfig,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_TilesGenPrev;

type
  TProviderTilesGenPrev = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapFactory: IBitmap32BufferFactory;
    FImageResamplerFactoryList: IImageResamplerFactoryList;
    FImageResamplerConfig: IImageResamplerConfig;
    FViewConfig: IGlobalViewMainConfig;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AViewConfig: IGlobalViewMainConfig;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapFactory: IBitmap32BufferFactory;
      const AImageResamplerFactoryList: IImageResamplerFactoryList;
      const AImageResamplerConfig: IImageResamplerConfig
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  end;


implementation

uses
  Classes,
  SysUtils,
  GR32,
  i_MapTypes,
  i_ContentTypeInfo,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  u_ThreadGenPrevZoom,
  u_ResStrings;

{ TProviderTilesGenPrev }

constructor TProviderTilesGenPrev.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AViewConfig: IGlobalViewMainConfig;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapFactory: IBitmap32BufferFactory;
  const AImageResamplerFactoryList: IImageResamplerFactoryList;
  const AImageResamplerConfig: IImageResamplerConfig
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
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
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      FImageResamplerFactoryList,
      FImageResamplerConfig
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTilesGenPrev));
end;

function TProviderTilesGenPrev.GetCaption: string;
begin
  Result := SAS_STR_OperationGenPrevCaption;
end;

procedure TProviderTilesGenPrev.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VInZooms: TByteDynArray;
  VMapType: IMapType;
  VResampler: IImageResamplerFactory;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VBgColor: TColor32;
  VThread: TThread;
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

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadGenPrevZoom.Create(
      VProgressInfo,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      FBitmapFactory,
      VInZooms,
      APolygon,
      VMapType.ContentType as IContentTypeInfoBitmap,
      VMapType,
      VMapType.VersionRequestConfig.GetStatic,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsReplace,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsSaveFullOnly,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsCreateAllFromFirstZoom,
      (ParamsFrame as IRegionProcessParamsFrameTilesGenPrev).IsUseTilesFromPrevZoom,
      VBgColor,
      VResampler
    );
  VThread.Resume;
end;

end.


