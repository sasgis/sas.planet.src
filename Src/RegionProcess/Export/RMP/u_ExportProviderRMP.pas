{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit u_ExportProviderRMP;

interface

uses
  Forms,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_LanguageManager,
  i_RegionProcessProgressInfoInternalFactory,
  i_ProjectionSetFactory,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoadFactory,
  i_ImageResamplerFactory,
  i_ImageResamplerConfig,
  i_MapTypeListChangeable,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_ExportRMP;

type
  TExportProviderRMP = class(TExportProviderAbstract)
  private
    FActiveMapsList: IMapTypeListChangeable;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FImageResamplerFactoryList: IImageResamplerFactoryList;
    FImageResamplerConfig: IImageResamplerConfig;
    FProjectionSetFactory: IProjectionSetFactory;
  protected
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AActiveMapsList: IMapTypeListChangeable;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AImageResamplerFactoryList: IImageResamplerFactoryList;
      const AImageResamplerConfig: IImageResamplerConfig;
      const AProjectionSetFactory: IProjectionSetFactory
    );
  end;


implementation

uses
  Types,
  Classes,
  SysUtils,
  c_CoordConverter,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  i_TileStorage,
  i_ProjectionSet,
  i_MapVersionRequest,
  i_MapType,
  u_ThreadExportToRMP,
  u_ResStrings;

{ TExportProviderRMP }

constructor TExportProviderRMP.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AActiveMapsList: IMapTypeListChangeable;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AImageResamplerFactoryList: IImageResamplerFactoryList;
  const AImageResamplerConfig: IImageResamplerConfig;
  const AProjectionSetFactory: IProjectionSetFactory
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder
  );
  FActiveMapsList := AActiveMapsList;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FImageResamplerFactoryList := AImageResamplerFactoryList;
  FImageResamplerConfig := AImageResamplerConfig;
  FProjectionSetFactory := AProjectionSetFactory;
end;

function TExportProviderRMP.CreateFrame: TFrame;
begin
  Result :=
    TfrExportRMP.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder,
      FActiveMapsList,
      FBitmap32StaticFactory,
      FBitmapTileSaveLoadFactory
    );

  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameRMPExport));
end;

function TExportProviderRMP.GetCaption: string;
begin
  Result := SAS_STR_ExportRMPExportCaption;
end;

procedure TExportProviderRMP.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  I: Integer;
  VPath: string;
  VZoomArr: TByteDynArray;
  VDirectTilesCopy: Boolean;
  VAlignSelection: Boolean;
  VProjectToLatLon: Boolean;
  VBitmapTileSaver: IBitmapTileSaver;
  VBitmapUniProvider: IBitmapUniProvider;
  VMapType: IMapType;
  VProjectionSet: IProjectionSet;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
  VMapVersion: IMapVersionRequest;
  VTileStorage: ITileStorage;
  VProduct, VProvider: AnsiString;
  VImageResamplerFactory: IImageResamplerFactory;
begin
  inherited;

  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;

  Assert(VMapType <> nil);

  VMapVersion := VMapType.VersionRequest.GetStatic;
  VTileStorage := VMapType.TileStorage;

  with (ParamsFrame as IRegionProcessParamsFrameRMPExport) do begin
    VDirectTilesCopy := DirectTilesCopy;
    VAlignSelection := AlignSelection;
    VProjectToLatLon := ProjectToLatLon;
    VProduct := RmpProduct;
    VProvider := RmpProvider;
    VBitmapTileSaver := BitmapTileSaver;
    VBitmapUniProvider := BitmapUniProvider;
  end;

  if VDirectTilesCopy or not VProjectToLatLon then begin
    VProjectionSet := VMapType.ProjectionSet;
  end else begin
    VProjectionSet :=
      FProjectionSetFactory.GetProjectionSetByCode(
        CGELonLatProjectionEPSG,
        CTileSplitQuadrate256x256
      );
  end;

  I := FImageResamplerFactoryList.GetIndexByGUID(FImageResamplerConfig.ActiveGUID);
  VImageResamplerFactory := FImageResamplerFactoryList.Items[I];

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadExportToRMP.Create(
      VProgressInfo,
      VPath,
      FVectorGeometryProjectedFactory,
      VProjectionSet,
      APolygon,
      VZoomArr,
      VMapType.TileStorage,
      VMapType.VersionRequest.GetStatic,
      VBitmapTileSaver,
      VBitmapUniProvider,
      FBitmap32StaticFactory,
      VImageResamplerFactory,
      VDirectTilesCopy,
      VAlignSelection,
      VProduct,
      VProvider
    );
  VThread.Resume;
end;

end.
