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

unit u_ExportProviderOgf2;

interface

uses
  Forms,
  i_GeometryLonLat,
  i_CoordConverterFactory,
  i_LocalCoordConverterFactorySimpe,
  i_GeometryProjectedFactory,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoadFactory,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_ExportToOgf2;

type
  TExportProviderOgf2 = class(TExportProviderAbstract)
  private
    FCoordConverterFactory: ICoordConverterFactory;
    FLocalConverterFactory: ILocalCoordConverterFactorySimpe;
    FProjectionFactory: IProjectionInfoFactory;
    FBitmapFactory: IBitmap32BufferFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmapFactory: IBitmap32BufferFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
      const ACoordConverterFactory: ICoordConverterFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  end;

implementation

uses
  Types,
  Classes,
  SysUtils,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  i_BitmapLayerProvider,
  i_BitmapTileSaveLoad,
  u_ThreadExportToOgf2,
  u_ResStrings;

{ TExportProviderOgf2 }

constructor TExportProviderOgf2.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmapFactory: IBitmap32BufferFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const ALocalConverterFactory: ILocalCoordConverterFactorySimpe;
  const ACoordConverterFactory: ICoordConverterFactory
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
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FCoordConverterFactory := ACoordConverterFactory;
  FLocalConverterFactory := ALocalConverterFactory;
end;

function TExportProviderOgf2.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToOgf2.Create(
      Self.LanguageManager,
      FProjectionFactory,
      FVectorGeometryProjectedFactory,
      FBitmapTileSaveLoadFactory,
      FBitmapFactory,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList,
      'OGF2 (*.ogf2) |*.ogf2',
      'ogf2'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneZoom));
  Assert(Supports(Result, IRegionProcessParamsFrameImageProvider));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToOgf2));
end;

function TExportProviderOgf2.GetCaption: string;
begin
  Result := SAS_STR_ExportOgf2PackCaption;
end;

procedure TExportProviderOgf2.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VTargetFile: string;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VImageProvider: IBitmapLayerProvider;
  VZoom: Byte;
  VSaver: IBitmapTileSaver;
  VTileSize: TPoint;
  VThread: TThread;
begin
  inherited;

  VTargetFile := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VZoom := (ParamsFrame as IRegionProcessParamsFrameOneZoom).Zoom;
  VImageProvider := (ParamsFrame as IRegionProcessParamsFrameImageProvider).Provider;
  VSaver := (ParamsFrame as IRegionProcessParamsFrameExportToOgf2).Saver;
  VTileSize := (ParamsFrame as IRegionProcessParamsFrameExportToOgf2).TileSize;

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadExportToOgf2.Create(
      VProgressInfo,
      FCoordConverterFactory,
      FLocalConverterFactory,
      FProjectionFactory,
      FBitmapFactory,
      FVectorGeometryProjectedFactory,
      VTargetFile,
      APolygon,
      VImageProvider,
      VZoom,
      VTileSize,
      VSaver
    );
  VThread.Resume;
end;

end.


