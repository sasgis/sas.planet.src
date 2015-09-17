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
  i_ProjectionSetFactory,
  i_GeometryProjectedFactory,
  i_Bitmap32BufferFactory,
  i_BitmapTileSaveLoadFactory,
  i_LanguageManager,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_ExportToOgf2;

type
  TExportProviderOgf2 = class(TExportProviderAbstract)
  private
    FProjectionSetFactory: IProjectionSetFactory;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
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
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AProjectionSetFactory: IProjectionSetFactory
    );
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
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AProjectionSetFactory: IProjectionSetFactory
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder
  );
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FProjectionSetFactory := AProjectionSetFactory;
end;

function TExportProviderOgf2.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToOgf2.Create(
      Self.LanguageManager,
      FVectorGeometryProjectedFactory,
      FBitmapTileSaveLoadFactory,
      FBitmap32StaticFactory,
      Self.MapSelectFrameBuilder,
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
  VImageProvider: IBitmapTileUniProvider;
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
      FProjectionSetFactory,
      FBitmap32StaticFactory,
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
