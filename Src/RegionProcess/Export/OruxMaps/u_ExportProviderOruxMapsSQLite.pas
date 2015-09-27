{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_ExportProviderOruxMapsSQLite;

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
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_ExportOruxMapsSQLite;

type
  TExportProviderOruxMapsSQLite = class(TExportProviderAbstract)
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
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
  i_BinaryData,
  i_BitmapTileSaveLoad,
  i_BitmapLayerProvider,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  i_TileStorage,
  i_MapVersionRequest,
  i_MapType,
  u_ThreadExportToOruxMapsSQLite,
  u_ResStrings;

{ TExportProviderOruxMapsSQLite }

constructor TExportProviderOruxMapsSQLite.Create(
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

function TExportProviderOruxMapsSQLite.CreateFrame: TFrame;
begin
  Result :=
    TfrExportOruxMapsSQLite.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder,
      FBitmap32StaticFactory,
      FBitmapTileSaveLoadFactory
    );

  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameImageProvider));
  Assert(Supports(Result, IRegionProcessParamsFrameOruxMapsSQLiteExport));
end;

function TExportProviderOruxMapsSQLite.GetCaption: string;
begin
  Result := SAS_STR_ExportOruxMapsSQLiteExportCaption;
end;

procedure TExportProviderOruxMapsSQLite.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VPath: string;
  VZoomArr: TByteDynArray;
  VDirectTilesCopy: Boolean;
  VBitmapTileSaver: IBitmapTileSaver;
  VBitmapProvider: IBitmapTileUniProvider;
  VMapType: IMapType;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
  VMapVersion: IMapVersionRequest;
  VTileStorage: ITileStorage;
  VBlankTile: IBinaryData;
begin
  inherited;

  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VBitmapProvider := (ParamsFrame as IRegionProcessParamsFrameImageProvider).Provider;

  VTileStorage := nil;
  VMapVersion := nil;
  if Assigned(VMapType) then begin
    VMapVersion := VMapType.VersionRequest.GetStatic;
    VTileStorage := VMapType.TileStorage;
  end;

  with (ParamsFrame as IRegionProcessParamsFrameOruxMapsSQLiteExport) do begin
    VDirectTilesCopy := DirectTilesCopy;
    VBlankTile := BlankTile;
    VBitmapTileSaver := BitmapTileSaver;
  end;

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadExportToOruxMapsSQLite.Create(
      VProgressInfo,
      VPath,
      FVectorGeometryProjectedFactory,
      FProjectionSetFactory,
      APolygon,
      VZoomArr,
      VTileStorage,
      VMapVersion,
      VBitmapTileSaver,
      VBitmapProvider,
      VBlankTile,
      VDirectTilesCopy
    );
  VThread.Resume;
end;

end.
