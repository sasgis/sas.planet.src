{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_ProviderTilesDownload;

interface

uses
  Forms,
  i_JclNotify,
  i_MapTypes,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_CoordConverterFactory,
  i_LanguageManager,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_ValueToStringConverter,
  i_GlobalDownloadConfig,
  i_DownloadInfoSimple,
  u_MapType,
  u_ExportProviderAbstract,
  fr_TilesDownload;

type
  TProviderTilesDownload = class(TExportProviderAbstract)
  private
    FAppClosingNotifier: INotifier;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    FDownloadConfig: IGlobalDownloadConfig;
    FDownloadInfo: IDownloadInfoSimple;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const AAppClosingNotifier: INotifier;
      const ALanguageManager: ILanguageManager;
      const AValueToStringConverterConfig: IValueToStringConverterConfig;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloadInfo: IDownloadInfoSimple
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
    procedure StartBySLS(const AFileName: string);
  end;


implementation

uses
  SysUtils,
  IniFiles,
  i_VectorItemProjected,
  i_ConfigDataProvider,
  i_RegionProcessParamsFrame,
  u_ConfigDataProviderByIniFile,
  u_LogForTaskThread,
  u_ThreadDownloadTiles,
  frm_ProgressDownload,
  u_ResStrings;

{ TProviderTilesDownload }

constructor TProviderTilesDownload.Create(
  const AAppClosingNotifier: INotifier;
  const ALanguageManager: ILanguageManager;
  const AValueToStringConverterConfig: IValueToStringConverterConfig;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloadInfo: IDownloadInfoSimple
);
begin
  inherited Create(
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FAppClosingNotifier := AAppClosingNotifier;
  FValueToStringConverterConfig := AValueToStringConverterConfig;
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FDownloadConfig := ADownloadConfig;
  FDownloadInfo := ADownloadInfo;
end;

function TProviderTilesDownload.CreateFrame: TFrame;
begin
  Result :=
    TfrTilesDownload.Create(
      Self.LanguageManager,
      FProjectionFactory,
      FVectorItmesFactory,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameOneZoom));
  Assert(Supports(Result, IRegionProcessParamsFrameTilesDownload));
end;

function TProviderTilesDownload.GetCaption: string;
begin
  Result := SAS_STR_OperationDownloadCaption;
end;

procedure TProviderTilesDownload.StartBySLS(const AFileName: string);
var
  VIni: TMemIniFile;
  VSLSData: IConfigDataProvider;
  VSessionSection: IConfigDataProvider;
  VLog: TLogSimpleProvider;
  VThread: TThreadDownloadTiles;
begin
  VIni := TMemIniFile.Create(AFileName);
  VSLSData := TConfigDataProviderByIniFile.Create(VIni);
  VSessionSection := VSLSData.GetSubItem('Session');
  VLog := TLogSimpleProvider.Create(5000, 0);
  VThread :=
    TThreadDownloadTiles.CreateFromSls(
      FAppClosingNotifier,
      FVectorItmesFactory,
      VLog,
      FullMapsSet,
      FProjectionFactory,
      VSessionSection,
      FDownloadConfig,
      FDownloadInfo
    );
  TfrmProgressDownload.Create(
    LanguageManager,
    FValueToStringConverterConfig,
    VThread,
    VLog
  );
end;

procedure TProviderTilesDownload.StartProcess(const APolygon: ILonLatPolygon);
var
  VMapType: TMapType;
  VZoom: byte;
  VLog: TLogSimpleProvider;
  VThread: TThreadDownloadTiles;
  VProjectedPolygon: IProjectedPolygon;
  VForAttachments: Boolean;
begin
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VZoom := (ParamsFrame as IRegionProcessParamsFrameOneZoom).Zoom;
  VForAttachments := (ParamsFrame as IRegionProcessParamsFrameTilesDownload).ForAttachments;

  VProjectedPolygon :=
    FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
      FProjectionFactory.GetByConverterAndZoom(VMapType.GeoConvert, VZoom),
      APolygon
    );
  VLog := TLogSimpleProvider.Create(5000, 0);
  VThread := TThreadDownloadTiles.Create(
    (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsStartPaused,
    FAppClosingNotifier,
    VLog,
    APolygon,
    VProjectedPolygon,
    FDownloadConfig,
    FDownloadInfo,
    (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplace,
    (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplaceIfDifSize,
    (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplaceIfOlder,
    (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsIgnoreTne,
    VZoom,
    VMapType,
    (ParamsFrame as IRegionProcessParamsFrameTilesDownload).ReplaceDate,
    VForAttachments
  );
  TfrmProgressDownload.Create(
    Self.LanguageManager,
    FValueToStringConverterConfig,
    VThread,
    VLog
  );
end;

end.

