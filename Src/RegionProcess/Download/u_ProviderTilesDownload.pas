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

unit u_ProviderTilesDownload;

interface

uses
  Types,
  Forms,
  i_DownloadSession,
  i_NotifierOperation,
  i_MapTypeSet,
  i_MapViewGoto,
  i_ActiveMapsConfig,
  i_RegionProcess,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_GeometryProjectedFactory,
  i_ConfigDataProvider,
  i_LanguageManager,
  i_ValueToStringConverter,
  i_GlobalDownloadConfig,
  i_DownloadInfoSimple,
  i_RegionProcessProgressInfoInternalFactory,
  i_RegionProcessProvider,
  u_ExportProviderAbstract,
  u_MarkDbGUIHelper,
  fr_MapSelect,
  fr_TilesDownload;

type
  IRegionProcessProviderDownload = interface(IRegionProcessProvider)
    ['{664082BF-E983-48E8-A554-C655E925C45E}']
    procedure StartBySLS(const AFileName: string);
  end;

  TProviderTilesDownload = class(TExportProviderAbstract, IRegionProcessProviderDownload)
  private
    FAppClosingNotifier: INotifierOneOperation;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FDownloadConfig: IGlobalDownloadConfig;
    FDownloadInfo: IDownloadInfoSimple;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FRegionProcess: IRegionProcess;
    FMapGoto: IMapViewGoto;
    FMarkDBGUI: TMarkDbGUIHelper;
    FFullMapsSet: IMapTypeSet;
    FMainConfig: IActiveMapConfig;
    procedure StartSession(
      const ASession: IDownloadSession;
      const ADownloadInfoSimple: IDownloadInfoSimple;
      const APaused: Boolean
    );
  protected
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  private
    procedure StartBySLS(const AFileName: string);
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AFullMapsSet: IMapTypeSet;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloadInfo: IDownloadInfoSimple;
      const ARegionProcess: IRegionProcess;
      const AMapGoto: IMapViewGoto;
      const AMarkDBGUI: TMarkDbGUIHelper;
      const AMainConfig: IActiveMapConfig
    );
  end;


implementation

uses
  Classes,
  SysUtils,
  Math,
  IniFiles,
  i_MapType,
  i_RegionProcessParamsFrame,
  i_LogSimple,
  i_LogSimpleProvider,
  i_MapVersionInfo,
  i_MapVersionRequest,
  u_ConfigDataProviderByIniFile,
  u_LogForTaskThread,
  u_ThreadDownloadTiles,
  u_RegionProcessProgressInfoDownload,
  u_Notifier,
  u_NotifierOperation,
  u_DownloadSession,
  u_DownloadInfoSimple,
  u_Synchronizer,
  u_ZoomArrayFunc,
  frm_ProgressDownload,
  u_ResStrings;

{ TProviderTilesDownload }

constructor TProviderTilesDownload.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AFullMapsSet: IMapTypeSet;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloadInfo: IDownloadInfoSimple;
  const ARegionProcess: IRegionProcess;
  const AMapGoto: IMapViewGoto;
  const AMarkDBGUI: TMarkDbGUIHelper;
  const AMainConfig: IActiveMapConfig
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder
  );
  FAppClosingNotifier := AAppClosingNotifier;
  FValueToStringConverter := AValueToStringConverter;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FFullMapsSet := AFullMapsSet;
  FDownloadConfig := ADownloadConfig;
  FDownloadInfo := ADownloadInfo;
  FRegionProcess := ARegionProcess;
  FMapGoto := AMapGoto;
  FMarkDBGUI := AMarkDBGUI;
  FMainConfig := AMainConfig;
end;

function TProviderTilesDownload.CreateFrame: TFrame;
begin
  Result :=
    TfrTilesDownload.Create(
      Self.LanguageManager,
      FVectorGeometryProjectedFactory,
      Self.MapSelectFrameBuilder
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameTilesDownload));
end;

function TProviderTilesDownload.GetCaption: string;
begin
  Result := SAS_STR_OperationDownloadCaption;
end;

procedure TProviderTilesDownload.StartSession(
  const ASession: IDownloadSession;
  const ADownloadInfoSimple: IDownloadInfoSimple;
  const APaused: Boolean
);
var
  VLog: TLogSimpleProvider;
  VLogSimple: ILogSimple;
  VLogProvider: ILogSimpleProvider;
  VForm: TfrmProgressDownload;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfoDownload;
  VThread: TThread;
begin
  VLog := TLogSimpleProvider.Create(5000, 0);
  VLogSimple := VLog;
  VLogProvider := VLog;

  VCancelNotifierInternal :=
    TNotifierOperation.Create(
      TNotifierBase.Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifier'))
    );

  VOperationID := VCancelNotifierInternal.CurrentOperation;

  VProgressInfo :=
    TRegionProcessProgressInfoDownload.Create(
      VLogSimple,
      VLogProvider,
      ASession,
      APaused
    );

  VForm := TfrmProgressDownload.Create(
    LanguageManager,
    FValueToStringConverter,
    VCancelNotifierInternal,
    VProgressInfo,
    ASession.Polygon,
    'z' + ZoomArrayToStr(ASession.ZoomArr) + ' ' + ASession.MapType.GUIConfig.Name.Value,
    FRegionProcess,
    FMapGoto,
    FMarkDBGUI,
    FMainConfig,
    ASession.MapType
  );
  Application.ProcessMessages;
  VForm.Show;

  if not VCancelNotifierInternal.IsOperationCanceled(VOperationID) then begin
    VThread :=
      TThreadDownloadTiles.Create(
        VCancelNotifierInternal,
        VOperationID,
        VProgressInfo,
        FAppClosingNotifier,
        ASession.MapType,
        ASession.VersionForCheck,
        ASession.VersionForDownload,
        ASession.Polygon,
        FVectorGeometryProjectedFactory,
        FDownloadConfig,
        ADownloadInfoSimple,
        ASession.ReplaceExistTiles,
        ASession.CheckExistTileSize,
        ASession.CheckExistTileDate,
        ASession.CheckTileDate,
        ASession.SecondLoadTNE,
        not IsNan(ASession.ReplaceTneOlderDate),
        ASession.ReplaceTneOlderDate,
        ASession.ZoomArr,
        ASession.Zoom,
        ASession.LastProcessedPoint,
        ASession.ElapsedTime
      );
    if not APaused then begin
      VThread.Resume;
    end;
  end;
end;

procedure TProviderTilesDownload.StartBySLS(const AFileName: string);
var
  VIniFile: TMemIniFile;
  VSLSData: IConfigDataProvider;
  VSessionSection: IConfigDataProvider;
  VSession: IDownloadSession;
begin
  VIniFile := TMemIniFile.Create(AFileName);
  try
    VSLSData := TConfigDataProviderByIniFile.CreateWithOwn(VIniFile);
    VIniFile := nil;
  finally
    VIniFile.Free;
  end;
  VSessionSection := VSLSData.GetSubItem('Session');

  VSession := TDownloadSession.Create;

  VSession.Load(
    VSessionSection,
    FFullMapsSet,
    FDownloadConfig,
    FVectorGeometryLonLatFactory
  );

  StartSession(
    VSession,
    TDownloadInfoSimple.Create(FDownloadInfo, VSession.DownloadedCount, VSession.DownloadedSize),
    True // start paused
  );
end;

procedure TProviderTilesDownload.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VMapType: IMapType;
  VZoomArr: TByteDynArray;
  VSession: IDownloadSession;
begin
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;

  VSession :=
    TDownloadSession.Create(
      VMapType,
      VMapType.VersionRequest.GetStatic,
      VMapType.VersionRequest.GetStatic.BaseVersion,
      VZoomArr[0],
      VZoomArr,
      APolygon,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsIgnoreTne,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).LoadTneOlderDate,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplace,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplaceIfDifSize,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplaceIfOlder,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).ReplaceDate
   );

  StartSession(
    VSession,
    TDownloadInfoSimple.Create(FDownloadInfo),
    (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsStartPaused
  );
end;

end.
