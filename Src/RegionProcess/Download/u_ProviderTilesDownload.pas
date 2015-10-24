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
  i_NotifierOperation,
  i_MapTypeSet,
  i_MapViewGoto,
  i_ActiveMapsConfig,
  i_RegionProcess,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_GeometryProjectedFactory,
  i_ProjectionSet,
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

  protected
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  private
    procedure StartBySLS(const AFileName: string);
    procedure ReadZoom(
      const AProjectionSet: IProjectionSet;
      const ASessionSection: IConfigDataProvider;
      out AZoom: Byte;
      out AZoomArr: TByteDynArray
    );
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
  IniFiles,
  i_MapType,
  i_RegionProcessParamsFrame,
  i_LogSimple,
  i_LogSimpleProvider,
  i_MapVersionInfo,
  i_MapVersionRequest,
  u_MapVersionRequest,
  u_ConfigDataProviderByIniFile,
  u_LogForTaskThread,
  u_ThreadDownloadTiles,
  u_ConfigProviderHelpers,
  u_RegionProcessProgressInfoDownload,
  u_Notifier,
  u_NotifierOperation,
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

procedure TProviderTilesDownload.ReadZoom(
  const AProjectionSet: IProjectionSet;
  const ASessionSection: IConfigDataProvider;
  out AZoom: Byte;
  out AZoomArr: TByteDynArray
);
  procedure CheckZoom(var AZoom: Byte);
  begin
    if AZoom > 0 then begin
      Dec(AZoom);
    end else begin
      raise Exception.Create('Unknown zoom: ' + IntToStr(AZoom));
    end;
    if not AProjectionSet.CheckZoom(AZoom) then begin
      raise Exception.Create('Unknown zoom: ' + IntToStr(AZoom));
    end;
  end;

var
  I: Integer;
begin
  AZoom := ASessionSection.ReadInteger('Zoom', 0);
  CheckZoom(AZoom);
  if not ZoomArrayFromStr(ASessionSection.ReadString('ZoomArr', ''), AZoomArr) then begin
    SetLength(AZoomArr, 1);
    AZoomArr[0] := AZoom + 1;
  end;
  for I := Low(AZoomArr) to High(AZoomArr) do begin
    CheckZoom(AZoomArr[I]);
  end;
  Assert(IsZoomInZoomArray(AZoom, AZoomArr));
end;

procedure TProviderTilesDownload.StartBySLS(const AFileName: string);
var
  VIniFile: TMemIniFile;
  VSLSData: IConfigDataProvider;
  VSessionSection: IConfigDataProvider;
  VLog: TLogSimpleProvider;
  VLogSimple: ILogSimple;
  VLogProvider: ILogSimpleProvider;
  VForm: TfrmProgressDownload;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfoDownload;
  VGuids: string;
  VGuid: TGUID;
  VZoom: Byte;
  VZoomArr: TByteDynArray;
  VReplaceExistTiles: Boolean;
  VCheckExistTileSize: Boolean;
  VCheckExistTileDate: Boolean;
  VCheckTileDate: TDateTime;
  VProcessedTileCount: Int64;
  VProcessedSize: Int64;
  VSecondLoadTNE: Boolean;
  VLastProcessedPoint: TPoint;
  VElapsedTime: TDateTime;
  VMapType: IMapType;
  VPolygon: IGeometryLonLatPolygon;
  VVersionForDownload: IMapVersionInfo;
  VVersionForCheck: IMapVersionRequest;
  VVersionString: string;
  VVersionCheckShowPrev: Boolean;
begin
  VIniFile := TMemIniFile.Create(AFileName);
  try
    VSLSData := TConfigDataProviderByIniFile.CreateWithOwn(VIniFile);
    VIniFile := nil;
  finally
    VIniFile.Free;
  end;
  VSessionSection := VSLSData.GetSubItem('Session');
  VLog := TLogSimpleProvider.Create(5000, 0);
  VLogSimple := VLog;
  VLogProvider := VLog;
  VCancelNotifierInternal :=
    TNotifierOperation.Create(
      TNotifierBase.Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifier'))
    );
  VOperationID := VCancelNotifierInternal.CurrentOperation;

  VReplaceExistTiles := False;
  VCheckExistTileSize := False;
  VCheckExistTileDate := False;
  VCheckTileDate := Now;
  VSecondLoadTNE := False;
  VElapsedTime := 0;
  VProcessedTileCount := 0;
  if VSessionSection = nil then begin
    raise Exception.Create('No SLS data');
  end;
  VGuids := VSessionSection.ReadString('MapGUID', '');
  if VGuids = '' then begin
    raise Exception.Create('Map GUID is empty');
  end;
  VGuid := StringToGUID(VGuids);
  VMapType := FFullMapsSet.GetMapTypeByGUID(VGuid);
  if VMapType = nil then begin
    raise Exception.CreateFmt('Map with GUID = %s not found', [VGuids]);
  end;
  VVersionString := VSessionSection.ReadString('VersionDownload', '');
  if VVersionString <> '' then begin
    VVersionForDownload :=
      VMapType.VersionFactory.GetStatic.CreateByStoreString(
        VVersionString
      );
  end else begin
    VVersionForDownload := VMapType.VersionRequest.GetStatic.BaseVersion;
  end;
  VVersionCheckShowPrev := VSessionSection.ReadBool('VersionCheckPrev', False);
  VVersionString := VSessionSection.ReadString('VersionCheck', '');
  if VVersionString <> '' then begin
    VVersionForCheck :=
      TMapVersionRequest.Create(
        VMapType.VersionFactory.GetStatic.CreateByStoreString(VVersionString),
        VVersionCheckShowPrev
      );
  end else begin
    VVersionForCheck :=
      TMapVersionRequest.Create(
        VVersionForDownload,
        VVersionCheckShowPrev
      );
  end;

  ReadZoom(VMapType.ProjectionSet, VSessionSection, VZoom, VZoomArr);

  VReplaceExistTiles := VSessionSection.ReadBool('ReplaceExistTiles', VReplaceExistTiles);
  VCheckExistTileSize := VSessionSection.ReadBool('CheckExistTileSize', VCheckExistTileSize);
  VCheckExistTileDate := VSessionSection.ReadBool('CheckExistTileDate', VCheckExistTileDate);
  VCheckTileDate := VSessionSection.ReadDate('CheckTileDate', VCheckTileDate);
  VProcessedTileCount := VSessionSection.ReadInteger('ProcessedTileCount', VProcessedTileCount);
  VProcessedSize := trunc(VSessionSection.ReadFloat('ProcessedSize', 0) * 1024);

  VSecondLoadTNE := VSessionSection.ReadBool('SecondLoadTNE', VSecondLoadTNE);
  VElapsedTime := VSessionSection.ReadFloat('ElapsedTime', VElapsedTime);
  if FDownloadConfig.IsUseSessionLastSuccess then begin
    VLastProcessedPoint.X := VSessionSection.ReadInteger('LastSuccessfulStartX', -1);
    VLastProcessedPoint.Y := VSessionSection.ReadInteger('LastSuccessfulStartY', -1);
  end else begin
    VLastProcessedPoint.X := VSessionSection.ReadInteger('StartX', -1);
    VLastProcessedPoint.Y := VSessionSection.ReadInteger('StartY', -1);
  end;

  VPolygon := ReadPolygon(VSessionSection, FVectorGeometryLonLatFactory);
  if not Assigned(VPolygon) then begin
    raise Exception.Create('Empty polygon');
  end;

  VProgressInfo :=
    TRegionProcessProgressInfoDownload.Create(
      VLogSimple,
      VLogProvider,
      VGuid,
      VVersionForCheck,
      VVersionForDownload,
      VZoom,
      VZoomArr,
      VPolygon,
      VSecondLoadTNE,
      VReplaceExistTiles,
      VCheckExistTileSize,
      VCheckExistTileDate,
      VCheckTileDate,
      False,
      VProcessedSize,
      VProcessedTileCount,
      VLastProcessedPoint,
      VElapsedTime
    );
  VForm := TfrmProgressDownload.Create(
    LanguageManager,
    FValueToStringConverter,
    VCancelNotifierInternal,
    VProgressInfo,
    VPolygon,
    'z' + ZoomArrayToStr(VZoomArr) + ' ' + VMapType.GUIConfig.Name.Value,
    FRegionProcess,
    FMapGoto,
    FMarkDBGUI,
    FMainConfig,
    VMapType
  );
  Application.ProcessMessages;
  VForm.Show;

  if not VCancelNotifierInternal.IsOperationCanceled(VOperationID) then begin
    TThreadDownloadTiles.Create(
      VCancelNotifierInternal,
      VOperationID,
      VProgressInfo,
      FAppClosingNotifier,
      VMapType,
      VVersionForCheck,
      VVersionForDownload,
      VPolygon,
      FVectorGeometryProjectedFactory,
      FDownloadConfig,
      TDownloadInfoSimple.Create(FDownloadInfo, VProcessedTileCount, VProcessedSize),
      VReplaceExistTiles,
      VCheckExistTileSize,
      VCheckExistTileDate,
      VCheckTileDate,
      VSecondLoadTNE,
      VZoomArr,
      VZoom,
      VLastProcessedPoint,
      VElapsedTime
    );
  end;
end;

procedure TProviderTilesDownload.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VMapType: IMapType;
  VZoom: byte;
  VZoomArr: TByteDynArray;
  VLog: TLogSimpleProvider;
  VLogSimple: ILogSimple;
  VLogProvider: ILogSimpleProvider;
  VForm: TfrmProgressDownload;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfoDownload;
  VThread: TThread;
begin
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VZoomArr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;

  VZoom := VZoomArr[0];

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
      VMapType.Zmp.GUID,
      VMapType.VersionRequest.GetStatic,
      VMapType.VersionRequest.GetStatic.BaseVersion,
      VZoom,
      VZoomArr,
      APolygon,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsIgnoreTne,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplace,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplaceIfDifSize,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplaceIfOlder,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).ReplaceDate,
      (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsStartPaused,
      0,
      0,
      Point(-1, -1),
      0
    );
  VForm := TfrmProgressDownload.Create(
    LanguageManager,
    FValueToStringConverter,
    VCancelNotifierInternal,
    VProgressInfo,
    APolygon,
    'z' + ZoomArrayToStr(VZoomArr) + ' ' + VMapType.GUIConfig.Name.Value,
    FRegionProcess,
    FMapGoto,
    FMarkDBGUI,
    FMainConfig,
    VMapType
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
        VMapType,
        VMapType.VersionRequest.GetStatic,
        VMapType.VersionRequest.GetStatic.BaseVersion,
        APolygon,
        FVectorGeometryProjectedFactory,
        FDownloadConfig,
        TDownloadInfoSimple.Create(FDownloadInfo),
        (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplace,
        (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplaceIfDifSize,
        (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsReplaceIfOlder,
        (ParamsFrame as IRegionProcessParamsFrameTilesDownload).ReplaceDate,
        (ParamsFrame as IRegionProcessParamsFrameTilesDownload).IsIgnoreTne,
        VZoomArr,
        VZoom,
        Point(-1, -1),
        0
      );
    VThread.Resume;
  end;
end;

end.
