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

unit u_MainFormConfig;

interface

uses
  i_MapTypeSet,
  i_MapTypeSetBuilder,
  i_ActiveMapsConfig,
  i_NavigationToPoint,
  i_MainFormConfig,
  i_MainFormBehaviourByGPSConfig,
  i_StringHistory,
  i_MainGeoCoderConfig,
  i_MainFormLayersConfig,
  i_KeyMovingConfig,
  i_MapMovingConfig,
  i_MapZoomingConfig,
  i_DownloadUIConfig,
  i_WindowPositionConfig,
  u_ConfigDataElementComplexBase;

type
  TMainFormConfig = class(TConfigDataElementComplexBase, IMainFormConfig)
  private
    FMainConfig: IMainFormMainConfig;
    FLayersConfig: IMainFormLayersConfig;
    FToolbarsLock: IMainWindowToolbarsLock;
    FNavToPoint: INavigationToPoint;
    FGPSBehaviour: IMainFormBehaviourByGPSConfig;
    FSearchHistory: IStringHistory;
    FMainGeoCoderConfig: IMainGeoCoderConfig;
    FMainMapsConfig: IMainMapsConfig;
    FDownloadUIConfig: IDownloadUIConfig;
    FKeyMovingConfig: IKeyMovingConfig;
    FMapZoomingConfig: IMapZoomingConfig;
    FMapMovingConfig: IMapMovingConfig;
    FMarksExplorerWindowConfig: IWindowPositionConfig;
  private
    function GetMainConfig: IMainFormMainConfig;
    function GetLayersConfig: IMainFormLayersConfig;
    function GetToolbarsLock: IMainWindowToolbarsLock;
    function GetNavToPoint: INavigationToPoint;
    function GetGPSBehaviour: IMainFormBehaviourByGPSConfig;
    function GetSearchHistory: IStringHistory;
    function GetMainGeoCoderConfig: IMainGeoCoderConfig;
    function GetMainMapsConfig: IMainMapsConfig;
    function GetDownloadUIConfig: IDownloadUIConfig;
    function GetKeyMovingConfig: IKeyMovingConfig;
    function GetMapZoomingConfig: IMapZoomingConfig;
    function GetMapMovingConfig: IMapMovingConfig;
    function GetMarksExplorerWindowConfig: IWindowPositionConfig;
  public
    constructor Create(
      const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
      const AMapsSet, ALayersSet: IMapTypeSet;
      const ADefaultMapGUID: TGUID
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_MainMapsConfig,
  u_MainWindowToolbarsLock,
  u_NavigationToPoint,
  u_MainFormLayersConfig,
  u_MainFormBehaviourByGPSConfig,
  u_StringHistory,
  u_MainGeoCoderConfig,
  u_MapMovingConfig,
  u_MapZoomingConfig,
  u_DownloadUIConfig,
  u_KeyMovingConfig,
  u_MainFormMainConfig,
  u_WindowPositionConfig;

{ TMainFormConfig }

constructor TMainFormConfig.Create(
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const AMapsSet, ALayersSet: IMapTypeSet;
  const ADefaultMapGUID: TGUID
);
begin
  inherited Create;
  FMainConfig := TMainFormMainConfig.Create;
  Add(FMainConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('View'));
  FToolbarsLock := TMainWindowToolbarsLock.Create;
  Add(FToolbarsLock, TConfigSaveLoadStrategyBasicProviderSubItem.Create('PANEL'));
  FNavToPoint := TNavigationToPoint.Create;
  Add(FNavToPoint, TConfigSaveLoadStrategyBasicProviderSubItem.Create('NavToPoint'));
  FGPSBehaviour := TMainFormBehaviourByGPSConfig.Create;
  Add(FGPSBehaviour, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MainFormGPSEvents'));
  FMainGeoCoderConfig := TMainGeoCoderConfig.Create;
  Add(FMainGeoCoderConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('GeoCoder'));
  FSearchHistory := TStringHistory.Create;
  Add(FSearchHistory, TConfigSaveLoadStrategyBasicProviderSubItem.Create('History'));
  FMainMapsConfig := TMainMapsConfig.Create(AMapTypeSetBuilderFactory, AMapsSet, ALayersSet, ADefaultMapGUID);
  Add(FMainMapsConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Maps'));
  FLayersConfig := TMainFormLayersConfig.Create(AMapTypeSetBuilderFactory, FMainMapsConfig);
  Add(FLayersConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
  FDownloadUIConfig := TDownloadUIConfig.Create;
  Add(FDownloadUIConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('ViewDownload'));
  FKeyMovingConfig := TKeyMovingConfig.Create;
  Add(FKeyMovingConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('KeyMoving'));
  FMapZoomingConfig := TMapZoomingConfig.Create;
  Add(FMapZoomingConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Zooming'));
  FMapMovingConfig := TMapMovingConfig.Create;
  Add(FMapMovingConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MouseMoving'));
  FMarksExplorerWindowConfig := TWindowPositionConfig.Create;
  Add(FMarksExplorerWindowConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarksExplorerWindow'));
end;

function TMainFormConfig.GetDownloadUIConfig: IDownloadUIConfig;
begin
  Result := FDownloadUIConfig;
end;

function TMainFormConfig.GetGPSBehaviour: IMainFormBehaviourByGPSConfig;
begin
  Result := FGPSBehaviour;
end;

function TMainFormConfig.GetKeyMovingConfig: IKeyMovingConfig;
begin
  Result := FKeyMovingConfig;
end;

function TMainFormConfig.GetLayersConfig: IMainFormLayersConfig;
begin
  Result := FLayersConfig;
end;

function TMainFormConfig.GetMainConfig: IMainFormMainConfig;
begin
  Result := FMainConfig;
end;

function TMainFormConfig.GetMainGeoCoderConfig: IMainGeoCoderConfig;
begin
  Result := FMainGeoCoderConfig;
end;

function TMainFormConfig.GetMainMapsConfig: IMainMapsConfig;
begin
  Result := FMainMapsConfig;
end;

function TMainFormConfig.GetMapZoomingConfig: IMapZoomingConfig;
begin
  Result := FMapZoomingConfig;
end;

function TMainFormConfig.GetMarksExplorerWindowConfig: IWindowPositionConfig;
begin
  Result := FMarksExplorerWindowConfig;
end;

function TMainFormConfig.GetMapMovingConfig: IMapMovingConfig;
begin
  Result := FMapMovingConfig;
end;

function TMainFormConfig.GetNavToPoint: INavigationToPoint;
begin
  Result := FNavToPoint;
end;

function TMainFormConfig.GetSearchHistory: IStringHistory;
begin
  Result := FSearchHistory;
end;

function TMainFormConfig.GetToolbarsLock: IMainWindowToolbarsLock;
begin
  Result := FToolbarsLock;
end;

end.
