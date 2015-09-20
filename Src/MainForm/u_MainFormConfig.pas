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
  i_ViewProjectionConfig,
  i_MarksExplorerConfig,
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
    FMainMapConfig: IActiveMapConfig;
    FMapLayersConfig: IActiveLayersConfig;
    FDownloadUIConfig: IDownloadUIConfig;
    FKeyMovingConfig: IKeyMovingConfig;
    FMapZoomingConfig: IMapZoomingConfig;
    FMapMovingConfig: IMapMovingConfig;
    FMarksExplorerConfig: IMarksExplorerConfig;
    FViewProjectionConfig: IViewProjectionConfig;
  private
    function GetMainConfig: IMainFormMainConfig;
    function GetLayersConfig: IMainFormLayersConfig;
    function GetToolbarsLock: IMainWindowToolbarsLock;
    function GetNavToPoint: INavigationToPoint;
    function GetGPSBehaviour: IMainFormBehaviourByGPSConfig;
    function GetSearchHistory: IStringHistory;
    function GetMainGeoCoderConfig: IMainGeoCoderConfig;
    function GetMainMapConfig: IActiveMapConfig;
    function GetMapLayersConfig: IActiveLayersConfig;
    function GetDownloadUIConfig: IDownloadUIConfig;
    function GetKeyMovingConfig: IKeyMovingConfig;
    function GetMapZoomingConfig: IMapZoomingConfig;
    function GetMapMovingConfig: IMapMovingConfig;
    function GetMarksExplorerConfig: IMarksExplorerConfig;
    function GetViewProjectionConfig: IViewProjectionConfig;
  public
    constructor Create(
      const ADefaultMapGUID: TGUID
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ConfigSaveLoadStrategyBasicUseProvider,
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
  u_ActiveMapConfig,
  u_ActiveLayersConfig,
  u_MainFormMainConfig,
  u_ViewProjectionConfig,
  u_MarksExplorerConfig;

{ TMainFormConfig }

constructor TMainFormConfig.Create(
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
  FMainMapConfig := TActiveMapConfig.Create(False, ADefaultMapGUID);
  Add(FMainMapConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Maps'));
  FMapLayersConfig := TActiveLayersConfig.Create;
  Add(FMapLayersConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Maps'));
  FLayersConfig := TMainFormLayersConfig.Create;
  Add(FLayersConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
  FDownloadUIConfig := TDownloadUIConfig.Create;
  Add(FDownloadUIConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('ViewDownload'));
  FKeyMovingConfig := TKeyMovingConfig.Create;
  Add(FKeyMovingConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('KeyMoving'));
  FMapZoomingConfig := TMapZoomingConfig.Create;
  Add(FMapZoomingConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Zooming'));
  FMapMovingConfig := TMapMovingConfig.Create;
  Add(FMapMovingConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MouseMoving'));
  FMarksExplorerConfig := TMarksExplorerConfig.Create;
  Add(FMarksExplorerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('MarksExplorer'));
  FViewProjectionConfig := TViewProjectionConfig.Create;
  Add(FViewProjectionConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('ViewProjection'));
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

function TMainFormConfig.GetMainMapConfig: IActiveMapConfig;
begin
  Result := FMainMapConfig;
end;

function TMainFormConfig.GetMapZoomingConfig: IMapZoomingConfig;
begin
  Result := FMapZoomingConfig;
end;

function TMainFormConfig.GetMarksExplorerConfig: IMarksExplorerConfig;
begin
  Result := FMarksExplorerConfig;
end;

function TMainFormConfig.GetMapLayersConfig: IActiveLayersConfig;
begin
  Result := FMapLayersConfig;
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

function TMainFormConfig.GetViewProjectionConfig: IViewProjectionConfig;
begin
  Result := FViewProjectionConfig;
end;

end.
