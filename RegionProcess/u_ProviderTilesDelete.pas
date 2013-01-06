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

unit u_ProviderTilesDelete;

interface

uses
  Windows,
  Forms,
  i_Notifier,
  i_NotifierTime,
  i_NotifierOperation,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_VectorItemLonLat,
  u_ExportProviderAbstract,
  fr_TilesDelete;

type
  TProviderTilesDelete = class(TExportProviderAbstract)
  private
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItemsFactory: IVectorItemsFactory;
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItemsFactory: IVectorItemsFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  SysUtils,
  i_RegionProcessParamsFrame,
  i_PredicateByTileInfo,
  u_Notifier,
  u_NotifierOperation,
  u_RegionProcessProgressInfo,
  i_VectorItemProjected,
  u_ThreadDeleteTiles,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TProviderTilesDelete }

constructor TProviderTilesDelete.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory
);
begin
  inherited Create(
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FProjectionFactory := AProjectionFactory;
  FVectorItemsFactory := AVectorItemsFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TProviderTilesDelete.CreateFrame: TFrame;
begin
  Result :=
    TfrTilesDelete.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameOneZoom));
  Assert(Supports(Result, IRegionProcessParamsFrameProcessPredicate));
end;

function TProviderTilesDelete.GetCaption: string;
begin
  Result := SAS_STR_OperationDeleteCaption;
end;

procedure TProviderTilesDelete.StartProcess(const APolygon: ILonLatPolygon);
var
  VMapType: TMapType;
  VZoom: byte;
  VProjectedPolygon: IProjectedPolygon;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
  VPredicate: IPredicateByTileInfo;
begin
  inherited;
  if (Application.MessageBox(pchar(SAS_MSG_DeleteTilesInRegionAsk), pchar(SAS_MSG_coution), 36) = IDYES) then begin
    VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
    VZoom := (ParamsFrame as IRegionProcessParamsFrameOneZoom).Zoom;
    VPredicate := (ParamsFrame as IRegionProcessParamsFrameProcessPredicate).Predicate;

    VProjectedPolygon :=
      FVectorItemsFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(VMapType.GeoConvert, VZoom),
        APolygon
      );

    VCancelNotifierInternal := TNotifierOperation.Create(TNotifierBase.Create);
    VOperationID := VCancelNotifierInternal.CurrentOperation;
    VProgressInfo := TRegionProcessProgressInfo.Create;

    TfrmProgressSimple.Create(
      Application,
      FAppClosingNotifier,
      FTimerNoifier,
      VCancelNotifierInternal,
      VProgressInfo
    );

    TThreadDeleteTiles.Create(
      VCancelNotifierInternal,
      VOperationID,
      VProgressInfo,
      APolygon,
      VProjectedPolygon,
      VZoom,
      VMapType,
      VMapType.VersionConfig.Version,
      VPredicate
    );
  end;
end;

end.


