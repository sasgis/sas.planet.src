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
  Controls,
  Forms,
  i_JclNotify,
  i_LanguageManager,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_VectorItemLonLat,
  u_ExportProviderAbstract,
  fr_TilesDelete;

type
  TProviderTilesDelete = class(TExportProviderAbstract)
  private
    FFrame: TfrTilesDelete;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;
    FAppClosingNotifier: IJclNotifier;
    FTimerNoifier: IJclNotifier;
  protected
    function CreateFrame: TFrame; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: IJclNotifier;
      const ATimerNoifier: IJclNotifier;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItmesFactory: IVectorItmesFactory
    );
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: ILonLatPolygon); override;
  end;


implementation

uses
  SysUtils,
  i_RegionProcessProgressInfo,
  i_RegionProcessParamsFrame,
  u_OperationNotifier,
  u_RegionProcessProgressInfo,
  i_VectorItemProjected,
  u_ThreadDeleteTiles,
  u_ResStrings,
  u_MapType,
  frm_ProgressSimple;

{ TProviderTilesDelete }

constructor TProviderTilesDelete.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: IJclNotifier;
  const ATimerNoifier: IJclNotifier;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItmesFactory: IVectorItmesFactory
);
begin
  inherited Create(
    ALanguageManager,
    AMainMapsConfig,
    AFullMapsSet,
    AGUIConfigList
  );
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
end;

function TProviderTilesDelete.CreateFrame: TFrame;
begin
  FFrame :=
    TfrTilesDelete.Create(
      Self.LanguageManager,
      Self.MainMapsConfig,
      Self.FullMapsSet,
      Self.GUIConfigList
    );
  Result := FFrame;
end;

function TProviderTilesDelete.GetCaption: string;
begin
  Result := SAS_STR_OperationDeleteCaption;
end;

procedure TProviderTilesDelete.StartProcess(const APolygon: ILonLatPolygon);
var
  VMapType: TMapType;
  VDelBySize: Boolean;
  VDelSize: Cardinal;
  VZoom: byte;
  VProjectedPolygon: IProjectedPolygon;
  VCancelNotifierInternal: IOperationNotifierInternal;
  VOperationID: Integer;
  VProgressInfo: TRegionProcessProgressInfo;
  VForAttachments: Boolean;
begin
  inherited;
  if (MessageBox(FFrame.handle, pchar(SAS_MSG_youasure), pchar(SAS_MSG_coution), 36) = IDYES) then begin

    // selected map
    VMapType := TMapType(FFrame.cbbMap.Items.Objects[FFrame.cbbMap.ItemIndex]);
    VForAttachments := (not AnsiSameText(FFrame.cbbMap.Items[FFrame.cbbMap.ItemIndex], VMapType.GUIConfig.Name.Value));

    // selected zoom
    if FFrame.cbbZoom.ItemIndex < 0 then begin
      FFrame.cbbZoom.ItemIndex := 0;
    end;

    // del only with specified size
    VDelSize := 0;
    VDelBySize := FFrame.chkDelBySize.Checked;
    if VDelBySize then begin
      VDelSize := FFrame.seDelSize.Value;
    end;
    VZoom := FFrame.cbbZoom.ItemIndex;

    VProjectedPolygon :=
      FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(VMapType.GeoConvert, VZoom),
        APolygon
      );

    VCancelNotifierInternal := TOperationNotifier.Create;
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
      FFrame.cbbZoom.ItemIndex,
      VMapType,
      VDelBySize,
      VDelSize,
      VForAttachments
    );
  end;
end;

end.
