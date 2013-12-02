{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit fr_ExportRMapsSQLite;

interface

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  i_LanguageManager,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_VectorItemLonLat,
  i_RegionProcessParamsFrame,
  i_Bitmap32StaticFactory,
  i_BitmapLayerProvider,
  i_BitmapTileSaveLoad,
  i_BitmapTileSaveLoadFactory,
  u_MapType,
  fr_MapSelect,
  fr_ZoomsSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameRMapsSQLiteExport = interface(IRegionProcessParamsFrameBase)
    ['{AF048EAA-5AE3-45CD-94FD-443DFCB580B6}']
    function GetForceDropTarget: Boolean;
    property ForceDropTarget: Boolean read GetForceDropTarget;

    function GetReplaceExistingTiles: Boolean;
    property ReplaceExistingTiles: Boolean read GetReplaceExistingTiles;

    function GetDirectTilesCopy: Boolean;
    property DirectTilesCopy: Boolean read GetDirectTilesCopy;

    function GetBitmapTileSaver: IBitmapTileSaver;
    property BitmapTileSaver: IBitmapTileSaver read GetBitmapTileSaver;
  end;

type
  TfrExportRMapsSQLite = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameImageProvider,
      IRegionProcessParamsFrameRMapsSQLiteExport
    )
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    pnlMain: TPanel;
    chkReplaceExistingTiles: TCheckBox;
    chkForceDropTarget: TCheckBox;
    lblMap: TLabel;
    dlgSaveSQLite: TSaveDialog;
    pnlMap: TPanel;
    PnlZoom: TPanel;
    chkDirectTilesCopy: TCheckBox;
    lblOverlay: TLabel;
    pnlOverlay: TPanel;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkDirectTilesCopyClick(Sender: TObject);
  private
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FBitmapFactory: IBitmap32StaticFactory;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    FfrMapSelect: TfrMapSelect;
    FfrOverlaySelect: TfrMapSelect;
    FfrZoomsSelect: TfrZoomsSelect;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: ILonLatPolygon
    );
    function Validate: Boolean;
  private
    function GetMapType: TMapType;
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
    function GetForceDropTarget: Boolean;
    function GetReplaceExistingTiles: Boolean;
    function GetDirectTilesCopy: Boolean;
    function GetAllowExport(AMapType: TMapType): Boolean;
    function GetProvider: IBitmapLayerProvider;
    function GetBitmapTileSaver: IBitmapTileSaver;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  c_CoordConverter,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  u_BitmapLayerProviderMapWithLayer;

{$R *.dfm}

constructor TfrExportRMapsSQLite.Create(
  const ALanguageManager: ILanguageManager;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory
);
begin
  inherited Create(ALanguageManager);

  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;

  FBitmapFactory := ABitmapFactory;
  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;

  FfrMapSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfMaps,          // show maps
      True,            // add -NO- to combobox
      False,           // show disabled map
      GetAllowExport
    );

  FfrOverlaySelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfLayers,        // show layers
      True,            // add -NO- to combobox
      False,           // show disabled map
      GetAllowExport
    );

  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager
    );
  FfrZoomsSelect.Init(0, 23);
end;

destructor TfrExportRMapsSQLite.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrOverlaySelect);
  FreeAndNil(FfrZoomsSelect);
  inherited;
end;

procedure TfrExportRMapsSQLite.chkDirectTilesCopyClick(Sender: TObject);
begin
  FfrOverlaySelect.cbbMap.Enabled := not chkDirectTilesCopy.Checked;
end;

procedure TfrExportRMapsSQLite.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveSQLite.Execute then begin
    edtTargetFile.Text := dlgSaveSQLite.FileName;
  end;
end;

function TfrExportRMapsSQLite.GetAllowExport(AMapType: TMapType): Boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

function TfrExportRMapsSQLite.GetMapType: TMapType;
var
  VMapType: TMapType;
begin
  VMapType := FfrMapSelect.GetSelectedMapType;
  if not Assigned(VMapType) then begin
    VMapType := FfrOverlaySelect.GetSelectedMapType;
  end;
  Result := VMapType;
end;

function TfrExportRMapsSQLite.GetForceDropTarget: Boolean;
begin
  Result := chkForceDropTarget.Checked;
end;

function TfrExportRMapsSQLite.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrExportRMapsSQLite.GetReplaceExistingTiles: Boolean;
begin
  Result := chkReplaceExistingTiles.Checked;
end;

function TfrExportRMapsSQLite.GetDirectTilesCopy: Boolean;
var
  VMap: TMapType;
  VLayer: TMapType;
begin
  Result := chkDirectTilesCopy.Checked;
  if not Result then begin
    VMap := FfrMapSelect.GetSelectedMapType;
    VLayer := FfrOverlaySelect.GetSelectedMapType;
    if Assigned(VMap) and not Assigned(VLayer) then begin
      if VMap.IsBitmapTiles then begin
        if VMap.GeoConvert.ProjectionEPSG = CGoogleProjectionEPSG then begin
          Result := True;
        end;
      end;
    end else if not Assigned(VMap) and Assigned(VLayer) then begin
      if VLayer.IsBitmapTiles then begin
        if VLayer.GeoConvert.ProjectionEPSG = CGoogleProjectionEPSG then begin
          Result := True;
        end;
      end;
    end;
  end;
end;

function TfrExportRMapsSQLite.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

function TfrExportRMapsSQLite.GetProvider: IBitmapLayerProvider;
var
  VMap: TMapType;
  VMapVersion: IMapVersionInfo;
  VLayer: TMapType;
  VLayerVersion: IMapVersionInfo;
begin
  VMap := FfrMapSelect.GetSelectedMapType;
  if Assigned(VMap) then begin
    VMapVersion := VMap.VersionConfig.Version;
  end else begin
    VMapVersion := nil;
  end;

  VLayer := FfrOverlaySelect.GetSelectedMapType;
  if Assigned(VLayer) then begin
    VLayerVersion := VLayer.VersionConfig.Version;
  end else begin
    VLayerVersion := nil;
  end;

  Result :=
    TBitmapLayerProviderMapWithLayer.Create(
      FBitmapFactory,
      VMap,
      VMapVersion,
      VLayer,
      VLayerVersion,
      True,
      True
    );
end;

function TfrExportRMapsSQLite.GetBitmapTileSaver: IBitmapTileSaver;

  function _GetSaver(const AMap: TMapType): IBitmapTileSaver;
  var
    VContentType: IContentTypeInfoBitmap;
  begin
    Result := nil;
    if Assigned(AMap) then begin
      if Supports(AMap.ContentType, IContentTypeInfoBitmap, VContentType) then begin
        Result := VContentType.GetSaver;
      end;
    end;
  end;

begin
  Result := _GetSaver(FfrMapSelect.GetSelectedMapType);

  if not Assigned(Result) then begin 
    Result := _GetSaver(FfrOverlaySelect.GetSelectedMapType);
  end;

  if not Assigned(Result) then begin
    // похоже, что собираемся экспортировать векторный слой
    Result := FBitmapTileSaveLoadFactory.CreatePngSaver;
  end;
end;

procedure TfrExportRMapsSQLite.Init;
begin
  FfrMapSelect.Show(pnlMap);
  FfrOverlaySelect.Show(pnlOverlay);
  FfrZoomsSelect.Show(pnlZoom);
  chkDirectTilesCopyClick(chkDirectTilesCopy);
end;

function TfrExportRMapsSQLite.Validate: Boolean;
begin
  Result := FfrZoomsSelect.Validate;
  if not Result then begin
    ShowMessage(_('Please select at least one zoom'));
  end;
end;

end.
