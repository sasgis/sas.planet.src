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

unit fr_TilesDownload;

interface

uses
  Classes,
  Controls,
  ComCtrls,
  ExtCtrls,
  Forms,
  SysUtils,
  StdCtrls,
  Windows,
  i_MapTypes,
  i_MapTypeSet,
  i_CoordConverterFactory,
  i_LanguageManager,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_ActiveMapsConfig,
  i_MapTypeGUIConfigList,
  i_RegionProcessParamsFrame,
  fr_MapSelect,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameTilesDownload = interface(IRegionProcessParamsFrameBase)
    ['{70B48431-5383-4CD2-A1EF-AF9291F6ABB0}']
    function GetIsStartPaused: Boolean;
    property IsStartPaused: Boolean read GetIsStartPaused;

    function GetIsIgnoreTne: Boolean;
    property IsIgnoreTne: Boolean read GetIsIgnoreTne;

    function GetIsReplace: Boolean;
    property IsReplace: Boolean read GetIsReplace;

    function GetIsReplaceIfDifSize: Boolean;
    property IsReplaceIfDifSize: Boolean read GetIsReplaceIfDifSize;

    function GetIsReplaceIfOlder: Boolean;
    property IsReplaceIfOlder: Boolean read GetIsReplaceIfOlder;

    function GetReplaceDate: TDateTime;
    property ReplaceDate: TDateTime read GetReplaceDate;
  end;

type
  TfrTilesDownload = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameOneZoom,
      IRegionProcessParamsFrameTilesDownload
    )
    lblZoom: TLabel;
    lblStat: TLabel;
    chkReplace: TCheckBox;
    chkReplaceIfDifSize: TCheckBox;
    chkReplaceOlder: TCheckBox;
    dtpReplaceOlderDate: TDateTimePicker;
    cbbZoom: TComboBox;
    chkTryLoadIfTNE: TCheckBox;
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlMain: TPanel;
    pnlTileReplaceCondition: TPanel;
    pnlReplaceOlder: TPanel;
    lblReplaceOlder: TLabel;
    chkStartPaused: TCheckBox;
    pnlMapSelect: TPanel;
    pnlZoom: TPanel;
    lblMapCaption: TLabel;
    pnlFrame: TPanel;
    procedure chkReplaceClick(Sender: TObject);
    procedure chkReplaceOlderClick(Sender: TObject);
    procedure cbbZoomChange(Sender: TObject);
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FPolygLL: IGeometryLonLatMultiPolygon;
    FMainMapsConfig: IMainMapsConfig;
    FFullMapsSet: IMapTypeSet;
    FGUIConfigList: IMapTypeGUIConfigList;
    FfrMapSelect: TfrMapSelect;

  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatMultiPolygon
    );
    function Validate: Boolean;
  private
    function GetMapType: IMapType;
    function GetZoom: Byte;
  private
    function GetIsStartPaused: Boolean;
    function GetIsIgnoreTne: Boolean;
    function GetIsReplace: Boolean;
    function GetIsReplaceIfDifSize: Boolean;
    function GetIsReplaceIfOlder: Boolean;
    function GetReplaceDate: TDateTime;
    function GetAllowDownload(const AMapType: IMapType): boolean; // чисто для проверки
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AMainMapsConfig: IMainMapsConfig;
      const AFullMapsSet: IMapTypeSet;
      const AGUIConfigList: IMapTypeGUIConfigList
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  t_GeoTypes,
  i_GeometryProjected,
  u_GeoFunc,
  u_ResStrings;

{$R *.dfm}

procedure TfrTilesDownload.cbbZoomChange(Sender: TObject);
var
  numd:int64 ;
  Vmt: IMapType;
  VZoom: byte;
  VPolyLL: IGeometryLonLatMultiPolygon;
  VProjected: IGeometryProjectedPolygon;
  VBounds: TDoubleRect;
  VPixelRect: TRect;
  VTileRect: TRect;
begin
  Vmt := FfrMapSelect.GetSelectedMapType;
  if Vmt <> nil then begin
    VZoom := cbbZoom.ItemIndex;
    Vmt.GeoConvert.CheckZoom(VZoom);
    VPolyLL := FPolygLL;
    if VPolyLL <> nil then begin
      VProjected :=
        FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
          FProjectionFactory.GetByConverterAndZoom(Vmt.GeoConvert, VZoom),
          VPolyLL
        );
      if not VProjected.IsEmpty then begin
        VBounds := VProjected.Bounds;
        VPixelRect := RectFromDoubleRect(VBounds, rrOutside);
        VTileRect := Vmt.GeoConvert.PixelRect2TileRect(VPixelRect, VZoom);
        numd := (VTileRect.Right - VTileRect.Left);
        numd := numd * (VTileRect.Bottom - VTileRect.Top);
        lblStat.Caption :=
          SAS_STR_filesnum+': '+
          inttostr(VTileRect.Right - VTileRect.Left)+'x'+
          inttostr(VTileRect.Bottom - VTileRect.Top)+
          '('+inttostr(numd)+')' +
          ', '+SAS_STR_Resolution + ' ' +
          inttostr(VPixelRect.Right - VPixelRect.Left)+'x'+
          inttostr(VPixelRect.Bottom - VPixelRect.Top);
      end;
    end;
  end;
end;

destructor TfrTilesDownload.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  inherited;
end;

procedure TfrTilesDownload.chkReplaceClick(Sender: TObject);
var
  VEnabled: Boolean;
begin
  VEnabled := chkReplace.Checked;
  chkReplaceIfDifSize.Enabled := VEnabled;
  chkReplaceOlder.Enabled := VEnabled;
  chkReplaceOlderClick(chkReplaceOlder);
end;

procedure TfrTilesDownload.chkReplaceOlderClick(Sender: TObject);
begin
  dtpReplaceOlderDate.Enabled := chkReplaceOlder.Enabled and chkReplaceOlder.Checked;
end;

constructor TfrTilesDownload.Create(
  const ALanguageManager: ILanguageManager;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AMainMapsConfig: IMainMapsConfig;
  const AFullMapsSet: IMapTypeSet;
  const AGUIConfigList: IMapTypeGUIConfigList
);
begin
  inherited Create(ALanguageManager);
  FProjectionFactory := AProjectionFactory;
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FMainMapsConfig := AMainMapsConfig;
  FFullMapsSet := AFullMapsSet;
  FGUIConfigList := AGUIConfigList;
  FfrMapSelect :=
    TfrMapSelect.Create(
      ALanguageManager,
      AMainMapsConfig,
      AGUIConfigList,
      AFullMapsSet,
      mfAll, // show maps and layers
      false,  // add -NO- to combobox
      false,  // show disabled map
      GetAllowDownload
    );
end;

function TfrTilesDownload.GetAllowDownload(const AMapType: IMapType): boolean; // чисто для проверки
begin
   Result := (AMapType.StorageConfig.GetAllowAdd) and (AMapType.TileDownloadSubsystem.State.GetStatic.Enabled);
end;

function TfrTilesDownload.GetIsIgnoreTne: Boolean;
begin
  Result := chkTryLoadIfTNE.Checked
end;

function TfrTilesDownload.GetIsReplace: Boolean;
begin
  Result := chkReplace.Checked;
end;

function TfrTilesDownload.GetIsReplaceIfDifSize: Boolean;
begin
  Result := chkReplaceIfDifSize.Checked;
end;

function TfrTilesDownload.GetIsReplaceIfOlder: Boolean;
begin
  Result := chkReplaceOlder.Checked;
end;

function TfrTilesDownload.GetIsStartPaused: Boolean;
begin
  Result := chkStartPaused.Checked;
end;

function TfrTilesDownload.GetMapType: IMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrTilesDownload.GetReplaceDate: TDateTime;
begin
  Result := dtpReplaceOlderDate.DateTime;
end;

function TfrTilesDownload.GetZoom: Byte;
begin
  if cbbZoom.ItemIndex < 0 then begin
    cbbZoom.ItemIndex := 0;
  end;
  Result := cbbZoom.ItemIndex;
end;

procedure TfrTilesDownload.Init(const AZoom: Byte; const APolygon: IGeometryLonLatMultiPolygon);
var
  i: integer;
begin
  FPolygLL := APolygon;
  cbbZoom.Items.Clear;
  for i:=1 to 24 do begin
    cbbZoom.Items.Add(inttostr(i));
  end;
  cbbZoom.ItemIndex := AZoom;
  dtpReplaceOlderDate.Date:=now;
  cbbZoomChange(nil);
  FfrMapSelect.Show(pnlFrame);
  cbbZoomChange(cbbzoom);

end;

function TfrTilesDownload.Validate: Boolean;
begin
  Result := True;
end;

end.
