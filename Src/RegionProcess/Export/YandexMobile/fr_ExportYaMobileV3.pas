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

unit fr_ExportYaMobileV3;

interface

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Spin,
  ExtCtrls,
  i_LanguageManager,
  i_GeometryLonLat,
  i_MapType,
  i_RegionProcessParamsFrame,
  fr_MapSelect,
  fr_ZoomsSelect,
  u_CommonFormAndFrameParents;

type
  TfrExportYaMobileV3 = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath
    )
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetPath: TLabel;
    edtTargetPath: TEdit;
    btnSelectTargetPath: TButton;
    chkReplaseTiles: TCheckBox;
    pnlZoom: TPanel;
    pnlMapsSelect: TPanel;
    grdpnlMaps: TGridPanel;
    lblMapCompress: TLabel;
    seMapCompress: TSpinEdit;
    seSatCompress: TSpinEdit;
    lblSatCompress: TLabel;
    lblCompress: TLabel;
    lblHybr: TLabel;
    lblMap: TLabel;
    lblSat: TLabel;
    lblMaps: TLabel;
    pnlHyb: TPanel;
    pnlMap: TPanel;
    pnlSat: TPanel;
    procedure btnSelectTargetPathClick(Sender: TObject);
  private
    FfrSatSelect: TfrMapSelect;
    FfrMapSelect: TfrMapSelect;
    FfrHybSelect: TfrMapSelect;
    FfrZoomsSelect: TfrZoomsSelect;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
    function GetAllowExport(const AMapType: IMapType): boolean;
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
  public
    function GetSat(): TfrMapSelect;
    function GetMap(): TfrMapSelect;
    function GetHyb(): TfrMapSelect;
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs,
  gnugettext,
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl;
  {$WARN UNIT_PLATFORM ON}

{$R *.dfm}

constructor TfrExportYaMobileV3.Create(
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder
);
begin
  inherited Create(ALanguageManager);
  FfrSatSelect :=
    AMapSelectFrameBuilder.Build(
      mfMaps, // show maps and layers
      True,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowExport
    );
  FfrMapSelect :=
    AMapSelectFrameBuilder.Build(
      mfMaps, // show maps and layers
      True,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowExport
    );
  FfrHybSelect :=
    AMapSelectFrameBuilder.Build(
      mfLayers, // show maps and layers
      True,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowExport
    );
  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager
    );
  FfrZoomsSelect.Init(0, 23);
end;

destructor TfrExportYaMobileV3.Destroy;
begin
  FreeAndNil(FfrSatSelect);
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrHybSelect);
  FreeAndNil(FfrZoomsSelect);
  inherited;
end;

function TfrExportYaMobileV3.GetAllowExport(const AMapType: IMapType): boolean;
begin
  Result := AMapType.IsBitmapTiles;
end;

procedure TfrExportYaMobileV3.btnSelectTargetPathClick(Sender: TObject);
var
  TempPath: string;
begin
  if SelectDirectory('', '', TempPath) then begin
    edtTargetPath.Text := IncludeTrailingPathDelimiter(TempPath);
  end;
end;

function TfrExportYaMobileV3.GetSat: TfrMapSelect;
begin
  Result := FfrSatSelect;
end;

function TfrExportYaMobileV3.GetMap: TfrMapSelect;
begin
  Result := FfrMapSelect;
end;

function TfrExportYaMobileV3.GetHyb: TfrMapSelect;
begin
  Result := FfrHybSelect;
end;

function TfrExportYaMobileV3.GetPath: string;
begin
  Result := IncludeTrailingPathDelimiter(edtTargetPath.Text);
end;

function TfrExportYaMobileV3.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

procedure TfrExportYaMobileV3.Init;
begin
  FfrSatSelect.Show(pnlSat);
  FfrMapSelect.Show(pnlMap);
  FfrHybSelect.Show(pnlHyb);
  FfrZoomsSelect.Show(pnlZoom);
end;

function TfrExportYaMobileV3.Validate: Boolean;
begin
  Result := FfrZoomsSelect.Validate;
  if not Result then begin
    ShowMessage(_('Please select at least one zoom'));
  end;
end;

end.
