{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit fr_ExportKml;

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
  i_GeometryLonLat,
  i_MapType,
  i_TileStorageTypeList,
  i_TileFileNameGenerator,
  i_TileFileNameGeneratorsList,
  i_RegionProcessParamsFrame,
  fr_MapSelect,
  fr_ZoomsSelect,
  fr_CacheTypeList,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameKmlExport = interface(IRegionProcessParamsFrameBase)
    ['{B2DFB5AD-EAD9-4F36-81F1-87A3D2F1A5B0}']
    function GetNotSaveNotExists: Boolean;
    property NotSaveNotExists: Boolean read GetNotSaveNotExists;

    function GetRelativePath: Boolean;
    property RelativePath: Boolean read GetRelativePath;

    function GetExtractTilesFromStorage: Boolean;
    property ExtractTilesFromStorage: Boolean read GetExtractTilesFromStorage;

    function GetTileFileNameGenerator: ITileFileNameGenerator;
    property TileFileNameGenerator: ITileFileNameGenerator read GetTileFileNameGenerator;
  end;

type
  TfrExportKml = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameKmlExport
    )
    pnlCenter: TPanel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    pnlZoom: TPanel;
    pnlMain: TPanel;
    chkNotSaveNotExists: TCheckBox;
    chkUseRelativePath: TCheckBox;
    lblMap: TLabel;
    dlgSaveKML: TSaveDialog;
    pnlMap: TPanel;
    chkExtractTiles: TCheckBox;
    pnlFileNameGenerator: TPanel;
    lblFileNameGenerator: TLabel;
    procedure btnSelectTargetFileClick(Sender: TObject);
    procedure chkExtractTilesClick(Sender: TObject);
  private
    FfrMapSelect: TfrMapSelect;
    FfrZoomsSelect: TfrZoomsSelect;
    FfrCacheTypeList: TfrCacheTypeList;
    FTileNameGeneratorList: ITileFileNameGeneratorsList;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  private
    function GetMapType: IMapType;
    function GetZoomArray: TByteDynArray;
    function GetPath: string;
    function GetNotSaveNotExists: Boolean;
    function GetRelativePath: Boolean;
    function GetExtractTilesFromStorage: Boolean;
    function GetTileFileNameGenerator: ITileFileNameGenerator;
    function GetAllowExport(const AMapType: IMapType): Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ATileNameGeneratorList: ITileFileNameGeneratorsList
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  i_TileStorageAbilities,
  u_FileSystemFunc;

{$R *.dfm}

constructor TfrExportKml.Create(
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ATileNameGeneratorList: ITileFileNameGeneratorsList
);
begin
  inherited Create(ALanguageManager);
  FfrMapSelect :=
    AMapSelectFrameBuilder.Build(
      mfAll, // show maps and layers
      False,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowExport
    );

  FfrZoomsSelect :=
    TfrZoomsSelect.Create(
      ALanguageManager
    );
  FfrZoomsSelect.Init(0, 23);

  FfrCacheTypeList :=
    TfrCacheTypeList.Create(
      ALanguageManager,
      ATileStorageTypeList,
      False,
      [tstcInSeparateFiles],
      [tsacAdd]
    );

  FTileNameGeneratorList := ATileNameGeneratorList;
end;

destructor TfrExportKml.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrZoomsSelect);
  FreeAndNil(FfrCacheTypeList);
  inherited Destroy;
end;

function TfrExportKml.GetAllowExport(const AMapType: IMapType): Boolean;
begin
  Result :=
    (AMapType.IsBitmapTiles) and
    (chkExtractTiles.Checked or (AMapType.TileStorage.StorageTypeAbilities.StorageClass = tstcInSeparateFiles));
end;

procedure TfrExportKml.chkExtractTilesClick(Sender: TObject);
begin
  FfrMapSelect.SetEnabled(True); // force refresh maps list
  SetControlEnabled(pnlFileNameGenerator, chkExtractTiles.Checked);
end;

function TfrExportKml.GetExtractTilesFromStorage: Boolean;
begin
  Result := chkExtractTiles.Checked;
end;

procedure TfrExportKml.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveKML.Execute then begin
    edtTargetFile.Text := dlgSaveKML.FileName;
  end;
end;

function TfrExportKml.GetMapType: IMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrExportKml.GetNotSaveNotExists: Boolean;
begin
  Result := chkNotSaveNotExists.Checked;
end;

function TfrExportKml.GetPath: string;
begin
  Result := Trim(edtTargetFile.Text);
end;

function TfrExportKml.GetRelativePath: Boolean;
begin
  Result := chkUseRelativePath.Checked;
end;

function TfrExportKml.GetTileFileNameGenerator: ITileFileNameGenerator;
begin
  Result := FTileNameGeneratorList.GetGenerator(FfrCacheTypeList.IntCode);
end;

function TfrExportKml.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

procedure TfrExportKml.Init;
begin
  FfrMapSelect.Show(pnlMap);
  FfrZoomsSelect.Show(pnlZoom);
  FfrCacheTypeList.Show(pnlFileNameGenerator);
  SetControlEnabled(pnlFileNameGenerator, chkExtractTiles.Checked);
end;

function TfrExportKml.Validate: Boolean;
begin
  Result := False;

  if not IsValidFileName(edtTargetFile.Text) then begin
    ShowMessage(_('Output file name is not set or incorrect!'));
    Exit;
  end;

  if not FfrZoomsSelect.Validate then begin
    ShowMessage(_('Please select at least one zoom'));
    Exit;
  end;

  if FfrMapSelect.GetSelectedMapType = nil then begin
    ShowMessage(_('Please select the map first!'));
    Exit;
  end;

  Result := True;
end;

end.
