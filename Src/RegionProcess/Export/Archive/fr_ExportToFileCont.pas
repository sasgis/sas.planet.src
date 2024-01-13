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

unit fr_ExportToFileCont;

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
  UITypes,
  i_LanguageManager,
  i_GeometryLonLat,
  i_MapType,
  i_TileFileNameGenerator,
  i_TileFileNameGeneratorsList,
  i_TileStorageTypeList,
  i_ArchiveReadWriteConfig,
  i_RegionProcessParamsFrame,
  fr_MapSelect,
  fr_ZoomsSelect,
  fr_CacheTypeList,
  u_CommonFormAndFrameParents;

type
  IRegionProcessParamsFrameExportToFileCont = interface(IRegionProcessParamsFrameBase)
    ['{0DB6292A-DE1D-4437-A110-3439923ED4B0}']
    function GetNameGenerator: ITileFileNameGenerator;
    property NameGenerator: ITileFileNameGenerator read GetNameGenerator;

    function GetArchiveWriteConfig: IArchiveWriteConfig;
    property ArchiveWriteConfig: IArchiveWriteConfig read GetArchiveWriteConfig;
  end;

type
  TfrExportToFileCont = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTargetPath,
      IRegionProcessParamsFrameExportToFileCont
    )
    pnlCenter: TPanel;
    pnlZoom: TPanel;
    pnlMain: TPanel;
    lblMap: TLabel;
    pnlTop: TPanel;
    lblTargetFile: TLabel;
    edtTargetFile: TEdit;
    btnSelectTargetFile: TButton;
    dlgSaveTargetFile: TSaveDialog;
    lblNamesType: TLabel;
    pnlMap: TPanel;
    pnlCacheTypes: TPanel;
    pnlArchiveWriteConfig: TPanel;
    procedure btnSelectTargetFileClick(Sender: TObject);
  private
    FTileNameGeneratorList: ITileFileNameGeneratorsList;
    FfrMapSelect: TfrMapSelect;
    FfrZoomsSelect: TfrZoomsSelect;
    FfrCacheTypeList: TfrCacheTypeList;
    FfrArchiveWriterConfig: TFrame;
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
    function GetNameGenerator: ITileFileNameGenerator;
    function GetAllowExport(const AMapType: IMapType): boolean;
    function GetArchiveWriteConfig: IArchiveWriteConfig;
  protected
    procedure OnShow(const AIsFirstTime: Boolean); override;
    procedure OnHide; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const ATileNameGeneratorList: ITileFileNameGeneratorsList;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const AFileFilters: string;
      const AFileExtDefault: string;
      const AArchiveWriterConfigFrame: TFrame = nil
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  i_TileStorageAbilities,
  u_FileSystemFunc;

{$R *.dfm}

constructor TfrExportToFileCont.Create(
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const ATileNameGeneratorList: ITileFileNameGeneratorsList;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const AFileFilters: string;
  const AFileExtDefault: string;
  const AArchiveWriterConfigFrame: TFrame
);
begin
  inherited Create(ALanguageManager);

  FTileNameGeneratorList := ATileNameGeneratorList;

  dlgSaveTargetFile.Filter := AFileFilters;
  dlgSaveTargetFile.DefaultExt := AFileExtDefault;

  FfrCacheTypeList :=
    TfrCacheTypeList.Create(
      ALanguageManager,
      ATileStorageTypeList,
      False,
      [tstcInSeparateFiles],
      [tsacAdd]
    );

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

  FfrArchiveWriterConfig := AArchiveWriterConfigFrame;
  if FfrArchiveWriterConfig <> nil then begin
    FfrArchiveWriterConfig.Parent := pnlArchiveWriteConfig;
  end;

  FPropertyState := CreateComponentPropertyState(
    Self, [pnlTop, pnlMap, pnlZoom], [], True, False, True, True
  );
end;

destructor TfrExportToFileCont.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrZoomsSelect);
  FreeAndNil(FfrCacheTypeList);
  FreeAndNil(FfrArchiveWriterConfig);
  inherited;
end;

procedure TfrExportToFileCont.OnHide;
begin
  inherited;
  FfrCacheTypeList.Hide;
  if FfrArchiveWriterConfig <> nil then begin
    FfrArchiveWriterConfig.Hide;
  end;
end;

procedure TfrExportToFileCont.OnShow(const AIsFirstTime: Boolean);
begin
  inherited;
  if not AIsFirstTime then begin
    FfrCacheTypeList.Visible := True;
    if FfrArchiveWriterConfig <> nil then begin
      FfrArchiveWriterConfig.Visible := True;
    end;
  end;
end;

procedure TfrExportToFileCont.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTargetFile.Execute then begin
    dlgSaveTargetFile.InitialDir := ExtractFileDir(dlgSaveTargetFile.FileName);
    edtTargetFile.Text := dlgSaveTargetFile.FileName;
  end;
end;

function TfrExportToFileCont.GetAllowExport(const AMapType: IMapType): boolean;
begin
  Result := True;
end;

function TfrExportToFileCont.GetArchiveWriteConfig: IArchiveWriteConfig;
begin
  Result := nil;
  if FfrArchiveWriterConfig <> nil then begin
    Result := (FfrArchiveWriterConfig as IArchiveWriteConfigFrame).GetWriteConfig;
  end;
end;

function TfrExportToFileCont.GetMapType: IMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrExportToFileCont.GetNameGenerator: ITileFileNameGenerator;
begin
  Result := FTileNameGeneratorList.GetGenerator(FfrCacheTypeList.IntCode);
end;

function TfrExportToFileCont.GetPath: string;
begin
  Result := Trim(edtTargetFile.Text);
end;

function TfrExportToFileCont.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

procedure TfrExportToFileCont.Init;
begin
  FfrMapSelect.Show(pnlMap);
  FfrZoomsSelect.Show(pnlZoom);
  FfrCacheTypeList.Show(pnlCacheTypes);
end;

function TfrExportToFileCont.Validate: Boolean;

  procedure ShowErr(const AMsg: string);
  begin
    MessageDlg(AMsg, mtError, [mbOk], -1);
  end;

begin
  Result := False;

  if not IsValidFileName(edtTargetFile.Text) then begin
    ShowErr(_('Output file name is not set or incorrect!'));
    Exit;
  end;

  if not FfrZoomsSelect.Validate then begin
    ShowErr(_('Please select at least one zoom!'));
    Exit;
  end;

  if
    (FfrArchiveWriterConfig <> nil) and
    (Self.GetArchiveWriteConfig = nil) then
  begin
    Assert(False);
    Exit;
  end;

  if FfrMapSelect.GetSelectedMapType = nil then begin
    ShowErr(_('Please select the map first!'));
    Exit;
  end;

  Result := True;
end;

end.
