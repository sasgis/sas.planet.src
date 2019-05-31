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
    pnlFrame: TPanel;
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
    procedure RefreshTranslation; override;
  end;

implementation

uses
  gnugettext,
  i_TileStorageAbilities;

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
end;

destructor TfrExportToFileCont.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  FreeAndNil(FfrZoomsSelect);
  FreeAndNil(FfrCacheTypeList);
  FreeAndNil(FfrArchiveWriterConfig);
  inherited;
end;

procedure TfrExportToFileCont.btnSelectTargetFileClick(Sender: TObject);
begin
  if dlgSaveTargetFile.Execute then begin
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
  Assert(Result <> nil);
end;

function TfrExportToFileCont.GetNameGenerator: ITileFileNameGenerator;
begin
  Result := FTileNameGeneratorList.GetGenerator(FfrCacheTypeList.IntCode);
end;

function TfrExportToFileCont.GetPath: string;
begin
  Result := edtTargetFile.Text;
end;

function TfrExportToFileCont.GetZoomArray: TByteDynArray;
begin
  Result := FfrZoomsSelect.GetZoomList;
end;

procedure TfrExportToFileCont.Init;
begin
  FfrMapSelect.Show(pnlFrame);
  FfrZoomsSelect.Show(pnlZoom);
  FfrCacheTypeList.Show(pnlCacheTypes);
  if FfrArchiveWriterConfig <> nil then begin
    FfrArchiveWriterConfig.Parent := pnlArchiveWriteConfig;
    FfrArchiveWriterConfig.Show;
  end;
end;

procedure TfrExportToFileCont.RefreshTranslation;
begin
  inherited;
end;

function TfrExportToFileCont.Validate: Boolean;

  procedure ShowErr(const AMsg: string);
  begin
    MessageDlg(AMsg, mtError, [mbOk], -1);
  end;

begin
  Result := (edtTargetFile.Text <> '');
  if not Result then begin
    ShowErr(_('Please, select output file first!'));
    Exit;
  end;

  Result := FfrZoomsSelect.Validate;
  if not Result then begin
    ShowErr(_('Please select at least one zoom!'));
    Exit;
  end;

  if FfrArchiveWriterConfig <> nil then begin
    Result := GetArchiveWriteConfig <> nil;
    if not Result then begin
      Exit;
    end;
  end;
end;

end.
