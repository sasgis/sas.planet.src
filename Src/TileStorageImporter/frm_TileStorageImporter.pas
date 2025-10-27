{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit frm_TileStorageImporter;

interface

uses
  Types,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Generics.Collections,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  t_TileStorageImporter,
  i_LanguageManager,
  i_StringListStatic,
  i_ContentTypeManager,
  i_ProjectionSetList,
  i_TileStorageTypeList,
  u_CommonFormAndFrameParents;

type
  TfrmTileStorageImporter = class(TFormWitghLanguageManager)
    btnOk: TButton;
    btnCancel: TButton;
    pnlBottomButtons: TPanel;
    pgcMain: TPageControl;
    tsParams: TTabSheet;
    tsMetadata: TTabSheet;
    lvMetadata: TListView;
    lblMapName: TLabel;
    edtMapName: TEdit;
    lblParentSubMenu: TLabel;
    edtParentSubMenu: TEdit;
    lblExtension: TLabel;
    cbbExt: TComboBox;
    lblProjection: TLabel;
    cbbProj: TComboBox;
    lblCacheType: TLabel;
    cbbCacheType: TComboBox;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    FContentTypeManager: IContentTypeManager;
    FProjectionSetList: IProjectionSetList;
    FTileStorageTypeList: ITileStorageTypeListStatic;

    FFileInfo: TTileStorageImporterFileInfo;

    procedure ShowMetadata(const AMetadata: TTileStorageImporterMetadataInfo);
    procedure PrepareExtentionsList(const AItems: TStrings; const AContentTypeManager: IContentTypeManager);
    procedure PrepareProjectionsList(const AItems: TStrings; const AProjectionSetList: IProjectionSetList);
    procedure PrepareCacheTypeList(const AItems: TStrings; const ATileStorageTypeList: ITileStorageTypeListStatic);
  public
    function ShowFileInfoModal(const AFileInfo: TTileStorageImporterFileInfo): Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AContentTypeManager: IContentTypeManager;
      const AProjectionSetList: IProjectionSetList;
      const ATileStorageTypeList: ITileStorageTypeListStatic
    ); reintroduce;
  end;

implementation

uses
  gnugettext;

{$R *.dfm}

constructor TfrmTileStorageImporter.Create(
  const ALanguageManager: ILanguageManager;
  const AContentTypeManager: IContentTypeManager;
  const AProjectionSetList: IProjectionSetList;
  const ATileStorageTypeList: ITileStorageTypeListStatic
);
begin
  inherited Create(ALanguageManager);

  FContentTypeManager := AContentTypeManager;
  FProjectionSetList := AProjectionSetList;
  FTileStorageTypeList := ATileStorageTypeList;

  PrepareExtentionsList(cbbExt.Items, FContentTypeManager);
  PrepareProjectionsList(cbbProj.Items, FProjectionSetList);
  PrepareCacheTypeList(cbbCacheType.Items, FTileStorageTypeList);
end;

procedure TfrmTileStorageImporter.FormShow(Sender: TObject);
begin
  pgcMain.ActivePageIndex := 0;
  btnOk.SetFocus;
end;

procedure TfrmTileStorageImporter.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FFileInfo := nil;
end;

function TfrmTileStorageImporter.ShowFileInfoModal(const AFileInfo: TTileStorageImporterFileInfo): Boolean;
var
  I: Integer;
begin
  FFileInfo := AFileInfo;

  if FFileInfo.FParentSubMenu = '' then begin
    FFileInfo.FParentSubMenu := _('Offline Maps');
  end;

  edtMapName.Text := FFileInfo.FMapName;
  edtParentSubMenu.Text := FFileInfo.FParentSubMenu;

  // content-type
  cbbExt.ItemIndex := cbbExt.Items.IndexOf(FFileInfo.FExt);

  // projection
  cbbProj.ItemIndex := -1;
  for I := 0 to FProjectionSetList.Count - 1 do begin
    if FFileInfo.FProjectionEpsg = FProjectionSetList.Items[I].Zooms[0].ProjectionType.ProjectionEPSG then begin
      cbbProj.ItemIndex := I;
      Break;
    end;
  end;

  // cache type
  cbbCacheType.ItemIndex := -1;
  for I := 0 to FTileStorageTypeList.Count - 1 do begin
    if FFileInfo.FCacheTypeCode = FTileStorageTypeList.Items[I].IntCode then begin
      cbbCacheType.ItemIndex := I;
      Break;
    end;
  end;

  ShowMetadata(FFileInfo.FMetadata);

  Result := ShowModal = mrOk;
end;

procedure TfrmTileStorageImporter.ShowMetadata(const AMetadata: TTileStorageImporterMetadataInfo);
var
  VKey: string;
  VKeys: TArray<string>;
  VListItem: TListItem;
begin
  lvMetadata.Items.BeginUpdate;
  try
    lvMetadata.Items.Clear;

    VKeys := AMetadata.Keys.ToArray;
    TArray.Sort<string>(VKeys);

    for VKey in VKeys do begin
      VListItem := lvMetadata.Items.Add;
      VListItem.Caption := VKey;
      VListItem.SubItems.Add(AMetadata.Items[VKey]);
    end;
  finally
    lvMetadata.Items.EndUpdate;
  end;
end;

procedure TfrmTileStorageImporter.btnOkClick(Sender: TObject);
var
  I: Integer;
begin
  FFileInfo.FMapName := Trim(edtMapName.Text);
  FFileInfo.FParentSubMenu := Trim(edtParentSubMenu.Text);

  // content-type
  I := cbbExt.ItemIndex;
  if I >= 0 then begin
    FFileInfo.FExt := cbbExt.Items[I];
    FFileInfo.FContentType := FContentTypeManager.GetInfoByExt(AnsiString(FFileInfo.FExt)).GetContentType;
  end;

  // projection
  I := cbbProj.ItemIndex;
  if I >= 0 then begin
    FFileInfo.FProjectionEpsg := FProjectionSetList.Items[I].Zooms[0].ProjectionType.ProjectionEPSG;
  end;

  // cache type
  I := cbbCacheType.ItemIndex;
  if I >= 0 then begin
    FFileInfo.FCacheTypeCode := FTileStorageTypeList.Items[I].IntCode;
  end;

  ModalResult := mrOk;
end;

procedure TfrmTileStorageImporter.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmTileStorageImporter.PrepareExtentionsList(
  const AItems: TStrings;
  const AContentTypeManager: IContentTypeManager
);

  procedure _RemoveAlias(const AExt: string; const AAlias: TStringDynArray);
  var
    I: Integer;
    VExt: string;
  begin
    if AItems.IndexOf(AExt) < 0 then begin
      Exit;
    end;
    for VExt in AAlias do begin
      I := AItems.IndexOf(VExt);
      if I >= 0 then begin
        AItems.Delete(I);
      end;
    end;
  end;

var
  I: Integer;
  VList: IStringListStatic;
begin
  VList := AContentTypeManager.GetKnownExtList;

  if (VList = nil) or (VList.Count <= 0) then begin
    AItems.Clear;
    Assert(False);
    Exit;
  end;

  AItems.BeginUpdate;
  try
    AItems.Clear;

    for I := 0 to VList.Count - 1 do begin
      AItems.Add(VList.Items[I]);
    end;

    _RemoveAlias('.jpg', ['.jpeg']);
    _RemoveAlias('.tif', ['.tiff']);
    _RemoveAlias('.geojson', ['.json']);
  finally
    AItems.EndUpdate;
  end;
end;

procedure TfrmTileStorageImporter.PrepareProjectionsList(
  const AItems: TStrings;
  const AProjectionSetList: IProjectionSetList
);
var
  I: Integer;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for I := 0 to AProjectionSetList.Count - 1 do begin
      AItems.Add(AProjectionSetList.Captions[I]);
    end;
  finally
    AItems.EndUpdate;
  end;
end;

procedure TfrmTileStorageImporter.PrepareCacheTypeList(
  const AItems: TStrings;
  const ATileStorageTypeList: ITileStorageTypeListStatic
);
var
  I: Integer;
begin
  AItems.BeginUpdate;
  try
    AItems.Clear;
    for I := 0 to ATileStorageTypeList.Count - 1 do begin
      AItems.Add(ATileStorageTypeList.Items[I].Caption);
    end;
  finally
    AItems.EndUpdate;
  end;
end;

end.
