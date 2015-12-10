{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit fr_FavoriteMapSetManager;

interface

uses
  Windows,
  Menus,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  frm_FavoriteMapSetEditor,
  i_LanguageManager,
  i_MapTypeSet,
  i_FavoriteMapSetConfig,
  i_FavoriteMapSetHelper,
  i_FavoriteMapSetItemStatic,
  u_CommonFormAndFrameParents;

type
  TfrFavoriteMapSetManager = class(TFrame)
    pnlRightButtons: TPanel;
    pnlBottom: TPanel;
    lvMapSets: TListView;
    spl1: TSplitter;
    btnEdit: TButton;
    btnAdd: TButton;
    lvInfo: TListView;
    btnDelete: TButton;
    btnUp: TButton;
    btnDown: TButton;
    pnlMapSets: TPanel;
    chkEditByDblClick: TCheckBox;
    btnSwitchOn: TButton;
    procedure btnEditClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure lvMapSetsClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure btnSwitchOnClick(Sender: TObject);
    procedure lvMapSetsDblClick(Sender: TObject);
  private
    FMapsSet: IMapTypeSet;
    FFavoriteMapSetConfig: IFavoriteMapSetConfig;
    FFavoriteMapSetHelper: IFavoriteMapSetHelper;
    FFavoriteMapSetEditor: TfrmFavoriteMapSetEditor;
    procedure _Clear;
    function _GetSelected: IFavoriteMapSetItemStatic;
    function _TrySelect(const AID: TGUID): Integer;
    procedure CustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapsSet: IMapTypeSet;
      const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
      const AFavoriteMapSetHelper: IFavoriteMapSetHelper;
      const AFavoriteMapSetEditor: TfrmFavoriteMapSetEditor
    ); reintroduce;
    destructor Destroy; override;
    procedure Init;
    procedure CancelChanges;
    procedure ApplyChanges;
  end;

implementation

uses
  gnugettext,
  c_ZeroGUID,
  i_MapType,
  i_GUIDListStatic,
  i_InterfaceListStatic;

{$R *.dfm}

{ TfrFavoriteMapSetManager }

constructor TfrFavoriteMapSetManager.Create(
  const ALanguageManager: ILanguageManager;
  const AMapsSet: IMapTypeSet;
  const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
  const AFavoriteMapSetHelper: IFavoriteMapSetHelper;
  const AFavoriteMapSetEditor: TfrmFavoriteMapSetEditor
);
begin
  inherited Create(ALanguageManager);
  FMapsSet := AMapsSet;
  FFavoriteMapSetConfig := AFavoriteMapSetConfig;
  FFavoriteMapSetHelper := AFavoriteMapSetHelper;
  FFavoriteMapSetEditor := AFavoriteMapSetEditor;
  lvMapSets.DoubleBuffered := True;
  lvMapSets.OnCustomDrawItem := Self.CustomDrawItem;
  lvInfo.DoubleBuffered := True;
  lvInfo.OnCustomDrawItem := Self.CustomDrawItem;
end;

destructor TfrFavoriteMapSetManager.Destroy;
begin
  _Clear;
  inherited Destroy;
end;

procedure TfrFavoriteMapSetManager._Clear;
var
  I: Integer;
  VData: Pointer;
  VItem: IFavoriteMapSetItemStatic;
begin
  for I := 0 to lvMapSets.Items.Count - 1 do begin
    VData := lvMapSets.Items.Item[I].Data;
    if VData <> nil then begin
      VItem := IFavoriteMapSetItemStatic(VData);
      VItem._Release;
    end;
  end;
  lvMapSets.Clear;
end;

function TfrFavoriteMapSetManager._GetSelected: IFavoriteMapSetItemStatic;
var
  VItem: TListItem;
begin
  Result := nil;
  VItem := lvMapSets.Selected;
  if VItem <> nil then begin
    Result := IFavoriteMapSetItemStatic(VItem.Data);
  end;
end;

function TfrFavoriteMapSetManager._TrySelect(const AID: TGUID): Integer;
var
  I: Integer;
  VData: Pointer;
  VItem: IFavoriteMapSetItemStatic;
begin
  Result := -1;
  for I := 0 to lvMapSets.Items.Count - 1 do begin
    VData := lvMapSets.Items.Item[I].Data;
    if VData <> nil then begin
      VItem := IFavoriteMapSetItemStatic(VData);
      if IsEqualGUID(VItem.ID, AID) then begin
        lvMapSets.ItemIndex := I;
        Result := I;
        Break;
      end;
    end;
  end;
end;

procedure TfrFavoriteMapSetManager.CustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
const
  cStripe = $F5F5F5;
begin
  if Item = nil then begin
    Exit;
  end;
  if Odd(Item.Index) then begin
    Sender.Canvas.Brush.Color := cStripe;
  end else begin
    Sender.Canvas.Brush.Color := clWindow;
  end;
end;

procedure _SetSubItem(AItem: TListItem; AIndex: Integer; const AValue: string);
var
  I: Integer;
begin
  if AIndex < AItem.SubItems.Count then begin
    AItem.SubItems.Strings[AIndex] := AValue;
  end else begin
    for I := AItem.SubItems.Count to AIndex - 1 do begin
      AItem.SubItems.Add('');
    end;
    AItem.SubItems.Add(AValue);
  end;
end;

procedure TfrFavoriteMapSetManager.Init;
var
  I: Integer;
  VItem, VSelected: IFavoriteMapSetItemStatic;
  VListItem: TListItem;
  VStatic: IInterfaceListStatic;
begin
  VSelected := _GetSelected;
  _Clear;
  VStatic := FFavoriteMapSetConfig.GetStatic;
  if Assigned(VStatic) and (VStatic.Count > 0) then begin
    lvMapSets.Items.BeginUpdate;
    try
      for I := 0 to VStatic.Count - 1 do begin
        VItem := IFavoriteMapSetItemStatic(VStatic.Items[I]);
        VListItem := lvMapSets.Items.Add;
        VListItem.Caption := VItem.Name;
        VListItem.Data := Pointer(VItem);
        VItem._AddRef;
        _SetSubItem(VListItem, 0, ShortCutToText(VItem.HotKey));
      end;
      I := -1;
      if Assigned(VSelected) then begin
        I := _TrySelect(VSelected.ID);
      end;
      if (lvMapSets.Items.Count > 0) and (I = -1) then begin
        lvMapSets.ItemIndex := 0;
      end;
    finally
      lvMapSets.Items.EndUpdate;
    end;
  end;
  lvMapSetsClick(nil);
end;

procedure TfrFavoriteMapSetManager.lvMapSetsClick(Sender: TObject);
var
  I: Integer;
  VInfoItem: TListItem;
  VLayers: IGUIDSetStatic;
  VMapType: IMapType;
  VItem: IFavoriteMapSetItemStatic;
begin
  lvInfo.Items.BeginUpdate;
  try
    lvInfo.Clear;
    VItem := _GetSelected;
    if VItem <> nil then begin
      if not IsEqualGUID(VItem.BaseMap, CGUID_Zero) then begin
        VInfoItem := lvInfo.Items.Add;
        VInfoItem.Caption := _('Map');
        VMapType := FMapsSet.GetMapTypeByGUID(VItem.BaseMap);
        if Assigned(VMapType) then begin
          _SetSubItem(VInfoItem, 0, VMapType.GUIConfig.Name.Value);
        end else begin
          _SetSubItem(VInfoItem, 0, GUIDToString(VItem.BaseMap));
        end;
      end;

      VLayers := VItem.Layers;
      if Assigned(VLayers) and (VLayers.Count > 0) then begin
        for I := 0 to VLayers.Count - 1 do begin
          VInfoItem := lvInfo.Items.Add;
          VInfoItem.Caption := _('Overlay layer') + ' ' + IntToStr(I + 1);
          VMapType := FMapsSet.GetMapTypeByGUID(VLayers.Items[I]);
          if Assigned(VMapType) then begin
            _SetSubItem(VInfoItem, 0, VMapType.GUIConfig.Name.Value);
          end else begin
            _SetSubItem(VInfoItem, 0, GUIDToString(VItem.BaseMap));
          end;
        end;
      end;

      if VItem.Zoom >= 0 then begin
        VInfoItem := lvInfo.Items.Add;
        VInfoItem.Caption := _('Zoom');
        _SetSubItem(VInfoItem, 0, IntToStr(VItem.Zoom + 1));
      end;
    end;
  finally
    lvInfo.Items.EndUpdate;
  end;
end;

procedure TfrFavoriteMapSetManager.lvMapSetsDblClick(Sender: TObject);
begin
  if chkEditByDblClick.Checked then begin
    btnEditClick(Sender);
  end else begin
    btnSwitchOnClick(Sender);
  end;
end;

procedure TfrFavoriteMapSetManager.btnAddClick(Sender: TObject);
begin
  if FFavoriteMapSetEditor.DoAdd then begin
    Init;
  end;
end;

procedure TfrFavoriteMapSetManager.btnDeleteClick(Sender: TObject);
var
  VMsg: string;
  VItem: IFavoriteMapSetItemStatic;
begin
  VItem := _GetSelected;
  if Assigned(VItem) then begin
    VMsg := Format(_('Delete favorite map set: ''%s''?'), [VItem.Name]);
    if MessageDlg(VMsg, mtConfirmation, [mbYes, mbCancel], 0) = mrYes then begin
      if FFavoriteMapSetConfig.Delete(VItem.ID) then begin
        Init;
      end;
    end;
  end;
end;

procedure TfrFavoriteMapSetManager.btnEditClick(Sender: TObject);
var
  VItem: IFavoriteMapSetItemStatic;
begin
  VItem := _GetSelected;
  if VItem <> nil then begin
    if FFavoriteMapSetEditor.DoUpdate(VItem.ID) then begin
      Init;
    end;
  end;
end;

procedure TfrFavoriteMapSetManager.btnSwitchOnClick(Sender: TObject);
var
  VErrMsg: string;
  VItem: IFavoriteMapSetItemStatic;
begin
  VItem := _GetSelected;
  if VItem <> nil then begin
    if not FFavoriteMapSetHelper.TrySwitchOn(VItem, VErrMsg) then begin
      MessageDlg(VErrMsg, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrFavoriteMapSetManager.btnUpClick(Sender: TObject);
var
  VItem: IFavoriteMapSetItemStatic;
begin
  VItem := _GetSelected;
  if VItem <> nil then begin
    if FFavoriteMapSetConfig.MoveUp(VItem.ID) then begin
      Init;
    end;
  end;
end;

procedure TfrFavoriteMapSetManager.btnDownClick(Sender: TObject);
var
  VItem: IFavoriteMapSetItemStatic;
begin
  VItem := _GetSelected;
  if VItem <> nil then begin
    if FFavoriteMapSetConfig.MoveDown(VItem.ID) then begin
      Init;
    end;
  end;
end;

procedure TfrFavoriteMapSetManager.CancelChanges;
begin
  // empty
end;

procedure TfrFavoriteMapSetManager.ApplyChanges;
begin
  // empty
end;

end.
