{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_SearchToolbarContainer;

interface

uses
  ActiveX,
  Actions,
  ActnList,
  Classes,
  Windows,
  TBX,
  TB2Item,
  TBXExtItems,
  i_Listener,
  i_GeoCoder,
  i_GeoCoderList,
  i_StringHistory,
  i_NotifierOperation,
  i_MainGeoCoderConfig,
  i_SearchTaskRunnerAsync,
  i_SearchResultPresenter,
  i_LocalCoordConverterChangeable;

type
  TSearchToolbarContainer = class
  private
    FGeoCoderMenu: TTBXSubmenuItem;
    FSearchTextEdit: TTBXComboBoxItem;
    FActionButton: TTBXItem;

    FGeoCoderList: IGeoCoderListStatic;
    FMainGeoCoderConfig: IMainGeoCoderConfig;
    FSearchHistory: IStringHistory;
    FCoordConverter: ILocalCoordConverterChangeable;
    FSearchPresenter: ISearchResultPresenter;

    FactlstGeoCoders: TActionList;

    FConfigChangeListener: IListener;
    FHistoryChangeListener: IListener;

    FTaskRunner: ISearchTaskRunnerAsync;

    procedure InitActionList;
    procedure actGeoCoderSetMain(Sender: TObject);

    procedure OnConfigChange;
    procedure OnHistoryChange;

    procedure OnAcceptText(
      Sender: TObject;
      var NewText: String;
      var Accept: Boolean
    );

    procedure DoSearch(const AText: string);

    procedure OnSearchResult(
      const ATaskData: PSearchTaskData;
      const AGeoCodeResult: IGeoCodeResult
    );
  public
    constructor Create(
      const AGeoCoderMenu: TTBXSubmenuItem;
      const ASearchTextEdit: TTBXComboBoxItem;
      const AActionButton: TTBXItem;
      const AAppClosingNotifier: INotifierOneOperation;
      const AGeoCoderList: IGeoCoderListStatic;
      const AMainGeoCoderConfig: IMainGeoCoderConfig;
      const ASearchHistory: IStringHistory;
      const ACoordConverter: ILocalCoordConverterChangeable;
      const ASearchPresenter: ISearchResultPresenter
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_LocalCoordConverter,
  u_SearchTaskRunnerAsync,
  u_ListenerByEvent,
  u_NotifierOperation;

procedure BuildSubMenuByActionList(
  const AParent: TTBCustomItem;
  const AActionList: TActionList
);
var
  I: Integer;
  VAction: TCustomAction;
  VMenuItem: TTBXItem;
begin
  for I := 0 to AActionList.ActionCount - 1 do begin
    VAction := TCustomAction(AActionList.Actions[I]);

    VMenuItem := TTBXItem.Create(AParent);
    VMenuItem.Action := VAction;
    if VAction.Name <> '' then begin
      VMenuItem.Name := 'tbitm' + VAction.Name;
    end;
    AParent.Add(VMenuItem);
  end;
end;

{ TSearchToolbarContainer }

constructor TSearchToolbarContainer.Create(
  const AGeoCoderMenu: TTBXSubmenuItem;
  const ASearchTextEdit: TTBXComboBoxItem;
  const AActionButton: TTBXItem;
  const AAppClosingNotifier: INotifierOneOperation;
  const AGeoCoderList: IGeoCoderListStatic;
  const AMainGeoCoderConfig: IMainGeoCoderConfig;
  const ASearchHistory: IStringHistory;
  const ACoordConverter: ILocalCoordConverterChangeable;
  const ASearchPresenter: ISearchResultPresenter
);
begin
  inherited Create;

  FGeoCoderMenu := AGeoCoderMenu;
  FSearchTextEdit := ASearchTextEdit;
  FActionButton := AActionButton;

  FGeoCoderList := AGeoCoderList;
  FMainGeoCoderConfig := AMainGeoCoderConfig;
  FSearchHistory := ASearchHistory;
  FCoordConverter := ACoordConverter;
  FSearchPresenter := ASearchPresenter;

  FactlstGeoCoders := TActionList.Create(nil);

  InitActionList;
  BuildSubMenuByActionList(FGeoCoderMenu, FactlstGeoCoders);

  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FMainGeoCoderConfig.ChangeNotifier.Add(FConfigChangeListener);

  FHistoryChangeListener := TNotifyNoMmgEventListener.Create(Self.OnHistoryChange);
  FSearchHistory.ChangeNotifier.Add(FHistoryChangeListener);

  FTaskRunner := // ToDo: get it as param
    TSearchTaskRunnerAsync.Create(
      AAppClosingNotifier,
      ACoordConverter
    );

  FSearchTextEdit.OnAcceptText := Self.OnAcceptText;
  FSearchTextEdit.ExtendedAccept := True;

  FActionButton.Visible := False; // ToDo: make search cancelable

  OnConfigChange;
  OnHistoryChange;
end;

destructor TSearchToolbarContainer.Destroy;
begin
  if (FConfigChangeListener <> nil) and (FMainGeoCoderConfig <> nil) then begin
    FMainGeoCoderConfig.ChangeNotifier.Remove(FConfigChangeListener);
    FConfigChangeListener := nil;
  end;

  if (FHistoryChangeListener <> nil) and (FSearchHistory <> nil) then begin
    FSearchHistory.ChangeNotifier.Remove(FHistoryChangeListener);
    FHistoryChangeListener := nil;
  end;

  FGeoCoderMenu.Clear;
  FactlstGeoCoders.Free;

  inherited;
end;

procedure TSearchToolbarContainer.InitActionList;
var
  I: Integer;
  VItem: IGeoCoderListEntity;
  VAction: TAction;
begin
  for I := 0 to FGeoCoderList.Count - 1 do begin
    VItem := FGeoCoderList.Items[I];

    VAction := TAction.Create(FactlstGeoCoders);
    VAction.Caption := VItem.Caption;
    VAction.Tag := Integer(VItem);
    VAction.OnExecute := Self.actGeoCoderSetMain;
    VAction.ActionList := FactlstGeoCoders;
  end;
end;

procedure TSearchToolbarContainer.actGeoCoderSetMain(Sender: TObject);
var
  VItem: IGeoCoderListEntity;
begin
  if Assigned(Sender) then begin
    VItem := IGeoCoderListEntity(TComponent(Sender).Tag);
    if VItem <> nil then begin
      FMainGeoCoderConfig.ActiveGeoCoderGUID := VItem.GetGUID;
    end;
  end;
end;

procedure TSearchToolbarContainer.OnConfigChange;
var
  I: Integer;
  VGUID: TGUID;
  VItem: IGeoCoderListEntity;
  VAction: TCustomAction;
begin
  VGUID := FMainGeoCoderConfig.ActiveGeoCoderGUID;
  for I := 0 to FactlstGeoCoders.ActionCount - 1 do begin
    VAction := TCustomAction(FactlstGeoCoders.Actions[I]);
    VItem := IGeoCoderListEntity(VAction.Tag);
    if VItem <> nil then begin
      if IsEqualGUID(VGUID, VItem.GetGUID) then begin
        VAction.Checked := True;
        FGeoCoderMenu.Caption := VAction.Caption;
      end else begin
        VAction.Checked := False;
      end;
    end;
  end;
end;

procedure TSearchToolbarContainer.OnHistoryChange;
var
  I: Integer;
begin
  FSearchTextEdit.Lines.Clear;
  FSearchHistory.LockRead;
  try
    for I := 0 to FSearchHistory.Count - 1 do begin
      FSearchTextEdit.Lines.Add(FSearchHistory.GetItem(I));
    end;
  finally
    FSearchHistory.UnlockRead;
  end;
end;

procedure TSearchToolbarContainer.OnAcceptText(
  Sender: TObject;
  var NewText: String;
  var Accept: Boolean
);

  function IsEnterPressed: Boolean;
  var
    VState: SHORT;
  begin
    // If the high-order bit is 1, the key is down; otherwise, it is up
    VState := GetKeyState(VK_RETURN);
    Result := (VState and (1 shl 7)) <> 0;
  end;

begin
  // when user press Enter key, triggers regular accept event - that's what we need
  // when Edit lost its focus, triggers Extended accept event and save the NewText
  if IsEnterPressed then begin
    DoSearch(NewText);
  end;
end;

procedure TSearchToolbarContainer.DoSearch(const AText: string);
var
  I: Integer;
  VData: PSearchTaskData;
  VItem: IGeoCoderListEntity;
  VNotifier: INotifierOperation;
begin
  if AText = '' then begin
    Exit;
  end;

  I := FGeoCoderList.GetIndexByGUID(FMainGeoCoderConfig.ActiveGeoCoderGUID);
  if I < 0 then begin
    Exit;
  end;

  VItem := FGeoCoderList.Items[I];
  if not Assigned(VItem) then begin
    Exit;
  end;

  VNotifier := TNotifierOperationFake.Create; // ToDo: make search cancelable

  New(VData);

  VData.Text := AText;
  VData.GeoCoder := VItem.GetGeoCoder;
  VData.OperationID := VNotifier.CurrentOperation;
  VData.CancelNotifier := VNotifier;

  FTaskRunner.Run(VData, Self.OnSearchResult);
end;

procedure TSearchToolbarContainer.OnSearchResult(
  const ATaskData: PSearchTaskData;
  const AGeoCodeResult: IGeoCodeResult
);
begin
  try
    FSearchHistory.AddItem(ATaskData.Text);
    FSearchPresenter.ShowSearchResults(AGeoCodeResult);
  finally
    Dispose(ATaskData);
  end;
end;

end.
