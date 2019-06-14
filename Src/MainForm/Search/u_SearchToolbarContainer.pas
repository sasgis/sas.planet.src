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
    const
      cBusyImageIndex = 49;
      cReadyImageIndex = 11;
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

    FCancelNotifier: INotifierOperationInternal;
    FAppClosingNotifier: INotifierOneOperation;

    FAppCloseListener: IListener;
    FConfigChangeListener: IListener;
    FHistoryChangeListener: IListener;

    FTaskRunner: ISearchTaskRunnerAsync;

    procedure InitActionList;
    procedure actGeoCoderSetMain(Sender: TObject);

    procedure OnAppClose;
    procedure OnConfigChange;
    procedure OnHistoryChange;

    procedure OnActionButtonClick(Sender: TObject);

    procedure OnAcceptText(
      Sender: TObject;
      var NewText: String;
      var Accept: Boolean
    );

    procedure RunSearchTask(const AText: string);

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
  u_Synchronizer,
  u_SearchTaskRunnerAsync,
  u_ListenerByEvent,
  u_Notifier,
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

  FAppClosingNotifier := AAppClosingNotifier;
  FGeoCoderList := AGeoCoderList;
  FMainGeoCoderConfig := AMainGeoCoderConfig;
  FSearchHistory := ASearchHistory;
  FCoordConverter := ACoordConverter;
  FSearchPresenter := ASearchPresenter;

  FactlstGeoCoders := TActionList.Create(nil);

  InitActionList;
  BuildSubMenuByActionList(FGeoCoderMenu, FactlstGeoCoders);

  FCancelNotifier :=
    TNotifierOperation.Create(
      TNotifierBase.Create(GSync.SyncStd.Make(Self.ClassName + 'CancelNotifier'))
    );

  FAppCloseListener := TNotifyNoMmgEventListener.Create(Self.OnAppClose);
  FAppClosingNotifier.Add(FAppCloseListener);

  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FMainGeoCoderConfig.ChangeNotifier.Add(FConfigChangeListener);

  FHistoryChangeListener := TNotifyNoMmgEventListener.Create(Self.OnHistoryChange);
  FSearchHistory.ChangeNotifier.Add(FHistoryChangeListener);

  FTaskRunner := // ToDo: get it as param
    TSearchTaskRunnerAsync.Create(
      FAppClosingNotifier,
      ACoordConverter
    );

  FSearchTextEdit.OnAcceptText := Self.OnAcceptText;
  FSearchTextEdit.ExtendedAccept := True;

  FActionButton.OnClick := Self.OnActionButtonClick;

  OnConfigChange;
  OnHistoryChange;
end;

destructor TSearchToolbarContainer.Destroy;
begin
  if (FAppCloseListener <> nil) and (FAppClosingNotifier <> nil) then begin
    FAppClosingNotifier.Remove(FAppCloseListener);
    FAppCloseListener := nil;
  end;
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

procedure TSearchToolbarContainer.OnAppClose;
begin
  FCancelNotifier.NextOperation;
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
    RunSearchTask(NewText);
  end;
end;

procedure TSearchToolbarContainer.OnActionButtonClick(Sender: TObject);
begin
  if FSearchTextEdit.Enabled then begin
    RunSearchTask(FSearchTextEdit.Text);
  end else begin
    FCancelNotifier.NextOperation;
  end;
end;

procedure TSearchToolbarContainer.RunSearchTask(const AText: string);
var
  I: Integer;
  VText: string;
  VData: PSearchTaskData;
  VItem: IGeoCoderListEntity;
begin
  VText := Trim(AText);

  if VText = '' then begin
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

  FCancelNotifier.NextOperation;

  New(VData);

  VData.Text := VText;
  VData.GeoCoder := VItem.GetGeoCoder;
  VData.OperationID := FCancelNotifier.CurrentOperation;
  VData.CancelNotifier := FCancelNotifier;

  FActionButton.ImageIndex := cBusyImageIndex;
  FSearchTextEdit.Enabled := False;
  FGeoCoderMenu.Enabled := False;

  FTaskRunner.Run(VData, Self.OnSearchResult);
end;

procedure TSearchToolbarContainer.OnSearchResult(
  const ATaskData: PSearchTaskData;
  const AGeoCodeResult: IGeoCodeResult
);
begin
  try
    FSearchHistory.AddItem(ATaskData.Text);
    if AGeoCodeResult <> nil then begin
      FSearchPresenter.ShowSearchResults(AGeoCodeResult);
    end;
  finally
    FActionButton.ImageIndex := cReadyImageIndex;
    FSearchTextEdit.Enabled := True;
    FGeoCoderMenu.Enabled := True;
    Dispose(ATaskData);
  end;
end;

end.
