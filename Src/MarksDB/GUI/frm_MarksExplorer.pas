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

unit frm_MarksExplorer;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  Controls,
  Messages,
  Menus,
  Forms,
  StdCtrls,
  ExtCtrls,
  ImgList,
  ImageList,
  TB2Dock,
  TB2Toolbar,
  TB2Item,
  TBX,
  TBXDkPanels,
  TBXGraphics,
  frm_MarkSystemConfigEdit,
  frm_MarksExplorerFilter,
  i_Listener,
  i_NotifierTime,
  i_RegionProcess,
  i_LanguageManager,
  i_InterfaceListStatic,
  i_GeometryLonLatFactory,
  i_LocalCoordConverterChangeable,
  i_NavigationToPoint,
  i_UsedMarksConfig,
  i_MapViewGoto,
  i_WindowPositionConfig,
  i_MarkId,
  i_VectorDataItemSimple,
  i_MarksExplorerFilter,
  i_MarksExplorerConfig,
  i_MarkCategoryList,
  i_MarkCategory,
  i_MarkSystemConfig,
  i_MarkSystemImplFactory,
  i_MergePolygonsPresenter,
  i_ElevationProfilePresenter,
  u_MarkDbGUIHelper,
  u_MarksExplorerView,
  u_MarksExplorerHelper,
  u_CommonFormAndFrameParents;

type
  TfrmMarksExplorer = class(TFormWitghLanguageManager)
    grpMarks: TGroupBox;
    grpCategory: TGroupBox;
    chkSetAllMarksInCategoryVisible: TCheckBox;
    pnlButtons: TPanel;
    pnlMainWithButtons: TPanel;
    pnlMain: TPanel;
    splCatMarks: TSplitter;
    btnExport: TTBXButton;
    PopupExport: TPopupMenu;
    NExportAll: TMenuItem;
    NExportVisible: TMenuItem;
    NExportSelected: TMenuItem;
    btnImport: TTBXButton;
    rgMarksShowMode: TRadioGroup;
    TBXDockMark: TTBXDock;
    TBXToolbar1: TTBXToolbar;
    btnEditMark: TTBXItem;
    btnDelMark: TTBXItem;
    TBXSeparatorItem1: TTBXSeparatorItem;
    btnGoToMark: TTBXItem;
    btnOpSelectMark: TTBXItem;
    btnNavOnMark: TTBXItem;
    TBXSeparatorItem2: TTBXSeparatorItem;
    btnSaveMark: TTBXItem;
    TBXDockCategory: TTBXDock;
    TBXToolbar2: TTBXToolbar;
    btnAddCategory: TTBXItem;
    btnDeleteCategory: TTBXItem;
    TBXSeparatorItem3: TTBXSeparatorItem;
    btnEditCategory: TTBXItem;
    btnExportCategory: TTBXItem;
    lblMarksCount: TStaticText;
    tbpmnCategories: TTBXPopupMenu;
    tbitmAddCategory: TTBXItem;
    tbitmEditCategory: TTBXItem;
    tbitmDeleteCategory: TTBXItem;
    tbsprtCategoriesPopUp: TTBXSeparatorItem;
    tbitmExportCategory: TTBXItem;
    tbpmnMarks: TTBXPopupMenu;
    tbitmAddMark: TTBXItem;
    tbitmEditMark: TTBXItem;
    tbitmDeleteMark: TTBXItem;
    tbsprtMarksPopUp: TTBXSeparatorItem;
    tbitmExportMark: TTBXItem;
    btnAddMark: TTBXItem;
    pnlMarksBottom: TPanel;
    pnlBottom: TPanel;
    lblReadOnly: TLabel;
    tbitmMarkInfo: TTBXItem;
    Panel1: TPanel;
    chkSetAllCategoriesVisible: TCheckBox;
    chkCascade: TCheckBox;
    tbitmAllVisible: TTBXItem;
    tbxtmAddToMergePolygons: TTBXItem;
    tbxtmCatAddToMergePolygons: TTBXItem;
    tbxtmUngroup: TTBXItem;
    TBXToolbar3: TTBXToolbar;
    tbxConfigList: TTBXSubmenuItem;
    tbxSep1: TTBXSeparatorItem;
    tbxAdd: TTBXItem;
    tbxEdit: TTBXItem;
    tbxDelete: TTBXItem;
    TBXImageList1: TTBXImageList;
    TBXSeparatorItem4: TTBXSeparatorItem;
    tbsprtMarksPopUp2: TTBXSeparatorItem;
    tbitmCopy: TTBXItem;
    tbitmPaste: TTBXItem;
    tbitmCut: TTBXItem;
    tbitmCopyAsText: TTBXItem;
    TBXSeparatorItem5: TTBXSeparatorItem;
    tbxShowElevProfile: TTBXItem;
    TBXSeparatorItem6: TTBXSeparatorItem;
    tbxSelectAllVisible: TTBXItem;
    tbxRevertSelection: TTBXItem;
    TBXSeparatorItem7: TTBXSeparatorItem;
    tbitmEditMarkPosition: TTBXItem;
    tbxtmGroup: TTBXItem;
    TBXSeparatorItem8: TTBXSeparatorItem;
    tbitmFilter: TTBXItem;
    tbxtmCopyBboxToClipboard: TTBXItem;
    pnlCategoriesTree: TPanel;
    pnlMarksList: TPanel;
    tbxSep2: TTBXSeparatorItem;
    tbxWarning: TTBXItem;

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormMove(var Msg: TWMMove); message WM_MOVE;
    procedure FormResize(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);

    procedure btnAddCategoryClick(Sender: TObject);
    procedure btnDeleteCategoryClick(Sender: TObject);
    procedure btnEditCategoryClick(Sender: TObject);
    procedure chkSetAllCategoriesVisibleClick(Sender: TObject);
    procedure chkSetAllMarksInCategoryVisibleClick(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
    procedure btnExportCategoryClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnEditMarkClick(Sender: TObject);
    procedure btnDelMarkClick(Sender: TObject);
    procedure btnGoToMarkClick(Sender: TObject);
    procedure btnOpSelectMarkClick(Sender: TObject);
    procedure btnNavOnMarkClick(Sender: TObject);
    procedure btnSaveMarkClick(Sender: TObject);
    procedure rgMarksShowModeClick(Sender: TObject);
    procedure tbpmnCategoriesPopup(Sender: TObject);
    procedure tbitmMarkInfoClick(Sender: TObject);
    procedure tbitmAllVisibleClick(Sender: TObject);
    procedure tbxtmAddToMergePolygonsClick(Sender: TObject);
    procedure tbxtmCatAddToMergePolygonsClick(Sender: TObject);
    procedure tbpmnMarksPopup(Sender: TObject);
    procedure tbxtmUngroupClick(Sender: TObject);
    procedure tbxConfigListItemClick(Sender: TObject);
    procedure tbxDeleteClick(Sender: TObject);
    procedure tbxAddClick(Sender: TObject);
    procedure tbxEditClick(Sender: TObject);
    procedure tbxWarningClick(Sender: TObject);
    procedure tbitmCopyClick(Sender: TObject);
    procedure tbitmCutClick(Sender: TObject);
    procedure tbitmPasteClick(Sender: TObject);
    procedure tbitmCopyAsTextClick(Sender: TObject);
    procedure tbxShowElevProfileClick(Sender: TObject);
    procedure tbxSelectAllVisibleClick(Sender: TObject);
    procedure tbxRevertSelectionClick(Sender: TObject);
    procedure tbitmEditMarkPositionClick(Sender: TObject);
    procedure tbxtmGroupClick(Sender: TObject);
    procedure tbitmFilterClick(Sender: TObject);
    procedure tbxtmCopyBboxToClipboardClick(Sender: TObject);
    procedure tbitmAddCategoryClick(Sender: TObject);
    procedure tbitmAddMarkClick(Sender: TObject);
    procedure chkCascadeClick(Sender: TObject);
  private
    type
      TCopyPasteAction = (cpNone, cpCopy, cpCut);
  private
    FfrmMarksExplorerFilter: TfrmMarksExplorerFilter;
    FfrmMarkSystemConfigEdit: TfrmMarkSystemConfigEdit;

    FMarksExplorerView: TMarksExplorerView;

    FCategoriesTreeState: TMarksExplorerView.TCategoriesTreeState;
    FMarksExplorerViewScrollInfo: TMarksExplorerView.TScrollInfo;

    FUseAsIndepentWindow: Boolean;
    FMapGoto: IMapViewGoto;
    FMarkDBGUI: TMarkDbGUIHelper;
    FMarkSystemConfig: IMarkSystemConfigListChangeable;
    FGeometryLonLatFactory: IGeometryLonLatFactory;
    FMarksShowConfig: IUsedMarksConfig;
    FMarksExplorerConfig: IMarksExplorerConfig;
    FWindowConfig: IWindowPositionConfig;
    FViewPortState: ILocalCoordConverterChangeable;
    FNavToPoint: INavigationToPoint;

    FMarkSystemConfigListener: IListener;
    FCategoryDBListener: IListener;
    FMarksDBListener: IListener;
    FMarksShowConfigListener: IListener;
    FConfigListener: IListener;
    FMarksSystemStateListener: IListener;
    FMarksExplorerFilterListener: IListener;

    FRegionProcess: IRegionProcess;
    FMergePolygonsPresenter: IMergePolygonsPresenter;

    FCopyPasteAction: TCopyPasteAction;
    FCopyPasteBuffer: IInterfaceListStatic;

    FElevationProfilePresenter: IElevationProfilePresenter;

    FMarksExplorerFilter: IMarksExplorerFilter;

    procedure PrepareCopyPasteBuffer(const AAction: TCopyPasteAction);
    procedure ResetCopyPasteBuffer;

    procedure OnMarkSystemConfigChange;
    procedure OnCategoryDbChanged;
    procedure OnMarksDbChanged;
    procedure OnMarksShowConfigChanged;
    procedure OnConfigChange;
    procedure OnMarkSystemStateChanged;
    procedure OnMarksExplorerFilterChanged;
    procedure OnMarksViewChanged(Sender: TObject);

    procedure CheckWarningsState;

    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure ToggleVisible;
  public
    constructor Create(
      const AUseAsIndepentWindow: Boolean;
      const AGuiSyncronizedTimerNoifier: INotifierTime;
      const ALanguageManager: ILanguageManager;
      const AGeometryLonLatFactory: IGeometryLonLatFactory;
      const AViewPortState: ILocalCoordConverterChangeable;
      const ANavToPoint: INavigationToPoint;
      const AMarksExplorerConfig: IMarksExplorerConfig;
      const AMarksShowConfig: IUsedMarksConfig;
      const AMergePolygonsPresenter: IMergePolygonsPresenter;
      const AMarkDBGUI: TMarkDbGUIHelper;
      const AMarkSystemFactoryList: IMarkSystemImplFactoryListStatic;
      const AMarkSystemConfig: IMarkSystemConfigListChangeable;
      const AMapGoto: IMapViewGoto;
      const AElevationProfilePresenter: IElevationProfilePresenter;
      const ARegionProcess: IRegionProcess
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  t_GeoTypes,
  t_Listener,
  i_MarkDb,
  i_InterfaceListSimple,
  i_Category,
  i_ImportConfig,
  i_MarkTemplate,
  i_MarkCategoryFactory,
  i_MarkFactoryConfig,
  i_GeometryLonLat,
  u_Dialogs,
  u_ClipboardFunc,
  u_MarksExplorerFilter,
  u_InterfaceListSimple,
  u_ListenerByEvent;

{$R *.dfm}

constructor TfrmMarksExplorer.Create(
  const AUseAsIndepentWindow: Boolean;
  const AGuiSyncronizedTimerNoifier: INotifierTime;
  const ALanguageManager: ILanguageManager;
  const AGeometryLonLatFactory: IGeometryLonLatFactory;
  const AViewPortState: ILocalCoordConverterChangeable;
  const ANavToPoint: INavigationToPoint;
  const AMarksExplorerConfig: IMarksExplorerConfig;
  const AMarksShowConfig: IUsedMarksConfig;
  const AMergePolygonsPresenter: IMergePolygonsPresenter;
  const AMarkDBGUI: TMarkDbGUIHelper;
  const AMarkSystemFactoryList: IMarkSystemImplFactoryListStatic;
  const AMarkSystemConfig: IMarkSystemConfigListChangeable;
  const AMapGoto: IMapViewGoto;
  const AElevationProfilePresenter: IElevationProfilePresenter;
  const ARegionProcess: IRegionProcess
);

  function MakeListener(const AEvent: TNotifyListenerNoMmgEvent): IListener;
  begin
    Result := TNotifyEventListenerGuiSync.Create(AGuiSyncronizedTimerNoifier, 100, AEvent);
  end;

begin
  inherited Create(ALanguageManager);

  FUseAsIndepentWindow := AUseAsIndepentWindow;
  FMergePolygonsPresenter := AMergePolygonsPresenter;
  FMarkDBGUI := AMarkDBGUI;
  FMarkSystemConfig := AMarkSystemConfig;
  FGeometryLonLatFactory := AGeometryLonLatFactory;
  FMapGoto := AMapGoto;
  FMarksExplorerConfig := AMarksExplorerConfig;
  FWindowConfig := FMarksExplorerConfig.WindowPositionConfig;
  FMarksShowConfig := AMarksShowConfig;
  FViewPortState := AViewPortState;
  FNavToPoint := ANavToPoint;
  FRegionProcess := ARegionProcess;
  FElevationProfilePresenter := AElevationProfilePresenter;

  FMarkSystemConfigListener := MakeListener(Self.OnMarkSystemConfigChange);
  FCategoryDBListener := MakeListener(Self.OnCategoryDbChanged);
  FMarksDBListener := MakeListener(Self.OnMarksDbChanged);
  FMarksShowConfigListener := MakeListener(Self.OnMarksShowConfigChanged);
  FConfigListener := MakeListener(Self.OnConfigChange);
  FMarksSystemStateListener := MakeListener(Self.OnMarkSystemStateChanged);
  FMarksExplorerFilterListener := MakeListener(Self.OnMarksExplorerFilterChanged);

  FfrmMarkSystemConfigEdit :=
    TfrmMarkSystemConfigEdit.Create(
      Self,
      AMarkSystemFactoryList,
      FMarkSystemConfig
    );

  FMarksExplorerFilter := TMarksExplorerFilter.Create;
  FMarksExplorerFilter.ChangeNotifier.Add(FMarksExplorerFilterListener);

  FMarksExplorerView :=
    TMarksExplorerView.Create(
      pnlCategoriesTree,
      tbpmnCategories,
      pnlMarksList,
      tbpmnMarks,
      FMarkDBGUI,
      FMarksExplorerFilter
    );

  FMarksExplorerView.OnMarksViewChange := Self.OnMarksViewChanged;
  FMarksExplorerView.OnMarksViewDblClick := Self.btnGoToMarkClick;
end;

destructor TfrmMarksExplorer.Destroy;
begin
  if Assigned(FWindowConfig) and Assigned(FConfigListener) then begin
    FWindowConfig.ChangeNotifier.Remove(FConfigListener);
    FConfigListener := nil;
  end;
  if Assigned(FMarksExplorerFilter) and Assigned(FMarksExplorerFilterListener) then begin
    FMarksExplorerFilter.ChangeNotifier.Remove(FMarksExplorerFilterListener);
    FMarksExplorerFilterListener := nil;
  end;

  FreeAndNil(FMarksExplorerView);

  FreeAndNil(FfrmMarkSystemConfigEdit);
  FreeAndNil(FfrmMarksExplorerFilter);

  inherited;
end;

procedure TfrmMarksExplorer.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FUseAsIndepentWindow then begin
    // WS_Ex_AppWindow - shows on taskbar separately from main form
    Params.ExStyle := Params.ExStyle or WS_Ex_AppWindow;
    // Instead of default Application.Handle, make this window independent
    Params.WndParent := GetDesktopWindow; // attach to desktop
  end;
end;

procedure TfrmMarksExplorer.FormCreate(Sender: TObject);
begin
  if IsRectEmpty(FWindowConfig.BoundsRect) then begin
    Self.Width := 700;
    Self.Height := 500;
    Self.Position := poMainFormCenter;
    FWindowConfig.SetWindowPosition(Self.BoundsRect);
  end;
  FCategoriesTreeState.Selected := CategoryInfoFromString(FMarksExplorerConfig.SelectedCategory);
  FCategoriesTreeState.Expanded := CategoryInfoArrayFromString(FMarksExplorerConfig.ExpandedCategories);
end;

procedure TfrmMarksExplorer.FormShow(Sender: TObject);
var
  VWidth: Integer;
begin
  tbxWarning.Visible := False;
  btnNavOnMark.Checked := FNavToPoint.IsActive;

  FMarksExplorerView.CascadeChange := chkCascade.Checked;
  FMarksExplorerView.UpdateFull(@FCategoriesTreeState, @FMarksExplorerViewScrollInfo);

  FMarksShowConfig.ChangeNotifier.Add(FMarksShowConfigListener);
  FMarkSystemConfig.ChangeNotifier.Add(FMarkSystemConfigListener);
  FMarkDBGUI.MarksDb.CategoryDB.ChangeNotifier.Add(FCategoryDBListener);
  FMarkDBGUI.MarksDb.MarkDb.ChangeNotifier.Add(FMarksDBListener);
  FMarkDBGUI.MarksDb.State.ChangeNotifier.Add(FMarksSystemStateListener);
  FWindowConfig.ChangeNotifier.Add(FConfigListener);

  OnConfigChange;
  OnMarkSystemStateChanged;
  OnMarkSystemConfigChange;

  Self.OnResize := FormResize;

  VWidth := FMarksExplorerConfig.CategoriesWidth;
  if VWidth > 0 then begin
    grpCategory.Width := VWidth;
  end;

  CheckWarningsState;
end;

procedure TfrmMarksExplorer.FormActivate(Sender: TObject);
begin
  OnMarksShowConfigChanged;
end;

procedure TfrmMarksExplorer.FormHide(Sender: TObject);
begin
  Self.OnResize := nil;
  FMarksExplorerConfig.CategoriesWidth := grpCategory.Width;

  FMarkSystemConfig.ChangeNotifier.Remove(FMarkSystemConfigListener);
  FWindowConfig.ChangeNotifier.Remove(FConfigListener);
  FMarkDBGUI.MarksDb.CategoryDB.ChangeNotifier.Remove(FCategoryDBListener);
  FMarkDBGUI.MarksDb.MarkDb.ChangeNotifier.Remove(FMarksDBListener);
  FMarksShowConfig.ChangeNotifier.Remove(FMarksShowConfigListener);
  FMarkDBGUI.MarksDb.State.ChangeNotifier.Remove(FMarksSystemStateListener);

  FMarksExplorerViewScrollInfo := FMarksExplorerView.ScrollInfo;
  FMarksExplorerView.GetCategoriesTreeState(@FCategoriesTreeState);
  FMarksExplorerConfig.SelectedCategory := CategoryInfoToString(FCategoriesTreeState.Selected);
  FMarksExplorerConfig.ExpandedCategories := CategoryInfoArrayToString(FCategoriesTreeState.Expanded);
  FMarksExplorerView.Reset;

  FCopyPasteBuffer := nil;
  if Assigned(FfrmMarksExplorerFilter) then begin
    FfrmMarksExplorerFilter.Close;
  end;
end;

procedure TfrmMarksExplorer.FormMove(var Msg: TWMMove);
begin
  inherited;
  if Assigned(Self.OnResize) then begin
    Self.OnResize(Self);
  end;
end;

procedure TfrmMarksExplorer.FormResize(Sender: TObject);
begin
  if Self.WindowState = wsNormal then begin
    FWindowConfig.SetWindowPosition(BoundsRect);
  end;
end;

procedure TfrmMarksExplorer.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 { VK_ESCAPE } then begin
    Close;
  end;
end;

procedure TfrmMarksExplorer.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  inherited;
  Msg.MinMaxInfo.ptMinTrackSize.X := 406;
  Msg.MinMaxInfo.ptMinTrackSize.Y := 309;
end;

procedure TfrmMarksExplorer.ToggleVisible;
begin
  if (not Self.Visible) or (Self.WindowState = wsMinimized) then begin
    Self.Visible := True;
    Self.WindowState := wsNormal;
  end else begin
    Self.Visible := False;
  end;
end;

procedure TfrmMarksExplorer.btnAddCategoryClick(Sender: TObject);
var
  VCategory: IMarkCategory;
begin
  VCategory := FMarkDBGUI.MarksDb.CategoryDB.Factory.CreateNew('');
  VCategory := FMarkDBGUI.EditCategoryModal(VCategory, True);
  if VCategory <> nil then begin
    FMarkDBGUI.MarksDb.CategoryDB.UpdateCategory(nil, VCategory);
  end;
end;

procedure TfrmMarksExplorer.btnImportClick(Sender: TObject);
var
  VList: IInterfaceListStatic;
  VMark: IVectorDataItem;
begin
  VList := FMarkDBGUI.ImportModal(Self.Handle);
  if (Vlist <> nil) and (VList.Count > 0) then begin
    VMark := IVectorDataItem(VList[VList.Count - 1]);
    if VMark <> nil then begin
      FMapGoto.FitRectToScreen(VMark.Geometry.Bounds.Rect);
    end;
  end;
end;

procedure TfrmMarksExplorer.btnDeleteCategoryClick(Sender: TObject);
var
  VCategory: IMarkCategory;
begin
  VCategory := FMarksExplorerView.GetSelectedCategory;
  if VCategory <> nil then begin
    FMarkDBGUI.DeleteCategoryModal(VCategory, Self.Handle);
  end;
end;

procedure TfrmMarksExplorer.btnExportClick(Sender: TObject);
var
  VCategoryList: IMarkCategoryList;
  VOnlyVisible: Boolean;
begin
  VOnlyVisible := (TComponent(Sender).Tag = 1);
  if VOnlyVisible then begin
    VCategoryList := FMarkDBGUI.MarksDb.CategoryDB.GetVisibleCategoriesIgnoreZoom;
  end else begin
    VCategoryList := FMarkDBGUI.MarksDb.CategoryDB.GetCategoriesList;
  end;
  FMarkDBGUI.ExportCategoryList(VCategoryList, not VOnlyVisible);
end;

procedure TfrmMarksExplorer.btnDelMarkClick(Sender: TObject);
var
  VMarkIdList: IInterfaceListStatic;
begin
  VMarkIdList := FMarksExplorerView.GetSelectedMarksIdList;
  if VMarkIdList <> nil then begin
    FMarkDBGUI.DeleteMarksModal(VMarkIdList, Self.Handle);
  end;
end;

procedure TfrmMarksExplorer.btnEditMarkClick(Sender: TObject);
var
  I: Integer;
  VMarkIdList: IInterfaceListStatic;
  VMarksList: IInterfaceListSimple;
  VMark: IVectorDataItem;
  VMarkNew: IVectorDataItem;
  VImportConfig: IImportConfig;
  VParams: IImportMarkParams;
  VCategory: ICategory;
  VMarkId: IMarkId;
  VVisible: Boolean;
  VResult: IVectorDataItem;
  VResultList: IInterfaceListStatic;
begin
  VMarkIdList := FMarksExplorerView.GetSelectedMarksIdList;
  if VMarkIdList <> nil then begin
    if VMarkIdList.Count = 1 then begin
      VMark := FMarkDBGUI.MarksDb.MarkDb.GetMarkByID(IMarkId(VMarkIdList[0]));
      VVisible := FMarkDBGUI.MarksDb.MarkDb.GetMarkVisible(VMark);
      VMarkNew := FMarkDBGUI.EditMarkModal(VMark, False, VVisible);
      if VMarkNew <> nil then begin
        VResult := FMarkDBGUI.MarksDb.MarkDb.UpdateMark(VMark, VMarkNew);
        if VResult <> nil then begin
          FMarkDBGUI.MarksDb.MarkDb.SetMarkVisible(VResult, VVisible);
        end;
      end;
    end else begin
      VImportConfig := FMarkDBGUI.MarksMultiEditModal(FMarksExplorerView.GetSelectedCategory);
      if VImportConfig <> nil then begin
        VCategory := VImportConfig.RootCategory;
        VMarksList := TInterfaceListSimple.Create;
        for I := 0 to VMarkIdList.Count - 1 do begin
          VMarkId := IMarkId(VMarkIdList[I]);
          VMark := FMarkDBGUI.MarksDb.MarkDb.GetMarkByID(VMarkId);
          VParams := nil;
          if Supports(VMark.Geometry, IGeometryLonLatPoint) then begin
            VParams := VImportConfig.PointParams;
          end else if Supports(VMark.Geometry, IGeometryLonLatLine) then begin
            VParams := VImportConfig.LineParams;
          end else if Supports(VMark.Geometry, IGeometryLonLatPolygon) then begin
            VParams := VImportConfig.PolyParams;
          end;
          VMarkNew :=
            FMarkDBGUI.MarksDb.MarkDb.Factory.PrepareMark(
              VMark,
              VMark.Name,
              VParams,
              VCategory
            );
          if VMarkNew <> nil then begin
            VMarksList.Add(VMarkNew);
          end else begin
            VMarksList.Add(VMark);
          end;
        end;
        if VMarksList <> nil then begin
          VResultList := FMarkDBGUI.MarksDb.MarkDb.UpdateMarkList(VMarkIdList, VMarksList.MakeStaticAndClear);
          if VResultList <> nil then begin
            FMarkDBGUI.MarksDb.MarkDb.SetMarkVisibleByIDList(VResultList, True);
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmMarksExplorer.tbitmEditMarkPositionClick(Sender: TObject);
var
  VMark: IVectorDataItem;
begin
  VMark := FMarksExplorerView.GetSelectedMarkFull;
  if Assigned(VMark) and Assigned(VMark.Geometry.Bounds) then begin
    FMapGoto.FitRectToScreen(VMark.Geometry.Bounds.Rect);
    FMarkDBGUI.EditMarkPosition(VMark);
  end;
end;

procedure TfrmMarksExplorer.tbitmFilterClick(Sender: TObject);
begin
  if FMarksExplorerFilter.Enabled then begin
    FMarksExplorerFilter.Enabled := False;
    Exit;
  end;
  if not Assigned(FfrmMarksExplorerFilter) then begin
    FfrmMarksExplorerFilter :=
      TfrmMarksExplorerFilter.Create(
        Self.LanguageManager,
        FMarksExplorerFilter
      );
  end;
  FfrmMarksExplorerFilter.Show;
end;

procedure TfrmMarksExplorer.btnGoToMarkClick(Sender: TObject);
var
  VMark: IVectorDataItem;
begin
  VMark := FMarksExplorerView.GetSelectedMarkFull;
  if Assigned(VMark) and Assigned(VMark.Geometry.Bounds) then begin
    FMapGoto.FitRectToScreen(VMark.Geometry.Bounds.Rect);
    FMapGoto.ShowMarker(VMark.Geometry.GetGoToPoint);
  end;
end;

procedure TfrmMarksExplorer.btnNavOnMarkClick(Sender: TObject);
var
  VMark: IVectorDataItem;
  LL: TDoublePoint;
  VMarkStringId: string;
begin
  if btnNavOnMark.Checked then begin
    VMark := FMarksExplorerView.GetSelectedMarkFull;
    if VMark <> nil then begin
      LL := VMark.Geometry.GetGoToPoint;
      VMarkStringId := FMarkDBGUI.MarksDb.GetStringIdByMark(VMark);
      FNavToPoint.StartNavToMark(VMarkStringId, LL);
    end else begin
      btnNavOnMark.Checked := not btnNavOnMark.Checked;
    end;
  end else begin
    FNavToPoint.StopNav;
  end;
end;

procedure TfrmMarksExplorer.btnOpSelectMarkClick(Sender: TObject);
var
  VMark: IVectorDataItem;
  VPolygon: IGeometryLonLatPolygon;
begin
  VMark := FMarksExplorerView.GetSelectedMarkFull;
  if VMark <> nil then begin
    VPolygon := FMarkDBGUI.PolygonForOperation(VMark.Geometry);
    if VPolygon <> nil then begin
      FRegionProcess.ProcessPolygon(VPolygon);
      ModalResult := mrOk;
    end;
  end;
end;

procedure TfrmMarksExplorer.btnSaveMarkClick(Sender: TObject);
var
  VMark: IVectorDataItem;
  VMarksIdList: IInterfaceListStatic;
  VSelectedCount: Integer;
begin
  VSelectedCount := FMarksExplorerView.GetMarksCount.Selected;
  if VSelectedCount = 1 then begin
    VMark := FMarksExplorerView.GetSelectedMarkFull;
    if VMark <> nil then begin
      FMarkDBGUI.ExportMark(VMark);
    end;
  end else
  if VSelectedCount > 1 then begin
    VMarksIdList := FMarksExplorerView.GetSelectedMarksIdList;
    if VMarksIdList <> nil then begin
      FMarkDBGUI.ExportMarksList(VMarksIdList);
    end;
  end else begin
    ShowErrorMessage(_('No placemarks selected!'));
  end;
end;

procedure TfrmMarksExplorer.btnEditCategoryClick(Sender: TObject);
var
  VCategoryOld: IMarkCategory;
  VCategoryNew: IMarkCategory;
begin
  VCategoryOld := FMarksExplorerView.GetSelectedCategory;
  if VCategoryOld <> nil then begin
    VCategoryNew := FMarkDBGUI.EditCategoryModal(VCategoryOld, False);
    if VCategoryNew <> nil then begin
      FMarkDBGUI.MarksDb.CategoryDB.UpdateCategory(VCategoryOld, VCategoryNew);
    end;
  end;
end;

procedure TfrmMarksExplorer.btnExportCategoryClick(Sender: TObject);
var
  VCategory: IMarkCategory;
  VOnlyVisible: Boolean;
begin
  VCategory := FMarksExplorerView.GetSelectedCategory;
  if VCategory <> nil then begin
    VOnlyVisible := (TComponent(Sender).tag = 1);
    FMarkDBGUI.ExportCategory(VCategory, not VOnlyVisible);
  end;
end;

procedure TfrmMarksExplorer.tbitmAddCategoryClick(Sender: TObject);
var
  VCategory: IMarkCategory;
  VNewName: string;
begin
  VNewName := '';
  VCategory := FMarksExplorerView.GetSelectedCategory;
  if VCategory <> nil then begin
    VNewName := VCategory.Name + '\';
  end;

  VCategory := FMarkDBGUI.MarksDb.CategoryDB.Factory.CreateNew(VNewName);
  VCategory := FMarkDBGUI.EditCategoryModal(VCategory, True);
  if VCategory <> nil then begin
    FMarkDBGUI.MarksDb.CategoryDB.UpdateCategory(nil, VCategory);
  end;
end;

procedure TfrmMarksExplorer.tbitmAddMarkClick(Sender: TObject);
var
  VLonLat: TDoublePoint;
  VPointTemplate: IMarkTemplatePoint;
  VTemplateConfig: IMarkPointTemplateConfig;
  VCategory: ICategory;
  VPoint: IGeometryLonLatPoint;
begin
  VLonLat := FViewPortState.GetStatic.GetCenterLonLat;
  VCategory := FMarksExplorerView.GetSelectedCategory;
  VPointTemplate := nil;
  if VCategory <> nil then begin
    VTemplateConfig := FMarkDBGUI.MarkFactoryConfig.PointTemplateConfig;
    VPointTemplate := VTemplateConfig.DefaultTemplate;
    VPointTemplate :=
      VTemplateConfig.CreateTemplate(
        VPointTemplate.Appearance,
        VCategory
      );
  end;
  VPoint := FGeometryLonLatFactory.CreateLonLatPoint(VLonLat);
  FMarkDBGUI.SaveMarkModal(nil, VPoint, False, '', VPointTemplate);
end;

procedure TfrmMarksExplorer.tbitmMarkInfoClick(Sender: TObject);
var
  VMark: IVectorDataItem;
begin
  VMark := FMarksExplorerView.GetSelectedMarkFull;
  if VMark <> nil then begin
    FMarkDBGUI.ShowMarkInfo(VMark);
  end;
end;

procedure TfrmMarksExplorer.chkSetAllCategoriesVisibleClick(Sender: TObject);
var
  VNewVisible: Boolean;
begin
  if FMarksExplorerView.GetCategoriesCount.Total > 0 then begin
    VNewVisible := chkSetAllCategoriesVisible.Checked;
    FMarkDBGUI.MarksDb.CategoryDB.SetAllCategoriesVisible(VNewVisible);
  end;
end;

procedure TfrmMarksExplorer.chkCascadeClick(Sender: TObject);
begin
  FMarksExplorerView.CascadeChange := chkCascade.Checked;
end;

procedure TfrmMarksExplorer.chkSetAllMarksInCategoryVisibleClick(Sender: TObject);
var
  VNewVisible: Boolean;
  VCategory: IMarkCategory;
begin
  VCategory := FMarksExplorerView.GetSelectedCategory;
  if VCategory <> nil then begin
    VNewVisible := chkSetAllMarksInCategoryVisible.Checked;
    FMarkDBGUI.MarksDb.MarkDb.SetAllMarksInCategoryVisible(VCategory, VNewVisible);
  end;
end;

procedure TfrmMarksExplorer.rgMarksShowModeClick(Sender: TObject);
begin
  FMarksShowConfig.LockWrite;
  try
    case rgMarksShowMode.ItemIndex of
      0: begin
        FMarksShowConfig.IsUseMarks := True;
        FMarksShowConfig.IgnoreCategoriesVisible := False;
        FMarksShowConfig.IgnoreMarksVisible := False;
      end;
      1: begin
        FMarksShowConfig.IsUseMarks := True;
        FMarksShowConfig.IgnoreCategoriesVisible := True;
        FMarksShowConfig.IgnoreMarksVisible := True;
      end;
    else
      FMarksShowConfig.IsUseMarks := False;
    end;
  finally
    FMarksShowConfig.UnlockWrite;
  end;
end;

procedure TfrmMarksExplorer.tbitmAllVisibleClick(Sender: TObject);
var
  VMarksDb: IMarkDb;
  VMarksList: IInterfaceListStatic;
begin
  VMarksDb := FMarkDBGUI.MarksDb.MarkDb;
  VMarksList := VMarksDb.GetAllMarkIdList;
  VMarksDb.SetMarkVisibleByIDList(VMarksList, True);
end;

procedure TfrmMarksExplorer.tbpmnCategoriesPopup(Sender: TObject);
begin
  if FMarksExplorerView.GetSelectedCategory = nil then begin
    tbitmAddCategory.Caption := _('Add Category');
  end else begin
    tbitmAddCategory.Caption := _('Add SubCategory');
  end;
end;

procedure TfrmMarksExplorer.tbpmnMarksPopup(Sender: TObject);
type
  TMarksCountId = (mcTotal, mcLine, mcMultiLine, mcPoly, mcMultiPoly);
var
  I: Integer;
  VMarkIdArray: TArrayOfMarkId;
  VCount: array [TMarksCountId] of Integer;
  VMark: IVectorDataItem;
  VGeometry: IGeometryLonLat;
  VLine: IGeometryLonLatMultiLine;
  VPolygon: IGeometryLonLatMultiPolygon;
begin
  FillChar(VCount, Length(VCount) * SizeOf(VCount[mcTotal]), 0);

  VMarkIdArray := FMarksExplorerView.GetSelectedMarksIdArray;

  for I := 0 to Length(VMarkIdArray) - 1 do begin
    if not Assigned(VMarkIdArray[I]) then begin
      Continue;
    end;

    VMark := FMarkDBGUI.MarksDb.MarkDb.GetMarkByID(VMarkIdArray[I]);
    if not Assigned(VMark) then begin
      Continue;
    end;

    Inc(VCount[mcTotal]);

    VGeometry := VMark.Geometry;
    if Supports(VGeometry, IGeometryLonLatSinglePolygon) then begin
      Inc(VCount[mcPoly]);
    end else
    if Supports(VGeometry, IGeometryLonLatMultiPolygon, VPolygon) then begin
      if VPolygon.Count = 1 then begin
        Inc(VCount[mcPoly]);
      end else begin
        Inc(VCount[mcMultiPoly]);
      end;
    end else
    if Supports(VGeometry, IGeometryLonLatSingleLine) then begin
      Inc(VCount[mcLine]);
    end else
    if Supports(VGeometry, IGeometryLonLatMultiLine, VLine) then begin
      if VLine.Count = 1 then begin
        Inc(VCount[mcLine]);
      end else begin
        Inc(VCount[mcMultiLine]);
      end;
    end else begin
      Dec(VCount[mcTotal]);
    end;
  end;

  tbxtmGroup.Visible :=
    (VCount[mcTotal] > 1) and
    ((VCount[mcTotal] = VCount[mcLine] + VCount[mcMultiLine]) or
    (VCount[mcTotal] = VCount[mcPoly] + VCount[mcMultiPoly]));

  tbxtmUngroup.Visible :=
    (VCount[mcTotal] = 1) and
    ((VCount[mcMultiPoly] = 1) or (VCount[mcMultiLine] = 1));

  tbxShowElevProfile.Visible :=
    (VCount[mcTotal] = 1) and
    ((VCount[mcLine] = 1) or (VCount[mcMultiLine] = 1));

  tbxtmAddToMergePolygons.Visible :=
    (VCount[mcPoly] > 0) or
    (VCount[mcMultiPoly] > 0);

  tbxtmCopyBboxToClipboard.Visible :=
    (VCount[mcTotal] = 1);
end;

procedure TfrmMarksExplorer.tbxSelectAllVisibleClick(Sender: TObject);
begin
  FMarksExplorerView.SelectAllVisibleMarks;
end;

procedure TfrmMarksExplorer.tbxRevertSelectionClick(Sender: TObject);
begin
  FMarksExplorerView.RevertSelectedMarks;
end;

procedure TfrmMarksExplorer.tbxShowElevProfileClick(Sender: TObject);
var
  VMark: IVectorDataItem;
begin
  VMark := FMarksExplorerView.GetSelectedMarkFull;
  if Assigned(VMark) and Supports(VMark.Geometry, IGeometryLonLatLine) then begin
    FMapGoto.FitRectToScreen(VMark.Geometry.Bounds.Rect);
    FElevationProfilePresenter.ShowProfile(VMark);
  end else begin
    Assert(False);
  end;
end;

procedure TfrmMarksExplorer.tbxtmGroupClick(Sender: TObject);
var
  VMarks: IInterfaceListStatic;
begin
  VMarks := FMarksExplorerView.GetSelectedMarksIdList(True);
  if Assigned(VMarks) then begin
    FMarkDBGUI.SaveMarksGroupModal(VMarks);
  end;
end;

procedure TfrmMarksExplorer.tbxtmUngroupClick(Sender: TObject);
var
  VMark: IVectorDataItem;
begin
  VMark := FMarksExplorerView.GetSelectedMarkFull;
  if Assigned(VMark) then begin
    FMarkDBGUI.SaveMarkUngroupModal(VMark, VMark.Geometry);
  end;
end;

procedure TfrmMarksExplorer.CheckWarningsState;
begin
  tbxWarning.Visible := FMarksExplorerView.VirtualCategoriesCount > 0;
end;

procedure TfrmMarksExplorer.tbxWarningClick(Sender: TObject);
var
  VMsg: string;
begin
  if FMarksExplorerView.VirtualCategoriesCount > 0 then begin
    VMsg := Format(_('Detected virtual categories: %d item(s)!'), [FMarksExplorerView.VirtualCategoriesCount]);
  end else begin
    VMsg := 'Internal error: Unknown warning reason!';
  end;
  ShowWarningMessage(VMsg);
end;

procedure TfrmMarksExplorer.tbxtmAddToMergePolygonsClick(Sender: TObject);
begin
  FMarkDBGUI.AddMarkIdListToMergePolygons(FMarksExplorerView.GetSelectedMarksIdList, FMergePolygonsPresenter);
end;

procedure TfrmMarksExplorer.tbxtmCatAddToMergePolygonsClick(Sender: TObject);
begin
  FMarkDBGUI.AddCategoryToMergePolygons(FMarksExplorerView.GetSelectedCategory, FMergePolygonsPresenter);
end;

procedure TfrmMarksExplorer.tbxtmCopyBboxToClipboardClick(Sender: TObject);
var
  VMark: IVectorDataItem;
begin
  VMark := FMarksExplorerView.GetSelectedMarkFull;
  if Assigned(VMark) then begin
    CopyStringToClipboard(Handle, FMarkDBGUI.MarkToBbox(VMark));
  end else begin
    Assert(False);
  end;
end;

procedure TfrmMarksExplorer.tbxConfigListItemClick(Sender: TObject);
var
  VMenuItem: TTBXItem;
  VConfig: IMarkSystemConfigStatic;
begin
  VMenuItem := Sender as TTBXItem;
  if VMenuItem.Checked then begin
    Exit;
  end;
  VConfig := FMarkSystemConfig.GetByID(VMenuItem.Tag);
  FMarkSystemConfig.ActiveConfigID := VConfig.ID;
end;

procedure TfrmMarksExplorer.tbxDeleteClick(Sender: TObject);
var
  VMsg: string;
  VConfig: IMarkSystemConfigStatic;
begin
  VConfig := FMarkSystemConfig.GetActiveConfig;
  VMsg := Format(_('Are you sure you want to delete database "%s"?'), [VConfig.DisplayName]);
  if ShowQuestionMessage(VMsg, MB_YESNO) = ID_YES then begin
    FMarkSystemConfig.DeleteByID(VConfig.ID);
  end;
end;

procedure TfrmMarksExplorer.PrepareCopyPasteBuffer(const AAction: TCopyPasteAction);
begin
  FCopyPasteBuffer := FMarksExplorerView.GetSelectedMarksIdList;
  if FCopyPasteBuffer <> nil then begin
    tbitmPaste.Visible := True;
    FCopyPasteAction := AAction;
  end else begin
    ResetCopyPasteBuffer;
  end;
end;

procedure TfrmMarksExplorer.ResetCopyPasteBuffer;
begin
  FCopyPasteBuffer := nil;
  tbitmPaste.Visible := False;
  FCopyPasteAction := cpNone;
end;

procedure TfrmMarksExplorer.tbitmCopyAsTextClick(Sender: TObject);
var
  VMarksIdList: IInterfaceListStatic;
begin
  VMarksIdList := FMarksExplorerView.GetSelectedMarksIdList;
  if VMarksIdList <> nil then begin
    CopyStringToClipboard(Handle, FMarkDBGUI.MarkIdListToText(VMarksIdList));
  end;
end;

procedure TfrmMarksExplorer.tbitmCopyClick(Sender: TObject);
begin
  PrepareCopyPasteBuffer(cpCopy);
end;

procedure TfrmMarksExplorer.tbitmCutClick(Sender: TObject);
begin
  PrepareCopyPasteBuffer(cpCut);
end;

procedure TfrmMarksExplorer.tbitmPasteClick(Sender: TObject);

  procedure PasteMarks(const AMove: Boolean);
  var
    I: Integer;
    VMarkIdListNew: IInterfaceListSimple;
    VCategoryNew: ICategory;
    VMark: IVectorDataItem;
    VMarkDb: IMarkDb;
  begin
    VCategoryNew := FMarksExplorerView.GetSelectedCategory;
    if VCategoryNew <> nil then begin
      VMarkDb := FMarkDBGUI.MarksDb.MarkDb;
      VMarkIdListNew := TInterfaceListSimple.Create;
      for I := 0 to FCopyPasteBuffer.Count - 1 do begin
        VMark := VMarkDb.GetMarkByID(IMarkId(FCopyPasteBuffer.Items[I]));
        VMarkIdListNew.Add(VMarkDb.Factory.ReplaceCategory(VMark, VCategoryNew));
      end;
      if AMove then begin
        VMarkDb.UpdateMarkList(FCopyPasteBuffer, VMarkIdListNew.MakeStaticAndClear);
      end else begin
        VMarkDb.UpdateMarkList(nil, VMarkIdListNew.MakeStaticAndClear);
      end;
    end;
  end;

begin
  Assert(FCopyPasteBuffer <> nil);

  case FCopyPasteAction of
    cpCopy: PasteMarks(False);
    cpCut: PasteMarks(True);
  else
    Assert(False);
  end;

  ResetCopyPasteBuffer;
end;

procedure TfrmMarksExplorer.tbxEditClick(Sender: TObject);
begin
  FfrmMarkSystemConfigEdit.EditActiveDatabaseConfig;
end;

procedure TfrmMarksExplorer.tbxAddClick(Sender: TObject);
begin
  FfrmMarkSystemConfigEdit.AddNewDatabaseConfig;
end;

procedure TfrmMarksExplorer.OnMarksViewChanged(Sender: TObject);
var
  VCount: TMarksExplorerView.PTreeCounts;
begin
  VCount := FMarksExplorerView.GetMarksCount;
  if VCount.Total < 0 then begin
    lblMarksCount.Caption := '';
  end else
  if VCount.Total > 0 then begin
    lblMarksCount.Caption := Format('(%d/%d)', [VCount.Visible, VCount.Total]);
  end else begin
    lblMarksCount.Caption := '(0/0)';
  end;
end;

procedure TfrmMarksExplorer.OnCategoryDbChanged;
begin
  FMarksExplorerView.UpdateFull;
  CheckWarningsState;
end;

procedure TfrmMarksExplorer.OnMarksDbChanged;
begin
  FMarksExplorerView.UpdateMarksList;
  CheckWarningsState;
end;

procedure TfrmMarksExplorer.OnMarksExplorerFilterChanged;
begin
  tbitmFilter.Checked := FMarksExplorerFilter.Enabled;
  FMarksExplorerView.UpdateMarksList;
end;

procedure TfrmMarksExplorer.OnConfigChange;
var
  VRect: TRect;
begin
  VRect := FWindowConfig.BoundsRect;
  if EqualRect(BoundsRect, VRect) then begin
    Exit;
  end;
  UpdateRectByMonitors(VRect);
  if EqualRect(BoundsRect, VRect) then begin
    Exit;
  end;
  BoundsRect := VRect;
end;

procedure TfrmMarksExplorer.OnMarksShowConfigChanged;
var
  VMarksConfig: IUsedMarksConfigStatic;
begin
  VMarksConfig := FMarksShowConfig.GetStatic;
  if VMarksConfig.IsUseMarks then begin
    if VMarksConfig.IgnoreCategoriesVisible and VMarksConfig.IgnoreMarksVisible then begin
      rgMarksShowMode.ItemIndex := 1;
    end else begin
      rgMarksShowMode.ItemIndex := 0;
    end;
  end else begin
    rgMarksShowMode.ItemIndex := 2;
  end;
end;

procedure TfrmMarksExplorer.OnMarkSystemStateChanged;
begin
  ResetCopyPasteBuffer;
  lblReadOnly.Visible := not FMarkDBGUI.MarksDb.State.GetStatic.WriteAccess;
end;

procedure TfrmMarksExplorer.OnMarkSystemConfigChange;
var
  VIsEnabled: Boolean;
begin
  RefreshConfigListMenu(
    tbxConfigList,
    True,
    Self.tbxConfigListItemClick,
    FMarkSystemConfig
  );

  VIsEnabled := tbxConfigList.Enabled;

  tbxEdit.Enabled := VIsEnabled;
  tbxDelete.Enabled := VIsEnabled;
end;

end.
