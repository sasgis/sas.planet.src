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

unit fr_MergePolygons;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  ComCtrls,
  Dialogs,
  ExtCtrls,
  TBX,
  TBXExtItems,
  TB2Item,
  TB2ExtItems,
  TB2Dock,
  TB2Toolbar,
  i_NotifierTime,
  i_NotifierOperation,
  i_MapViewGoto,
  i_RegionProcess,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_VectorItemSubset,
  i_VectorDataFactory,
  i_VectorDataItemSimple,
  i_LanguageManager,
  i_MergePolygonsResult,
  i_MergePolygonsProgress,
  t_MergePolygonsProcessor,
  u_MergePolygonsProcessor,
  u_MarkDbGUIHelper,
  u_CommonFormAndFrameParents,
  frm_MergePolygonsProgress;

type
  TfrMergePolygons = class(TFrame)
    tvPolygonsList: TTreeView;
    tbTop: TTBXToolbar;
    tbMerge: TTBItem;
    tbxSep1: TTBXSeparatorItem;
    tbxSep2: TTBXSeparatorItem;
    tbtmSelect: TTBItem;
    tbtmSave: TTBItem;
    tbSep3: TTBSeparatorItem;
    tmrProgressCheck: TTimer;
    tbtmClear: TTBItem;
    tbOperationType: TTBSubmenuItem;
    tbtmAND: TTBItem;
    tbtmOR: TTBItem;
    tbtmNOT: TTBItem;
    tbtmXOR: TTBItem;
    tbSep4: TTBSeparatorItem;
    tbtmGroup: TTBItem;
    procedure tvPolygonsListAddition(Sender: TObject; Node: TTreeNode);
    procedure tbMergeClick(Sender: TObject);
    procedure tbUpClick(Sender: TObject);
    procedure tbDownClick(Sender: TObject);
    procedure tbDelClick(Sender: TObject);
    procedure tvPolygonsListDblClick(Sender: TObject);
    procedure tbtmSaveClick(Sender: TObject);
    procedure tbtmSelectClick(Sender: TObject);
    procedure tmrProgressCheckTimer(Sender: TObject);
    procedure tvPolygonsListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvPolygonsListDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvPolygonsListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tbtmClearClick(Sender: TObject);
    procedure OnOperationClick(Sender: TObject);
  private
    FfrmProgress: TfrmMergePolygonsProgress;
    FAppClosingNotifier: INotifierOneOperation;
    FNotifierTimeInternal: INotifierTimeInternal;
    FCancelNotifier: INotifierOperationInternal;
    FCurrentOperation: Integer;
    FNeedShowProgress: Boolean;
    FMapGoto: IMapViewGoto;
    FRegionProcess: IRegionProcess;
    FMarkDBGUI: TMarkDbGUIHelper;
    FItems: TMergePolygonsItemArray;
    FMergeProcessor: TMergePolygonsProcessor;
    FMergeResultInternal: IVectorDataItem;
    FMergePolygonsResult: IMergePolygonsResult;
    FMergePolygonsProgress: IMergePolygonsProgress;
    FOperation: TMergeOperation;
    procedure ResetMergeResult;
    procedure RebuildTree;
    function IsDublicate(const AItem: TMergePolygonsItem): Boolean;
    procedure SwapItems(const A, B: Integer);
    procedure SwapNodesText(const A, B: TTreeNode);
    procedure OnMergeFinished;
    function AddItemInternal(const AItem: IVectorDataItem): Boolean;
  public
    procedure AddItem(const AItem: IVectorDataItem);
    procedure AddItems(const AItems: IVectorItemSubset);
    procedure Clear;
  public
    constructor Create(
      AParent: TWinControl;
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AMergePolygonsResult: IMergePolygonsResult;
      const AMapGoto: IMapViewGoto;
      const ARegionProcess: IRegionProcess;
      const AMarkDBGUI: TMarkDbGUIHelper
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  DateUtils,
  t_GeoTypes,
  u_Synchronizer,
  u_Notifier,
  u_NotifierTime,
  u_NotifierOperation,
  u_MergePolygonsProgress;

resourcestring
  rsSubject = '[subject]';
  rsClip = '[clip]';

  rsErrorNeedMerge = 'You must MERGE polygons first!';
  rsErrorTwoPolygons = 'You must add at least TWO polygons!';

  rsMergeFail = 'Merge failed!';

  rsMergeFinish = 'Merge process completed successfully!' + #13#10 +
                  'Result contains %d polygon(s) with %d hole(s)';

  rsProcessedAt = 'Processed at: %.8f sec.';

{$R *.dfm}

procedure MarkAsSubject(ANode: TTreeNode); inline;
begin
  if Pos(rsSubject, ANode.Text) = 0 then begin
    ANode.Text := StringReplace(ANode.Text, rsClip, '', [rfReplaceAll, rfIgnoreCase]);
    ANode.Text := Trim(ANode.Text) + ' ' + rsSubject;
  end;
end;

procedure MarkAsClip(ANode: TTreeNode); inline;
begin
  if Pos(rsClip, ANode.Text) = 0 then begin
    ANode.Text := StringReplace(ANode.Text, rsSubject, '', [rfReplaceAll, rfIgnoreCase]);
    ANode.Text := Trim(ANode.Text) + ' ' + rsClip;
  end;
end;

procedure CopyItem(const ASrc: TMergePolygonsItem; out ADest: TMergePolygonsItem); inline;
begin
  ADest.Name := ASrc.Name;
  ADest.VectorData := ASrc.VectorData;
  ADest.MultiPolygon := ASrc.MultiPolygon;
  ADest.SinglePolygon := ASrc.SinglePolygon;
end;

procedure InitItem(
  const AVectorData: IVectorDataItem;
  out AItem: TMergePolygonsItem
);
begin
  AItem.VectorData := AVectorData;
  AItem.MultiPolygon := nil;
  AItem.SinglePolygon := nil;

  if Supports(AVectorData.Geometry, IGeometryLonLatMultiPolygon, AItem.MultiPolygon) then begin
    if AItem.MultiPolygon.Count = 1 then begin
      AItem.SinglePolygon := AItem.MultiPolygon.Item[0];
      AItem.MultiPolygon := nil;
    end;
  end else begin
    if not Supports(AVectorData.Geometry, IGeometryLonLatSinglePolygon, AItem.SinglePolygon) then begin
      raise Exception.Create('Unsupported GeometryLonLatPolygon interface!');
    end;
  end;

  if Assigned(AItem.MultiPolygon) then begin
    AItem.Name := AVectorData.Name + ' (' + 'Multi ' + IntToStr(AItem.MultiPolygon.Count) + ')';
  end else begin
    AItem.Name := AVectorData.Name + ' (' + 'Single' + ')';
  end;
end;

{ TfrMergePolygons }

constructor TfrMergePolygons.Create(
  AParent: TWinControl;
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AMergePolygonsResult: IMergePolygonsResult;
  const AMapGoto: IMapViewGoto;
  const ARegionProcess: IRegionProcess;
  const AMarkDBGUI: TMarkDbGUIHelper
);
begin
  inherited Create(ALanguageManager);

  Parent := AParent;
  FAppClosingNotifier := AAppClosingNotifier;
  FMergePolygonsResult := AMergePolygonsResult;
  FMapGoto := AMapGoto;
  FRegionProcess := ARegionProcess;
  FMarkDBGUI := AMarkDBGUI;

  SetLength(FItems, 0);

  FNotifierTimeInternal :=
    TNotifierTime.Create(GSync.SyncStd.Make(Self.ClassName + 'TimerNotifier'));

  FCancelNotifier :=
    TNotifierOperation.Create(
      TNotifierBase.Create(GSync.SyncStd.Make(Self.ClassName + 'CancelNotifier'))
    );

  FCurrentOperation := FCancelNotifier.CurrentOperation;
  FCancelNotifier.NextOperation;

  FMergePolygonsProgress := TMergePolygonsProgress.Create;

  FMergeProcessor :=
    TMergePolygonsProcessor.Create(
      FMergePolygonsProgress,
      AAppClosingNotifier,
      FCancelNotifier,
      AVectorDataFactory,
      AVectorGeometryLonLatFactory
    );

  ResetMergeResult;

  FOperation := moOR;
  tbtmOR.Click;

  FfrmProgress :=
    TfrmMergePolygonsProgress.Create(
      Self,
      tmrProgressCheck.Interval,
      FAppClosingNotifier,
      FNotifierTimeInternal,
      FCancelNotifier,
      FMergePolygonsProgress
    );
end;

destructor TfrMergePolygons.Destroy;
begin
  Clear;
  FreeAndNil(FMergeProcessor);
  FreeAndNil(FfrmProgress);
  inherited Destroy;
end;

function TfrMergePolygons.AddItemInternal(const AItem: IVectorDataItem): Boolean;
var
  I: Integer;
  VItem: TMergePolygonsItem;
begin
  Result := False;

  InitItem(AItem, VItem);

  if not IsDublicate(VItem) then begin
    I := Length(FItems);
    SetLength(FItems, I+1);

    CopyItem(VItem, FItems[I]);

    tvPolygonsList.Items.AddChildObject(nil, FItems[I].Name, Pointer(I));

    Result := True;
  end;
end;

procedure TfrMergePolygons.AddItem(const AItem: IVectorDataItem);
begin
  Assert(Assigned(AItem));
  if Supports(AItem.Geometry, IGeometryLonLatPolygon) then begin
    if AddItemInternal(AItem) then begin
      ResetMergeResult;
    end;
  end;
end;

procedure TfrMergePolygons.AddItems(const AItems: IVectorItemSubset);
var
  I: Integer;
  VCount: Integer;
  VVectorItem: IVectorDataItem;
begin
  Assert(Assigned(AItems));

  VCount := 0;

  tvPolygonsList.Items.BeginUpdate;
  try
    for I := 0 to AItems.Count - 1 do begin
      VVectorItem := AItems.Items[I];
      if Supports(VVectorItem.Geometry, IGeometryLonLatPolygon) then begin
        if AddItemInternal(VVectorItem) then begin
          Inc(VCount);
        end;
      end;
    end;
  finally
    tvPolygonsList.Items.EndUpdate;
  end;

  if VCount > 0 then begin
    ResetMergeResult;
  end;
end;

function TfrMergePolygons.IsDublicate(const AItem: TMergePolygonsItem): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Length(FItems) - 1 do begin
    if Assigned(FItems[I].MultiPolygon) and Assigned(AItem.MultiPolygon) then begin
      if FItems[I].MultiPolygon.IsSame(AItem.MultiPolygon) then begin
        Exit;
      end;
    end else if Assigned(FItems[I].SinglePolygon) and Assigned(AItem.SinglePolygon) then begin
      if FItems[I].SinglePolygon.IsSame(AItem.SinglePolygon) then begin
        Exit;
      end;
    end;
  end;
  Result := False;
end;

procedure TfrMergePolygons.tbMergeClick(Sender: TObject);
begin
  if Length(FItems) < 2 then begin
    MessageDlg(rsErrorTwoPolygons, mtError, [mbOK], 0);
    Exit;
  end;

  ResetMergeResult;

  Self.Enabled := False;
  tmrProgressCheck.Enabled := True;
  FNeedShowProgress := True;

  FCancelNotifier.NextOperation;
  FCurrentOperation := FCancelNotifier.CurrentOperation;

  FMergeProcessor.MergeAsync(FItems, FOperation);
end;

procedure TfrMergePolygons.OnMergeFinished;
var
  VMessage: string;
  VTime: Double;
  VPolygonsCount: Integer;
  VHolesCount: Integer;
  VVectorItem: IVectorDataItem;
begin
  Self.Enabled := True;

  if not FCancelNotifier.IsOperationCanceled(FCurrentOperation) then begin
    FCancelNotifier.NextOperation;
    FMergePolygonsProgress.GetProgress(
      VPolygonsCount, VHolesCount, VTime, VVectorItem);
    if Assigned(VVectorItem) then begin
      FMergeResultInternal := VVectorItem;
      FMergePolygonsResult.Polygon := (VVectorItem.Geometry as IGeometryLonLatPolygon);
      VMessage := Format(rsMergeFinish, [VPolygonsCount, VHolesCount]);
      {$IFDEF DEBUG}
      VMessage := VMessage + #13#10 + #13#10 +
        Format(rsProcessedAt, [VTime]);
      {$ENDIF}
      MessageDlg(VMessage, mtInformation, [mbOK], 0);
    end else begin
      MessageDlg(rsMergeFail, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrMergePolygons.tbtmClearClick(Sender: TObject);
begin
  Clear;
  RebuildTree;
end;

procedure TfrMergePolygons.tbtmSaveClick(Sender: TObject);
begin
  if Assigned(FMergeResultInternal) then begin
    FMarkDBGUI.SaveMarkModal(
      FMergeResultInternal,
      FMergeResultInternal.Geometry,
      True
    );
  end else begin
    MessageDlg(rsErrorNeedMerge, mtError, [mbOK], 0);
  end;
end;

procedure TfrMergePolygons.tbtmSelectClick(Sender: TObject);
begin
  if Assigned(FMergeResultInternal) then begin
    FRegionProcess.ProcessPolygon(FMergePolygonsResult.Polygon);
  end else begin
    MessageDlg(rsErrorNeedMerge, mtError, [mbOK], 0);
  end;
end;

procedure TfrMergePolygons.tbUpClick(Sender: TObject);
var
  I, J: Integer;
  VNode, VPrev: TTreeNode;
begin
  tvPolygonsList.Items.BeginUpdate;
  try
    VNode := tvPolygonsList.Selected;
    if Assigned(VNode) then begin
      VPrev := VNode.GetPrev;
      if Assigned(VPrev) then begin
        I := Integer(VNode.Data);
        J := Integer(VPrev.Data);
        SwapItems(I, J);
        SwapNodesText(VNode, VPrev);
        tvPolygonsList.Select(VPrev);
        ResetMergeResult;
      end;
    end;
  finally
    tvPolygonsList.Items.EndUpdate;
  end;
end;

procedure TfrMergePolygons.tbDelClick(Sender: TObject);
var
  I, J: Integer;
  VDelIndex: Integer;
  VNode: TTreeNode;
  VItems: TMergePolygonsItemArray;
begin
  VNode := tvPolygonsList.Selected;
  if Assigned(VNode) then begin
    VDelIndex := Integer(VNode.Data);
    SetLength(VItems, Length(FItems)-1);
    J := 0;
    for I := 0 to Length(FItems) - 1 do begin
      if I <> VDelIndex then begin
        CopyItem(FItems[I], VItems[J]);
        Inc(J);
      end;
    end;
    FItems := VItems;
    RebuildTree;
    ResetMergeResult;
  end;
end;

procedure TfrMergePolygons.tbDownClick(Sender: TObject);
var
  I, J: Integer;
  VNode, VNext: TTreeNode;
begin
  tvPolygonsList.Items.BeginUpdate;
  try
    VNode := tvPolygonsList.Selected;
    if Assigned(VNode) then begin
      VNext := VNode.GetNext;
      if Assigned(VNext) then begin
        I := Integer(VNode.Data);
        J := Integer(VNext.Data);
        SwapItems(I, J);
        SwapNodesText(VNode, VNext);
        tvPolygonsList.Select(VNext);
        ResetMergeResult;
      end;
    end;
  finally
    tvPolygonsList.Items.EndUpdate;
  end;
end;

procedure TfrMergePolygons.tvPolygonsListAddition(Sender: TObject; Node: TTreeNode);
var
  I: Integer;
begin
  tvPolygonsList.Items.BeginUpdate;
  try
    for I := 0 to tvPolygonsList.Items.Count - 1 do begin
      if Integer(tvPolygonsList.Items[I].Data) <> 0 then begin
        MarkAsClip(tvPolygonsList.Items[I]);
      end else begin
        MarkAsSubject(tvPolygonsList.Items[I]);
      end;
    end;
  finally
    tvPolygonsList.Items.EndUpdate;
  end;
end;

procedure TfrMergePolygons.tvPolygonsListDblClick(Sender: TObject);
var
  I: Integer;
  VNode: TTreeNode;
  VGoToPoint: TDoublePoint;
begin
  VNode := tvPolygonsList.Selected;
  if Assigned(VNode) then begin
    I := Integer(VNode.Data);
    if Assigned(FItems[I].MultiPolygon) then begin
      VGoToPoint := FItems[I].MultiPolygon.GetGoToPoint;
    end else begin
      VGoToPoint := FItems[I].SinglePolygon.GetGoToPoint;
    end;
    FMapGoto.GotoLonLat(VGoToPoint, False);
  end;
end;

procedure TfrMergePolygons.tvPolygonsListDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  I, J: Integer;
  VNode, VNext: TTreeNode;
begin
  VNext := tvPolygonsList.GetNodeAt(X, Y);
  tvPolygonsList.Items.BeginUpdate;
  try
    VNode := tvPolygonsList.Selected;
    if Assigned(VNode) then begin
      if Assigned(VNext) then begin
        I := Integer(VNode.Data);
        J := Integer(VNext.Data);
        SwapItems(I, J);
        SwapNodesText(VNode, VNext);
        tvPolygonsList.Select(VNext);
        ResetMergeResult;
      end;
    end;
  finally
    tvPolygonsList.Items.EndUpdate;
  end;
end;

procedure TfrMergePolygons.tvPolygonsListDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Assigned(tvPolygonsList.GetNodeAt(X, Y));
end;

procedure TfrMergePolygons.tvPolygonsListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    tvPolygonsListDblClick(Sender);
    Key := 0;
  end else if Key = VK_DELETE then begin
    tbDelClick(Sender);
    Key := 0;
  end else if (Key = VK_UP) and (ssShift in Shift) then begin
    tbUpClick(Sender);
    Key := 0;
  end else if (Key = VK_DOWN) and (ssShift in Shift) then begin
    tbDownClick(Sender);
    Key := 0;
  end;
end;

procedure TfrMergePolygons.tmrProgressCheckTimer(Sender: TObject);
begin
  FNotifierTimeInternal.Notify(GetTickCount);

  if FNeedShowProgress and (SecondsBetween(Now, FMergePolygonsProgress.StartedAt) >= 1)  then begin
    FNeedShowProgress := False;
    FfrmProgress.Show;
  end;

  if FMergePolygonsProgress.IsFinished then begin
    tmrProgressCheck.Enabled := False;
    FfrmProgress.Hide;
    OnMergeFinished;
  end;
end;

procedure TfrMergePolygons.Clear;
begin
  FCancelNotifier.NextOperation;
  tvPolygonsList.Items.Clear;
  SetLength(FItems, 0);
  ResetMergeResult;
  Self.Enabled := True;
end;

procedure TfrMergePolygons.RebuildTree;
var
  I: Integer;
begin
  tvPolygonsList.Items.BeginUpdate;
  try
    tvPolygonsList.Items.Clear;
    for I := 0 to Length(FItems) - 1 do begin
      tvPolygonsList.Items.AddChildObject(nil, FItems[I].Name, Pointer(I));
    end;
  finally
    tvPolygonsList.Items.EndUpdate;
  end;
end;

procedure TfrMergePolygons.SwapItems(const A, B: Integer);
var
  VTmp: TMergePolygonsItem;
begin
  CopyItem(FItems[A], VTmp);
  CopyItem(FItems[B], FItems[A]);
  CopyItem(VTmp, FItems[B]);
end;

procedure TfrMergePolygons.SwapNodesText(const A, B: TTreeNode);
var
  I: Integer;
begin
  I := Integer(A.Data);

  A.Text := FItems[I].Name;

  if I = 0 then begin
    MarkAsSubject(A);
  end else begin
    MarkAsClip(A);
  end;

  I := Integer(B.Data);

  B.Text := FItems[I].Name;

  if I = 0 then begin
    MarkAsSubject(B);
  end else begin
    MarkAsClip(B);
  end;
end;

procedure TfrMergePolygons.ResetMergeResult;
begin
  FMergePolygonsResult.Polygon := nil;
  FMergeResultInternal := nil;
  
  tmrProgressCheck.Enabled := False;
  FMergePolygonsProgress.ResetProgress;
end;

procedure TfrMergePolygons.OnOperationClick(Sender: TObject);
var
  VItem: TTBItem;
  VOperation: TMergeOperation;
begin
  if Sender is TTBItem then begin
    VItem := (Sender as TTBItem);
    VOperation := TMergeOperation(VItem.Tag);

    tbOperationType.ImageIndex := VItem.ImageIndex;
    tbOperationType.Hint := VItem.Caption;

    if FOperation <> VOperation then begin
      FOperation := VOperation;
      ResetMergeResult;
    end;
  end;
end;

end.
