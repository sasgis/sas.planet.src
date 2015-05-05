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
  SysUtils,
  Classes,
  Controls,
  Forms,
  ComCtrls,
  Dialogs,
  TBX,
  TBXExtItems,
  TB2Item,
  TB2ExtItems,
  TB2Dock,
  TB2Toolbar,
  i_NotifierOperation,
  i_MapViewGoto,
  i_RegionProcess,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_VectorDataFactory,
  i_VectorDataItemSimple,
  i_MergePolygonsResult,
  i_MergePolygonsProgress,
  t_MergePolygonsProcessor,
  u_MergePolygonsProcessor,
  u_MarkDbGUIHelper, ExtCtrls;

type
  TfrMergePolygons = class(TFrame)
    tvPolygonsList: TTreeView;
    tbTop: TTBXToolbar;
    tbxOperation: TTBXComboBoxItem;
    tbMerge: TTBItem;
    tbxSep1: TTBXSeparatorItem;
    tbxSep2: TTBXSeparatorItem;
    tbtmSelect: TTBItem;
    tbtmSave: TTBItem;
    tbSep3: TTBSeparatorItem;
    tmrProgressCheck: TTimer;
    procedure tvPolygonsListAddition(Sender: TObject; Node: TTreeNode);
    procedure tbMergeClick(Sender: TObject);
    procedure tbUpClick(Sender: TObject);
    procedure tbDownClick(Sender: TObject);
    procedure tbDelClick(Sender: TObject);
    procedure tvPolygonsListDblClick(Sender: TObject);
    procedure tbtmSaveClick(Sender: TObject);
    procedure tbtmSelectClick(Sender: TObject);
    procedure tbxOperationChange(Sender: TObject);
    procedure tmrProgressCheckTimer(Sender: TObject);
  private
    FMapGoto: IMapViewGoto;
    FRegionProcess: IRegionProcess;
    FMarkDBGUI: TMarkDbGUIHelper;
    FItems: TMergePolygonsItemArray;
    FMergeProcessor: TMergePolygonsProcessor;
    FMergeResultInternal: IVectorDataItem;
    FMergePolygonsResult: IMergePolygonsResult;
    FMergePolygonsProgress: IMergePolygonsProgress;
    procedure ResetMergeResult;
    procedure RebuildTree;
    function IsDublicate(const AItem: TMergePolygonsItem): Boolean;
    procedure SwapItems(const A, B: Integer);
    procedure SwapNodesText(const A, B: TTreeNode);
    procedure OnMergeFinished;
  public
    procedure AddItem(const AItem: IVectorDataItem);
    procedure Clear;
  public
    constructor Create(
      AOwner: TComponent;
      AParent: TWinControl;
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
  t_GeoTypes,
  u_MergePolygonsProgress;

resourcestring
  rsSubject = '[subject]';
  rsClip = '[clip]';

  rsErrorNeedMerge = 'You must MERGE polygons first!';
  rsErrorTwoPolygons = 'You must add at least TWO polygons!';

  rsMergeFail = 'Merge failed!';

  rsMergeFinish = 'Merge finished successful!' + #13#10 +
                  'New item have %d polygon(s) with %d hole(s)';

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
  AOwner: TComponent;
  AParent: TWinControl;
  const AAppClosingNotifier: INotifierOneOperation;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AMergePolygonsResult: IMergePolygonsResult;
  const AMapGoto: IMapViewGoto;
  const ARegionProcess: IRegionProcess;
  const AMarkDBGUI: TMarkDbGUIHelper
);
begin
  inherited Create(AOwner);

  Parent := AParent;
  FMergePolygonsResult := AMergePolygonsResult;
  FMapGoto := AMapGoto;
  FRegionProcess := ARegionProcess;
  FMarkDBGUI := AMarkDBGUI;

  SetLength(FItems, 0);

  FMergePolygonsProgress := TMergePolygonsProgress.Create;

  FMergeProcessor :=
    TMergePolygonsProcessor.Create(
      FMergePolygonsProgress,
      AAppClosingNotifier,
      AVectorDataFactory,
      AVectorGeometryLonLatFactory
    );

  ResetMergeResult;

  tbxOperation.ItemIndex := Integer(moOR);
end;

destructor TfrMergePolygons.Destroy;
begin
  FreeAndNil(FMergeProcessor);
  inherited Destroy;
end;

procedure TfrMergePolygons.AddItem(const AItem: IVectorDataItem);
var
  I: Integer;
  VItem: TMergePolygonsItem;
begin
  Assert(Assigned(AItem));
  
  InitItem(AItem, VItem);

  if not IsDublicate(VItem) then begin
    I := Length(FItems);
    SetLength(FItems, I+1);

    CopyItem(VItem, FItems[I]);

    tvPolygonsList.Items.AddChildObject(nil, FItems[I].Name, Pointer(I));

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

  FMergeProcessor.MergeAsync(FItems, TMergeOperation(tbxOperation.ItemIndex));
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

  if not FMergePolygonsProgress.IsAborted then begin
    FMergePolygonsProgress.GetProgress(
      VPolygonsCount, VHolesCount, VTime, VVectorItem);
    if Assigned(VVectorItem) then begin
      FMergeResultInternal := VVectorItem;
      FMergePolygonsResult.Polygon := (VVectorItem.Geometry as IGeometryLonLatPolygon);
      VMessage := Format(rsMergeFinish, [VPolygonsCount, VHolesCount]);
      {$IFDEF DEBUG}
      VMessage := VMessage + #13#10 + #13#10 +
        'Processed at: ' + Format('%.8f sec.', [VTime]);
      {$ENDIF}
      MessageDlg(VMessage, mtInformation, [mbOK], 0);
    end else begin
      MessageDlg(rsMergeFail, mtError, [mbOK], 0);
    end;
  end else begin
    FMergeProcessor.AbortOperation;
  end;
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

procedure TfrMergePolygons.tbxOperationChange(Sender: TObject);
begin
  ResetMergeResult;
end;

procedure TfrMergePolygons.tmrProgressCheckTimer(Sender: TObject);
begin
  if FMergePolygonsProgress.IsFinished or FMergePolygonsProgress.IsAborted then begin
    tmrProgressCheck.Enabled := False;
    OnMergeFinished;
  end;
end;

procedure TfrMergePolygons.Clear;
begin
  FMergeProcessor.AbortOperation;
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

end.
