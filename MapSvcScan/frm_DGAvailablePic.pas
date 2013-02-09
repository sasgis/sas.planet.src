{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit frm_DGAvailablePic;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  ComCtrls,
  CommCtrl,
  ExtCtrls,
  i_LanguageManager,
  i_VectorItemsFactory,
  i_InetConfig,
  i_LocalCoordConverter,
  i_NotifierOperation,
  i_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_Downloader,
  i_PathConfig,
  i_MapSvcScanStorage,
  t_GeoTypes,
  u_CommonFormAndFrameParents,
  u_AvailPicsAbstract,
  u_AvailPicsDG,
  u_AvailPicsDG2,
  u_AvailPicsBing,
  u_AvailPicsNMC,
  u_AvailPicsTerra,
  u_AvailPicsESRI,
  u_AvailPicsDD,
  u_AvailPicsGeoFuse,
  u_DownloadResultFactory,
  u_DownloaderHttp,
  u_MarksDbGUIHelper,
  Grids,
  ValEdit;

type
  TfrmDGAvailablePic = class(TFormWitghLanguageManager)
    gbImageParams: TGroupBox;
    gbAvailImages: TGroupBox;
    tvFound: TTreeView;
    btnUp: TButton;
    btnDown: TButton;
    btnCopy: TButton;
    pnlRight: TPanel;
    btnRefresh: TButton;
    veImageParams: TValueListEditor;
    spltDesc: TSplitter;
    btnMakePoly: TButton;
    chkALLImages: TCheckBox;
    lbNMC: TLabel;
    lbNMCZoom: TLabel;
    lbZoom: TLabel;
    cbDGstacks: TComboBox;
    chkBing: TCheckBox;
    chkALLServices: TCheckBox;
    chkTerraserver: TCheckBox;
    chkNMC20: TCheckBox;
    chkNMC18: TCheckBox;
    chkNMC16: TCheckBox;
    chkNMC15: TCheckBox;
    chkLowResolutionToo: TCheckBox;
    chkESRI: TCheckBox;
    chkDG2: TCheckBox;
    chkDG: TCheckBox;
    PnlSearch: TGroupBox;
    ChkDD1: TCheckBox;
    ChkDD2: TCheckBox;
    ChkDD3: TCheckBox;
    ChkDD4: TCheckBox;
    ChkDD5: TCheckBox;
    LabelDatadoors: TLabel;
    Up: TPanel;
    chkGeoFuse: TCheckBox;
    chkMNCasColorOnly: TCheckBox;
    chkShowOnlyNew: TCheckBox;
    procedure btnUpClick(Sender: TObject);
    procedure btnDownClick(Sender: TObject);
    procedure tvFoundMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnCopyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure tvFoundDeletion(Sender: TObject; Node: TTreeNode);
    procedure tvFoundClick(Sender: TObject);
    procedure tvFoundChange(Sender: TObject; Node: TTreeNode);
    procedure spltDescCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    procedure FormShow(Sender: TObject);
    procedure chkALLImagesClick(Sender: TObject);
    procedure btnMakePolyClick(Sender: TObject);
    procedure chkALLServicesClick(Sender: TObject);
    procedure chkLowResolutionTooClick(Sender: TObject);
    procedure veImageParamsDblClick(Sender: TObject);
    procedure chkShowOnlyNewClick(Sender: TObject);
  private
    FBing: TAvailPicsBing;
    FDG2: TAvailPicsDG2;
    FNMCs: TAvailPicsNMCs;
    FDDs: TAvailPicsDataDoors;
    FTerraserver: TAvailPicsTerraserver;
    FESRI: TAvailPicsESRI;
    FDGStacks: TAvailPicsDGs;
    FGeoFuse: TAvailPicsGeoFuse;

    FAvailPicsTileInfo: TAvailPicsTileInfo;
    FCallIndex: DWORD;
    FVertResizeFactor: Integer;
    FCSAddNode: IReadWriteSync;
    FALLClicking: Boolean;
    FStartRefresh: TDateTime;
    // object from main form
    FMarkDBGUI: TMarksDbGUIHelper;
    FMapSvcScanPath: IPathConfig;
    FMapSvcScanStorage: IMapSvcScanStorage;
    FVectorItemsFactory: IVectorItemsFactory;

  private
    procedure MakePicsVendors;
    procedure KillPicsVendors;

    procedure UpdateZoomLabel;
    procedure PropagateLocalConverter;

    // cleanup info
    procedure ClearAvailableImages;

    // update information about selected node
    procedure ClearInfoByNode;
    procedure UpdateInfoByNode(const ANode: TTreeNode);

    // run working thread
    procedure RunImageThread(const AChkBox: TCheckBox;
                             const AImgVendor: TAvailPicsAbstract);

    // get (find and create if not exists) node
    function GetImagesNode(const AParentNode: TTreeNode;
                           const AText: String;
                           const AExisting: Boolean;
                           const ADateDiff: Integer;
                           var AResultNode: TTreeNode): Boolean;

    // add item to images
    function AddAvailImageItem(Sender: TObject;
                               const ADate: String;
                               const AId: String;
                               const AExisting: Boolean;
                               const AFetched: TDateTime;
                               var AParams: TStrings): Boolean;

    // get tid list (for DG only)
    function Get_DG_tid_List: String;

    procedure UpdateALLImagesState;
    procedure UpdateALLServicesState;
    procedure ApplyALLCheckboxState(const AChkBox: TCheckBox; const AHasState: Byte);
    procedure ApplyServicesCheckboxHandlers;
    function IsCommonServiceCheckbox(const ABox: TControl): Boolean;
    function GetImageParamsValue(const ACol, ARow: Integer): String;
    function OpenFromImageParams(const AItemValue: String): Boolean;
  private
    FLocalConverter: ILocalCoordConverter;
    FInetConfig: IInetConfig;
    FResultFactory: IDownloadResultFactory;
  public
    constructor Create(
      const AMarkDBGUI: TMarksDbGUIHelper;
      const AMapSvcScanPath: IPathConfig;
      const ALanguageManager: ILanguageManager;
      const AVectorItemsFactory: IVectorItemsFactory;
      const AInetConfig: IInetConfig
    ); reintroduce;
    destructor Destroy; override;

    procedure ShowInfo(const AVisualPoint: TPoint;
                       const ALocalConverter: ILocalCoordConverter);
  end;

implementation

uses
  i_ImportConfig,
  i_MarksSimple,
  i_VectorItemLonLat,
  i_CoordConverter,
  i_DoublePointsAggregator,
  u_Clipboard,
  u_Synchronizer,
  u_Notifier,
  u_NotifierOperation,
  u_InetFunc,
  u_DoublePointsAggregator,
  u_MapSvcScanStorage,
  u_GeoFun,
  u_GeoToStr;

const
  c_ALLBox_None        = $00;
  c_ALLBox_Checked     = $01;
  c_ALLBox_Unchecked   = $02;
  c_ALLBox_Both        = $03;

  c_StateIndex_Checked = $01;

type
  TGetList = class(TThread)
  public
    FDownloaderHttp: IDownloader; // TDownloaderHttp;
    FHttpErrorCode: Cardinal;
    FHttpErrorText: String;
    
  private
    FInetConfig: IInetConfig;
    FForm: TfrmDGAvailablePic;
    FChkBox: TCheckBox;
    FAvailPicsSrc: TAvailPicsAbstract;
    FCallIndex: DWORD;
    FResultOk: IDownloadResultOk;
    function CallIndexActual: Boolean;
    procedure PostFinishedMessage;
  protected
    procedure Execute; override;
    procedure ShowList;
    procedure ShowError;
  public
    constructor Create(
      const AInetConfig: IInetConfig;
      const AAvailPicsSrc: TAvailPicsAbstract;
      const AForm: TfrmDGAvailablePic;
      const AChkBox: TCheckBox
    );

    destructor Destroy; override;
  end;

{$R *.dfm}

function TGetList.CallIndexActual: Boolean;
begin
  try
    Result := (FCallIndex = FForm.FCallIndex);
  except
    Result := FALSE;
  end;
end;

constructor TGetList.Create(
      const AInetConfig: IInetConfig;
      const AAvailPicsSrc: TAvailPicsAbstract;
      const AForm: TfrmDGAvailablePic;
      const AChkBox: TCheckBox
);
begin
  inherited Create(True);
  FHttpErrorCode:=0;
  FHttpErrorText:='';
  FInetConfig := AInetConfig;
  FreeOnTerminate:=true;
  Priority:=tpLower;
  FAvailPicsSrc:=AAvailPicsSrc;
  FForm := AForm;
  FChkBox := AChkBox;
  FCallIndex := AForm.FCallIndex;
  FDownloaderHttp:=TDownloaderHttp.Create(AForm.FResultFactory);
end;

destructor TGetList.Destroy;
begin
  FInetConfig:=nil;
  inherited;
end;

procedure TGetList.ShowError;
var s1: String;
begin
  if (0=FHttpErrorCode) then
    s1:='ERROR'
  else
    s1:=IntToStr(FHttpErrorCode);
  // add to params
  FForm.veImageParams.Strings.Add(s1+'='+FHttpErrorText);
end;


procedure TGetList.PostFinishedMessage;
begin
  try
    FChkBox.Enabled := TRUE;
  except
  end;
end;

procedure TGetList.ShowList;
begin
  if not CallIndexActual then
    Exit;

  if (0<FAvailPicsSrc.ParseResponse(FResultOk)) then
  try
    FForm.tvFound.AlphaSort;
  except
  end;
end;

procedure TGetList.Execute;
var
  Result: boolean;
  VRequest: IDownloadRequest; // TDownloadRequest
  VResult: IDownloadResult;
  VResultWithRespond: IDownloadResultWithServerRespond;
  VDownloadResultError: IDownloadResultError;
  VDownloadResultDataNotExists: IDownloadResultDataNotExists;
  VResultOk: IDownloadResultOk;
  VCancelNotifier: INotifierOperation;
begin
  try
   Result:=FALSE;
   try
     VRequest:=FAvailPicsSrc.GetRequest(FInetConfig);
     VCancelNotifier:=TNotifierOperation.Create(TNotifierBase.Create);
     VResult:=FDownloaderHttp.DoRequest(
                  VRequest,
                  VCancelNotifier,
                  VCancelNotifier.CurrentOperation
              );

     // check result
     if not Assigned(VResult) then begin
       // fail
       FHttpErrorText:='No result';
     end else if Supports(VResult, IDownloadResultWithServerRespond, VResultWithRespond) then begin
       // obtain result
       FHttpErrorCode := VResultWithRespond.StatusCode;
       if Supports(VResult, IDownloadResultOk, VResultOk) then begin
         // save to stream
         FResultOk := VResultOk;
         if (System.Pos(FAvailPicsSrc.ContentType, VResultOk.ContentType)>0) then
           Result:=TRUE;           // ok
       end;
     end else if Supports(VResult, IDownloadResultError, VDownloadResultError) then begin
       // error
       FHttpErrorText:=VDownloadResultError.ErrorText;
     end else if Supports(VResult, IDownloadResultDataNotExists, VDownloadResultDataNotExists) then begin
       // no data
       FHttpErrorText:=VDownloadResultDataNotExists.ReasonText;
     end;
   finally
     VRequest:=nil;
     VCancelNotifier:=nil;
   end;

    if Result then begin
      // ok
      if not(Terminated) then
        Synchronize(ShowList);
    end else begin
      // failed
      if not(Terminated) then
        Synchronize(ShowError);
    end;
  finally
    PostFinishedMessage;
  end;
end;

procedure TfrmDGAvailablePic.ShowInfo(const AVisualPoint: TPoint;
                                      const ALocalConverter: ILocalCoordConverter);
const
  maxReqSize = 3000;
const
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы
var
  VSize: TPoint;
  VRad: Extended;
  VPixelsAtZoom: Double;
  VConverter: ICoordConverter;
  VMapPixel: TDoublePoint;
  VTilePosFloat: TDoublePoint;
  VTilePos: TPoint;
begin
  Inc(FCallIndex);

  FLocalConverter := ALocalConverter;

  UpdateZoomLabel;
  PropagateLocalConverter;

  Show;

  // update position info
  VSize := FLocalConverter.GetLocalRectSize;
  VConverter := FLocalConverter.GetGeoConverter;
  FAvailPicsTileInfo.Zoom := FLocalConverter.GetZoom;
  VMapPixel := FLocalConverter.LocalPixel2MapPixelFloat(AVisualPoint);
  VConverter.CheckPixelPosFloatStrict(VMapPixel, FAvailPicsTileInfo.Zoom, True);
  FAvailPicsTileInfo.LonLat := VConverter.PixelPosFloat2LonLat(VMapPixel, FAvailPicsTileInfo.Zoom);
  // full tile rect
  VTilePosFloat := VConverter.PixelPosFloat2TilePosFloat(VMapPixel, FAvailPicsTileInfo.Zoom);
  VTilePos.X := Trunc(VTilePosFloat.X);
  VTilePos.Y := Trunc(VTilePosFloat.Y);
  FAvailPicsTileInfo.TileRect := VConverter.TilePos2LonLatRect(VTilePos, FAvailPicsTileInfo.Zoom);

  VRad := VConverter.Datum.GetSpheroidRadiusA;
  VPixelsAtZoom := VConverter.PixelsAtZoomFloat(FAvailPicsTileInfo.Zoom);

  FAvailPicsTileInfo.mpp:=1/((VPixelsAtZoom/(2*PI))/(VRad*cos(FAvailPicsTileInfo.LonLat.y*D2R)));
  FAvailPicsTileInfo.hi:=round(FAvailPicsTileInfo.mpp*15);
  FAvailPicsTileInfo.wi:=round(FAvailPicsTileInfo.mpp*15);

  if FAvailPicsTileInfo.hi>maxReqSize then
    FAvailPicsTileInfo.hi:=maxReqSize;
  if FAvailPicsTileInfo.wi>maxReqSize then
    FAvailPicsTileInfo.wi:=maxReqSize;
  if FAvailPicsTileInfo.hi<VSize.Y then
    FAvailPicsTileInfo.hi:=256;
  if FAvailPicsTileInfo.wi<VSize.X then
    FAvailPicsTileInfo.wi:=256;
  if FAvailPicsTileInfo.mpp>8 then
    FAvailPicsTileInfo.mpp:=8;

  // refresh
  btnRefreshClick(nil);
end;

procedure TfrmDGAvailablePic.spltDescCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
  if (NewSize<gbImageParams.Constraints.MinHeight) then
    Accept:=FALSE
  else if (NewSize>ClientHeight-gbAvailImages.Top-gbAvailImages.Constraints.MinHeight-spltDesc.Height) then
    Accept:=FALSE;
end;

function TfrmDGAvailablePic.AddAvailImageItem(
  Sender: TObject;
  const ADate, AId: String;
  const AExisting: Boolean;
  const AFetched: TDateTime;
  var AParams: TStrings
): Boolean;

  procedure _CopyNewLines(dst: TStrings; const src: TStrings);
  var
    i: Integer;
    v,s: String;
  begin
    for i := 0 to src.Count - 1 do begin
      s := src.Names[i];
      v := dst.Values[s];
      if (0=Length(v)) then
        dst.Values[s] := src.ValueFromIndex[i];
    end;
  end;
  
var
  //VImageService: String;
  // VVendorNode: TTreeNode;
  VDateNode, VItemNode: TTreeNode;
  VExistDiff: Integer;
begin
  Result:=FALSE;

  if AExisting then begin
    VExistDiff := Round(FStartRefresh-AFetched);
    // if do not show OLD items
    if FAvailPicsTileInfo.ShowOnlyNewItems then
    if VExistDiff > 1 then
      Exit;
    if VExistDiff < 0 then
      VExistDiff := 0;
  end else begin
    VExistDiff := 0;
  end;
 
  FCSAddNode.BeginWrite;
  try
    // lookup for node as "2011/12/30" -> "DG" -> NODE (TID + PARAMS)
    if GetImagesNode(nil, ADate, AExisting, VExistDiff, VDateNode) then
    //if GetImagesNode(VDateNode, VImageService, VVendorNode) then
    begin
      // find existing node or create new one
      if GetImagesNode(VDateNode, AId, AExisting, VExistDiff, VItemNode) then begin
        if (VItemNode.Data<>nil) then begin
        // check for 2 images on 1 day 
          if TStrings(VItemNode.Data).Values['Date'] <> TStrings(AParams).Values['Date'] then begin
            VItemNode := tvFound.Items.AddChild(VDateNode, AID+' ('+copy(TStrings(AParams).Values['Date'],12,8)+')');
            VItemNode.Data := AParams;
            AParams := nil; // own object
          end else begin
          // add new lines
            _CopyNewLines(TStrings(VItemNode.Data), AParams);
            FreeAndNil(AParams);
          end
        end else begin
          // set params
          VItemNode.Data := AParams;
          AParams := nil; // own object
        end;
        Inc(Result);
      end;

      (*
      // old version
      VItemNode := tvFound.Items.AddChild(VDateNode, AId); // for subnides - replace with VVendorNode
      VItemNode.Data := AParams;
      AParams := nil; // own object
      Inc(Result);
      *)
    end;
  finally
    FCSAddNode.EndWrite;
  end;
end;

procedure TfrmDGAvailablePic.btnRefreshClick(Sender: TObject);
var
  VDGstack: TAvailPicsDG;
  j: TAvailPicsNMCZoom;
  i: TAvailPicsDataDoorsID;
  VComp: TComponent;
begin
  FStartRefresh := Now;
  // clear
  ClearAvailableImages;

  // run thread for every image source
  RunImageThread(chkBing, FBing);
  RunImageThread(chkDG2, FDG2);

  for j := Low(TAvailPicsNMCZoom) to High(TAvailPicsNMCZoom) do begin
    // chkNMC15, chkNMC16, chkNMC18, chkNMC20
    VComp := FindComponent('chkNMC' + IntToStr(Ord(j)));
    if Assigned(VComp) then
    if (VComp is TCheckBox) then begin
      RunImageThread(TCheckBox(VComp), FNMCs[j, chkMNCasColorOnly.Checked]);
    end;
  end;

  for i := Low(TAvailPicsDataDoorsID) to High(TAvailPicsDataDoorsID) do begin
    // ChkDD1, ChkDD2, ChkDD3, ChkDD4, ChkDD5
    VComp := FindComponent('ChkDD' + IntToStr(Ord(i)));
    if Assigned(VComp) then
    if (VComp is TCheckBox) then begin
      // do not request for GeoEye and IKONOS if GeoFuse.GeoEye is checked
      if (Ord(i)<4) or (not chkGeoFuse.Checked) then
        RunImageThread(TCheckBox(VComp), FDDs[i]);
    end;
  end;

  RunImageThread(chkTerraserver, FTerraserver);

  RunImageThread(chkESRI, FESRI);

  // run for GeoFuse.GeoEye search (both GeoEye and IKONOS with full metadata)
  RunImageThread(chkGeoFuse, FGeoFuse);

  // for DG - for current stack
  VDGstack:=nil;
  if (0<cbDGstacks.Items.Count) and (0<=cbDGstacks.ItemIndex) and (cbDGstacks.ItemIndex<cbDGstacks.Items.Count) then
    VDGstack:=FDGStacks[cbDGstacks.ItemIndex];
  RunImageThread(chkDG, VDGstack);
end;

procedure TfrmDGAvailablePic.btnUpClick(Sender: TObject);
begin
 if tvFound.Selected<>nil then
 if tvFound.Selected.HasChildren
  then if tvFound.Selected.GetPrev<>nil
        then tvFound.Selected.MoveTo(tvFound.Selected.getPrevSibling,naInsert)
        else
  else if tvFound.Selected.Parent<>tvFound.Selected.GetPrev
        then tvFound.Selected.MoveTo(tvFound.Selected.GetPrev,naInsert)
end;

procedure TfrmDGAvailablePic.chkALLImagesClick(Sender: TObject);
var
  i: Integer;
begin
  if chkALLImages.state<>cbGrayed then
  if tvFound.Items.Count>0 then
  for i := 0 to tvFound.Items.Count-1 do
  with tvFound.Items.Item[i] do begin
    if chkALLImages.State = cbChecked then
      StateIndex := StateIndex or c_StateIndex_Checked
    else
      StateIndex := StateIndex and (not c_StateIndex_Checked);
  end;
end;

procedure TfrmDGAvailablePic.chkALLServicesClick(Sender: TObject);
var
  i: Integer;
  VBox: TControl;
begin
  if FALLClicking then
    Exit;

  // common service checkboxes
  if (Sender<>chkALLServices) then begin
    UpdateALLServicesState;
    Exit;
  end;

  // apply to all checkbox in gbImagesSource (except sender)
  if chkALLServices.state<>cbGrayed then
  if PnlSearch.ControlCount>0 then begin
    FALLClicking := TRUE;
    try
      for i := 0 to PnlSearch.ControlCount-1 do begin
        VBox := PnlSearch.Controls[i];
        if IsCommonServiceCheckbox(VBox) then begin
          // apply to service checkbox
          TCheckBox(VBox).State := chkALLServices.State;
        end;
      end;
    finally
      FALLClicking := FALSE;
    end;
  end;
end;

procedure TfrmDGAvailablePic.chkLowResolutionTooClick(Sender: TObject);
begin
  FAvailPicsTileInfo.LowResToo := chkLowResolutionToo.Checked;
end;

procedure TfrmDGAvailablePic.chkShowOnlyNewClick(Sender: TObject);
begin
  FAvailPicsTileInfo.ShowOnlyNewItems := chkShowOnlyNew.Checked;
end;

procedure TfrmDGAvailablePic.btnDownClick(Sender: TObject);
begin
 if tvFound.Selected<>nil then
 if tvFound.Selected.HasChildren
 then
 if tvFound.Selected.GetNextSibling<>nil then
  if tvFound.Selected.GetNextSibling.GetNextSibling<>nil
   then tvFound.Selected.MoveTo(tvFound.Selected.GetNextSibling.GetNextSibling,naInsert)
   else tvFound.Selected.MoveTo(tvFound.Selected.GetNextSibling,naAdd)
  else
 else
 if tvFound.Selected.Parent.GetLastChild<>tvFound.Selected then
  if tvFound.Selected.GetNext<>tvFound.Selected.Parent.GetLastChild
   then tvFound.Selected.MoveTo(tvFound.Selected.GetNext.GetNext,naInsert)
   else tvFound.Selected.MoveTo(tvFound.Selected.GetNext,naAdd)
end;

procedure TfrmDGAvailablePic.btnMakePolyClick(Sender: TObject);

  function _ExtractFloat(var AParsedText: String; const ACommaAsDelimiter: Boolean): Double;
  var
    p,q: Integer;
    s: String;
  begin
    if (0=Length(AParsedText)) then
      Abort;
    p := System.Pos(' ', AParsedText);
    if (p>0) then begin
      // check if comma before space
      q := System.Pos(',', AParsedText);
      if (q>0) and (q<p) then
        p:=q;
    end;
    if (p>0) then begin
      // found
      s := System.Copy(AParsedText,1,p-1);
      System.Delete(AParsedText, 1, p);
    end else begin
      // not found
      s := AParsedText;
      AParsedText := '';
    end;
    Result := StrPointToFloat(s);
  end;

  function _ExtractPoint(var AParsedText: String; const AInvert, ACommaAsDelimiter: Boolean): TDoublePoint;
  var d: Double;
  begin
    Result.Y := _ExtractFloat(AParsedText, ACommaAsDelimiter);
    Result.X := _ExtractFloat(AParsedText, ACommaAsDelimiter);
    if AInvert then begin
      d := Result.Y;
      Result.Y := Result.X;
      Result.X := d;
    end;
  end;

  procedure _AddWithBR(var AFullDesc: String; const ACaption, AValue: String);
  begin
    if (0<Length(AValue)) then begin
      AFullDesc := AFullDesc + '<br>' + ACaption + ':' + AValue;
    end;
  end;

var
  i,k: Integer;
  VXCommaY, VCommaAsDelimiter: Boolean;
  VName, VDesc, VGeometry, VDate: String;
  VImportConfig: IImportConfig;
  VPointsAggregator: IDoublePointsAggregator;
  VPoint: TDoublePoint;
  VValidPoint: Boolean;
  VPolygon: ILonLatPolygon;
  VMark: IMark;
  VAllNewMarks: IInterfaceList;
begin
  if (nil=FMarkDBGUI) then
    Exit;

  k := tvFound.Items.Count;

  if (0<k) then
  for i := 0 to k-1 do
  if (nil<>tvFound.Items.Item[i].Data) then
  if ((tvFound.Items.Item[i].StateIndex and c_StateIndex_Checked) <> 0) then begin
    // prepare values
    VXCommaY := FALSE;
    VGeometry := '';
    with TStrings(tvFound.Items.Item[i].Data) do

    try
      VGeometry := Values['VposList'];
      if 0=length(VGeometry) then
      if length(Values['Geometry'])>0 then begin
         VGeometry := Values['Geometry'];
         VXCommaY := TRUE;
      end;

      (*
      if (0=Length(VGeometry)) then begin
        // allow import geometry for NokiaMapCreator - tile-bounded only!
        VGeometry := Values['geometry'];
        VGeometry := StringReplace(VGeometry, ',', ' ', [rfReplaceAll]);
        VXCommaY := TRUE;
      end;
      *)

      VDate := Values['Date'];
      if (0=Length(VDate)) then
        VDate := Values['acquisitionDate'];
      if (0=Length(VDate)) then
        VDate := Values['acq_date'];


      if (0<Length(VGeometry)) then begin

        VDesc := Values['FeatureId'];
        if 0<length(VDesc)then begin
          Vname := copy(VDate,1,10)+' '+VDesc;
          VDesc := 'FeatureId:'+VDesc;
        end;

         if 0=length(VDesc) then begin
          VDesc := Values['uid'];
          if 0=length(VDesc) then
            VDesc := Values['IMAGE_ID'];
          Vname := copy(VDate,1,10)+' '+VDesc;
          VDesc := 'uid:'+VDesc;
         end;
        // add Date
        _AddWithBR(VDesc, 'Date', VDate);

        // add other values - use VDate as temp buffer
        // add Color
        VDate := Values['Color'];
        if (0=Length(VDate)) then
          VDate := Values['productType'];
        _AddWithBR(VDesc, 'Color', VDate);

        // add Resolution
        VDate := Values['Resolution'];
        if (0=Length(VDate)) then
          VDate := Values['groundSampleDistance'];
        if (0=Length(VDate)) then
          VDate := Values['GSD'];
        _AddWithBR(VDesc, 'Resolution', VDate);

        // add dataLayer
        VDate := Values['dataLayer'];
        _AddWithBR(VDesc, 'DataLayer', VDate);

        // add Source
        VDate := Values['Source'];
        _AddWithBR(VDesc, 'Source', VDate);

        // add ID if exist
        VDate := Values['LegacyId'];
        _AddWithBR(VDesc, 'LegacyId', VDate);

        if 0=length(VDate) then begin
          VDate := Values['CatalogID'];
          _AddWithBR(VDesc, 'CatalogID', VDate);
        end;

        (*
        VDate := Values['IMAGE_ID'];
        if (0<>Length(VDate)) then
        _AddWithBR(VDesc, 'IMAGE_ID', VDate);
        *)
        
        VDate := Values['SCENE_ID'];
        _AddWithBR(VDesc, 'SCENE_ID', VDate);

        // add Provider
        VDate := Values['Provider'];
        if (0=Length(VDate)) then
          VDate := tvFound.Items.Item[i].text; // only from DG and NokiaMapCreator and GeoFuse.GeoEye
        _AddWithBR(VDesc, 'Provider', VDate);

        // add Preview (from more info to less info)
        VDate := Values['IMAGE_FILE_URL'];
        if (0<>Length(VDate)) then
          _AddWithBR(VDesc, 'PreviewLink', '<a href='+VDate+'>'+VDate+'</a>');

        VDate := Values['FULL_METADATA_URL'];
        if (0=Length(VDate)) then
          VDate := Values['METADATA_URL'];
        if (0<>Length(VDate)) then
          _AddWithBR(VDesc, 'MetadataLink', '<a href='+VDate+'>'+VDate+'</a>');

      end;
    except
    end;

    if (VGeometry <> '') then begin
      // show dialog
      if (nil=VImportConfig) then begin
        // single time only!
        VImportConfig := FMarkDBGUI.EditModalImportConfig;
        if (nil=VImportConfig) then
          Exit;
        VPointsAggregator := TDoublePointsAggregator.Create;
        if (nil=VImportConfig.TemplateNewPoly) then
          Exit;
      end;

      // fill with coords
      VCommaAsDelimiter := (System.Pos(',',VGeometry)>0);
      VPointsAggregator.Clear;
      repeat
        // cut pairs (space-separated y and x)
        try
          VPoint := _ExtractPoint(VGeometry, VXCommaY, VCommaAsDelimiter);
          VValidPoint := (not PointIsEmpty(VPoint)) and ((Abs(VPoint.X) <= 180) and (Abs(VPoint.Y) <= 90));
        except
          VValidPoint := FALSE;
        end;
        // add pair
        if VValidPoint then begin
          VPointsAggregator.Add(VPoint);
        end;
      until (0=Length(VGeometry));

      if (VPointsAggregator.Count>0) then begin
        // create lonlats
        VPolygon := FVectorItemsFactory.CreateLonLatPolygon(VPointsAggregator.Points, VPointsAggregator.Count);
        if (VPolygon <> nil) and (VPolygon.Count > 0) then begin
          // make polygon
          VMark := VImportConfig.MarkDB.Factory.CreateNewPoly(
            VPolygon,
            Vname,
            VDesc,
            VImportConfig.TemplateNewPoly
          );

          if (nil<>VMark) then begin
            // apply to database
            // VImportConfig.MarkDB.UpdateMark(nil, VMark);
            if (nil=VAllNewMarks) then
              VAllNewMarks := TInterfaceList.Create;
            VAllNewMarks.Add(VMark);
          end;
        end;
      end;

    end;
  end;

  if Assigned(VAllNewMarks) then
  if (nil<>VImportConfig) then
  if (nil<>VImportConfig.MarkDB) then
    VImportConfig.MarkDB.UpdateMarksList(nil, VAllNewMarks);
end;

function TfrmDGAvailablePic.GetImageParamsValue(const ACol, ARow: Integer): String;
begin
  if (ACol>0) and (ARow>0) and (ARow<veImageParams.RowCount) then
    Result := veImageParams.Cells[ACol,ARow] // +1 {veImageParams.FixedRows}
  else
    Result := '';
end;

function TfrmDGAvailablePic.GetImagesNode(
  const AParentNode: TTreeNode;
  const AText: String;
  const AExisting: Boolean;
  const ADateDiff: Integer;
  var AResultNode: TTreeNode
): Boolean;

  procedure _CheckNewNode;
  var
    VItem: TTVItem;
  begin
    if AExisting and (ADateDiff>0) then
      Exit;
    with VItem do begin
      mask := TVIF_STATE or TVIF_HANDLE;
      hItem := AResultNode.ItemId;
      stateMask := TVIS_BOLD;
      state := TVIS_BOLD;
      TreeView_SetItem(AResultNode.Handle, VItem);
    end;
  end;
  
var
  i,k: Integer;
begin
  Result:=FALSE;
  AResultNode:=nil;

  if (nil=AParentNode) then
    k:=tvFound.Items.Count
  else
    k:=AParentNode.Count;

  if (0<k) then
  for i := 0 to k-1 do begin
    // get item
    if (nil=AParentNode) then
      AResultNode:=tvFound.Items.Item[i]
    else
      AResultNode:=AParentNode.Item[i];

    // check text
    if (nil<>AResultNode) then
    if SameText(AResultNode.Text, AText) then begin
      // found
      Inc(Result);
      // mark if new
      _CheckNewNode;
      Exit;
    end;
  end;

  // not found - should create
  if (nil=AParentNode) then begin
    AResultNode := tvFound.Items.Add(nil, AText);
  end else begin
    AResultNode := tvFound.Items.AddChild(AParentNode, AText);
  end;
  _CheckNewNode;
  Result := TRUE;
end;

function TfrmDGAvailablePic.Get_DG_tid_List: String;
var
  i,k: Integer;
  single_tid: String;
begin
  Result := '';
  k := tvFound.Items.Count;
  if (0<k) then
  for i := 0 to k-1 do
  if (nil<>tvFound.Items.Item[i].Data) then
  if ((tvFound.Items.Item[i].StateIndex and c_StateIndex_Checked) <> 0) then
  try
    // get tid for DG items
    single_tid := TStrings(tvFound.Items.Item[i].Data).Values['tid'];
    if single_tid = '' then single_tid := TStrings(tvFound.Items.Item[i].Data).Values['FeatureId'];
    if single_tid = '' then single_tid := TStrings(tvFound.Items.Item[i].Data).Values['layer'];
    if (0<Length(single_tid)) then begin
      if (0<Length(Result)) then
        Result:=Result+',';
      Result:=Result+single_tid;
    end;
  except
  end;
end;

function TfrmDGAvailablePic.IsCommonServiceCheckbox(const ABox: TControl): Boolean;
begin
  Result := (ABox <> chkALLServices) and
            (ABox <> chkLowResolutionToo) and
            (ABox <> chkMNCasColorOnly) and
            (ABox is TCheckBox);
end;

procedure TfrmDGAvailablePic.KillPicsVendors;
var
  i,k: Integer;
  j: TAvailPicsNMCZoom;
  jj: TAvailPicsDataDoorsID;
begin
  // simple
  FreeAndNil(FBing);
  FreeAndNil(FDG2);
  FreeAndNil(FTerraserver);
  FreeAndNil(FESRI);
  FreeAndNil(FGeoFuse);

  // fixed array
  for j := Low(TAvailPicsNMCZoom) to High(TAvailPicsNMCZoom) do begin
    FreeAndNil(FNMCs[j]);
  end;

  for jj := Low(TAvailPicsDataDoorsID) to High(TAvailPicsDataDoorsID) do begin
    FreeAndNil(FDDs[jj]);
  end;
  // list
  cbDGstacks.Items.Clear;
  k:=Length(FDGStacks);
  if (0<k) then begin
    for i := k-1 downto 0 do begin
      FreeAndNil(FDGStacks[i]);
    end;
    setLength(FDGStacks, 0);
  end;
end;

procedure TfrmDGAvailablePic.MakePicsVendors;
var i,k: Integer;
begin
  // make for bing
  if (nil=FBing) then
    FBing := TAvailPicsBing.Create(@FAvailPicsTileInfo, FMapSvcScanStorage);

  // make for DigitalGlobe2
  if (nil=FDG2) then
    FDG2 := TAvailPicsDG2.Create(@FAvailPicsTileInfo, FMapSvcScanStorage);

  // make for nokia map creator
  GenerateAvailPicsNMC(FNMCs, @FAvailPicsTileInfo, FMapSvcScanStorage);

  // make for datadoors
  GenerateAvailPicsDD(FDDs, FResultFactory, @FAvailPicsTileInfo, FMapSvcScanStorage);

  // make for terraserver
  if (nil=FTerraserver) then
    FTerraserver := TAvailPicsTerraserver.Create(@FAvailPicsTileInfo, FMapSvcScanStorage);

  // make for ESRI
  if (nil=FESRI) then
    FESRI := TAvailPicsESRI.Create(@FAvailPicsTileInfo, FMapSvcScanStorage);

  // make for GeoFuse.GeoEye
  if (nil=FGeoFuse) then
    FGeoFuse := TAvailPicsGeoFuse.Create(@FAvailPicsTileInfo, FMapSvcScanStorage);

  // make for digital globe
  if (0=Length(FDGStacks)) then
    GenerateAvailPicsDG(FDGStacks, @FAvailPicsTileInfo, FMapSvcScanStorage);

  // fill cbDGstacks
  cbDGstacks.Items.Clear;
  k:=Length(FDGStacks);
  if (0<k) then
  for i := 0 to k-1 do begin
    cbDGstacks.Items.Add(FDGStacks[i].GUI_Name);
  end;

  // select last item
  if (0<cbDGstacks.Items.Count) and (0>cbDGstacks.ItemIndex) then
    cbDGstacks.ItemIndex:=(cbDGstacks.Items.Count-1);
end;

function TfrmDGAvailablePic.OpenFromImageParams(const AItemValue: String): Boolean;
begin
  Result := (Length(AItemValue)>4) and SameText(System.Copy(AItemValue,1,4),'http');
end;

procedure TfrmDGAvailablePic.PropagateLocalConverter;
var
  i,k: Integer;
  j: TAvailPicsNMCZoom;
  r: Boolean;
  jj: TAvailPicsDataDoorsID;
begin
  if (nil<>FBing) then
    FBing.SetLocalConverter(FLocalConverter);

  if (nil<>FDG2) then
    FDG2.SetLocalConverter(FLocalConverter);

  for r := FALSE to TRUE do
  for j := Low(TAvailPicsNMCZoom) to High(TAvailPicsNMCZoom) do begin
    if (FNMCs[j,r]<>nil) then
      FNMCs[j,r].SetLocalConverter(FLocalConverter);
  end;

  for jj := Low(TAvailPicsDataDoorsID) to High(TAvailPicsDataDoorsID) do begin
    if (FDDs[jj]<>nil) then
      FDDs[jj].SetLocalConverter(FLocalConverter);
  end;

  if (nil<>FTerraserver) then
    FTerraserver.SetLocalConverter(FLocalConverter);

  if (nil<>FESRI) then
    FESRI.SetLocalConverter(FLocalConverter);

  if (nil<>FGeoFuse) then
    FGeoFuse.SetLocalConverter(FLocalConverter);

  k:=Length(FDGStacks);
  if (0<k) then
  for i := 0 to k-1 do begin
    FDGStacks[i].SetLocalConverter(FLocalConverter);
  end;
end;

procedure TfrmDGAvailablePic.RunImageThread(const AChkBox: TCheckBox;
                                            const AImgVendor: TAvailPicsAbstract);
begin
  if Assigned(AImgVendor) then
  if AChkBox.Checked and AChkBox.Enabled then begin
    // disable
    AChkBox.Enabled := FALSE;
    // run
    with TGetList.Create(FInetConfig,
                         AImgVendor,
                         Self,
                         AChkBox
                         ) do
    begin
      Resume;
    end;
  end;
end;

procedure TfrmDGAvailablePic.tvFoundChange(Sender: TObject; Node: TTreeNode);
begin
  UpdateInfoByNode(Node);
end;

procedure TfrmDGAvailablePic.tvFoundClick(Sender: TObject);
begin
  UpdateInfoByNode(tvFound.Selected);
end;

procedure TfrmDGAvailablePic.tvFoundDeletion(Sender: TObject; Node: TTreeNode);
var obj: TObject;
begin
  try
    if (nil<>Node) then
    if (nil<>Node.Data) then begin
      obj := TObject(Node.Data);
      FreeAndNil(obj);
      Node.Data:=nil;
    end;
  except
  end;
end;

procedure TfrmDGAvailablePic.tvFoundMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MH:THitTests;
  VNode:TTreeNode;
  i:integer;
begin
  MH:= tvFound.GetHitTestInfoAt(X,Y);

  if (htOnStateIcon in MH) then begin
    VNode:= tvFound.GetNodeAt(X,Y);

    if (nil=VNode) then
      Exit;
      
    with VNode do begin
      if ((StateIndex and c_StateIndex_Checked) = 0) then
        StateIndex := StateIndex or c_StateIndex_Checked
      else
        StateIndex := StateIndex and (not c_StateIndex_Checked);
    end;

    for i:=0 to VNode.Count-1 do
    with VNode.Item[i] do begin
      if ((StateIndex and c_StateIndex_Checked) = 0) then
        StateIndex := StateIndex or c_StateIndex_Checked
      else
        StateIndex := StateIndex and (not c_StateIndex_Checked);
    end;
  end;
  
  if (htOnLabel in MH) then begin
    UpdateInfoByNode(tvFound.GetNodeAt(X,Y));
  end;
end;

procedure TfrmDGAvailablePic.UpdateALLImagesState;
var
  i: Integer;
  VHasState: Byte;
begin
  // obtain ALLImages checkbox state
  VHasState := c_ALLBox_None;
  if tvFound.Items.Count>0 then
  for i := 0 to tvFound.Items.Count-1 do begin
    // keep state
    if ((tvFound.Items.Item[i].StateIndex and c_StateIndex_Checked) <> 0) then
      VHasState := (VHasState or c_ALLBox_Checked) // checked
    else
      VHasState := (VHasState or c_ALLBox_Unchecked);
    // check both exist
    if (c_ALLBox_Both = VHasState) then
      break;
  end;

  // apply ALLImages checkbox state
  ApplyALLCheckboxState(chkALLImages, VHasState);
end;

procedure TfrmDGAvailablePic.UpdateALLServicesState;
var
  i: Integer;
  VBox: TControl;
  VHasState: Byte;
begin
  // obtain chkALLServices checkbox state
  VHasState := c_ALLBox_None;
  if PnlSearch.ControlCount>0 then
  for i := 0 to PnlSearch.ControlCount-1 do begin
    VBox := PnlSearch.Controls[i];
    if IsCommonServiceCheckbox(VBox) then begin
      // keep state
      if (cbChecked = TCheckBox(VBox).State) then
        VHasState := (VHasState or c_ALLBox_Checked) // checked
      else
        VHasState := (VHasState or c_ALLBox_Unchecked);
      // check both exist
      if (c_ALLBox_Both = VHasState) then
        break;
    end;
  end;

  // apply chkALLServices checkbox state
  ApplyALLCheckboxState(chkALLServices, VHasState);
end;

procedure TfrmDGAvailablePic.UpdateInfoByNode(const ANode: TTreeNode);
begin
  veImageParams.Strings.BeginUpdate;
  try
    ClearInfoByNode;
    
    if (nil<>ANode) then
    if (nil<>ANode.Data) then begin
      // update info
      veImageParams.TopRow := 1; //veImageParams.FixedRows;
      veImageParams.Strings.Assign(TStrings(ANode.Data));
    end;
  finally
    veImageParams.Strings.EndUpdate;
  end;

  UpdateALLImagesState;
end;

procedure TfrmDGAvailablePic.UpdateZoomLabel;
var
  VActualZoom: Byte;
  VZoomStr: String;
begin
  // Bing minimal zoom
  if Assigned(FLocalConverter) then begin
    VActualZoom:=FLocalConverter.Zoom;
    AdjustMinimalBingHiResZoom(VActualZoom);
    Inc(VActualZoom);
    VZoomStr:=IntToStr(VActualZoom);
  end else begin
    VZoomStr:='-';
  end;
  lbZoom.Caption:=StringReplace(lbZoom.Hint,'%',VZoomStr,[]);
end;

procedure TfrmDGAvailablePic.veImageParamsDblClick(Sender: TObject);
var
  VURLText: String;
begin
  // check if URL
  VURLText := GetImageParamsValue(veImageParams.Col, veImageParams.Row);
  if OpenFromImageParams(VURLText) then begin
    // open URL
    OpenUrlInBrowser(VURLText);
  end;
end;

procedure TfrmDGAvailablePic.ClearAvailableImages;
begin
  tvFound.Items.Clear;
  ClearInfoByNode;
  chkALLImages.Checked := FALSE;
end;

procedure TfrmDGAvailablePic.ClearInfoByNode;
begin
  veImageParams.Strings.Clear;
end;

constructor TfrmDGAvailablePic.Create(
  const AMarkDBGUI: TMarksDbGUIHelper;
  const AMapSvcScanPath: IPathConfig;
  const ALanguageManager: ILanguageManager;
  const AVectorItemsFactory: IVectorItemsFactory;
  const AInetConfig: IInetConfig
);
begin
  FMarkDBGUI := AMarkDBGUI;
  FMapSvcScanPath := AMapSvcScanPath;
  FALLClicking := FALSE;
  FVertResizeFactor:=0;
  FCallIndex:=0;
  FBing:=nil;
  FDG2:=nil;
  FillChar(FNMCs, sizeof(FNMCs), 0);
  FillChar(FDDs, sizeof(FDDs), 0);
  FTerraserver:=nil;
  FESRI:=nil;
  FGeoFuse:=nil;
  SetLength(FDGStacks, 0);

  ZeroMemory(@FAvailPicsTileInfo, sizeof(FAvailPicsTileInfo));
  FAvailPicsTileInfo.AddImageProc := AddAvailImageItem;

  inherited Create(ALanguageManager);

  FCSAddNode := MakeSync_Tiny(Self, FALSE);

  FLocalConverter := nil;
  FVectorItemsFactory := AVectorItemsFactory;
  FInetConfig := AInetConfig;
  FStartRefresh := Now;
  FMapSvcScanStorage := TMapSvcScanStorage.Create(AMapSvcScanPath);
  FResultFactory := TDownloadResultFactory.Create;
end;

destructor TfrmDGAvailablePic.Destroy;
begin
  FMarkDBGUI:=nil;
  // kill vendors objects
  KillPicsVendors;
  // interfaces
  FResultFactory:=nil;
  FVectorItemsFactory:=nil;
  FInetConfig:=nil;
  FLocalConverter:=nil;
  FCSAddNode:=nil;
  FMapSvcScanStorage:=nil;
  FMapSvcScanPath:=nil;
  inherited;
end;

procedure TfrmDGAvailablePic.ApplyALLCheckboxState(const AChkBox: TCheckBox; const AHasState: Byte);
begin
  case AHasState of
    c_ALLBox_Both: begin
      // both
      AChkBox.State := cbGrayed;
    end;
    c_ALLBox_Checked: begin
      // all checked
      AChkBox.State := cbChecked;
    end;
    else begin
      // empty or all unchecked
      AChkBox.State := cbUnchecked;
    end;
  end;
end;

procedure TfrmDGAvailablePic.ApplyServicesCheckboxHandlers;
var
  i: Integer;
  VBox: TControl;
begin
  // set OnClick if empty from chkALLServices
  if PnlSearch.ControlCount>0 then
  for i := 0 to PnlSearch.ControlCount-1 do begin
    VBox := PnlSearch.Controls[i];
    if IsCommonServiceCheckbox(VBox) then begin
      // apply handler
      TCheckBox(VBox).OnClick := chkALLServices.OnClick;
    end;
  end;
end;

procedure TfrmDGAvailablePic.btnCopyClick(Sender: TObject);
begin
  CopyStringToClipboard(Handle, Get_DG_tid_List);
end;

procedure TfrmDGAvailablePic.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
var
  VMinNewHeight: Integer;
begin
  VMinNewHeight := (
    gbAvailImages.Top +
    gbAvailImages.Constraints.MinHeight +
    spltDesc.Height +
    gbImageParams.Height +
    FVertResizeFactor
  );
  if NewHeight < VMinNewHeight then begin
    NewHeight := VMinNewHeight;
  end;
end;

procedure TfrmDGAvailablePic.FormCreate(Sender: TObject);
begin
  // make checkboxes in list
  SetWindowLong(tvFound.Handle,GWL_STYLE,GetWindowLong(tvFound.Handle,GWL_STYLE) or TVS_CHECKBOXES);
  // make vendors and fill list of dg stacks
  MakePicsVendors;
end;

procedure TfrmDGAvailablePic.FormShow(Sender: TObject);
begin
  FVertResizeFactor:=Height-gbAvailImages.Top-gbAvailImages.Height-spltDesc.Height-gbImageParams.Height;
  ApplyServicesCheckboxHandlers;
end;

end.
