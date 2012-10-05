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
  Controls,
  Forms,
  StdCtrls,
  ComCtrls,
  CommCtrl,
  ExtCtrls,
  u_CommonFormAndFrameParents,
  i_LanguageManager,
  i_InetConfig,
  i_LocalCoordConverter,
  u_AvailPicsAbstract,
  u_AvailPicsDG,
  u_AvailPicsDG2,
  u_AvailPicsBing,
  u_AvailPicsNMC,
  u_AvailPicsTerra,
  u_AvailPicsESRI,
  u_AvailPicsDD,
  i_NotifierOperation,
  u_NotifierOperation,
  i_DownloadRequest,
  u_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  u_DownloadResultFactory,
  i_Downloader,
  u_DownloaderHttp,
  t_GeoTypes,
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
  private
    FBing: TAvailPicsBing;
    FDG2: TAvailPicsDG2;
    FNMCs: TAvailPicsNMCs;
    FDDs: TAvailPicsDataDoors;
    FTerraserver: TAvailPicsTerraserver;
    FESRI: TAvailPicsESRI;
    FDGStacks: TAvailPicsDGs;
    FAvailPicsTileInfo: TAvailPicsTileInfo;
    FCallIndex: DWORD;
    FVertResizeFactor: Integer;
    FCSAddNode: IReadWriteSync;
    FALLClicking: Boolean;
    // object from main form
    FMarkDBGUI: TMarksDbGUIHelper;

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
                           var AResultNode: TTreeNode): Boolean;

    // add item to images
    function AddAvailImageItem(Sender: TObject;
                               const ADate: String;
                               const AId: String;
                               var AParams: TStrings): Boolean;

    // get tid list (for DG only)
    function Get_DG_tid_List: String;

    procedure UpdateALLImagesState;
    procedure UpdateALLServicesState;
    procedure ApplyALLCheckboxState(const AChkBox: TCheckBox; const AHasState: Byte);
    procedure ApplyServicesCheckboxHandlers;
    function IsCommonServiceCheckbox(const ABox: TControl): Boolean;
  private
    FLocalConverter: ILocalCoordConverter;
    FInetConfig: IInetConfig;
    FResultFactory: IDownloadResultFactory;
  public
    constructor Create(
      const AMarkDBGUI: TMarksDbGUIHelper;
      const ALanguageManager: ILanguageManager;
      const AInetConfig: IInetConfig
    ); reintroduce;
    destructor Destroy; override;

    procedure ShowInfo(const AVisualPoint: TPoint;
                       const ALocalConverter: ILocalCoordConverter);
  end;

implementation

uses
  u_Clipboard,
  u_Synchronizer,
  i_ImportConfig,
  i_MarksSimple,
  i_VectorItmesFactory,
  i_VectorItemLonLat,
  i_DoublePointsAggregator,
//  u_BinaryDataByMemStream,
  u_DoublePointsAggregator,
  u_VectorItmesFactorySimple,
  u_GeoFun,
  u_GeoToStr,
  i_CoordConverter;

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
     VCancelNotifier:=TNotifierOperation.Create;
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

function TfrmDGAvailablePic.AddAvailImageItem(Sender: TObject;
                                              const ADate, AId: String;
                                              var AParams: TStrings): Boolean;

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
begin
  Result:=FALSE;

  (*
  // IF need for subnodes for different services
  // TODO: get image service by sender without this "simplicity"
  if (Sender=FBing) then
    VImageService:=chkBing.Caption
  else if (Sender=FNMC) then
    VImageService:=chkNMC.Caption
  else
    VImageService:=chkDG.Caption;
  *)
  
  FCSAddNode.BeginWrite;
  try
    // lookup for node as "2011/12/30" -> "DG" -> NODE (TID + PARAMS)
    if GetImagesNode(nil, ADate, VDateNode) then
    //if GetImagesNode(VDateNode, VImageService, VVendorNode) then
    begin
      // find existing node or create new one
      if GetImagesNode(VDateNode, AId, VItemNode) then begin
        if (VItemNode.Data<>nil) then begin
          // add new lines
          _CopyNewLines(TStrings(VItemNode.Data), AParams);
          FreeAndNil(AParams);
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
      RunImageThread(TCheckBox(VComp), FNMCs[j]);
    end;
  end;

  for i := Low(TAvailPicsDataDoorsID) to High(TAvailPicsDataDoorsID) do begin
    // ChkDD1, ChkDD2, ChkDD3, ChkDD4, ChkDD5
    VComp := FindComponent('ChkDD' + IntToStr(Ord(i)));
    if Assigned(VComp) then
    if (VComp is TCheckBox) then begin
      RunImageThread(TCheckBox(VComp), FDDs[i]);
    end;
  end;

  RunImageThread(chkTerraserver, FTerraserver);

  RunImageThread(chkESRI, FESRI);

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
      StateIndex := 2
    else
      StateIndex := 1;
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

  function _ExtractFloat(var AParsedText: String): Double;
  var
    p: Integer;
    s: String;
  begin
    if (0=Length(AParsedText)) then
      Abort;
    p := System.Pos(' ', AParsedText);
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

  function _ExtractPoint(var AParsedText: String; const AInvert: Boolean): TDoublePoint;
  var d: Double;
  begin
    Result.Y := _ExtractFloat(AParsedText);
    Result.X := _ExtractFloat(AParsedText);
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
  VXCommaY: Boolean;
  VName, VDesc, VGeometry, VDate: String;
  VImportConfig: IImportConfig;
  VVectorItmesFactory: IVectorItmesFactory;
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
  if (2=tvFound.Items.Item[i].StateIndex) then begin
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
        _AddWithBR(VDesc, 'Resolution', VDate);

        // add Source
        _AddWithBR(VDesc, 'Source', Values['Source']);

        // add ID if exist
        VDate := '';
        VDate := Values['LegacyId'];
        if (0<>Length(VDate)) then
        _AddWithBR(VDesc, 'LegacyId', VDate);

        if 0=length(VDate) then begin
          VDate := Values['CatalogID'];
          _AddWithBR(VDesc, 'CatalogID', VDate);
        end;

        // add Provider
        VDate := Values['Provider'];
        if (0=Length(VDate)) then
          VDate := tvFound.Items.Item[i].text; // only from DG and NokiaMapCreator
        _AddWithBR(VDesc, 'Provider', VDate);

      end;
    except
    end;

    if (VGeometry <> '') then begin
      // show dialog
      if (nil=VImportConfig) then begin
        // single time only!
        VImportConfig := FMarkDBGUI.EditModalImportConfig;
        VVectorItmesFactory := TVectorItmesFactorySimple.Create;
        VPointsAggregator := TDoublePointsAggregator.Create;
        // check for crazy errors
        if (nil=VPointsAggregator) then
          Exit;
        if (nil=VVectorItmesFactory) then
          Exit;
        if (nil=VImportConfig) then
          Exit;
        if (nil=VImportConfig.TemplateNewPoly) then
          Exit;
      end;

      // fill with coords
      VPointsAggregator.Clear;
      repeat
        // cut pairs (space-separated y and x)
        try
          VPoint := _ExtractPoint(VGeometry, VXCommaY);
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
        VPolygon := VVectorItmesFactory.CreateLonLatPolygon(VPointsAggregator.Points, VPointsAggregator.Count);
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

function TfrmDGAvailablePic.GetImagesNode(const AParentNode: TTreeNode;
                                          const AText: String;
                                          var AResultNode: TTreeNode): Boolean;
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
      Exit;
    end;
  end;

  // not found - should create
  if (nil=AParentNode) then begin
    AResultNode := tvFound.Items.Add(nil, AText);
    Result := TRUE;
  end else begin
    AResultNode := tvFound.Items.AddChild(AParentNode, AText);
    Result := TRUE;
  end;
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
  if (2=tvFound.Items.Item[i].StateIndex) then
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
  Result := (ABox <> chkALLServices) and (ABox <> chkLowResolutionToo) and (ABox is TCheckBox);
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
    FBing := TAvailPicsBing.Create(@FAvailPicsTileInfo);

  // make for DigitalGlobe2
  if (nil=FDG2) then
    FDG2 := TAvailPicsDG2.Create(@FAvailPicsTileInfo);

  // make for nokia map creator
  GenerateAvailPicsNMC(FNMCs, @FAvailPicsTileInfo);

  // make for datadoors
  GenerateAvailPicsDD(FDDs, @FAvailPicsTileInfo);

  // make for terraserver
  if (nil=FTerraserver) then
    FTerraserver := TAvailPicsTerraserver.Create(@FAvailPicsTileInfo);

  // make for ESRI
  if (nil=FESRI) then
    FESRI := TAvailPicsESRI.Create(@FAvailPicsTileInfo);

  // make for digital globe
  if (0=Length(FDGStacks)) then
    GenerateAvailPicsDG(FDGStacks, @FAvailPicsTileInfo);

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

procedure TfrmDGAvailablePic.PropagateLocalConverter;
var
  i,k: Integer;
  j: TAvailPicsNMCZoom;
  jj: TAvailPicsDataDoorsID;
begin
  if (nil<>FBing) then
    FBing.SetLocalConverter(FLocalConverter);

  if (nil<>FDG2) then
    FDG2.SetLocalConverter(FLocalConverter);

  for j := Low(TAvailPicsNMCZoom) to High(TAvailPicsNMCZoom) do begin
    if (FNMCs[j]<>nil) then
      FNMCs[j].SetLocalConverter(FLocalConverter);
  end;

  for jj := Low(TAvailPicsDataDoorsID) to High(TAvailPicsDataDoorsID) do begin
    if (FDDs[jj]<>nil) then
      FDDs[jj].SetLocalConverter(FLocalConverter);
  end;

  if (nil<>FTerraserver) then
    FTerraserver.SetLocalConverter(FLocalConverter);

  if (nil<>FESRI) then
    FESRI.SetLocalConverter(FLocalConverter);

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
      
    if(VNode.StateIndex <> 2) then
      VNode.StateIndex := 2
    else
      VNode.StateIndex := 1;

    for i:=0 to VNode.Count-1 do
      VNode.Item[i].StateIndex:=VNode.StateIndex;
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
  VHasState := 0;
  if tvFound.Items.Count>0 then
  for i := 0 to tvFound.Items.Count-1 do begin
    // keep state
    if (2=tvFound.Items.Item[i].StateIndex) then
      VHasState := (VHasState or $01) // checked
    else
      VHasState := (VHasState or $02);
    // check both exist
    if ($03 = VHasState) then
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
  VHasState := 0;
  if PnlSearch.ControlCount>0 then
  for i := 0 to PnlSearch.ControlCount-1 do begin
    VBox := PnlSearch.Controls[i];
    if IsCommonServiceCheckbox(VBox) then begin
      // keep state
      if (cbChecked = TCheckBox(VBox).State) then
        VHasState := (VHasState or $01) // checked
      else
        VHasState := (VHasState or $02);
      // check both exist
      if ($03 = VHasState) then
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
  const ALanguageManager: ILanguageManager;
  const AInetConfig: IInetConfig
);
begin
  FMarkDBGUI := AMarkDBGUI;
  FALLClicking := FALSE;
  FVertResizeFactor:=0;
  FCallIndex:=0;
  FBing:=nil;
  FDG2:=nil;
  FillChar(FNMCs, sizeof(FNMCs), 0);
  FillChar(FDDs, sizeof(FDDs), 0);
  FTerraserver:=nil;
  FESRI:=nil;
  SetLength(FDGStacks, 0);

  ZeroMemory(@FAvailPicsTileInfo, sizeof(FAvailPicsTileInfo));
  FAvailPicsTileInfo.AddImageProc := AddAvailImageItem;

  inherited Create(ALanguageManager);

  FCSAddNode := MakeSync_Tiny(Self, FALSE);

  FLocalConverter := nil;
  FInetConfig := AInetConfig;
  FResultFactory := TDownloadResultFactory.Create;

end;

destructor TfrmDGAvailablePic.Destroy;
begin
  FMarkDBGUI:=nil;
  // kill vendors objects
  KillPicsVendors;
  // interfaces
  FResultFactory:=nil;
  FInetConfig:=nil;
  FLocalConverter:=nil;
  FCSAddNode:=nil;
  inherited;
end;

procedure TfrmDGAvailablePic.ApplyALLCheckboxState(const AChkBox: TCheckBox; const AHasState: Byte);
begin
  case AHasState of
    $03: begin
      // both
      AChkBox.State := cbGrayed;
    end;
    $01: begin
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
var VMinNewHeight: Integer;
begin
  VMinNewHeight:=(gbAvailImages.Top+gbAvailImages.Constraints.MinHeight+spltDesc.Height+gbImageParams.Height+FVertResizeFactor);
  if (NewHeight<VMinNewHeight) then
    NewHeight:=VMinNewHeight;
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
