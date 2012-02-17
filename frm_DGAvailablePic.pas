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
  u_AvailPicsBing,
  u_AvailPicsNMC,
  i_OperationNotifier,
  u_OperationNotifier,
  i_DownloadRequest,
  u_DownloadRequest,
  i_DownloadResult,
  i_DownloadResultFactory,
  u_DownloadResultFactory,
  i_DownloadResultTextProvider,
  u_DownloadResultTextProvider,
  i_Downloader,
  u_DownloaderHttp,
  t_GeoTypes,
  Grids,
  ValEdit;

type
  TfrmDGAvailablePic = class(TFormWitghLanguageManager)
    gbImageParams: TGroupBox;
    gbAvailImages: TGroupBox;
    tvFound: TTreeView;
    btnUp: TButton;
    btnDown: TButton;
    gbImagesSource: TGroupBox;
    cbDGstacks: TComboBox;
    btnCopy: TButton;
    pnlRight: TPanel;
    chkBing: TCheckBox;
    chkNMC: TCheckBox;
    chkDG: TCheckBox;
    btnRefresh: TButton;
    veImageParams: TValueListEditor;
    lbZoom: TLabel;
    spltDesc: TSplitter;
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
  private
    FBing: TAvailPicsBing;
    FNMC: TAvailPicsNMC;
    FDGStacks: TAvailPicsDGs;
    FAvailPicsTileInfo: TAvailPicsTileInfo;
    FCallIndex: DWORD;
    FVertResizeFactor: Integer;
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
  private
    FLocalConverter: ILocalCoordConverter;
    FInetConfig: IInetConfig;
    FDownloadResultTextProvider: IDownloadResultTextProvider;
    FResultFactory: IDownloadResultFactory;
    procedure CopyStringToClipboard(s: Widestring);
  public
    constructor Create(ALanguageManager: ILanguageManager;
                       const AInetConfig: IInetConfig); reintroduce;
    destructor Destroy; override;
    
    procedure ShowInfo(const AVisualPoint: TPoint;
                       const ALocalConverter: ILocalCoordConverter);
  end;

implementation

uses
  i_CoordConverter,
  i_ProxySettings,
  u_ResStrings;

type
  TGetList = class(TThread)
  public
    FLinkToService: String;
    FContentType: String;
    FHttpErrorCode: Cardinal;
    FHttpErrorText: String;
  private
    FInetConfig: IInetConfig;
    FForm: TfrmDGAvailablePic;
    FAvailPicsSrc: TAvailPicsAbstract;
    FCallIndex: DWORD;
    FMemoryStream: TMemoryStream;
    function CallIndexActual: Boolean;
    function GetStreamFromURL1: Boolean;
  protected
    procedure Execute; override;
    procedure ShowList;
    procedure ShowError;
  public
    constructor Create(
      const AInetConfig: IInetConfig;
      const AAvailPicsSrc: TAvailPicsAbstract;
      const AForm: TfrmDGAvailablePic
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
      const AForm: TfrmDGAvailablePic
);
begin
  inherited Create(True);
  FHttpErrorCode:=0;
  FHttpErrorText:='';
  FInetConfig := AInetConfig;
  FreeOnTerminate:=true;
  Priority:=tpLower;
  FAvailPicsSrc:=AAvailPicsSrc;
  FLinkToService:=AAvailPicsSrc.LinkToImages;
  FContentType:=AAvailPicsSrc.ContentType;
  FForm := AForm;
  FCallIndex := AForm.FCallIndex;
  FMemoryStream := TMemoryStream.Create;
end;

destructor TGetList.Destroy;
begin
  FInetConfig:=nil;
  FreeAndNil(FMemoryStream);
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

function TGetList.GetStreamFromURL1: Boolean;
var
  VDownloaderHttp: IDownloader; // TDownloaderHttp;
  VRequest: IDownloadRequest; // TDownloadRequest
  VResult: IDownloadResult;
  VResultWithRespond: IDownloadResultWithServerRespond;
  VDownloadResultError: IDownloadResultError;
  VDownloadResultDataNotExists: IDownloadResultDataNotExists;
  VResultOk: IDownloadResultOk;
  VCancelNotifier: IOperationNotifier;
begin
  Result:=FALSE;
  try
    VDownloaderHttp:=TDownloaderHttp.Create(FForm.FResultFactory);
    VRequest:=TDownloadRequest.Create(FLinkToService, '', FInetConfig.GetStatic);
    VCancelNotifier:=TOperationNotifier.Create;

    // download
    VResult:=VDownloaderHttp.DoRequest(VRequest, VCancelNotifier, VCancelNotifier.CurrentOperation);

    // check result
    if not Assigned(VResult) then begin
      // fail
      FHttpErrorText:='No result';
    end else if Supports(VResult, IDownloadResultWithServerRespond, VResultWithRespond) then begin
      // obtain result
      FHttpErrorCode := VResultWithRespond.StatusCode;
      if Supports(VResult, IDownloadResultOk, VResultOk) then begin
        // save to stream
        FMemoryStream.Position:=0;
        FMemoryStream.SetSize(VResultOk.Size);
        CopyMemory(FMemoryStream.Memory, VResultOk.Buffer, VResultOk.Size);
        Result:=TRUE;
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
    VDownloaderHttp:=nil;
    VCancelNotifier:=nil;
  end;
end;

procedure TGetList.ShowList;
begin
  if not CallIndexActual then
    Exit;

  if (0<FAvailPicsSrc.ParseResponse(FMemoryStream)) then
  try
    FForm.tvFound.AlphaSort;
  except
  end;
end;

procedure TGetList.Execute;
begin
  try
    if GetStreamFromURL1 then begin
      // ok
      if not(Terminated) then
        Synchronize(ShowList);
    end else begin
      // failed
      if not(Terminated) then
        Synchronize(ShowError);
    end;
  finally
    FreeAndNil(FMemoryStream);
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
  
  // lookup for node as "2011/12/30" -> "DG" -> NODE (TID + PARAMS)
  if GetImagesNode(nil, ADate, VDateNode) then
  //if GetImagesNode(VDateNode, VImageService, VVendorNode) then
  begin
    VItemNode := tvFound.Items.AddChild(VDateNode, AId); // for subnides - replace with VVendorNode
    VItemNode.Data := AParams;
    AParams := nil; // own object
    Inc(Result);
  end;
end;

procedure TfrmDGAvailablePic.btnRefreshClick(Sender: TObject);
var
  VDGstack: TAvailPicsDG;
begin
  // clear
  ClearAvailableImages;

  // run thread for every image source
  RunImageThread(chkBing, FBing);
  RunImageThread(chkNMC, FNMC);

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
    if (0<Length(single_tid)) then begin
      if (0<Length(Result)) then
        Result:=Result+',';
      Result:=Result+single_tid;
    end;
  except
  end;
end;

procedure TfrmDGAvailablePic.KillPicsVendors;
var i,k: Integer;
begin
  // simple
  FreeAndNil(FBing);
  FreeAndNil(FNMC);
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

  // make for nokia map creator
  if (nil=FNMC) then
    FNMC := TAvailPicsNMC.Create(@FAvailPicsTileInfo);

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
var i,k: Integer;
begin
  if (nil<>FBing) then
    FBing.SetLocalConverter(FLocalConverter);

  if (nil<>FNMC) then
    FNMC.SetLocalConverter(FLocalConverter);

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
  if AChkBox.Checked then
  with TGetList.Create(FInetConfig,
                       AImgVendor,
                       Self) do
  begin
    Resume;
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

procedure TfrmDGAvailablePic.UpdateInfoByNode(const ANode: TTreeNode);
begin
  if (nil=ANode) then
    ClearInfoByNode
  else if (nil=ANode.Data) then
    ClearInfoByNode
  else begin
    // update info
    veImageParams.Strings.Assign(TStrings(ANode.Data));
  end;
end;

procedure TfrmDGAvailablePic.UpdateZoomLabel;
var
  VActualZoom: Byte;
  VZoomStr: String;
begin
  if Assigned(FLocalConverter) then begin
    VActualZoom:=FLocalConverter.Zoom;
    AdjustMinimalHiResZoom(VActualZoom);
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
end;

procedure TfrmDGAvailablePic.ClearInfoByNode;
begin
  veImageParams.Strings.Clear;
end;

procedure TfrmDGAvailablePic.CopyStringToClipboard(s: Widestring);
var hg: THandle;
    P: PChar;
begin
  if OpenClipboard(Handle) then
  begin
    try
      EmptyClipBoard;
      hg:=GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, Length(S)+1);
      try
        P:=GlobalLock(hg);
        try
          StrPCopy(P, s);
          SetClipboardData(CF_TEXT, hg);
        finally
          GlobalUnlock(hg);
        end;
      except
        GlobalFree(hg);
        raise
      end;
    finally
      CloseClipboard;
    end;
  end
end;

constructor TfrmDGAvailablePic.Create(ALanguageManager: ILanguageManager;
                                      const AInetConfig: IInetConfig);
begin
  FVertResizeFactor:=0;
  FCallIndex:=0;
  FBing:=nil;
  FNMC:=nil;
  SetLength(FDGStacks, 0);

  ZeroMemory(@FAvailPicsTileInfo, sizeof(FAvailPicsTileInfo));
  FAvailPicsTileInfo.AddImageProc := AddAvailImageItem;

  inherited Create(ALanguageManager);

  FLocalConverter := nil;
  FInetConfig := AInetConfig;
  FDownloadResultTextProvider := TDownloadResultTextProvider.Create(ALanguageManager);
  FResultFactory := TDownloadResultFactory.Create(FDownloadResultTextProvider);
end;

destructor TfrmDGAvailablePic.Destroy;
begin
  // kill vendors objects
  KillPicsVendors;
  // interfaces
  FResultFactory:=nil;
  FDownloadResultTextProvider:=nil;
  FInetConfig:=nil;
  FLocalConverter:=nil;
  inherited;
end;

procedure TfrmDGAvailablePic.btnCopyClick(Sender: TObject);
begin
  CopyStringToClipboard(Get_DG_tid_List);
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
end;

end.
