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
  wininet,
  Dialogs,
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
  t_GeoTypes, Grids, ValEdit;

type
  TfrmDGAvailablePic = class(TFormWitghLanguageManager)
    GroupBox1: TGroupBox;
    GroupBox3: TGroupBox;
    TreeView1: TTreeView;
    Button1: TButton;
    Button2: TButton;
    GroupBox4: TGroupBox;
    cbDGstacks: TComboBox;
    Button3: TButton;
    pnlRight: TPanel;
    chkBing: TCheckBox;
    chkNMC: TCheckBox;
    chkDG: TCheckBox;
    btnRefresh: TButton;
    ValueListEditor1: TValueListEditor;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TreeView1Deletion(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
  private
    FBing: TAvailPicsBing;
    FNMC: TAvailPicsNMC;
    FDGStacks: TAvailPicsDGs;
    FAvailPicsTileInfo: TAvailPicsTileInfo;
    FCallIndex: DWORD;
  private
    procedure MakePicsVendors;
    procedure KillPicsVendors;
    
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
    procedure CopyStringToClipboard(s: Widestring);
  public
    constructor Create(ALanguageManager: ILanguageManager;
                       const AInetConfig: IInetConfig); reintroduce;
    
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
    FErrCode: Integer;
  private
    FInetConfig: IInetConfig;
    FForm: TfrmDGAvailablePic;
    FAvailPicsSrc: TAvailPicsAbstract;
    FCallIndex: DWORD;
    FMemoryStream: TMemoryStream;
    function CallIndexActual: Boolean;
    function GetStreamFromURL1:integer;
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
begin
  case FErrCode of
  -3: ShowMessage(SAS_ERR_Authorization);
  -1: ShowMessage(SAS_ERR_TileNotExists);
   0: ShowMessage(SAS_ERR_Noconnectionstointernet);
   else ShowMessage(SAS_ERR_Noconnectionstointernet);
 end;
end;

function TGetList.GetStreamFromURL1:integer;
var par,ty:string;
    err:boolean;
    Buffer:array [1..64535] of char;
    BufferLen:LongWord;
    hSession,hFile:Pointer;
    dwtype: array [1..20] of char;
    dwindex, dwcodelen,dwReserv: dword;
  VProxyConfig: IProxyConfig;
  VUselogin: Boolean;
  VLogin: string;
  VPassword: string;
begin
  Result := 0;
  VProxyConfig := FInetConfig.ProxyConfig;
  VProxyConfig.LockRead;
  try
    VUselogin := (not VProxyConfig.GetUseIESettings) and VProxyConfig.GetUseProxy and VProxyConfig.GetUseLogin;
    VLogin := VProxyConfig.GetLogin;
    VPassword := VProxyConfig.GetPassword;
  finally
    VProxyConfig.UnlockRead;
  end;
  hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
  if Assigned(hSession) then
  try
    hFile:=InternetOpenURL(hSession,PChar(FLinkToService),PChar(par),length(par),INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_RELOAD,0);
    if Assigned(hFile)then
    try
      dwcodelen:=150; dwReserv:=0; dwindex:=0;
      if HttpQueryInfo(hFile,HTTP_QUERY_STATUS_CODE,@dwtype, dwcodelen, dwReserv)
       then dwindex:=strtoint(pchar(@dwtype));
      if (dwindex=HTTP_STATUS_PROXY_AUTH_REQ) then
       begin
        if VUselogin then
         begin
          InternetSetOption (hFile, INTERNET_OPTION_PROXY_USERNAME,PChar(VLogin), length(VLogin));
          InternetSetOption (hFile, INTERNET_OPTION_PROXY_PASSWORD,PChar(VPassword), length(VPassword));
          HttpSendRequest(hFile, nil, 0,Nil, 0);
         end;
        dwcodelen:=150; dwReserv:=0; dwindex:=0;
        if HttpQueryInfo(hFile,HTTP_QUERY_STATUS_CODE,@dwtype, dwcodelen, dwReserv)
         then dwindex:=strtoint(pchar(@dwtype));
        if (dwindex=HTTP_STATUS_PROXY_AUTH_REQ) then //Неверные пароль логин
         begin
          result:=-3;
          InternetCloseHandle(hFile);
          InternetCloseHandle(hSession);
          exit;
         end;
       end;
      dwindex:=0; dwcodelen:=150; ty:='';
      fillchar(dwtype,sizeof(dwtype),0);
      if HttpQueryInfo(hfile,HTTP_QUERY_CONTENT_TYPE, @dwtype,dwcodelen,dwindex)
       then ty:=PChar(@dwtype);

      if (System.Pos(FContentType,ty)>0) then
      repeat
       err:=not(internetReadFile(hFile,@Buffer,SizeOf(Buffer),BufferLen));
       FMemoryStream.Write(Buffer,BufferLen);
      until (BufferLen=0)and(BufferLen<SizeOf(Buffer))and(err=false)
      else
        result:=-1; // no such content type
    finally
      InternetCloseHandle(hFile);
      FMemoryStream.Position:=0;
    end;
  finally
    InternetCloseHandle(hSession);
  end;
end;

procedure TGetList.ShowList;
begin
  if not CallIndexActual then
    Exit;

  if (0<FAvailPicsSrc.ParseResponse(FMemoryStream)) then
  try
    FForm.TreeView1.AlphaSort;
  except
  end;
end;

procedure TGetList.Execute;
begin
  try
    FErrCode := GetStreamFromURL1;

    if (0=FErrCode) then begin
      // ok
      if not(Terminated) then
        Synchronize(ShowList);
    end else begin
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
    VItemNode := TreeView1.Items.AddChild(VDateNode, AId); // for subnides - replace with VVendorNode
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

procedure TfrmDGAvailablePic.Button1Click(Sender: TObject);
begin
 if TreeView1.Selected<>nil then
 if TreeView1.Selected.HasChildren
  then if TreeView1.Selected.GetPrev<>nil
        then TreeView1.Selected.MoveTo(TreeView1.Selected.getPrevSibling,naInsert)
        else
  else if TreeView1.Selected.Parent<>TreeView1.Selected.GetPrev
        then TreeView1.Selected.MoveTo(TreeView1.Selected.GetPrev,naInsert)
end;

procedure TfrmDGAvailablePic.Button2Click(Sender: TObject);
begin
 if TreeView1.Selected<>nil then
 if TreeView1.Selected.HasChildren
 then
 if TreeView1.Selected.GetNextSibling<>nil then
  if TreeView1.Selected.GetNextSibling.GetNextSibling<>nil
   then TreeView1.Selected.MoveTo(TreeView1.Selected.GetNextSibling.GetNextSibling,naInsert)
   else TreeView1.Selected.MoveTo(TreeView1.Selected.GetNextSibling,naAdd)
  else
 else
 if TreeView1.Selected.Parent.GetLastChild<>TreeView1.Selected then
  if TreeView1.Selected.GetNext<>TreeView1.Selected.Parent.GetLastChild
   then TreeView1.Selected.MoveTo(TreeView1.Selected.GetNext.GetNext,naInsert)
   else TreeView1.Selected.MoveTo(TreeView1.Selected.GetNext,naAdd)
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
    k:=TreeView1.Items.Count
  else
    k:=AParentNode.Count;

  if (0<k) then
  for i := 0 to k-1 do begin
    // get item
    if (nil=AParentNode) then
      AResultNode:=TreeView1.Items.Item[i]
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
    AResultNode := TreeView1.Items.Add(nil, AText);
    Result := TRUE;
  end else begin
    AResultNode := TreeView1.Items.AddChild(AParentNode, AText);
    Result := TRUE;
  end;
end;

function TfrmDGAvailablePic.Get_DG_tid_List: String;
var
  i,k: Integer;
  single_tid: String;
begin
  Result := '';
  k := TreeView1.Items.Count;
  if (0<k) then
  for i := 0 to k-1 do
  if (nil<>TreeView1.Items.Item[i].Data) then
  if (2=TreeView1.Items.Item[i].StateIndex) then
  try
    // get tid for DG items
    single_tid := TStrings(TreeView1.Items.Item[i].Data).Values['tid'];
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
  k:=Length(FDGStacks);
  if (0<k) then begin
    for i := 0 to k-1 do begin
      FreeAndNil(FDGStacks[k]);
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

procedure TfrmDGAvailablePic.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  UpdateInfoByNode(Node);
end;

procedure TfrmDGAvailablePic.TreeView1Click(Sender: TObject);
begin
  UpdateInfoByNode(TreeView1.Selected);
end;

procedure TfrmDGAvailablePic.TreeView1Deletion(Sender: TObject; Node: TTreeNode);
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

procedure TfrmDGAvailablePic.TreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MH:THitTests;
  VNode:TTreeNode;
  i:integer;
begin
  MH:= TreeView1.GetHitTestInfoAt(X,Y);

  if (htOnStateIcon in MH) then begin
    VNode:= TreeView1.GetNodeAt(X,Y);

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
    UpdateInfoByNode(TreeView1.GetNodeAt(X,Y));
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
    ValueListEditor1.Strings.Assign(TStrings(ANode.Data));
  end;
end;

procedure TfrmDGAvailablePic.ClearAvailableImages;
begin
  TreeView1.Items.Clear;
  ClearInfoByNode;
end;

procedure TfrmDGAvailablePic.ClearInfoByNode;
begin
  ValueListEditor1.Strings.Clear;
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
  FCallIndex:=0;
  FBing:=nil;
  FNMC:=nil;
  SetLength(FDGStacks, 0);

  ZeroMemory(@FAvailPicsTileInfo, sizeof(FAvailPicsTileInfo));
  FAvailPicsTileInfo.AddImageProc := AddAvailImageItem;

  inherited Create(ALanguageManager);

  FLocalConverter := nil;
  FInetConfig := AInetConfig;
end;

procedure TfrmDGAvailablePic.Button3Click(Sender: TObject);
begin
  CopyStringToClipboard(Get_DG_tid_List);
end;

procedure TfrmDGAvailablePic.FormCreate(Sender: TObject);
begin
  // make checkboxes in list
  SetWindowLong(TreeView1.Handle,GWL_STYLE,GetWindowLong(TreeView1.Handle,GWL_STYLE) or TVS_CHECKBOXES);
  // make vendors and fill list of dg stacks
  MakePicsVendors;
end;

procedure TfrmDGAvailablePic.FormDestroy(Sender: TObject);
begin
  // kill vendors objects
  KillPicsVendors;
  // interfaces
  FInetConfig:=nil;
  FLocalConverter:=nil;
  // base
  inherited;
end;

end.
