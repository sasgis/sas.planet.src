unit frm_DGAvailablePic;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  wininet,
  Dialogs,
  StdCtrls,
  ComCtrls,
  CommCtrl,
  ExtCtrls,
  u_CommonFormAndFrameParents,
  i_LocalCoordConverter,
  u_ResStrings,
  t_GeoTypes;

type
  TfrmDGAvailablePic = class(TCommonFormParent)
    GroupBox1: TGroupBox;
    LabelDate: TLabel;
    LabelResolution: TLabel;
    LabelColor: TLabel;
    LabelProv: TLabel;
    GroupBox3: TGroupBox;
    TreeView1: TTreeView;
    Button1: TButton;
    Button2: TButton;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    GroupBox4: TGroupBox;
    ComboBox2: TComboBox;
    Button3: TButton;
    pnlRight: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ComboBox2Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    ms:TMemoryStream;
    FLonLat:TDoublePoint;
    tids,ls:string;
    mpp:extended;
    hi,wi:integer;
    procedure CreateTree;
    procedure FormTidList;
    procedure CopyStringToClipboard(s: Widestring);
  public
    procedure setup(ALocalConverter: ILocalCoordConverter; AVisualPoint: TPoint);
  end;

var
  frmDGAvailablePic: TfrmDGAvailablePic;
  Stacks : array [0..13,0..3] of string =
            (
             ('227400001','1','GlobeXplorer Premium Stack','020100S'),
             ('227400001','2','USGS 1:24k Topo Stack','020100S'),
             ('2133000801','4','GlobeXplorer Premium Portal Stack','060100W'),
             ('dfe278a2-8c6e-494f-927e-8937470893fc','6','APUSA Stack','020100S'),
             ('dfe278a2-8c6e-494f-927e-8937470893fc','7','DigitalGlobe Stack','020100S'),
             ('dfe278a2-8c6e-494f-927e-8937470893fc','11','CitiPix by GlobeXplorer ODI stack','020100S'),
             ('dfe278a2-8c6e-494f-927e-8937470893fc','13','DOQQ Stack','020100S'),
             ('dfe278a2-8c6e-494f-927e-8937470893fc','14','I-cubed Image Stack','020100S'),
             ('dfe278a2-8c6e-494f-927e-8937470893fc','18','CitiPix by GlobeXplorer ODI plus RDI stack','020100S'),
             ('227400001','19','WMS Premium','020100S'),
             ('dfe278a2-8c6e-494f-927e-8937470893fc','20','National Map Data Stack','020100S'),
             ('227400001','27','NAIP Stack','020100S'),
             ('2133000801','32','Current Events Stack','060100W'),
//             ('4844000213','33', 'GlobeXplorer Deluxe Stack','030603A'),
//             ('4844000213','34', 'GlobeXplorer Deluxe Portal Stack','030603A'),
             ('dfe278a2-8c6e-494f-927e-8937470893fc','49','Country Coverage','020100S')
             );
{ Stacks : array [0..32,0..3] of string =
            (
             ('227400001','1','GlobeXplorer Premium Stack','020100S'),
             ('227400001','2','USGS 1:24k Topo Stack','020100S'),
             ('227400001','3','GlobeXplorer Basic Stack','020100S'),
             ('2133000801','4','GlobeXplorer Premium Portal Stack','060100W'),
             ('ca4046dd-bba5-425c-8966-0a553e0deb3a','6','APUSA Stack','020100S'),
             ('ca4046dd-bba5-425c-8966-0a553e0deb3a','7','DigitalGlobe Stack','020100S'),
             ('7327000291','10', 'GlobeXplorer Standard Stack','020100S'),
             ('ca4046dd-bba5-425c-8966-0a553e0deb3a','11','CitiPix by GlobeXplorer ODI stack','020100S'),
             ('ca4046dd-bba5-425c-8966-0a553e0deb3a','13','DOQQ Stack','020100S'),
             ('ca4046dd-bba5-425c-8966-0a553e0deb3a','14','I-cubed Image Stack','020100S'),
             ('7327000291','15', 'I-cubed Map Stack','020100S'),
             ('7327000291','16', 'STDB Demo Stack','020100S'),
             ('ca4046dd-bba5-425c-8966-0a553e0deb3a','18','CitiPix by GlobeXplorer ODI plus RDI stack','020100S'),
             ('227400001','19','WMS Premium','020100S'),
             ('ca4046dd-bba5-425c-8966-0a553e0deb3a','20','National Map Data Stack','020100S'),
             ('7327000291','21', 'NavTech Accuracy Data Stack','020100S'),
             ('7327000291','22', 'NavTech Propietary Data Stack','020100S'),
             ('7327000291','26', 'GlobeXplorer Premium 3D Data Stack','020100S'),
             ('227400001','27','NAIP Stack','020100S'),
             ('7327000291','28', 'EarthSat Stack','020100S'),
             ('2133000801','32','Current Events Stack','060100W'),
             ('7327000291','33', 'GlobeXplorer Deluxe Stack','020100S'),
             ('7327000291','34', 'GlobeXplorer Deluxe Portal Stack','020100S'),
             ('7327000291','38','GlobeXplorer Premium Portal Data NB Stack','020100S'),
             ('7327000291','41','DigitalGlobe owned','020100S'),
             ('7327000291','47','DIBU Data','020100S'),
             ('7327000291','48','Oil and Gas NB','020100S'),
             ('ca4046dd-bba5-425c-8966-0a553e0deb3a','49','Country Coverage','020100S'),
             ('7327000291','51','WorldView-01','020100S'),
             ('7327000291','52','DigitalGlobe CitySphere','020100S'),
             ('7327000291','53','Country Coverage2','020100S'),
             ('7327000291','54','DigitalGlobe GeoCells','020100S'),
             ('7327000291','55','DigitalGlobe NGA Ortho Imagery','020100S')
             );  }
implementation

uses
  i_CoordConverter,
  i_ProxySettings,
  u_GlobalState,
  u_GeoToStr;
function EncodeDG(S: string): string;
var i: integer;
begin
 result:=S;
 for i:=1 to length(s) do
  if ord(s[i]) mod 2 = 0 then result[i]:=chr(ord(s[i])+1)
                         else result[i]:=chr(ord(s[i])-1);
end;

function Encode64(S: string): string;
const Codes64 = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var i,a,x,b: Integer;
begin
 Result:='';
 a:=0;
 b:=0;
 for i := 1 to Length(s) do
  begin
   x:=Ord(s[i]);
   b:=b*256+x;
   a:=a+8;
   while a >= 6 do
    begin
     a := a-6;
     x := b div (1 shl a);
     b := b mod (1 shl a);
     Result := Result + Codes64[x + 1];
    end;
  end;
 if a>0 then Result:=Result+Codes64[(b shl (6-a))+1];
end;

type
  TDGPicture = class
   tid:string;
   date:string;
   provider:string;
   color:string;
   resolution:string;
  end;

  TGetList = class(TThread)
    Link:string;
    ErrCode:integer;
  private
    FForm: TfrmDGAvailablePic;
    list:TStringList;
    function GetStreamFromURL1(var ms:TMemoryStream;url:string;conttype:string):integer;
  protected
    procedure Execute; override;
    procedure ShowList;
    procedure ShowError;
  public
    constructor Create(ALink:string; AForm: TfrmDGAvailablePic);
  end;

const
  maxReqSize = 3000;
const
  D2R: Double = 0.017453292519943295769236907684886;// Константа для преобразования градусов в радианы


var
  GetListThId:THandle;

{$R *.dfm}
function GetWord(Str, Smb: string; WordNmbr: Byte): string;
var SWord: string;
    StrLen, N: Byte;
begin
  StrLen := SizeOf(Str);
  N := 1;
  while ((WordNmbr >= N) and (StrLen <> 0)) do
  begin
    StrLen := System.Pos(Smb, str);
    if StrLen <> 0 then
    begin
      SWord := Copy(Str, 1, StrLen - 1);
      Delete(Str, 1, StrLen);
      Inc(N);
    end
    else SWord := Str;
  end;
  if WordNmbr <= N then Result := SWord
                   else Result := '';
end;

constructor TGetList.Create(ALink:string; AForm: TfrmDGAvailablePic);
begin
  inherited Create(True);
  FreeOnTerminate:=true;
  Priority:=tpLower;
  Link:=ALink;
  FForm := AForm;
end;

procedure TGetList.ShowError;
begin
  case ErrCode of
  -3: ShowMessage(SAS_ERR_Authorization);
  -1: ShowMessage(SAS_ERR_TileNotExists);
   0: ShowMessage(SAS_ERR_Noconnectionstointernet);
   else ShowMessage(SAS_ERR_Noconnectionstointernet);
 end;
end;

function TGetList.GetStreamFromURL1(var ms:TMemoryStream;url:string;conttype:string):integer;
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
  VProxyConfig := GState.InetConfig.ProxyConfig;
  VProxyConfig.LockRead;
  try
    VUselogin := (not VProxyConfig.GetUseIESettings) and VProxyConfig.GetUseProxy and VProxyConfig.GetUseLogin;
    VLogin := VProxyConfig.GetLogin;
    VPassword := VProxyConfig.GetPassword;
  finally
    VProxyConfig.UnlockRead;
  end;
  hSession:=InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'),INTERNET_OPEN_TYPE_PRECONFIG,nil,nil,0);
 if Assigned(hSession)
  then begin
        hFile:=InternetOpenURL(hSession,PChar(URL),PChar(par),length(par),INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_RELOAD,0);
        if Assigned(hFile)then
         begin
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
          if (System.Pos(conttype,ty)>0) then
          repeat
           err:=not(internetReadFile(hFile,@Buffer,SizeOf(Buffer),BufferLen));
           ms.Write(Buffer,BufferLen);
          until (BufferLen=0)and(BufferLen<SizeOf(Buffer))and(err=false)
          else result:=-1;
         end
        else result:=0;
       end
  else result:=0;
  ms.Position:=0;
  InternetCloseHandle(hFile);
  InternetCloseHandle(hSession);
end;

procedure TGetList.ShowList;
var datesat:string;
    i,j:integer;
    added:boolean;
    node:TTreeNode;
begin
 if ThreadID=GetListThId then
 begin
 for i:=0 to list.Count-1 do
  try
   datesat:=GetWord(list[i], ',', 2);
   datesat[5]:=DateSeparator;
   datesat[8]:=DateSeparator;
   added:=false;
   for j:=0 to FForm.TreeView1.Items.Count-1 do
    if FForm.TreeView1.Items.Item[j].Text=datesat then
     begin
      node:=FForm.TreeView1.Items.AddChild(FForm.TreeView1.Items.Item[j],GetWord(list[i], ',', 1));
      added:=true;
      break;
     end;
   if not(added) then
    node:=FForm.TreeView1.Items.AddChild(FForm.TreeView1.Items.Add(nil,datesat),GetWord(List[i], ',', 1));
   node.Data:=TDGPicture.Create;
   with TDGPicture(node.Data) do
    begin
     tid:=GetWord(list[i], ',', 1);
     date:=GetWord(list[i], ',', 2);
     provider:=GetWord(list[i], ',', 3);
     color:=GetWord(list[i], ',', 6);
     resolution:=GetWord(list[i], ',', 5);
    end;
  except
  end;
  FForm.TreeView1.AlphaSort();
 end;
end;

procedure TGetList.Execute;
var ms:TMemoryStream;
begin
 List:=TStringList.Create;
 ms:=TMemoryStream.Create;
 ErrCode:=GetStreamFromURL1(ms,Link,'text/plain');
 if ErrCode>0 then
  begin
   List.LoadFromStream(ms);
   ms.Free;
   if not(Terminated) then Synchronize(ShowList);
  end
  else
  begin
   if not(Terminated) then Synchronize(ShowError);
  end;
 List.Free;
end;

procedure TfrmDGAvailablePic.CreateTree;
var pltstr:TStringList;
    datesat:string;
    i,j:integer;
    added:boolean;
    node:TTreeNode;
begin
 pltstr:=TStringList.Create;
 pltstr.LoadFromStream(ms);
 for i:=0 to pltstr.Count-1 do
  try
   datesat:=GetWord(pltstr[i], ',', 2);
   datesat[5]:=DateSeparator;
   datesat[8]:=DateSeparator;
   added:=false;
   for j:=0 to TreeView1.Items.Count-1 do
    if TreeView1.Items.Item[j].Text=datesat then
     begin
      node:=TreeView1.Items.AddChild(TreeView1.Items.Item[j],GetWord(pltstr[i], ',', 1));
      added:=true;
      break;
     end;
   if not(added) then
    node:=TreeView1.Items.AddChild(TreeView1.Items.Add(nil,datesat),GetWord(pltstr[i], ',', 1));
   node.Data:=TDGPicture.Create;
   with TDGPicture(node.Data) do
    begin
     tid:=GetWord(pltstr[i], ',', 1);
     date:=GetWord(pltstr[i], ',', 2);
     provider:=GetWord(pltstr[i], ',', 3);
     color:=GetWord(pltstr[i], ',', 6);
     resolution:=GetWord(pltstr[i], ',', 5);
    end;
  except
  end;
 TreeView1.AlphaSort();
 pltstr.Free;
end;

procedure TfrmDGAvailablePic.setup(ALocalConverter: ILocalCoordConverter; AVisualPoint: TPoint);
var
  VSize: TPoint;
  VRad: Extended;
  VPixelsAtZoom: Double;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapPixel: TDoublePoint;
begin
  Show;
  VSize := ALocalConverter.GetLocalRectSize;
  VConverter := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapPixel := ALocalConverter.LocalPixel2MapPixelFloat(AVisualPoint);
  VConverter.CheckPixelPosFloatStrict(VMapPixel, VZoom, True);
  FLonLat := VConverter.PixelPosFloat2LonLat(VMapPixel, VZoom);
  VRad := VConverter.Datum.GetSpheroidRadiusA;
  VPixelsAtZoom := VConverter.PixelsAtZoomFloat(VZoom);
 mpp:=1/((VPixelsAtZoom/(2*PI))/(VRad*cos(FLonLat.y*D2R)));
 hi:=round(mpp*15);
 wi:=round(mpp*15);
 if hi>maxReqSize then hi:=maxReqSize;
 if wi>maxReqSize then wi:=maxReqSize;
 if hi<VSize.Y then hi:=256;
 if wi<VSize.X then wi:=256;
 if mpp>8 then mpp:=8;
 ComboBox2Change(nil);
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

procedure TfrmDGAvailablePic.FormTidList;
var
    i:integer;
    added:boolean;
begin
 tids:='';
 added:=false;
 for i:=0 to TreeView1.Items.Count-1 do
  begin
    if not(TreeView1.Items.Item[i].HasChildren) then
     if TreeView1.Items.Item[i].StateIndex=2 then
      if added then tids:=tids+','+TDGPicture(TreeView1.Items.Item[i].Data).tid
               else begin
                     added:=true;
                     tids:=tids+TDGPicture(TreeView1.Items.Item[i].Data).tid;
                    end;
  end;
end;


procedure TfrmDGAvailablePic.TreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var MH:THitTests;
    Node:TTreeNode;
    i:integer;
begin
 MH:= TreeView1.GetHitTestInfoAt(X,Y);
 if (htOnStateIcon in MH)
  then
   begin
    Node:= TreeView1.GetNodeAt(X,Y);
    if(Node.StateIndex <> 2)
     then Node.StateIndex := 2
     else Node.StateIndex := 1;
     for i:=0 to node.Count-1 do node.Item[i].StateIndex:=Node.StateIndex;
    FormTidList;
   end;
  if (htOnLabel in MH) then
   begin
    Node:= TreeView1.GetNodeAt(X,Y);
    if node.Data<>nil then
    with TDGPicture(node.Data) do
     begin
      LabelDate.Caption:=date;
      LabelProv.Caption:=provider;
      LabelColor.Caption:=color;
      LabelResolution.Caption:=resolution;
     end
    else
     begin
      LabelDate.Caption:='-';
      LabelProv.Caption:='-';
      LabelColor.Caption:='-';
      LabelResolution.Caption:='-';
     end;
   end;
end;

procedure TfrmDGAvailablePic.ComboBox2Change(Sender: TObject);
var
    encrypt:string;
begin
 TIDs:='';
 LabelDate.Caption:='-';
 LabelProv.Caption:='-';
 LabelColor.Caption:='-';
 LabelResolution.Caption:='-';
 TreeView1.Items.Clear;
 ls:=stacks[ComboBox2.ItemIndex,1];
 GetWord(ComboBox2.Text, ',', 1);
 encrypt:= Encode64(EncodeDG('cmd=info&id='+stacks[ComboBox2.ItemIndex,0]+'&appid='+stacks[ComboBox2.ItemIndex,3]+'&ls='+ls+'&xc='+R2StrPoint(FLonLat.x)+'&yc='+R2StrPoint(FLonLat.y)+'&mpp='+R2StrPoint(mpp)+'&iw='+inttostr(wi)+'&ih='+inttostr(hi)+'&extentset=all'));

 with TGetList.Create('http://image.globexplorer.com/gexservlets/gex?encrypt='+encrypt, Self) do
  begin
   GetListThId:=ThreadID;
   Resume;
  end;
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

procedure TfrmDGAvailablePic.Button3Click(Sender: TObject);
begin
  CopyStringToClipboard(TIDs);
end;

procedure TfrmDGAvailablePic.FormCreate(Sender: TObject);
var i:integer;
begin
 SetWindowLong(TreeView1.Handle,GWL_STYLE,GetWindowLong(TreeView1.Handle,GWL_STYLE) or TVS_CHECKBOXES);
 for i:=0 to length(Stacks)-1 do
   ComboBox2.Items.Add(Stacks[i,1]+', '+Stacks[i,2]);
 ComboBox2.ItemIndex:=ComboBox2.Items.Count-1;
end;

end.
