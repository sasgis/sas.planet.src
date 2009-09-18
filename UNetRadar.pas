unit UNetRadar;

interface

uses Windows,Menus,Dialogs,UResStrings,GR32_Polygons,Controls,strUtils,Classes,UImgFun,StdCtrls,ComCtrls,DB,PNGImage,GR32,GR32_layers, Provider, DBClient,DISQLite3DataSet,GR32_Resamplers,CommCtrl,uGeoFun,DISQLite3Database,wininet, DISQLite3Api,TBXGraphics,Graphics,IdHTTP;

type
 TNodeType = (ndCategory,ndObject,ndstopover,ndTrackList,ndTrack,ndCurrent);

 TNodeParams = record
  id:integer;
  type_:TNodeType;
 end;

type
 TResField = record
  asString:string;
  asFloat:Extended;
  asInteger:integer;
  asPointer:Pointer;
  asStream:TMemoryStream;
 end;

type
 TNetRadar = class(TThread)
  IdHTTP1: TIdHTTP;
  new:boolean;
  Icons:TStringList;
  LayerMap:TBitmapLayer;
  TreeView:Pointer;
  MemoObjectInfo:TMemo;
  imahelist:TImageList;
  db:TDISQLite3Database;
  UDObject:TDISQLite3UniDirQuery;
  UDCategory:TDISQLite3UniDirQuery;
  UDTrack:TDISQLite3UniDirQuery;
  UDTrackID:TDISQLite3UniDirQuery;
  UDstopover:TDISQLite3UniDirQuery;
  UDTrPoint:TDISQLite3UniDirQuery;
  DSPObject:TDataSetProvider;
  DSPCategory:TDataSetProvider;
  DSPTrack:TDataSetProvider;
  DSPTrackID:TDataSetProvider;
  DSPTrPoint:TDataSetProvider;
  DSPstopover:TDataSetProvider;
  DSObject:TClientDataSet;
  DSCategory:TClientDataSet;
  DSTrack:TClientDataSet;
  DSTrackID:TClientDataSet;
  DSTrPoint:TClientDataSet;
  DSstopover:TClientDataSet;
  params:array [1..13] of TStringList;
  public
   constructor create(CrSusp:Boolean;ATreeView:Pointer; AMemoObjectInfo:TMemo);
   destructor destroy; override;
   procedure DrawLayer;
   procedure PopUpGoHim(Sender: TObject);
   procedure SetGoHim(Sender: TObject);
   procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
   procedure UpdateDB;
   procedure drawPath(pathll:array of TExtendedPoint;color1:TColor32;linew:integer);
   procedure DrawTree;
   procedure DrawGoHim;
   procedure Execute; override;
 end;

var
 ParamsName: array [1..13] of string = ('trcls','clrs','dttms','infos','ids','names','uts','lats','lons','spds','crss','imgs','ips');

implementation

uses unit1,SysUtils, ImgList, IdCookie, IdCookieManager;

procedure TNetRadar.PopUpGoHim(Sender: TObject);
begin
 if TNodeParams(TTreeView(TreeView).Selected.Data^).type_=ndObject
  then if (DSObject.Locate('id',TNodeParams(TTreeView(TreeView).Selected.Data^).id,[]))and(DSObject.fieldByName('GoHim').AsInteger=-1)
        then FMain.NGoHim.Checked:=true
        else FMain.NGoHim.Checked:=false
  else FMain.NGoHim.Visible:=false;
end;

procedure TNetRadar.SetGoHim(Sender: TObject);
begin
 if TNodeParams(TTreeView(TreeView).Selected.Data^).type_=ndObject then
  begin
   if DSObject.Locate('GoHim',-1,[]) then
    begin
     DSObject.Edit;
     DSObject.fieldByName('GoHim').AsInteger:=0;
     DSObject.ApplyRange;
     DSObject.ApplyUpdates(0);
    end;
   if not(TMenuItem(sender).Checked) then
   if DSObject.Locate('id',TNodeParams(TTreeView(TreeView).Selected.Data^).id,[]) then
    begin
     DSObject.Edit;
     DSObject.fieldByName('GoHim').AsInteger:=-1;
     DSObject.ApplyRange;
     DSObject.ApplyUpdates(0);
    end;
  end;
end;


procedure TNetRadar.TreeView1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var MH:THitTests;
    Node:TTreeNode;
    i:integer;
begin
 MH:= TTreeView(TreeView).GetHitTestInfoAt(X,Y);

 if (htOnStateIcon in MH)
  then
   begin
    Node:= TTreeView(TreeView).GetNodeAt(X,Y);
    if(Node.StateIndex <> 2)
     then Node.StateIndex := 2
     else Node.StateIndex := 1;
    if TNodeParams(node.Data^).type_=ndObject then
     begin
      DSObject.Locate('id',TNodeParams(node.Data^).id,[]);
      DSObject.Edit;
      if Node.StateIndex=2 then DSObject.FieldByName('visible').AsInteger:=-1
                           else DSObject.FieldByName('visible').AsInteger:=0;
      DSObject.ApplyRange;
      DSObject.ApplyUpdates(0);
     end;
    if TNodeParams(node.Data^).type_=ndTrackList then
     begin
      DSObject.Locate('id',TNodeParams(node.Data^).id,[]);
      DSObject.Edit;
      if Node.StateIndex=2 then DSObject.FieldByName('trvisible').AsInteger:=-1
                           else DSObject.FieldByName('trvisible').AsInteger:=0;
      DSObject.ApplyRange;
      DSObject.ApplyUpdates(0);
     end;
    Synchronize(DrawLayer);
   end;
 if (htOnLabel in MH) then
   begin
    Node:= TTreeView(TreeView).GetNodeAt(X,Y);
    if TNodeParams(node.Data^).type_=ndObject then
     begin
      DSObject.Locate('id',TNodeParams(node.Data^).id,[]);
      MemoObjectInfo.Text:=DSObject.fieldByname('info').AsString;
     end;
   end;
end;

function HexToInt(HexStr : string) : Int64;
var RetVar : Int64;
    i : byte;
begin
  HexStr := UpperCase(HexStr);
  if HexStr[length(HexStr)] = 'H' then Delete(HexStr,length(HexStr),1);
  if HexStr[1] = '#' then Delete(HexStr,1,1);
  RetVar := 0;
  for i := 1 to length(HexStr) do
   begin
    RetVar := RetVar shl 4;
    if HexStr[i] in ['0'..'9'] then RetVar := RetVar + (byte(HexStr[i]) - 48)
     else if HexStr[i] in ['A'..'F'] then RetVar := RetVar + (byte(HexStr[i]) - 55)
                                     else begin
                                           Retvar := 0;
                                           break;
                                          end;
   end;
  Result := RetVar;
end;

destructor TNetRadar.destroy;
begin
 LayerMap.Free;
 inherited ;
end;

constructor TNetRadar.create(CrSusp:Boolean;ATreeView:Pointer; AMemoObjectInfo:TMemo);
begin
 new:=true;

 IdHTTP1:=TIdHTTP.Create(nil);
 if not(InetConnect.userwinset) then
  if InetConnect.proxyused then
   try
    IdHTTP1.ProxyParams.ProxyServer:=copy(InetConnect.proxystr,1,PosEx(':',InetConnect.proxystr)-1);
    IdHTTP1.ProxyParams.ProxyPort:=strtoint(copy(InetConnect.proxystr,PosEx(':',InetConnect.proxystr)+1,length(InetConnect.proxystr)-PosEx(':',InetConnect.proxystr)));
    if InetConnect.uselogin then
     begin
      IdHTTP1.ProxyParams.ProxyUsername:=InetConnect.loginstr;
      IdHTTP1.ProxyParams.ProxyPassword:=InetConnect.passstr;
     end;
   except
    ShowMessage(SAS_ERR_ProxyStrFormat);
   end;

 LayerMap:=TBitmapLayer.Create(FMain.map.Layers);
 LayerMap.bitmap.DrawMode:=dmBlend;
 LayerMap.Bitmap.CombineMode:=cmMerge;
 LayerMap.bitmap.Font.Charset:=RUSSIAN_CHARSET;
 LayerMap.Bitmap.Font.Name:='Tahoma';
 LayerMap.Bitmap.Font.Style:=[];
 LayerMap.Bitmap.Width:=xhgpx;
 LayerMap.Bitmap.Height:=yhgpx;
 LayerMap.Location:=Unit1.LayerMap.Location;

 Icons:=TStringList.Create;
 imahelist:=TImageList.Create(nil);
 imahelist.Width:=16;
 imahelist.Height:=16;
 TreeView:=ATreeView;
 TTreeView(TreeView).Images:=imahelist;
 TTreeView(TreeView).OnMouseDown:=TreeView1MouseDown;
 FMain.NGoHim.OnClick:=SetGoHim;
 FMain.PMNRObject.OnPopup:=PopUpGoHim;
 MemoObjectInfo:=AMemoObjectInfo;
 SetWindowLong(TTreeView(TreeView).Handle,GWL_STYLE,GetWindowLong(TTreeView(TreeView).Handle,GWL_STYLE) or TVS_CHECKBOXES);
 sqlite3_initialize;
 db:=TDISQLite3Database.Create(nil);
 db.DatabaseName:=ExtractFilePath(ParamStr(0))+'NetRadar.sqlitedb';
 if not(FileExists(db.DatabaseName))
  then
   begin
    db.CreateDatabase;
    db.Execute('CREATE TABLE category('+
                              '"Name" TEXT,' + #13#10 +
                              '"visible" INTEGER,' + #13#10 +
                              '"ShowAfterScale" INTEGER,' + #13#10 +
                              '"ShowBeforeScale" INTEGER)');
    db.Execute('CREATE TABLE object('+
                              '"id" FLOAT,' + #13#10 +
                              '"CategoryID" FLOAT,' + #13#10 +
                              '"Name" TEXT,' + #13#10 +
                              '"visible" FLOAT,' + #13#10 +
                              '"trvisible" FLOAT,' + #13#10 +
                              '"trcl" FLOAT,' + #13#10 +
                              '"clr" FLOAT,' + #13#10 +
                              '"dttm" TEXT,' + #13#10 +
                              '"info" BLOB,' + #13#10 +
                              '"ut" TEXT,' + #13#10 +
                              '"lat" FLOAT,' + #13#10 +
                              '"lon" FLOAT,' + #13#10 +
                              '"spd" FLOAT,' + #13#10 +
                              '"crs" FLOAT,' + #13#10 +
                              '"stoptime" FLOAT,' + #13#10 +
                              '"img" BLOB,' + #13#10 +
                              '"GoHim" FLOAT,' + #13#10 +
                              '"ip" TEXT)');
    db.Execute('CREATE TABLE track('+
                              '"ID" INTEGER PRIMARY KEY,' + #13#10 +
                              '"ObjectID" FLOAT,' + #13#10 +
                              '"visible" FLOAT,' + #13#10 +
                              '"LatT" FLOAT,' + #13#10 +
                              '"LatB" FLOAT,' + #13#10 +
                              '"LonL" FLOAT,' + #13#10 +
                              '"LonR" FLOAT)');
    db.Execute('CREATE TABLE trpoint('+
                              '"TrackID" FLOAT,' + #13#10 +
                              '"lat" FLOAT,' + #13#10 +
                              '"lon" FLOAT)');
    db.Execute('CREATE TABLE stopover('+
                              '"ID" FLOAT PRIMARY KEY,' + #13#10 +
                              '"ObjectID" FLOAT,' + #13#10 +
                              '"info" TEXT,' + #13#10 +
                              '"lat" FLOAT,' + #13#10 +
                              '"lon" FLOAT,' + #13#10 +
                              '"visible" FLOAT)');
   end
  else
   db.Open;
 UDObject:=TDISQLite3UniDirQuery.Create(nil);
 UDObject.Database:=db;
 UDObject.SelectSQL:='SELECT * FROM object';
 UDCategory:=TDISQLite3UniDirQuery.Create(nil);
 UDCategory.Database:=db;
 UDCategory.SelectSQL:='SELECT Name,TRIM(visible) as visible,TRIM(ShowAfterScale) as ShowAfterScale,TRIM(ShowBeforeScale) as ShowBeforeScale FROM Category';
 UDCategory.InsertSQL:='insert into Category (Name,visible,ShowAfterScale,ShowBeforeScale) values (:Name,:visible,:ShowAfterScale,:ShowBeforeScale)';
 UDTrack:=TDISQLite3UniDirQuery.Create(nil);
 UDTrack.Database:=db;
 UDTrack.SelectSQL:='SELECT TRIM(ID) as ID,ObjectID,visible,LatT,LatB,LonL,LonR FROM track';
 UDTrack.InsertSQL:='insert into track (ObjectID,visible,LatT,LatB,LonL,LonR) values (:ObjectID,:visible,:LatT,:LatB,:LonL,:LonR)';
 UDTrack.ModifySQL:='update track set ObjectID=:ObjectID,visible=:visible,LatT=:LatT,LatB=:LatB,LonL=:LonL,LonR=:LonR';
 UDTrackID:=TDISQLite3UniDirQuery.Create(nil);
 UDTrackID.Database:=db;
 UDTrackID.SelectSQL:='SELECT ID,ObjectID FROM track';
 UDTrPoint:=TDISQLite3UniDirQuery.Create(nil);
 UDTrPoint.Database:=db;
 UDTrPoint.SelectSQL:='SELECT TrackID,lat,lon FROM trpoint';
 UDStopover:=TDISQLite3UniDirQuery.Create(nil);
 UDStopover.Database:=db;
 UDStopover.SelectSQL:='SELECT * FROM Stopover;';

 DSPObject:=TDataSetProvider.Create(nil);
 DSPObject.DataSet:=UDObject;
 DSPCategory:=TDataSetProvider.Create(nil);
 DSPCategory.DataSet:=UDCategory;
 DSPTrack:=TDataSetProvider.Create(nil);
 DSPTrack.DataSet:=UDTrack;
 DSPTrackID:=TDataSetProvider.Create(nil);
 DSPTrackID.DataSet:=UDTrackID;
 DSPTrPoint:=TDataSetProvider.Create(nil);
 DSPTrPoint.DataSet:=UDTrPoint;
 DSPstopover:=TDataSetProvider.Create(nil);
 DSPstopover.DataSet:=UDStopover;

 DSObject:=TClientDataSet.Create(nil);
 DSObject.SetProvider(DSPObject);
 DSObject.Open;
 DSCategory:=TClientDataSet.Create(nil);
 DSCategory.SetProvider(DSPCategory);
 DSCategory.Open;
 DSTrack:=TClientDataSet.Create(nil);
 DSTrack.SetProvider(DSPTrack);
 DSTrack.Open;
 DSTrackID:=TClientDataSet.Create(nil);
 DSTrackID.SetProvider(DSPTrackID);
 DSTrackID.Open;
 DSTrPoint:=TClientDataSet.Create(nil);
 DSTrPoint.SetProvider(DSPTrPoint);
 DSTrPoint.Open;
 DSstopover:=TClientDataSet.Create(nil);
 DSstopover.SetProvider(DSPstopover);
 DSstopover.Open;
 inherited Create(CrSusp);
end;

procedure TNetRadar.drawPath(pathll:array of TExtendedPoint;color1:TColor32;linew:integer);
var i,adp,j:integer;
    k1,k2,k4:TPoint;
    k3:TextendedPoint;
    polygon: TPolygon32;
begin
 try
 polygon:=TPolygon32.Create;
 polygon.Antialiased:=true;
 polygon.AntialiasMode:=am4times;
 polygon.Closed:=false;
 if length(pathll)>0 then
 begin
  for i:=0 to length(pathll)-1 do
   begin
    k1:=point(FMain.Lon2X(pathll[i].x)+(pr_x-mWd2),FMain.Lat2Y(pathll[i].y)+(pr_y-mHd2));
    if (k1.x<32767)and(k1.x>-32767)and(k1.y<32767)and(k1.y>-32767) then
      polygon.Add(FixedPoint(k1));
    if i<length(pathll)-1 then
     begin
      k2:=point(FMain.Lon2X(pathll[i+1].x)+(pr_x-mWd2),FMain.Lat2Y(pathll[i+1].y)+(pr_y-mHd2));
      if (k2.x-k1.x)>(k2.y-k1.y) then adp:=(k2.x-k1.x)div 32767+2
                                 else adp:=(k2.y-k1.y)div 32767+2;
      k3:=extPoint(((k2.X-k1.x)/adp),((k2.y-k1.y)/adp));
      if adp>2 then
       for j:=1 to adp-1 do
        begin
         k4:=Point(round(k1.x+k3.x*j),round(k1.Y+k3.y*j));
         if(k4.x<32767)and(k4.x>-32767)and(k4.y<32767)and(k4.y>-32767)then polygon.Add(FixedPoint(k4.x,k4.y));
        end;
     end;
   end;
  with Polygon.Outline do
   begin
    with Grow(Fixed(linew / 2), 0.5) do
     begin
      FillMode := pfWinding;
      DrawFill(LayerMap.Bitmap, Color32(color1));
      free;
     end;
    free;
   end;
 end;
 polygon.Free;
 except
 end;
end;

procedure TNetRadar.DrawLayer;
var lon_l,lon_r,lat_t,lat_d:real;
    i,lenTrArr:integer;
    xy:Tpoint;
    btm:TBitmap32;
    png:TPNGObject;
    TestArrLenP1,TestArrLenP2:TPoint;
    buf_line_arr:array of TExtendedPoint;
    ms:TMemoryStream;
    indexmi:integer;
    imw,texth:integer;
    marksFilter:string;
begin
 lon_l:=Fmain.X2Lon(-(pr_x-mWd2));
 lon_r:=Fmain.X2Lon(pr_x+mWd2);
 lat_t:=Fmain.Y2Lat(-(pr_y-mHd2));
 lat_d:=Fmain.Y2Lat(pr_y+mHd2);
 marksFilter:='';
 if show_point=2 then
  begin
   DSCategory.Filter:='visible = 1 and ( ShowAfterScale <= '+inttostr(zoom_size)+' and ShowBeforeScale >= '+inttostr(zoom_size)+' )';
   DSCategory.Filtered:=true;
   marksFilter:=marksFilter+'visible=-1';
   DSCategory.First;
   if not(DSCategory.Eof) then
    begin
     marksFilter:=marksFilter+' and (';
     while not(DSCategory.Eof) do
      begin
       marksFilter:=marksFilter+'categoryid='+DSCategory.fieldbyname('id').AsString;
       DSCategory.Next;
       if not(DSCategory.Eof) then marksFilter:=marksFilter+' or ';
      end;
     marksFilter:=' or categoryid=0'+marksFilter+')';
    end;
   DSCategory.Filtered:=false;
  end;
 DSObject.Filter:=marksFilter;
 DSObject.Filtered:=true;
 DSObject.First;
 if DSObject.Eof then begin
                       LayerMap.Visible:=false;
                       DSObject.Filtered:=false;
                       exit;
                      end
                 else begin
                       LayerMap.Bitmap.Clear(clBlack);
                       LayerMap.Location:=floatrect(bounds(mWd2-pr_x,mHd2-pr_y,xhgpx,yhgpx));
                       LayerMap.Visible:=true;
                      end;
 btm:=TBitmap32.Create;
 btm.DrawMode:=dmBlend;
 btm.Resampler:=TLinearResampler.Create;
 While not(DSObject.Eof) do
  begin
    if DSObject.FieldByName('trvisible').AsInteger=-1 then
    begin
     DSTrack.Filter:='(ObjectID='+DSObject.fieldByname('ID').AsString+') and (visible=-1)';
     DSTrack.Filtered:=true;
     While not(DSTrack.Eof) do
      begin
       DSTrPoint.Filter:='TrackID='+DSTrack.fieldByname('ID').AsString;
       DSTrPoint.Filtered:=true;
       While not(DSTrPoint.Eof) do
        begin
          SetLength(buf_line_arr,length(buf_line_arr)+1);
          lenTrArr:=length(buf_line_arr);
          buf_line_arr[lenTrArr-1].X:=DSTrPoint.fieldByName('lon').AsFloat;
          buf_line_arr[lenTrArr-1].Y:=DSTrPoint.fieldByName('lat').AsFloat;
          DSTrPoint.Next;
        end;
       drawPath(buf_line_arr,TColor(DSObject.fieldByname('trcl').AsInteger),3);
       SetLength(buf_line_arr,0);
       DSTrPoint.Filtered:=false;
       DSTrack.Next;
      end;
     DSTrack.Filtered:=false;
    end;

    xy:=Point(FMain.Lon2X(DSObject.fieldByname('Lon').asFloat)+(pr_x-mWd2),FMain.lat2Y(DSObject.fieldByname('Lat').asFloat)+(pr_y-mHd2));
    png:=TPNGObject.Create;
    ms:=TMemoryStream.Create;
    TBlobField(DSObject.fieldByname('img')).SaveToStream(ms);
    ms.Position:=0;
    png.LoadFromStream(ms);
    ms.Free;
    imw:=png.Width;
    PNGintoBitmap32(btm,png);
    LayerMap.Bitmap.Draw(bounds(xy.x-(png.Width div 2),xy.y-(png.Height div 2),png.Width,png.Height),bounds(0,0,btm.Width,btm.Height),btm);

    DSObject.Next;
  end;
 DSObject.Filtered:=false;
 btm.Free;
end;

procedure TNetRadar.DrawGoHim;
begin
 if DSObject.Locate('GoHim',-1,[]) then
  Fmain.topos(DSObject.FieldByName('lat').AsFloat,DSObject.FieldByName('lon').AsFloat,zoom_size,false);
end;

procedure TNetRadar.DrawTree;
var i: Integer;
    next:boolean;
    nodeB,nodeBTSP,nodeTr:TTreeNode;
    png:TPNGObject;
    ms:TMemoryStream;
    btm:TBitmap;
    Bitmap,Bitmap2: TBitmap32;
begin
 DSObject.First;
 imahelist.Clear;
 while not(DSObject.Eof) do
  begin
   next:=false;
   ms:=TMemoryStream.Create;
   TBlobField(DSObject.fieldByName('img')).SaveToStream(ms);
   ms.Position:=0;
   png:=TPNGObject.Create;
   png.LoadFromStream(ms);
   ms.Free;

   Bitmap:=TBitmap32.Create;
   Bitmap2:=TBitmap32.Create;
   Bitmap.SetSize(png.Width,png.Height);
   Bitmap2.SetSize(16,16);
   Bitmap.Clear(clWhite32);
   Bitmap.Assign(png);
   Bitmap.Resampler:=TKernelResampler.Create;
   TKernelResampler(Bitmap.Resampler).Kernel:=TCubicKernel.Create;
   Bitmap2.Draw(Bounds(0, 0, 16,16),Bounds(0, 0, Bitmap.Width,Bitmap.Height),Bitmap);
   if DSObject.fieldByName('GoHim').AsInteger=-1 then
    begin
     Bitmap2.LineS(12,9,12,15,clRed32);
     Bitmap2.LineS(12,15,15,12,clRed32);
     Bitmap2.LineS(15,12,12,9,clRed32);
    end;
   btm:=TBitmap.Create;
   btm.Assign(Bitmap2);
  imahelist.AddMasked(btm,clBlack);
   Bitmap2.Free;
   Bitmap.Free;
   btm.Free;
   png.Free;
   For i:=0 to TTreeView(TreeView).Items.Count-1 do
    if(TNodeParams(TTreeView(TreeView).Items[i].Data^).type_=ndObject)and(TNodeParams(TTreeView(TreeView).Items[i].Data^).id=DSObject.fieldByName('id').asInteger) then
      begin
       next:=true;
       DSObject.Next;
       break;
      end;
   if next then Continue;

   nodeB:=TTreeView(TreeView).items.Add(nil,DSObject.fieldByName('name').asString);
   nodeB.Data:=GetMemory(sizeOf(TNodeParams));
   nodeB.ImageIndex:=imahelist.Count-1;
   nodeB.SelectedIndex:=imahelist.Count-1;
   TNodeParams(nodeB.Data^).id:=DSObject.fieldByName('id').asInteger;
   TNodeParams(nodeB.Data^).type_:=ndObject;
   if DSObject.fieldByName('visible').asInteger=0 then nodeB.StateIndex:=1
                                                  else nodeB.StateIndex:=2;
   nodeBTSP:=TTreeView(TreeView).items.AddChild(nodeB,'Текущая позиция');
   nodeBTSP.ImageIndex:=-1;
   nodeBTSP.SelectedIndex:=-1;
   nodeBTSP.Data:=GetMemory(sizeOf(TNodeParams));
   TNodeParams(nodeBTSP.Data^).id:=DSObject.fieldByName('id').asInteger;
   TNodeParams(nodeBTSP.Data^).type_:=ndCurrent;
   nodeBTSP.StateIndex:=2;

   DSTrack.Filter:='ObjectId='+DSObject.fieldByName('id').AsString;
   DSTrack.Filtered:=true;
   i:=1;
   nodeTr:=TTreeView(TreeView).items.AddChild(nodeB,'Треки');
   nodeTr.ImageIndex:=-1;
   nodeTr.SelectedIndex:=-1;
   if DSObject.fieldByName('trvisible').asInteger=0 then nodeTr.StateIndex:=1
                                                    else nodeTr.StateIndex:=2;
   nodeTr.Data:=GetMemory(4);
   TNodeParams(nodeTr.Data^).id:=DSObject.fieldByName('id').asInteger;
   TNodeParams(nodeTr.Data^).type_:=ndTrackList;
   while not(DSTrack.Eof) do
    begin
     nodeBTSP:=TTreeView(TreeView).items.AddChild(nodeTr,'Трек'+inttostr(i));
     nodeBTSP.ImageIndex:=-1;
     nodeBTSP.SelectedIndex:=-1;
     nodeBTSP.Data:=GetMemory(4);
     TNodeParams(nodeBTSP.Data^).id:=DSTrack.fieldByName('id').asInteger;
     TNodeParams(nodeBTSP.Data^).type_:=ndTrack;
     if DSTrack.fieldByName('visible').asInteger=0 then nodeBTSP.StateIndex:=1
                                                   else nodeBTSP.StateIndex:=2;
     inc(i);
     DSTrack.Next;
    end;
   DSTrack.Filtered:=false;
   nodeBTSP:=TTreeView(TreeView).items.AddChild(nodeB,'Остановки');
   nodeBTSP.ImageIndex:=-1;
   nodeBTSP.SelectedIndex:=-1;
   nodeBTSP.Data:=GetMemory(4);
   TNodeParams(nodeBTSP.Data^).id:=DSObject.fieldByName('id').asInteger;
   TNodeParams(nodeBTSP.Data^).type_:=ndstopover;
   nodeBTSP.StateIndex:=2;
   DSObject.Next
  end;

end;


procedure TNetRadar.UpdateDB;
var i,j:integer;
    l:string;
    ms:TMemoryStream;
begin
   for i:=1 to params[5].Count-1 do
    begin
     if not(DSObject.Locate('id',strtoint(params[5][i]),[]))
      then begin
            DSObject.Insert;
            DSObject.FieldByName('id').AsInteger:=strtoint(params[5][i]);
            DSObject.FieldByName('CategoryId').AsInteger:=0;
            DSObject.FieldByName('visible').AsInteger:=-1;
           end
      else begin
            DSObject.Edit;
           end;
     if (DSObject.State=dsInsert)or(new) then
      begin
       DSObject.FieldByName('GoHim').AsInteger:=0;
       DSObject.FieldByName('Trvisible').AsInteger:=-1;

       DSTrack.Insert;
       DSTrack.FieldByName('ObjectID').AsInteger:=DSObject.FieldByName('id').AsInteger;
       DSTrack.FieldByName('visible').AsInteger:=-1;
       DSTrack.ApplyUpdates(-1);
      end;
     DSObject.FieldByName('name').AsString:=Utf8ToAnsi(params[6][i]);
     DSObject.FieldByName('ut').AsString:=params[7][i];
     DSObject.FieldByName('ip').AsString:=params[13][i];
     DSObject.FieldByName('dttm').AsString:=params[3][i];
     DSObject.FieldByName('info').AsString:=Utf8ToAnsi(params[4][i]);
     DSObject.FieldByName('spd').AsFloat:=Fmain.Str2r(params[10][i]);
     DSObject.FieldByName('trcl').AsInteger:=hextoint(params[1][i]);
     DSObject.FieldByName('clr').AsInteger:=hextoint(params[2][i]);
     DSObject.FieldByName('crs').AsInteger:=strtoint(params[11][i]);
     DSObject.FieldByName('lat').AsFloat:=Fmain.Str2r(params[8][i]);
     DSObject.FieldByName('lon').AsFloat:=Fmain.Str2r(params[9][i]);

     l:=params[12][i];
     j:=Icons.IndexOf(l);
     if j>-1 then TBlobField(DSObject.FieldByName('img')).LoadFromStream(TMemoryStream(Icons.Objects[j]));

     //if DSObject.FieldByName('spd').AsFloat>0 then
      begin
       DSTrack.Filter:='ObjectID='+DSObject.FieldByName('id').AsString;
       DSTrack.Filtered:=true;
       DSTrack.Last;
       DSTrPoint.Filter:='TrackId='+DSTrack.FieldByName('id').AsString;
       DSTrPoint.Filtered:=true;
       DSTrPoint.Last;
       if DSTrPoint.Eof then
        begin
         DSTrack.Open;
         DSTrack.Edit;
         DSTrack.FieldByName('LonL').AsFloat:=Fmain.Str2r(params[9][i]);
         DSTrack.FieldByName('LonR').AsFloat:=Fmain.Str2r(params[9][i]);
         DSTrack.FieldByName('LatT').AsFloat:=Fmain.Str2r(params[8][i]);
         DSTrack.FieldByName('LatB').AsFloat:=Fmain.Str2r(params[8][i]);
         DSTrack.ApplyRange;
         DSTrack.ApplyUpdates(0);
         DSTrack.Refresh;
        end else
        begin
         DSTrack.Edit;
         if DSTrack.FieldByName('LonL').AsFloat>Fmain.Str2r(params[9][i]) then
            DSTrack.FieldByName('LonL').AsFloat:=Fmain.Str2r(params[9][i]);
         if DSTrack.FieldByName('LonR').AsFloat<Fmain.Str2r(params[9][i]) then
            DSTrack.FieldByName('LonR').AsFloat:=Fmain.Str2r(params[9][i]);
         if DSTrack.FieldByName('LatT').AsFloat<Fmain.Str2r(params[8][i]) then
            DSTrack.FieldByName('LatT').AsFloat:=Fmain.Str2r(params[8][i]);
         if DSTrack.FieldByName('LatB').AsFloat>Fmain.Str2r(params[8][i]) then
            DSTrack.FieldByName('LatB').AsFloat:=Fmain.Str2r(params[8][i]);
         DSTrack.ApplyRange;
         DSTrack.ApplyUpdates(0);
         DSTrack.Refresh;
        end;
       if ((DSTrPoint.FieldByName('lon').AsFloat<>Fmain.Str2r(params[9][i]))or
          (DSTrPoint.FieldByName('lat').AsFloat<>Fmain.Str2r(params[8][i]))) then
         begin
           DSTrPoint.Insert;
           DSTrPoint.FieldByName('TrackId').AsInteger:=DSTrack.FieldByName('id').AsInteger;
           DSTrPoint.FieldByName('lon').AsFloat:=Fmain.Str2r(params[9][i]);
           DSTrPoint.FieldByName('lat').AsFloat:=Fmain.Str2r(params[8][i]);
           DSTrPoint.ApplyUpdates(-1);
           DSTrPoint.Refresh;
         end;
       DSTrPoint.Filtered:=false;
       DSTrack.Filtered:=false;
      end;

     DSObject.ApplyUpdates(-1);
     DSObject.Refresh;
    end;
 new:=false;
end;

procedure TNetRadar.Execute;//GetFromServer;
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
      StrLen:=StrLen+length(Smb)-1;
      SWord := Copy(Str, 1, StrLen - length(Smb));
      Delete(Str, 1, StrLen);
      Inc(N);
    end
    else SWord := Str;
  end;
  if WordNmbr <= N then Result := SWord
                   else Result := '_';
end;


procedure SetMyCookies;
var i:integer;
    temp:TStringList;
begin
Temp:=TStringList.Create;
IdHTTP1.Response.RawHeaders.Extract('Set-cookie', Temp);
for i:=0 to Temp.Count-1 do
begin
	Temp.Strings[i]:=Temp.Strings[i]+' path=/';
	IdHTTP1.CookieManager.AddCookie(Temp.Strings[i], IdHTTP1.Request.Host);
end;
Temp.free;
end;

var ms,mspic:TMemoryStream;
    ErrCode,p1,p2,i,j,k:integer;
    req,paramss,s,l,URL:string;
    NRCookie,poststr,uid,login,passw:String;
    NRCookieSize:cardinal;
    Stmt: TDISQLite3Statement;
    post:TMemoryStream;
    CM    : TidCookieManager;
begin

 IdHTTP1.AllowCookies:=true;
 poststr:= 'username=test&userpass=test&userlogin='+URLEncode(AnsiToUtf8('Вход'));
 post:=TMemoryStream.Create;
 post.Write(poststr[1],length(poststr));
 IdHTTP1.Request.Accept:='image/gif, image/jpeg, image/pjpeg, image/pjpeg, application/x-shockwave-flash, application/vnd.ms-excel, application/msword, */*';
 IdHTTP1.Request.ContentType:='application/x-www-form-urlencoded';
 IdHTTP1.Request.AcceptCharSet:='windows-1251, cp-1251';
 try
 NRCookie:=IdHTTP1.Post('http://netradar.ru/index.php',post);
 except
 end;
 if IdHTTP1.ResponseCode<>302 then exit;
 uid:=IdHTTP1.CookieManager.CookieCollection.Cookie['PHPSESSID','netradar.ru'].Value;
 post.Free;
 repeat
 ms:=TMemoryStream.Create;
 URL:='http://netradar.ru/nr_googlemap_u_aja.php?PHPSESSID='+PChar(uid)+'&JsHttpRequest=12507670064882-xml';
 req:=IdHTTP1.Get(URL);
 if length(req)>0 then
  begin
   for j:=1 to 13 do
    begin
     params[j]:=TStringList.Create;
     p1:=PosEx(ParamsName[j],req,0);
     if p1>0 then
      begin
       p1:=PosEx('[ "',req,p1)+3;
       p2:=PosEx('" ]',req,p1);
       paramss:=copy(req,p1,p2-p1);
       i:=0;
       params[j].Add(GetWord(paramss,'", "',i));
       while params[j][i]<>'_' do
        begin
         inc(i);
         params[j].Add(GetWord(paramss,'", "',i));
        end;
       params[j].Delete(params[j].Count-1);
      end;
    end;

  for i:=1 to params[12].Count-1 do
   begin
    l:=params[12][i];
    k:=Icons.IndexOf(l);
    if k<0
     then begin
           While Pos('\',l)>0 do Delete(l,Pos('\',l),1);
           mspic:=TMemoryStream.Create;
           try
            IdHTTP1.Get('http://netradar.ru/'+l,mspic);
            mspic.Position:=0;
           //GetStreamFromURL(mspic,'http://netradar.ru/'+l,'image/png');
            Icons.AddObject(params[12][i],mspic);
           except
           end;
          end;
   end;
  Synchronize(UpdateDB);
  end
 else
  begin
  // if not(Terminated) then Synchronize(ShowError);
  end;
 ms.Free;
 Synchronize(DrawTree);
 Synchronize(DrawLayer);
 Synchronize(DrawGoHim);
 Sleep(5000);
 until terminated;
end;

end.
