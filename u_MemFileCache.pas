unit u_MemFileCache;

interface

uses
  SysUtils,
  Classes,
  GR32;

type
  TMemFileCache = class
  private
    FCacheElemensMaxCnt: integer;
    FCacheList:TStringList;
    FSync: TMultiReadExclusiveWriteSynchronizer;
    function GetFileSize(namefile: string): Integer;
    function LoadJPG32(FileName : string;Btm:TBitmap32):boolean;
    procedure SetCacheElemensMaxCnt(const Value: integer);
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear;
    function LoadFile(btm:Tobject;path:string;caching:boolean):boolean;
    procedure DeleteFileFromCache(path:string);
    procedure AddTileToCache(btm:Tobject; APath:string);
    function TryLoadFileFromCache(btm:Tobject; APath:string):boolean;

    property CacheElemensMaxCnt: integer read FCacheElemensMaxCnt write SetCacheElemensMaxCnt;
  end;

implementation

uses
  Graphics,
  pngimage,
  IJL,
  jpeg;

{ TMemFileCache }

constructor TMemFileCache.Create;
begin
  FCacheElemensMaxCnt := 100;
  FCacheList := TStringList.Create;
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TMemFileCache.Destroy;
begin
  Clear;
  FreeAndNil(FSync);
  FreeAndNil(FCacheList);
  inherited;
end;

procedure TMemFileCache.Clear;
var
  i: integer;
begin
  FSync.BeginWrite;
  try
    for i:=0 to FCacheList.Count-1 do
      TBitmap32(FCacheList.Objects[i]).Free;
    FCacheList.Clear;
  finally
    FSync.EndWrite;
  end;
end;

function TMemFileCache.LoadFile(btm: Tobject; path: string;
  caching: boolean): boolean;
begin
  result:=false;
  if GetFileSize(path)=0 then begin
    exit;
  end;
  try
    if (btm is TBitmap32) then begin
      if not(caching) then begin
        if ExtractFileExt(path)='.jpg' then begin
          if not(LoadJPG32(path,TBitmap32(btm))) then begin
            result:=false;
            exit;
          end;
        end else begin
          TBitmap32(btm).LoadFromFile(path);
        end;
        result:=true;
      end else begin
        if not TryLoadFileFromCache(btm, path) then begin
          if ExtractFileExt(path)='.jpg' then begin
            if not(LoadJPG32(path,TBitmap32(btm))) then begin
              result:=false;
              exit;
            end
          end else begin
            TBitmap32(btm).LoadFromFile(path);
          end;
          AddTileToCache(btm, path);
        end;
      end;
    end else begin
      if (btm is TGraphic) then
        TGraphic(btm).LoadFromFile(path)
      else if (btm is TPicture) then
        TPicture(btm).LoadFromFile(path)
      else if (btm is TJPEGimage) then
        TJPEGimage(btm).LoadFromFile(path)
      else if (btm is TPNGObject) then
        TPNGObject(btm).LoadFromFile(path);
    end;
    result:=true;
  except
  end;
end;

function TMemFileCache.GetFileSize(namefile: string): Integer;
var
  InfoFile: TSearchRec;
begin
  if FindFirst(namefile, faAnyFile, InfoFile) <> 0 then begin
    Result := -1;
  end else begin
    Result := InfoFile.Size;
  end;
  SysUtils.FindClose(InfoFile);
end;

function TMemFileCache.LoadJPG32(FileName: string; Btm: TBitmap32): boolean;
  procedure RGBA2BGRA2(pData : Pointer; Width, Height : Integer);
  var W, H : Integer;
      p : PInteger;
  begin
    p := PInteger(pData);
    for H := 0 to Height-1 do
    begin
      for W := 0 to Width-1 do
      begin
        p^:= (byte(p^ shr 24) shl 24) or byte(p^ shr 16) or
            (integer(byte(p^ shr 8)) shl 8) or (integer(byte(p^)) shl 16);
        Inc(p);
      end;
    end;
  end;
const
  sRead : array [Boolean] of String = ('JFILE_READ = ','JBUFF_READ = ');
var
  iWidth, iHeight, iNChannels : Integer;
  iStatus : Integer;
  jcprops : TJPEG_CORE_PROPERTIES;
begin
 try
    result:=true;
    iStatus := ijlInit(@jcprops);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
    jcprops.JPGFile := PChar(FileName);
    iStatus := ijlRead(@jcprops,IJL_JFILE_READPARAMS);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
    iWidth := jcprops.JPGWidth;
    iHeight := jcprops.JPGHeight;
    iNChannels := 4;
    Btm.SetSize(iWidth,iHeight);
    jcprops.DIBWidth := iWidth;
    jcprops.DIBHeight := iHeight;
    jcprops.DIBChannels := iNChannels;
    jcprops.DIBColor := IJL_RGBA_FPX;
    jcprops.DIBPadBytes := ((((iWidth*iNChannels)+3) div 4)*4)-(iWidth*iNChannels);
    jcprops.DIBBytes := PByte(Btm.Bits);// PByte(DIB.dsBm.bmBits);
    if (jcprops.JPGChannels = 3) then
      jcprops.JPGColor := IJL_YCBCR
    else if (jcprops.JPGChannels = 4) then
      jcprops.JPGColor := IJL_YCBCRA_FPX
    else if (jcprops.JPGChannels = 1) then
      jcprops.JPGColor := IJL_G
    else
    begin
      jcprops.DIBColor := TIJL_COLOR (IJL_OTHER);
      jcprops.JPGColor := TIJL_COLOR (IJL_OTHER);
    end;
    iStatus := ijlRead(@jcprops,IJL_JFILE_READWHOLEIMAGE);
    if iStatus < 0 then
     begin
      result:=false;
      exit;
     end;
    if jcprops.DIBColor = IJL_RGBA_FPX then
      RGBA2BGRA2(jcprops.DIBBytes,iWidth,iHeight);
    ijlFree(@jcprops);
  except
    on E: Exception do
    begin
      result:=false;
      ijlFree(@jcprops);
    end;
  end;
end;

procedure TMemFileCache.SetCacheElemensMaxCnt(const Value: integer);
begin
  FCacheElemensMaxCnt := Value;
end;

procedure TMemFileCache.DeleteFileFromCache(path: string);
var
  i: Integer;
begin
  if FSync.BeginWrite then begin
    try
      i := FCacheList.IndexOf(AnsiUpperCase(Path));
      if i >= 0 then begin
        FCacheList.Objects[i].Free;
        FCacheList.Delete(i);
      end;
    finally
      FSync.EndWrite;
    end;
  end;

end;

procedure TMemFileCache.AddTileToCache(btm: Tobject; APath: string);
var
  btmcache:TObject;
  i:integer;
  VPath: string;
begin
  VPath := AnsiUpperCase(APath);
  btmcache:=TBitmap32.Create;
  TBitmap32(btmcache).Assign(TBitmap32(btm));
  if FSync.BeginWrite then begin
    try
      i:=FCacheList.IndexOf(VPath);
      if i < 0 then begin
        FCacheList.AddObject(VPath, btmcache);
        if FCacheList.Count > FCacheElemensMaxCnt then begin
          FCacheList.Objects[0].Free;
          FCacheList.Delete(0);
        end;
      end else begin
        FreeAndNil(btmcache);
      end;
    finally
      FSync.EndWrite;
    end;
  end;
end;

function TMemFileCache.TryLoadFileFromCache(btm: Tobject;
  APath: string): boolean;
var
  i: integer;
  VPath: string;
begin
  Result := false;
  VPath := AnsiUpperCase(APath);
  FSync.BeginRead;
  try
    i:=FCacheList.IndexOf(VPath);
    if i>=0 then begin
      TBitmap32(btm).Assign(TBitmap32(FCacheList.Objects[i]));
      result:=true;
    end;
  finally
    FSync.EndRead;
  end;
end;

end.
