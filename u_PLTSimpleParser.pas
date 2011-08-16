unit u_PLTSimpleParser;

interface

uses
  Classes,
  SysUtils,
  t_GeoTypes,
  i_VectorDataLoader,
  i_HtmlToHintTextConverter,
  i_InternalPerformanceCounter,
  i_VectorDataItemSimple;

type
  TPLTSimpleParser = class(TInterfacedObject, IVectorDataLoader)
  private
    FLoadStreamCounter: IInternalPerformanceCounter;
    FHintConverter: IHtmlToHintTextConverter;
    procedure ParseStringList(AStringList: TStringList; out APoints: TArrayOfDoublePoint);
    function GetWord(Str, Smb: string; WordNmbr: Byte): string;
    function GetRect(APoints: TArrayOfDoublePoint): TDoubleRect;
  protected
    procedure LoadFromStream(AStream: TStream; out AItems: IVectorDataItemList); virtual;
    procedure LoadFromFile(FileName:string; out AItems: IVectorDataItemList);
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      APerfCounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  Math,
  u_VectorDataItemList,
  u_VectorDataItemPolygon,
  u_GeoFun,
  u_GeoToStr;

constructor TPLTSimpleParser.Create(AHintConverter: IHtmlToHintTextConverter;
  APerfCounterList: IInternalPerformanceCounterList);
begin
  FHintConverter := AHintConverter;
  FLoadStreamCounter := APerfCounterList.CreateAndAddNewCounter('LoadPltStream');
end;

procedure TPLTSimpleParser.LoadFromStream(AStream: TStream;
  out AItems: IVectorDataItemList);
var
  pltstr: TStringList;
  trackname: string;
  VList: IInterfaceList;
  VPoints: TArrayOfDoublePoint;
  VItem: IVectorDataItemSimple;
begin
  AItems := nil;
  pltstr:=TStringList.Create;
  try
    pltstr.LoadFromStream(AStream);
    if pltstr.Count > 7 then begin
      ParseStringList(pltstr, VPoints);
      if Length(VPoints) > 0 then begin
        trackname:=GetWord(pltstr[4], ',', 4);
        VItem :=
          TVectorDataItemPath.Create(
            FHintConverter,
            trackname,
            '',
            VPoints,
            GetRect(VPoints)
          );
        VList := TInterfaceList.Create;
        VList.Add(VItem);
        AItems := TVectorDataItemList.Create(VList);
      end;
    end;
  finally
    pltstr.Free;
  end;
end;

procedure TPLTSimpleParser.ParseStringList(
  AStringList: TStringList;
  out APoints: TArrayOfDoublePoint
);
var
  i,j:integer;
  str: string;
  VPoint: TDoublePoint;
  VValidPoint: Boolean;
begin
  APoints := nil;
  for i:=6 to AStringList.Count-1 do begin
    try
      j:=1;
      str:=AStringList[i];
      while j<length(str) do begin
        if str[j]=' ' then begin
          delete(str,j,1);
        end else begin
          inc(j);
        end;
      end;
      if (GetWord(AStringList[i], ',', 3)='1') and (i>6) then begin
        VPoint := DoublePoint(NAN, NAN);
        SetLength(APoints,length(APoints)+1);
        APoints[length(APoints)-1] := VPoint;
      end;
      VValidPoint := True;
      try
        VPoint.y := str2r(GetWord(str, ',', 1));
        VPoint.x := str2r(GetWord(str, ',', 2));
      except
        VValidPoint := False;
      end;
      if VValidPoint then begin
        SetLength(APoints,length(APoints)+1);
        APoints[length(APoints)-1] := VPoint;
      end;
    except
    end;
  end;
end;

procedure TPLTSimpleParser.LoadFromFile(FileName:string; out AItems: IVectorDataItemList);
var
  pltstr: TStringList;
  trackname: string;
  VList: IInterfaceList;
  VPoints: TArrayOfDoublePoint;
  VItem: IVectorDataItemSimple;
begin
  AItems := nil;
  if FileExists(FileName) then begin
    pltstr:=TStringList.Create;
    try
      pltstr.LoadFromFile(FileName);
      if pltstr.Count > 7 then begin
        ParseStringList(pltstr, VPoints);
        if Length(VPoints) > 0 then begin
          trackname:=ChangeFileExt(ExtractFileName(FileName), '');
          VItem :=
            TVectorDataItemPath.Create(
              FHintConverter,
              trackname,
              '',
              VPoints,
              GetRect(VPoints)
            );
          VList := TInterfaceList.Create;
          VList.Add(VItem);
          AItems := TVectorDataItemList.Create(VList);
        end;
      end;
    finally
      pltstr.Free;
    end;
  end;
end;

function TPLTSimpleParser.GetRect(APoints: TArrayOfDoublePoint): TDoubleRect;
var
  i: Integer;
begin
  Result.TopLeft := APoints[0];
  Result.BottomRight := APoints[0];
  for i := 0 to length(APoints) - 1 do begin
    if Result.Left > APoints[i].X then begin
      Result.Left := APoints[i].X;
    end;
    if Result.Right < APoints[i].X then begin
      Result.Right := APoints[i].X;
    end;
    if Result.Top < APoints[i].y then begin
      Result.Top := APoints[i].y;
    end;
    if Result.Bottom > APoints[i].y then begin
      Result.Bottom := APoints[i].y;
    end;
  end;
end;

function TPLTSimpleParser.GetWord(Str, Smb: string; WordNmbr: Byte): string;
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

end.
